
module Database.LSM.Core where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified BTree as BT
import Data.Int (Int64)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, renameFile, removeFile)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader (runReaderT, asks, ask)
import Control.Monad.State (runStateT, modify, gets)
import System.FilePath ((</>))

import Database.LSM.Utils
import Database.LSM.Types
import Database.LSM.Recovery
import qualified Database.LSM.MemTable as MT

-- default options
def :: DBOptions
def = DBOptions "mydb" True True 10 1000 twoMB False
        where twoMB = 2 * 1024 * 1024

-- unwrap
runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: DBOptions -> LSM a -> IO a
withLSM opts action = do
    -- create directory and throw error if necessary
    let dir = dbName opts
    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist) (throwIOAlreadyExists dir)
    when (createIfMissing opts) (createDirectoryIfMissing False dir)

    -- create the LOCK file and begin do runLSM
    createFileIfMissing (fileNameLock dir)
    res <- withFileLock (fileNameLock dir) Exclusive
        (\_ -> do
            mvar <- newEmptyMVar
            let st = DBState MT.new MT.new 0 mvar False
            runLSM opts st (openLSM >> action >>= closeLSM)
        )
    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    dir <- asks dbName
    let currentFile = fileNameCurrent dir
    version <- io $ readFile currentFile
    lsmLog ("Loading existing LSM, version: " ++  version ++ ".")
    when (null version) (io $ throwIOFileEmpty (currentFile </> version))

    performRecovery (fileNameIMemtableLog dir)
    performRecovery (fileNameMemtableLog dir)
    lsmLog "Loading Completed."

-- This function will throw an exception if anything goes wrong with the recovery process.
-- Most likely this is due to corrupted log files.
-- The user must manually delete the log files if they wish to open the database again.
-- TODO a better way is supply a database option, e.g. errorOnFailedRecovery
performRecovery :: FilePath -> LSM ()
performRecovery logfile = do
    exist <- io $ doesFileExist logfile
    when exist $ do
            epairs <- io $ readPairsFromFile logfile
            case epairs of
                Left  m     -> io $ throwIORecoveryFailure m
                Right pairs -> recover pairs
    where recover pairs = do
            _ <- lsmMapToTree (Map.fromList pairs) >>= lsmMergeToDisk
            io $ removeFile logfile

initLSM :: LSM ()
initLSM = do
    lsmLog "Initialising LSM."
    opts <- ask
    let dir = dbName opts
    let currentFile = fileNameCurrent dir
    version <- io randomVersion
    lsmLog ("Initial version is: " ++ version)
    io $ BT.fromOrderedToFile
            (btreeOrder opts)
            (btreeSize opts)
            (dir </> version)
            (mapToProducer MT.new)
    io $ createFile currentFile
    writeVersion version
    lsmLog "Initialisation completed."
    -- TODO write checksum of db

openLSM :: LSM ()
openLSM = do
    currFile <- fileNameCurrent <$> asks dbName
    currExists <- io $ doesFileExist currFile
    if currExists then loadLSM else initLSM

closeLSM :: a -> LSM a
closeLSM a = do
    lsmLog "Closing LSM."
    syncToDisk
    return a

get :: Bs -> LSM (Maybe Bs)
get k = do
    updateVersionNoBlock
    mv1 <- Map.lookup k <$> gets dbMemTable
    mv2 <- Map.lookup k <$> gets dbIMemTable
    mv3 <- nameAndVersion
            >>= io . openTree
            >>= (\t -> return $ BT.lookup t k)
    return $ checkForEmpty (mv1 `firstJust` mv2 `firstJust` mv3)
    where checkForEmpty (Just v) = if B.null v
                                   then Nothing
                                   else Just v
          checkForEmpty Nothing = Nothing

add :: Bs -> Bs -> LSM ()
add k v = do
    checkKeyValuePair k v
    addWithoutNullCheck k v

addWithoutNullCheck :: Bs -> Bs -> LSM ()
addWithoutNullCheck k v = do
    lsmLog ("LSM addition where k = "
                    ++ show (B.unpack k) ++ " and v = "
                    ++ show (B.unpack v) ++ ".")
    updateVersionNoBlock
    currMemTable <- gets dbMemTable
    currMemTableSize <- gets memTableSize
    let newSize = computeNewSize currMemTable k v currMemTableSize
    lsmLog ("New size: " ++ show newSize)

    memtableLog <- fileNameMemtableLog <$> asks dbName
    io $ appendPairToFile memtableLog (k, v)
    modify (\s -> s
                { dbMemTable = Map.insert k v currMemTable
                , memTableSize = newSize
                })

    threshold <- asks memtableThreshold
    asyncWriteToDisk newSize threshold
    lsmLog "LSM addition completed."

asyncWriteToDisk :: Int64 -> Int64 -> LSM ()
asyncWriteToDisk sz t = when (sz > t) $ do
    updateVersionBlock -- wait for async process to finish before starting a new one
    lsmLog ("Threshold reached (" ++ show sz ++ " > " ++ show t ++ ").")

    dir <- asks dbName
    io $ renameFile (fileNameMemtableLog dir) (fileNameIMemtableLog dir)

    oldMemTable <- gets dbMemTable
    modify (\s -> s { dbIMemTable = oldMemTable
                    , dbMemTable = MT.new
                    , memTableSize = 0 })

    -- TODO this is a bit ugly, can't use lsmMergeToDisk inside forkIO
    order <- asks btreeOrder
    oldVer <- readVersion
    newVer <- io randomVersion
    tree <- gets dbIMemTable >>= lsmMapToTree
    mvar <- gets dbMVar
    modify (\s -> s { dbAsyncRunning = True })
    lsmLog ("Background merging started, from " ++ oldVer ++ " to " ++ newVer ++ ".")
    _ <- io $ forkIO (mergeToDisk order dir oldVer newVer tree >>= putMVar mvar)
    return ()

update :: Bs -> Bs -> LSM ()
update k v = do
    exist <- doesKeyExist k
    when exist $ add k v

delete :: Bs -> LSM ()
delete k = do
    exist <- doesKeyExist k
    when exist $ addWithoutNullCheck k (C.pack "")

doesKeyExist :: Bs -> LSM Bool
doesKeyExist k =
    get k >>=
    \x -> case x of
        Just _  -> return True
        Nothing -> return False

-- NOTE: mergeToDisk does not update the version number!
-- If merging in the foreground, use lsmMergeToDisk instead,
-- otherwise remember to update the version number upon completion.
mergeToDisk :: BT.Order -> FilePath -> String -> String -> BT.LookupTree Bs Bs -> IO String
mergeToDisk order path oldVer newVer newTree = do
    let oldPath = path </> oldVer
    let newPath = path </> newVer
    oldTree <- openTree oldPath
    merge order newPath oldTree newTree
    return newVer

updateVersionNoBlock :: LSM ()
updateVersionNoBlock = do
    lsmLog "Updating version - non blocking."
    mvar <- gets dbMVar
    res <- io $ tryTakeMVar mvar
    case res of
        Nothing -> return ()
        Just v  -> writeVersion v >> modify (\s -> s { dbAsyncRunning = False })

updateVersionBlock :: LSM ()
updateVersionBlock = do
    lsmLog "Updating version - blocking."
    running <- gets dbAsyncRunning
    when running $ do
        mvar <- gets dbMVar
        v <- io $ takeMVar mvar -- blocks
        writeVersion v
        modify (\s -> s { dbAsyncRunning = False })

syncToDisk :: LSM ()
syncToDisk = do
    lsmLog "Syncing to disk."
    updateVersionBlock -- immutable table should be redundant after version update
    tree <- gets dbMemTable >>= lsmMapToTree
    _ <- lsmMergeToDisk tree

    -- no need of log files after memtables are all written to btree
    fileNameIMemtableLog <$> asks dbName >>= io . removeFileIfExist
    fileNameMemtableLog <$> asks dbName >>= io . removeFileIfExist

computeNewSize :: MemTable -> Bs -> Bs -> Int64 -> Int64
computeNewSize memTable k v oldSize = oldSize + entrySize - correction k (MT.lookup k memTable)
    where correction _ Nothing = 0
          correction x (Just y) = B.length x + B.length y
          entrySize = fromIntegral (B.length k + B.length v)

lsmMergeToDisk :: BT.LookupTree Bs Bs -> LSM FilePath
lsmMergeToDisk tree = do
    order <- asks btreeOrder
    dir <- asks dbName
    oldVer <- readVersion
    newVer <- io randomVersion
    res <- io $ mergeToDisk order dir oldVer newVer tree
    writeVersion newVer
    return res

lsmMapToTree :: ImmutableTable -> LSM (BT.LookupTree Bs Bs)
lsmMapToTree table = do
    order <- asks btreeOrder
    size <- asks btreeSize
    io $ mapToTree order size table

checkKeyValuePair :: Bs -> Bs -> LSM ()
checkKeyValuePair k v = do
    when (B.null k) (io $ throwIOBadKey "")
    when (B.null v) (io $ throwIOBadValue "")
 
