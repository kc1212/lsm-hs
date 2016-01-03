
module Database.LSM.Core where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified BTree as BT
import Data.Maybe (fromJust)
import Data.Int (Int64)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FileLock (lockFile, unlockFile, SharedExclusive(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader (runReaderT, asks, ask)
import Control.Monad.State (runStateT, modify, gets)
import System.FilePath ((</>))

import Database.LSM.Utils
import Database.LSM.Types
import qualified Database.LSM.MemTable as MT

-- default options
def :: DBOptions
def = DBOptions "mydb" True True 10 1000 twoMB
        where twoMB = 2 * 1024 * 1024

runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: DBOptions -> LSM a -> IO a
withLSM opts action = do
    mvar <- newEmptyMVar
    let st = DBState MT.new MT.new 0 Nothing mvar False
    res <- runLSM opts st (openLSM >> action >>= closeLSM)
    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    currentFile <- fileNameCurrent <$> asks dbName
    version <- io $ readFile currentFile
    io $ logStdErr ("Loading existing LSM, version: " ++  version ++ ".")
    when (null version) (io $ throwIOFileEmpty (currentFile </> version))
    io $ logStdErr "Loading Completed."

initLSM :: LSM ()
initLSM = do
    io $ logStdErr "Initialising LSM."
    opts <- ask
    let dir = dbName opts
    let currentFile = fileNameCurrent dir
    version <- io randomVersion
    io $ logStdErr ("Initial version is: " ++ version)
    io $ BT.fromOrderedToFile
            (btreeOrder opts)
            (btreeSize opts)
            (dir </> version)
            (mapToProducer MT.new)
    io $ createFile currentFile
    writeVersion version
    io $ logStdErr "Initialisation completed."
    -- TODO write checksum of db

openLSM :: LSM ()
openLSM = do
    opts <- ask
    let dir = dbName opts

    dirExist <- io $ doesDirectoryExist dir
    io $ when (errorIfExists opts && dirExist)
                (throwIOAlreadyExists dir)

    io $ when (createIfMissing opts)
                (createDirectoryIfMissing False dir)

    -- create LOCK file, note lockFile blocks until the lock is available
    io $ createFileIfMissing (fileNameLock dir)
    flock <- io $ lockFile (fileNameLock dir) Exclusive
    modify (\s -> s { dbFileLock = Just flock })

    currExists <- io $ doesFileExist (fileNameCurrent dir)
    if currExists then loadLSM else initLSM

closeLSM :: a -> LSM a
closeLSM a = do
    io $ logStdErr "Closing LSM."
    syncToDisk                              -- sync memtable with btree
    flock <- fromJust <$> gets dbFileLock   -- the lock must be present
    io $ unlockFile flock
    return a

get :: Bs -> LSM (Maybe Bs)
get k = do
    updateVersionNoBlock
    mv1 <- Map.lookup k <$> gets dbMemTable
    mv2 <- Map.lookup k <$> gets dbIMemTable
    mv3 <- nameAndVersion
            >>= io . openTree
            >>= (\t -> return $ BT.lookup t k)
    return $ mv1 `firstJust` mv2 `firstJust` mv3

add :: Bs -> Bs -> LSM ()
add k v = do
    io $ logStdErr ("LSM addition where k = " ++ show (BS.unpack k) ++ " and v = " ++ show (BS.unpack v) ++ ".")
    updateVersionNoBlock
    let entrySize = fromIntegral (BS.length k + BS.length v)
    newSize <- fmap (entrySize +) (gets memTableSize)
    io $ logStdErr ("New size: " ++ show newSize)
    currMemTable <- gets dbMemTable
    modify (\s -> s
                { dbMemTable = Map.insert k v currMemTable
                , memTableSize = newSize
                })

    threshold <- asks memtableThreshold
    writeToIMem newSize threshold
    io $ logStdErr ("LMS addition completed.")
    
writeToIMem :: Int64 -> Int64 -> LSM ()
writeToIMem sz t = when (sz > t) $ do 
    syncToDisk -- wait for async process to finish before starting a new one
    io $ logStdErr ("Threshold reached (" ++ show sz ++ " > " ++ show t ++ ").")
    oldMemTable <- gets dbMemTable
    modify (\s -> s { dbIMemTable = oldMemTable
                    , dbMemTable = MT.new
                    , memTableSize = 0 })
    order <- asks btreeOrder
    size <- asks btreeSize
    name <- asks dbName
    oldVer <- readVersion
    newVer <- io randomVersion
    tree <- gets dbIMemTable >>= io . mapToTree order size
    mvar <- gets dbMVar
    modify (\s -> s { dbAsyncRunning = True })
    _ <- io $ forkIO (mergeToDisk order name oldVer newVer tree >>= putMVar mvar)
    return ()

mergeToDisk :: BT.Order -> FilePath -> String -> String -> BT.LookupTree Bs Bs -> IO String
mergeToDisk order path oldVer newVer newTree = do
    let oldPath = path </> oldVer
    let newPath = path </> newVer
    oldTree <- openTree oldPath
    io $ logStdErr ("Merging started, from " ++ oldPath ++ " to " ++ newPath ++ ".")
    merge order newPath oldTree newTree
    io $ logStdErr "Merging finished."
    return newVer

updateVersionNoBlock :: LSM ()
updateVersionNoBlock = do
    io $ logStdErr "Updating version - non blocking."
    mvar <- gets dbMVar
    res <- io $ tryTakeMVar mvar
    emptyMVar <- io $ newEmptyMVar
    case res of
        Nothing -> return ()
        Just v  -> writeVersion v >> modify (\s -> s 
            { dbAsyncRunning = True
            , dbMVar = emptyMVar 
            })

updateVersionBlock :: LSM ()
updateVersionBlock = do
    io $ logStdErr "Updating version - blocking."
    running <- gets dbAsyncRunning
    when running $ do
        mvar <- gets dbMVar
        v <- io $ takeMVar mvar -- blocks
        writeVersion v
        modify (\s -> s { dbAsyncRunning = False })

syncToDisk :: LSM ()
syncToDisk = do
    io $ logStdErr "Syncing to disk."
    updateVersionBlock
    -- immutable table should be redundant after version update
    order <- asks btreeOrder
    size <- asks btreeSize
    name <- asks dbName
    oldVer <- readVersion
    newVer <- io randomVersion
    io $ logStdErr ("Syncthing to disk, new version: " ++ newVer)
    tree <- gets dbMemTable >>= io . mapToTree order size
    ver <- io $ mergeToDisk order name oldVer newVer tree
    writeVersion ver


