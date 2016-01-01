
module Database.LSM.Core where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified BTree as BT
import Data.Maybe (fromJust)
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
def = DBOptions "mydb" True False 10 1000 twoMB
        where twoMB = 2 * 1024 * 1024

runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: DBOptions -> LSM a -> IO a
withLSM opts action = do
    mvar <- newEmptyMVar
    let st = DBState MT.new MT.new 0 Nothing mvar
    res <- runLSM opts st (openLSM >> action >>= closeLSM)
    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    currentFile <- fileNameCurrent <$> asks dbName
    version <- io $ readFile currentFile
    when (null version) (io $ throwIOFileEmpty (currentFile </> version))

initLSM :: LSM ()
initLSM = do
    opts <- ask
    let dir = dbName opts
    let currentFile = fileNameCurrent dir
    version <- io randomVersion
    io $ BT.fromOrderedToFile
            (btreeOrder opts)
            (btreeSize opts)
            (dir </> version)
            (mapToProducer MT.new)
    io $ createFile currentFile
    io $ writeFile currentFile version
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
    -- the lock must be present
    flock <- fromJust <$> gets dbFileLock
    io $ unlockFile flock
    return a
    -- TODO sync memtable with btree

get :: Bs -> LSM (Maybe Bs)
get k = do
    checkAsync
    mv1 <- Map.lookup k <$> gets dbMemTable
    mv2 <- Map.lookup k <$> gets dbIMemTable
    mv3 <- nameAndVersion
            >>= io . openTree
            >>= (\t -> return $ BT.lookup t k)
    return $ mv1 `firstJust` mv2 `firstJust` mv3

add :: Bs -> Bs -> LSM ()
add k v = do
    checkAsync
    let entrySize = fromIntegral (BS.length k + BS.length v)
    newSize <- fmap (entrySize +) (gets memTableSize)

    currMemTable <- gets dbMemTable
    modify (\s -> s
                { dbMemTable = Map.insert k v currMemTable
                , memTableSize = newSize
                })

    threshold <- asks memtableThreshold
    when (newSize > threshold) $ do
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
        _ <- io $ forkIO (mergeToDisk order name oldVer newVer tree >>= putMVar mvar)
        return ()

mergeToDisk :: BT.Order -> FilePath -> String -> String -> BT.LookupTree Bs Bs -> IO String
mergeToDisk order path oldVer newVer newTree = do
    let oldPath = path </> oldVer
    let newPath = path </> newVer
    oldTree <- openTree oldPath
    merge order newPath oldTree newTree
    return newVer

-- if async action finished, update the verion
checkAsync :: LSM ()
checkAsync = do
    mvar <- gets dbMVar
    mvarExist <- io $ isEmptyMVar mvar
    unless mvarExist $ do
        ver <- io $ takeMVar mvar
        writeVersion ver


