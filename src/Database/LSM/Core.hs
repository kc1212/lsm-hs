
module Database.LSM.Core where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified BTree as BT
import Data.Maybe (fromJust)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FileLock (lockFile, unlockFile, SharedExclusive(..))
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

-- default state
defState :: DBState
defState = DBState MT.new MT.new 0 "0" Nothing

runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: DBOptions -> LSM a -> IO a
withLSM opts action = do
    res <- runLSM opts defState (openLSM >> action >>= closeLSM)
    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    currentFile <- fileNameCurrent <$> asks dbName
    version <- io $ readFile currentFile
    if null version
        then io $ throwIOFileEmpty (currentFile </> version)
        else modify (\s -> s {currentVersion = version})

initLSM :: LSM ()
initLSM = do
    opts <- ask
    let dir = dbName opts
    version <- io $ (++ extension) <$> randomVersion
    let currentFile = fileNameCurrent dir
    io $ BT.fromOrderedToFile
            (btreeOrder opts)
            (btreeSize opts)
            (dir </> version)
            (mapToProducer MT.new)
    io $ createFile currentFile
    io $ writeFile currentFile version
    modify (\s -> s {currentVersion = version})
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
    -- this should be evaluated lazily right?
    mv1 <- Map.lookup k <$> gets dbMemTable
    mv2 <- Map.lookup k <$> gets dbIMemTable
    mv3 <- nameAndVersion >>= io . BT.open
            >>= (\t -> return $ BT.lookup (fromRight t) k)
    return $ mv1 `firstJust` mv2 `firstJust` mv3

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust p q =
    case p of
        Just _  -> p
        _       -> q

add :: Bs -> Bs -> LSM ()
add k v = do
    let entrySize = fromIntegral (BS.length k + BS.length v)
    threshold <- asks memtableThreshold
    newSize <- fmap (entrySize +) (gets memTableSize)
    if newSize < threshold
    then do
        currMemTable <- gets dbMemTable
        modify (\s -> s
                    { dbMemTable = Map.insert k v currMemTable
                    , memTableSize = newSize
                    })
    else do
        error "Write to BTree is unimplemented."


