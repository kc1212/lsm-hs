
module Database.LSM.Core where

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Int (Int64)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Monad
import Control.Monad.Reader (runReaderT, asks, ask)
import Control.Monad.State (runStateT, modify, gets)
import System.FilePath ((</>))

import Database.LSM.Utils
import Database.LSM.Types
import qualified Database.LSM.MemTable as MT

runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: DBOptions -> LSM a -> IO a
withLSM opts action = do

    -- memTable always start in empty state
    let memTable        = MT.new
    let immutableTable  = MT.new
    let dir             = dbName opts

    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist)
         (throwIOAlreadyExists dir)

    when (createIfMissing opts)
         (createDirectoryIfMissing False dir)

    -- create a LOCK file and perform actions while the lock is being held
    -- withFileLock blocks until the lock is available
    createFileIfMissing (fileNameLock dir)
    res <- io $ withFileLock (fileNameLock dir) Exclusive
        -- below we use a dummy state as the state is not fully implemented yet
        (\_ -> runLSM opts (DBState memTable immutableTable 0 "")
                (openLSM >> action))

    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    currentFile <- fileNameCurrent <$> asks dbName
    currentDB <- io $ readFile currentFile
    if null currentDB
        then io $ throwIOFileEmpty (currentFile </> currentDB)
        else modify (\s -> s {currentVersion = currentDB})

initLSM :: LSM ()
initLSM = do
    -- TODO create db file
    opts <- ask
    let dir = dbName opts
    version <- io $ (++ extension) <$> randomVersion
    let currentFile = fileNameCurrent dir
    io $ when (createIfMissing opts)
         (createFileIfMissing currentFile)
    io $ writeFile currentFile version
    modify (\s -> s {currentVersion = version})
    -- write checksum of db (in the future)

openLSM :: LSM ()
openLSM = do
    dir <- asks dbName
    exists <- io $ doesFileExist (fileNameCurrent dir)
    if exists then loadLSM else initLSM

get :: Bs -> LSM (Maybe Bs)
get k = do
    mv1 <- Map.lookup k <$> gets dbMemTable
    mv2 <- Map.lookup k <$> gets dbIMemTable
    let mv3 = Nothing -- TODO read from file
    return $ mv1 `firstJust` mv2 `firstJust` mv3

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust p q =
    case p of
        Just _  -> p
        _       -> q

-- TODO move threshold to reader monad
threshold :: Int64
threshold = 2 * 1024 * 1024

add :: Bs -> Bs -> LSM ()
add k v = do
    let entrySize = fromIntegral (BS.length k + BS.length v)
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


