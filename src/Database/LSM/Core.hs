
module Database.LSM.Core where

import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Monad
import Control.Monad.Reader (runReaderT, asks, ask)
import Control.Monad.State (runStateT, modify)
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
get = undefined

add :: Bs -> Bs -> LSM ()
add = undefined


