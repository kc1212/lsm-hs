
module Database.LSM.Core where

import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Monad
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State (runStateT, modify)
import Control.Exception (throwIO)
import System.FilePath ((</>))

import Database.LSM.Utils
import Database.LSM.Types
import qualified Database.LSM.MemTable as MT

runLSM :: DBOptions -> DBState -> LSM a -> IO (a, DBState)
runLSM ops st (LSM a) = runStateT (runReaderT a ops) st

withLSM :: FilePath -> DBOptions -> LSM a -> IO a
withLSM dir opts action = do

    -- memTable always start in empty state
    let memTable        = MT.new
    let immutableTable  = MT.new

    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist)
         (throwIO $ userError (dir ++ " already exists"))

    when (createIfMissing opts)
         (createDirectoryIfMissing False dir)

    -- create a LOCK file and perform actions while the lock is being held
    -- withFileLock blocks until the lock is available
    createFileIfMissing (fileNameLock dir)
    res <- io $ withFileLock (fileNameLock dir) Exclusive
        (\_ -> do
            when (createIfMissing opts)
                 (createFileIfMissing (fileNameCurrent dir))

            -- below we use a dummy state as the state is not fully implemented yet
            runLSM opts (DBState memTable immutableTable 0 "")
                (openLSM >> action)
        )

    return $ fst res

loadLSM :: LSM ()
loadLSM = do
    dir <- asks dbName
    currDB <- io $ readFile (fileNameCurrent dir)
    dbExist <- io $ doesFileExist (dir </> currDB)
    if dbExist
        then modify (\s -> s {currentVersion = currDB})
        else io $ throwIODoesNotExist currDB

initLSM :: LSM ()
initLSM = undefined
-- create db file
-- create current file
-- write current file
-- write checksum of db

openLSM :: LSM ()
openLSM = do
    dir <- asks dbName
    currExist <- io $ doesFileExist (fileNameCurrent dir)
    if currExist then loadLSM else initLSM

get :: Bs -> LSM (Maybe Bs)
get = undefined

add :: Bs -> Bs -> LSM ()
add = undefined


