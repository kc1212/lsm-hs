
module Database.LevelDB.Core where

import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (liftIO, runStateT)
import Control.Exception (throwIO)

import Database.LevelDB.Utils
import Database.LevelDB.Version
import Database.LevelDB.Types
import qualified Database.LevelDB.MemTable as MT

runLevelDB :: DBOptions -> DBState -> LevelDB a -> IO (a, DBState)
runLevelDB ops st (LevelDB a) = runStateT (runReaderT a ops) st

withLevelDB :: FilePath -> DBOptions -> LevelDB a -> IO a
withLevelDB dir opts action = do
    -- TODO check compression
    -- TODO use custom comparator

    let memtable = MT.new
    -- TODO start compaction thread
    -- TODO create TableCache

    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist)
         (throwIO $ userError (dir ++ " already exists"))

    when (createIfMissing opts)
         (createDirectoryIfMissing False dir)

    -- create a LOCK file and perform actions while the lock is being held
    -- withFileLock blocks until the lock is available
    createFileIfMissing (fileNameLock dir)
    res <- liftIO $ withFileLock (fileNameLock dir) Exclusive
        (\_ -> do
            when (createIfMissing opts)
                 (createFileIfMissing (fileNameCurrent dir))

            -- below we use a dummy state as the state is not fully implemented yet
            runLevelDB opts (DBState dir memtable 1 2 3)
                (initOrRecoverVersion >> action)
        )

    return $ fst res

get :: Bs -> LevelDB (Maybe Bs)
get = undefined

add :: Bs -> Bs -> LevelDB ()
add = undefined


