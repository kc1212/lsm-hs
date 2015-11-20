{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.LevelDB.Core where

import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FileLock (withFileLock, SharedExclusive(..))
import Control.Monad
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (liftIO, MonadIO, MonadState, StateT, runStateT)
import Control.Exception (throwIO)

import Database.LevelDB.Utils
import qualified Database.LevelDB.MemTable as MT

newtype LevelDB a = LevelDB (ReaderT DBOptions (StateT DBState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState DBState, MonadReader DBOptions)

instance Applicative LevelDB where
    pure = return
    (<*>) = ap

instance (Monoid a) => Monoid (LevelDB a) where
    mempty  = return mempty
    mappend = liftM2 mappend

data DBState = DBState
    { dbFilePath        :: FilePath
    , dbMemTable        :: MT.MemTable
    , dbTableCache      :: Int -- dummy
    , dbLock            :: Int -- dummy
    , dbVersionSet      :: Int -- dummy
    -- and other properties
    }

data DBOptions = DBOptions
    { createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , paranoidChecks    :: Bool
    -- add more options here
    } deriving (Show)

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

            -- TODO create version and then recover/create (big part)
            -- this is dummy state as the state is not fully implemented yet
            let st = DBState dir memtable 1 2 3
            runLevelDB opts st action
        )

    return $ fst res

get :: MT.LookupKey -> LevelDB (Maybe Bs)
get = undefined

add :: MT.LookupKey -> Bs -> LevelDB ()
add = undefined


