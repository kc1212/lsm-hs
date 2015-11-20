{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.LevelDB.Types where

import Control.Monad
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadIO, MonadState, StateT)

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


type InternalKey = Int -- dummy

