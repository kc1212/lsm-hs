{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.LSM.Types where

import Control.Monad
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadIO, MonadState, StateT)
import qualified BTree as BT

import qualified Database.LSM.MemTable as MT

newtype LSM a = LSM (ReaderT DBOptions (StateT DBState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState DBState, MonadReader DBOptions)

instance Applicative LSM where
    pure = return
    (<*>) = ap

instance (Monoid a) => Monoid (LSM a) where
    mempty  = return mempty
    mappend = liftM2 mappend

data DBState = DBState
    { dbMemTable        :: MT.MemTable
    , dbIMemTable       :: MT.ImmutableTable
    , memTableSize      :: Int
    , currentVersion    :: String
    -- and other properties
    }

data DBOptions = DBOptions
    { dbName            :: String
    , createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , bTreeOrder        :: BT.Order
    , bTreeSize         :: BT.Size
    } deriving (Show)


type InternalKey = Int -- dummy

