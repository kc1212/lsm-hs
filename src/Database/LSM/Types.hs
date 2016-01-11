{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.LSM.Types where

import qualified BTree as BT
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Int (Int64)
import Control.Concurrent.MVar (MVar)
import Control.Monad
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadIO, MonadState, StateT)

newtype LSM a = LSM (ReaderT DBOptions (StateT DBState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState DBState, MonadReader DBOptions)

instance Applicative LSM where
    pure = return
    (<*>) = ap

instance (Monoid a) => Monoid (LSM a) where
    mempty  = return mempty
    mappend = liftM2 mappend

data DBState = DBState
    { dbMemTable        :: MemTable
    , dbIMemTable       :: ImmutableTable
    , memTableSize      :: Int64
    , dbMVar            :: MVar String
    , dbAsyncRunning    :: Bool
    }

data DBOptions = DBOptions
    { dbName            :: String
    , createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , btreeOrder        :: BT.Order
    , btreeSize         :: BT.Size
    , memtableThreshold :: Int64
    , debugLog          :: Bool
    } deriving (Show)

type MemTable = Map.Map Bs Bs
type ImmutableTable = MemTable

type InternalKey = Int -- dummy

type Bs = B.ByteString

