{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.LSM.Types where

import qualified BTree as BT
import qualified Data.ByteString as BS
import qualified Data.Map as Map
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

type MemTable = Map.Map Bs Bs
type ImmutableTable = MemTable

type InternalKey = Int -- dummy

type Bs = BS.ByteString
