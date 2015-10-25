
module Database.LevelDB.Core where

import Control.Monad.Reader

data DB = DB
    { filepath  :: FilePath
    , options   :: Options
    }

data Options = Options
    { comparator        :: Comparator
    , createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , paranoidChecks    :: Bool
    -- add more options here
    } deriving (Show)

data Comparator = A | B deriving (Show) -- dummy comparator

open :: MonadIO m => FilePath -> Options -> m DB
open = undefined



