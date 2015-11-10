
module Database.LevelDB.Core where

import System.IO
import Control.Monad.Reader
import System.FileLock

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

-- lock tryLockFile "LOCK" Exclusive
lock :: Maybe FileLock -> IO ()
lock (Just l)  = unlockFile l
lock Nothing = error "Database is used by another process"

operation :: IO () 
operation = putStrLn "Operation"

