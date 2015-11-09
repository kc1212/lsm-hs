
module Database.LevelDB.Core where

import System.IO
import System.Directory
import qualified System.FilePath as FP
import Control.Monad

data DB = DB
    { filepath  :: FilePath
    , manifestHandle :: Handle
    -- and other properties
    }

data Options = Options
    { comparator        :: Comparator
    , createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , paranoidChecks    :: Bool
    -- add more options here
    } deriving (Show)

data Comparator = A | B deriving (Show) -- dummy comparator

withLevelDB :: FilePath -> Options -> (DB -> IO a) -> IO a
withLevelDB dir opts action = do
    -- createDirectory should fail with isAlreadyExistsError if dir does not exit
    when (errorIfExists opts) (createDirectory dir)
    when (createIfMissing opts) (createDirectoryIfMissing False dir)
    manifest <- openFile (FP.combine dir "MANIFEST") ReadWriteMode
    result <- action (DB dir manifest)
    hClose manifest
    return result


