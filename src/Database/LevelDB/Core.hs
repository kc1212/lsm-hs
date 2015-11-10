
module Database.LevelDB.Core where

import System.IO
import System.Directory
import qualified System.FilePath as FP
import Control.Monad
import Control.Exception
import qualified Data.ByteString as BS

type Bs = BS.ByteString

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
    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist)
         (throwIO $ userError (dir ++ " already exists"))
    when (createIfMissing opts)
         (createDirectoryIfMissing False dir)

    manifest <- openFile (FP.combine dir "MANIFEST") ReadWriteMode
    result <- action (DB dir manifest)
    hClose manifest
    return result


