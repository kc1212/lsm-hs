
module Database.LevelDB.Core where

import System.IO
import System.Directory
import qualified System.FilePath as FP
import Control.Monad
import Control.Exception
import Control.Monad.State

import Database.LevelDB.Utils
import qualified Database.LevelDB.MemTable as MT

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

withLevelDB :: FilePath -> Options -> StateT DB IO () -> IO ()
withLevelDB dir opts action = do
    dirExist <- doesDirectoryExist dir
    when (errorIfExists opts && dirExist)
         (throwIO $ userError (dir ++ " already exists"))
    when (createIfMissing opts)
         (createDirectoryIfMissing False dir)

    manifest <- openFile (FP.combine dir "MANIFEST") ReadWriteMode
    result <- runStateT action (DB dir manifest)
    -- TODO check the results here?
    hClose manifest


get :: MT.LookupKey -> StateT DB IO (Maybe Bs)
get = undefined


add :: MT.LookupKey -> Bs -> StateT DB IO ()
add = undefined


