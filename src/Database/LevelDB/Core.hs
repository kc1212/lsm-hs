
module Database.LevelDB.Core where

import System.Directory
import qualified System.FilePath as FP
import Control.Monad
import Control.Exception
import Control.Monad.State

import Database.LevelDB.Utils
import qualified Database.LevelDB.MemTable as MT

data DB = DB
    { dbFilePath        :: FilePath
    , dbMemTable        :: MT.MemTable
    , dbTableCache      :: Int -- dummy
    , dbLock            :: Int -- dummy
    , dbVersionSet      :: Int -- dummy
    -- and other properties
    }

data Options = Options
    { createIfMissing   :: Bool
    , errorIfExists     :: Bool
    , paranoidChecks    :: Bool
    -- add more options here
    } deriving (Show)

withLevelDB :: FilePath -> Options -> StateT DB IO () -> IO ()
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

    lockExist <- doesFileExist lock
    when (lockExist) 
         (throwIO $ userError ("Resource is busy"))
    
    writeFile lock ""

    when (createIfMissing opts)
         (createFileIfMissing (FP.combine dir fileNameCurrent))

    -- TODO create VersionSet and then recover (big part)

    result <- runStateT action (DB dir memtable 1 2 3)
    -- TODO check the results
    
    removeFile lock

    return ()

get :: MT.LookupKey -> StateT DB IO (Maybe Bs)
get = undefined

add :: MT.LookupKey -> Bs -> StateT DB IO ()
add = undefined

operation :: IO () 
operation = putStrLn "Operation"

