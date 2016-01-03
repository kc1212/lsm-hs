
module ThresholdTest where

import qualified Data.ByteString.Lazy.Char8 as BSC
import Control.Monad (unless)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Directory

import Database.LSM
import Database.LSM.Utils
import Database.LSM.MemTable as MT
import Database.LSM.Types

testDir = "/tmp/tmpdb"
basicOptions = def { dbName = testDir }

myRemoveDir dir =
    catchIOError
    (removeDirectoryRecursive dir)
    (\e -> unless (isDoesNotExistError e) (ioError e))

main = do
    myRemoveDir testDir
    withLSM basicOptions { memtableThreshold = 10 } $ do
        add (BSC.pack "ABC") (BSC.pack "ABC")
        add (BSC.pack "DEF") (BSC.pack "DEF")
        add (BSC.pack "GHI") (BSC.pack "GHI")
