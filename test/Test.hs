
module Main where

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import System.Directory

import Database.LSM

emptyAction = return ()
testDir = "/tmp/tmpdb"
basicOptions = DBOptions testDir True False 10 100

prop_createLevelDB :: Property
prop_createLevelDB = monadicIO $ do res <- run $ withLSM basicOptions emptyAction
                                    let dir = dbName basicOptions
                                    dirExist <- run $ doesDirectoryExist dir
                                    assert dirExist
                                    run $ removeDirectoryRecursive testDir

main = do
    quickCheck prop_createLevelDB

