
module Main where

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import System.Directory

import Database.LevelDB

basicOptions = DBOptions True False False
emptyAction = return ()
testDir = "/tmp/tmpdb"

prop_createLevelDB :: Property
prop_createLevelDB = monadicIO $ do res <- run $ withLSM dir basicOptions emptyAction
                                    dirExist <- run $ doesDirectoryExist dir
                                    assert dirExist
                                    run $ removeDirectoryRecursive testDir

main = do
    quickCheck prop_createLevelDB

