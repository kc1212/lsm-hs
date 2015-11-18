
module Main where

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import System.Directory

import Database.LevelDB
import Database.LevelDB.Core

basicOptions = DBOptions True False False
emptyAction = return ()

prop_createLevelDB :: Property
prop_createLevelDB = monadicIO $ do res <- run $ withLevelDB dir basicOptions emptyAction
                                    dirExist <- run $ doesDirectoryExist dir
                                    assert dirExist
                                    run $ removeDirectoryRecursive dir
                                    where
                                        dir = "/tmp/tmpdb"

main = do
    quickCheck prop_createLevelDB

