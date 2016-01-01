
module Main where

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import System.Directory
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as C
import qualified BTree as BT
import Pipes

import Database.LSM
import Database.LSM.Utils
import Database.LSM.MemTable as MT

emptyAction = return ()
testDir = "/tmp/tmpdb"
basicOptions = DBOptions testDir True False 10 100

prop_createLSM :: Property
prop_createLSM = monadicIO $ do
    res <- run $ withLSM basicOptions emptyAction
    let dir = dbName basicOptions
    dirExist <- run $ doesDirectoryExist dir
    assert dirExist
    run $ removeDirectoryRecursive testDir

prop_simpleLSM :: String -> String -> Property
prop_simpleLSM k v = monadicIO $ do
    run $ withLSM basicOptions $ do
            let key = C.pack k
            let val = C.pack v
            add key val
            -- res <- get key
    assert True

-- TODO this is just a port of the BTreeTest.hs
-- better to randomly generate the entries
prop_mergeBTree :: Property
prop_mergeBTree = monadicIO $ do
    run $ createDirectoryIfMissing False testDir
    s <- run $ mapToTree a
    t <- run $ mapToTree b
    run $ merge treePath s t
    mergedTree <- run $ fromRight <$> BT.open treePath
    let res = BT.lookup mergedTree (C.pack "c")
    assert (res == Just (C.pack "cc"))
    run $ removeDirectoryRecursive testDir
    where
        treePath = testDir </> "tree"
        a :: MemTable
        a = MT.insert (C.pack "b") (C.pack "bb")
                (MT.insert (C.pack "a") (C.pack "aa") MT.new)
        b :: MemTable
        b = MT.insert (C.pack "c") (C.pack "cc")
                (MT.insert (C.pack "d") (C.pack "dd") MT.new)


fromRight :: Either String b -> b
fromRight x =
    case x of
        Left m -> error m
        Right m -> m

main = do
    quickCheck prop_createLSM
    quickCheck prop_simpleLSM
    quickCheck prop_mergeBTree

