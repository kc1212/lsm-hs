
module Main where

import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, monadic, pick, pre, run)
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

prop_singleLSM :: (String, String) -> Property
prop_singleLSM (k, v) = monadicIO $ do
    res <- run $ withLSM basicOptions $ do
            let key = C.pack k
            let val = C.pack v
            add key val
            get key
    assert (fmap C.unpack res == Just v)

-- TODO this is just a port of the BTreeTest.hs
-- better to randomly generate the entries
prop_mergeBTree :: Property
prop_mergeBTree = monadicIO $ do
    run $ createDirectoryIfMissing False testDir
    s <- run $ mapToTree order size a
    t <- run $ mapToTree order size b
    run $ merge order treePath s t
    mergedTree <- run $ fromRight <$> BT.open treePath
    let res = BT.lookup mergedTree (C.pack "c")
    assert (res == Just (C.pack "cc"))
    run $ removeDirectoryRecursive testDir
    where
        order = 10
        size = 100
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
    quickCheck prop_singleLSM
    quickCheck prop_mergeBTree

