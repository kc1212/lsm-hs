
module Main where

import Control.Monad.State (gets)
import Data.Maybe (isJust)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Directory
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified BTree as BT

import Database.LSM
import Database.LSM.Utils
import Database.LSM.MemTable as MT
import Database.LSM.Types

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink xs = B.pack <$> shrink (B.unpack xs)

instance CoArbitrary B.ByteString where
    coarbitrary = coarbitrary . B.unpack

emptyAction = return ()
testDir = "/tmp/tmpdb"
basicOptions = def { dbName = testDir }

prop_createLSM :: Property
prop_createLSM = monadicIO $ do
    res <- run $ withLSM basicOptions emptyAction
    let dir = dbName basicOptions
    dirExist <- run $ doesDirectoryExist dir
    assert dirExist
    run $ removeDirectoryRecursive testDir

prop_singleEntry :: (Bs, Bs) -> Property
prop_singleEntry (k, v) = monadicIO $ do
    res <- run $ withLSM basicOptions $ do
            add k v
            r1 <- get k
            r2 <- C.pack <$> io randomVersion >>= get
            return (r1, r2)
    assert (res == (Just v, Nothing))

prop_multiEntryAndSize :: Positive Int -> Property
prop_multiEntryAndSize (Positive n) = monadicIO $
    forAllM (vector n) $ \xs -> do
        (res, sz) <- run $ withLSM basicOptions $ do
                mapM_ (uncurry add) xs
                res <- mapM (get . fst) xs
                sz <- gets memTableSize
                return (res, sz)
        let actualSz = sum (map (\(k, v) -> B.length k + B.length v) xs)
        assert (all isJust res)
        assert (sz == actualSz)

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


main = do
    quickCheck prop_mergeBTree
    quickCheck prop_createLSM
    quickCheck prop_singleEntry
    quickCheck prop_multiEntryAndSize
    -- quickCheck (prop_multiEntryAndSize (Positive 100))

