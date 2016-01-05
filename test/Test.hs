
module Main where

import Control.Monad (unless)
import Control.Monad.State (gets)
import Data.Maybe (isJust, isNothing)
import Data.List (nubBy)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers ( NonEmptyList(..) )
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError, isUserError)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified BTree as BT

import Database.LSM
import Database.LSM.Utils
import Database.LSM.MemTable as MT
import Database.LSM.Types

instance Arbitrary C.ByteString where
    arbitrary = C.pack <$> (suchThat arbitrary (\a -> not (null a)))
    shrink xs = C.pack <$> shrink (C.unpack xs)

instance CoArbitrary C.ByteString where
    coarbitrary = coarbitrary . C.unpack

myRemoveDir dir =
    catchIOError
    (removeDirectoryRecursive dir)
    (\e -> unless (isDoesNotExistError e) (ioError e))

emptyAction = return ()
testDir = "/tmp/tmpdb"
basicOptions = def { dbName = testDir }

prop_createLSM :: Property
prop_createLSM = monadicIO $ do
    run $ myRemoveDir testDir
    res <- run $ withLSM basicOptions emptyAction
    let dir = dbName basicOptions
    dirExist <- run $ doesDirectoryExist dir
    assert dirExist

prop_singleEntry :: (Bs, Bs) -> Bs -> Property
prop_singleEntry (k, v) k2 = monadicIO $ do
    run $ myRemoveDir testDir
    -- TODO refactor to use generator rather than if/else
    -- forAllM (\(Gen (k, v) k2) -> k2 /= k) $ \(k, v) k2 ->
    res <- run $ withLSM basicOptions $ do
            add k v
            r1 <- get k
            r2 <- if k2 == k || C.null k2
                    then return Nothing
                    else get k2
            return (r1, r2)
    assert (res == (Just v, Nothing))

-- prop_multiEntryOneByOne
prop_multiEntry :: Positive Int -> Property
prop_multiEntry (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        res <- run $ withLSM basicOptions $ do
                mapM_ (uncurry add) xs
                res <- mapM (get . fst) xs
                return res
        assert (all isJust res)

prop_memtableSize :: Positive Int -> Property
prop_memtableSize (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        -- memtable size is only relevant to memtable, so we prevent merging
        -- by setting the memtableThreshold to a high value, otherwise the test fails
        res <- run $ withLSM basicOptions { memtableThreshold = 99999999999 } $ do
                mapM_ (uncurry add) xs
                gets memTableSize
        let actualSize = sum (map (\(k, v) -> C.length k + C.length v) (uniqueLast xs))
        assert (res == actualSize)
        where
            uniqueLast = reverse . nubBy (\(x, _) (y, _) -> x == y) . reverse

prop_readingFromDisk :: Positive Int -> Property
prop_readingFromDisk (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        run $ withLSM basicOptions (mapM_ (uncurry add) xs)
        res <- run $ withLSM basicOptions { createIfMissing = False
                                          , errorIfExists = False }
                             (mapM (get . fst) xs)
        assert (all isJust res)

prop_smallThreshold :: Positive Int -> Property
prop_smallThreshold (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        res <- run $ withLSM basicOptions { memtableThreshold = 10 } $ do
                mapM_ (uncurry add) xs
                mapM (get . fst) xs
        assert (all isJust res)

prop_mergeBTree :: [(Bs,Bs)] -> [(Bs,Bs)] -> [Bs] -> Property
prop_mergeBTree xs ys zs = monadicIO $ do
    run $ createDirectoryIfMissing False testDir
    xt <- run $ mapToTree order size xm
    yt <- run $ mapToTree order size ym
    run $ merge order treePath xt yt
    mergedTree <- run $ openTree treePath
    let res = map (BT.lookup mergedTree) keys :: [Maybe Bs]
    let res2 = map (BT.lookup mergedTree) badKeys :: [Maybe Bs]
    assert (all isJust res)
    assert (all isNothing res2)
    run $ removeDirectoryRecursive testDir
    where
        order = 10
        size = 100
        treePath = testDir </> "tree"
        xm = foldr (uncurry MT.insert) MT.new xs :: ImmutableTable
        ym = foldr (uncurry MT.insert) MT.new ys :: ImmutableTable
        keys = map fst (xs ++ ys)
        -- TODO refactor to use generator rather than filtering
        badKeys = filter (\x -> notElem x keys && (not . C.null) x) zs

prop_emptyValueException :: Property
prop_emptyValueException = monadicIO $ do
    run $ myRemoveDir testDir
    run $ catchIOError
            (withLSM basicOptions $ do
                let key = C.pack "key"
                let val = C.pack ""
                add key val)
            (\e -> unless (isUserError e) (ioError e))

main = do
    quickCheck prop_mergeBTree
    quickCheck prop_createLSM
    quickCheck prop_singleEntry
    quickCheck prop_multiEntry
    quickCheck prop_readingFromDisk
    quickCheck prop_smallThreshold
    quickCheck prop_memtableSize
    quickCheckWith stdArgs { maxSuccess = 1 } prop_emptyValueException
    -- quickCheck (prop_multiEntryAndSize (Positive 100))

