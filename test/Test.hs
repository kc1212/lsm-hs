
module Main where

import Control.Monad.State (gets)
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers ( NonEmptyList(..) )
import System.Directory
import System.FilePath ((</>))
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

prop_singleEntry :: (Bs, Bs) -> Bs -> Property
prop_singleEntry (k, v) k2 = monadicIO $ do
    -- TODO refactor to use generator rather than if/else
    -- forAllM (\(Gen (k, v) k2) -> k2 /= k) $ \(k, v) k2 ->
    res <- run $ withLSM basicOptions $ do
            add k v
            r1 <- get k
            r2 <- if k2 == k || B.null k2
                    then return Nothing
                    else get k2
            return (r1, r2)
    assert (res == (Just v, Nothing))

-- prop_multiEntryOneByOne
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
        badKeys = filter (\x -> notElem x keys && (not . B.null) x) zs


main = do
    quickCheck prop_mergeBTree
    quickCheck prop_createLSM
    quickCheck prop_singleEntry
    quickCheck prop_multiEntryAndSize
    -- quickCheck (prop_multiEntryAndSize (Positive 100))

