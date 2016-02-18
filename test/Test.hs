
module Main where

import Control.Monad (unless)
import Control.Monad.State (gets)
import Control.Exception (throwIO)
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (nubBy)
import Data.Int (Int64)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError, isUserError)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified BTree as BT

import Database.LSM
import Database.LSM.Utils
import Database.LSM.MemTable as MT
import Database.LSM.Recovery

instance Arbitrary C.ByteString where
    arbitrary = C.pack <$> suchThat arbitrary (not . null)
    shrink xs = C.pack <$> shrink (C.unpack xs)

instance CoArbitrary C.ByteString where
    coarbitrary = coarbitrary . C.unpack

prop_createLSM :: Property
prop_createLSM = monadicIO $ do
    run $ myRemoveDir testDir
    run $ withLSM basicOptions emptyAction
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

prop_multiEntry :: Positive Int64 -> Positive Int -> Property
prop_multiEntry (Positive t) (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        res <- run $ withLSM basicOptions { memtableThreshold = t} $ do
                mapM_ (uncurry add) xs
                mapM (get . fst) xs
        assert (all isJust res)
        logFileShouldNotExist testDir

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

prop_readingFromDisk :: Positive Int64 -> Positive Int -> Property
prop_readingFromDisk (Positive t) (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        run $ withLSM basicOptions { memtableThreshold = t } (mapM_ (uncurry add) xs)
        logFileShouldNotExist testDir
        res <- run $ withLSM basicOptions { memtableThreshold = t
                                          , createIfMissing = False
                                          , errorIfExists = False }
                             (mapM (get . fst) xs)
        assert (all isJust res)
        logFileShouldNotExist testDir

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

prop_addEmptyValueException :: Property
prop_addEmptyValueException = monadicIO $ do
    run $ myRemoveDir testDir
    run $ catchIOError
            (withLSM basicOptions $ do
                let key = C.pack "key"
                let val = C.pack ""
                add key val)
            (\e -> unless (isUserError e) (ioError e))

prop_getNonExistingKey :: Bs -> Property
prop_getNonExistingKey k = monadicIO $ do
    run $ myRemoveDir testDir
    res <- run $ withLSM basicOptions (get k)
    assert (isNothing res)

prop_delete :: Positive Int -> Property
prop_delete (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \xs -> do
        let (xa, xb) = splitAt (length xs `div` 2) xs
        (resa, resb) <- run $ withLSM basicOptions $ do
                mapM_ (uncurry add) xs          -- add everything
                mapM_ (delete . fst) xa         -- delete half of it - xa
                resa <- mapM (get . fst) xa     -- should be all Nothing
                resb <- mapM (get . fst) xb     -- some of it should be Just something
                return (resa, resb)
        assert (all isNothing resa)
        assert (any isJust resb)
        logFileShouldNotExist testDir

prop_recoveryParser :: Positive Int -> Property
prop_recoveryParser (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir >> createDirectoryIfMissing False testDir
    forAllM (vector n) $ \pairs -> do
        run $ mapM_ (appendPairToFile logdir) pairs
        res <- run $ if null pairs then return (Right []) else readPairsFromFile logdir
        assert (Right pairs == res)
        where logdir = testDir </> "prop_recoveryParser"

prop_recoveryParserFailure :: Bs -> Property
prop_recoveryParserFailure bs = monadicIO $ do
    run $ myRemoveDir testDir >> createDirectoryIfMissing False testDir
    run $ B.writeFile logdir bs -- write some random data
    res <- run $ readPairsFromFile logdir
    assert (isLeft res)
    where logdir = testDir </> "prop_recoveryParser"

prop_recovery :: Positive Int64 -> Positive Int -> Property
prop_recovery (Positive t) (Positive n) = monadicIO $ do
    run $ myRemoveDir testDir
    forAllM (vector n) $ \pairs -> do
        run $ catchIOError
                (withLSM basicOptions { memtableThreshold = t } $ do
                    mapM_ (uncurry add) pairs
                    io $ throwIO $ userError "")
                (\e -> unless (isUserError e) (ioError e))
        mExist <- run $ doesFileExist (testDir </> "memtable.log")
        iExist <- run $ doesFileExist (testDir </> "imemtable.log")
        assert (mExist || iExist)
        res <- run $ withLSM basicOptions { createIfMissing = False, errorIfExists = False }
                             (mapM (get . fst) pairs)
        assert (map fromJust res == map snd (uniqueLast pairs))


-- helper functions
emptyAction :: LSM ()
emptyAction = return ()

testDir :: FilePath
testDir = "/tmp/tmpdb"

basicOptions :: DBOptions
basicOptions = def { dbName = testDir, debugLog = False }

logFileShouldNotExist :: FilePath -> PropertyM IO ()
logFileShouldNotExist path = do
        run (doesFileExist (path </> "memtable.log")) >>= assert . not
        run (doesFileExist (path </> "imemtable.log")) >>= assert . not

uniqueLast :: Eq a => [(a, t)] -> [(a, t)]
uniqueLast = reverse . nubBy (\(x, _) (y, _) -> x == y) . reverse

myRemoveDir :: FilePath -> IO ()
myRemoveDir dir =
    catchIOError
    (removeDirectoryRecursive dir)
    (\e -> unless (isDoesNotExistError e) (ioError e))


main :: IO ()
main = do
    let twoMB = 2 * 1024 * 1024;

    quickCheck prop_mergeBTree
    quickCheck prop_createLSM
    quickCheck prop_singleEntry

    quickCheck prop_recoveryParser
    quickCheck prop_recoveryParserFailure
    quickCheck prop_recovery
    quickCheck $ prop_recovery (Positive twoMB)

    quickCheck prop_multiEntry
    quickCheck $ prop_multiEntry (Positive twoMB)

    quickCheck prop_memtableSize
    quickCheckWith stdArgs { maxSuccess = 1 } prop_addEmptyValueException
    quickCheckWith stdArgs { maxSuccess = 1 } prop_getNonExistingKey

    quickCheck $ prop_delete (Positive 2)
    quickCheck $ prop_delete (Positive 10)

    quickCheck prop_readingFromDisk
    quickCheck $ prop_readingFromDisk (Positive twoMB)

