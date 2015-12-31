
module Database.LSM.Utils where

import qualified BTree as BT
import qualified Data.Map as Map
import Pipes
import System.FilePath ((</>))
import Control.Monad (unless, liftM)
import Control.Exception (throwIO)
import System.Directory (doesFileExist)
import System.IO.Error (alreadyExistsErrorType, doesNotExistErrorType, mkIOError)
import System.Random (randomIO)

import Database.LSM.Types (ImmutableTable, Bs)

-- TODO: What should the order and size be?
btreeOrder :: BT.Order
btreeOrder = 10

btreeSize :: BT.Size
btreeSize = 1000

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing name = doesFileExist name >>= \e -> unless e (writeFile name "")

fileNameCurrent :: FilePath -> FilePath
fileNameCurrent d = d </> "CURRENT"

fileNameLock :: FilePath -> FilePath
fileNameLock d = d </> "LOCK"

createFile :: FilePath -> IO ()
createFile name = do
    exist <- doesFileExist name
    if exist
        then throwIOAlreadyExists name
        else writeFile name ""

throwIOAlreadyExists :: String -> IO a
throwIOAlreadyExists name =
    throwIO $ mkIOError alreadyExistsErrorType "" Nothing (Just name)

throwIODoesNotExist :: String -> IO a
throwIODoesNotExist name =
    throwIO $ mkIOError doesNotExistErrorType "" Nothing (Just name)

throwIOFileEmpty :: String -> IO a
throwIOFileEmpty name =
    throwIO $ userError ("File: " ++ name ++ " is empty!")

randomVersion :: IO String
randomVersion = show <$> (randomIO :: IO Int)

extension :: String
extension = ".db"

io :: MonadIO m => IO a -> m a
io = liftIO

fromMapToProducer :: Monad m => ImmutableTable -> Producer (BT.BLeaf Bs Bs) m ()
fromMapToProducer table = each (map (uncurry BT.BLeaf) (Map.toAscList table))

getBTreeM :: Monad m => Producer (BT.BLeaf Bs Bs) m () -> m (BT.LookupTree Bs Bs)
getBTreeM producer = do
    tree <- liftM BT.fromByteString (BT.fromOrderedToByteString btreeOrder btreeSize producer)
    return $ case tree of
        Left m -> error m
        Right x -> x

mapToTree :: Monad m => ImmutableTable -> m (BT.LookupTree Bs Bs)
mapToTree = getBTreeM . fromMapToProducer

merge :: FilePath -> BT.LookupTree Bs Bs -> BT.LookupTree Bs Bs -> IO ()
merge fpath t1 t2 = BT.mergeTrees (\a _ -> return a) btreeOrder fpath [t1, t2]

