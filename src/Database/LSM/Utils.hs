
module Database.LSM.Utils where

import qualified Data.ByteString as BS
import qualified BTree as BT
import qualified Data.Map as Map
import Pipes
import System.FilePath ((</>))
import Control.Monad (unless, liftM)
import Control.Exception (throwIO)
import Control.Monad.State (liftIO, MonadIO)
import System.Directory (doesFileExist)
import System.IO.Error (alreadyExistsErrorType, doesNotExistErrorType, mkIOError)
import System.Random (randomIO)
import System.IO

import Database.LSM.Types (ImmutableTable, Bs)

-- TODO: What should the order and size be?
btreeOrder :: BT.Order
btreeOrder = 10

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

fromMapToProducer :: ImmutableTable -> Producer (BT.BLeaf Bs Bs) IO ()
fromMapToProducer table = each (map (uncurry BT.BLeaf) (Map.toAscList table))
--Map.foldlWithKey (\_ k v -> yield (BT.BLeaf k v)) (return ()) table

getTreeIO :: Producer (BT.BLeaf Bs Bs) IO () -> IO (BT.LookupTree Bs Bs)
getTreeIO producer = do
    let size = 100
    tree <- liftM BT.fromByteString (BT.fromOrderedToByteString btreeOrder size producer)
    return $ case tree of
        Left m -> error m
        Right x -> x

mapToTree :: ImmutableTable -> IO (BT.LookupTree Bs Bs)
mapToTree = getTreeIO . fromMapToProducer

merge :: FilePath -> BT.LookupTree Bs Bs -> BT.LookupTree Bs Bs -> IO ()
merge fpath t1 t2 = BT.mergeTrees (\a _ -> return a) btreeOrder fpath [t1, t2]

