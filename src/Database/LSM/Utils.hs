
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
fromMapToProducer table = Map.foldlWithKey (\_ k v -> yield (BT.BLeaf k v)) (return ()) table

# TODO: What should the order and size be?
getTreeIO :: Producer (BT.BLeaf Bs Bs) IO () -> IO (BT.LookupTree Bs Bs)
getTreeIO producer = do
    let order = 10
    let size = 100
    hSetBuffering stdin LineBuffering
    tree <- liftM BT.fromByteString (BT.fromOrderedToByteString order size producer)
    return $ case tree of
        Left _ -> error "Whut?"
        Right x -> x

merge :: (BT.LookupTree Bs Bs) -> (BT.LookupTree Bs Bs) -> IO (BT.LookupTree Bs Bs)
merge t1 t2 = do
    BT.mergeTrees (\a _ -> return a) 10 fname [t1, t2]
    eitherTree <- BT.open fname
    return $ case eitherTree of
        Left _ -> error "Whut?"
        Right x -> x
    where fname = "/tmp/tree.tmp"
