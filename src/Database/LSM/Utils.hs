
module Database.LSM.Utils where

import qualified BTree as BT
import qualified Data.Map as Map
import Pipes
import System.FilePath ((</>))
import System.IO (stderr, hPutStrLn)
import Control.Monad (when, unless, liftM)
import Control.Monad.Reader (asks)
import Control.Exception (throwIO)
import System.Directory (doesFileExist, renameFile, removeFile)
import System.IO.Error (alreadyExistsErrorType, doesNotExistErrorType, mkIOError)
import System.Random (randomIO)

import Database.LSM.Types

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing name = doesFileExist name >>= \e -> unless e (writeFile name "")

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist f = doesFileExist f >>= \e -> when e (removeFile f)

fileNameCurrent :: FilePath -> FilePath
fileNameCurrent = (</> "CURRENT")

fileNameLock :: FilePath -> FilePath
fileNameLock = (</> "LOCK")

fileNameMemtableLog :: FilePath -> FilePath
fileNameMemtableLog = (</> "memtable.log")

fileNameIMemtableLog :: FilePath -> FilePath
fileNameIMemtableLog = (</> "imemtable.log")

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

-- TODO instead of using userError, we should make our own error types

throwIOFileEmpty :: String -> IO a
throwIOFileEmpty name =
    throwIO $ userError ("File: " ++ name ++ " is empty!")

throwIOBadBTree :: String -> IO a
throwIOBadBTree string =
    throwIO $ userError ("Cannot open btree: " ++ string)

throwIOBadValue :: String -> IO a
throwIOBadValue string =
    throwIO $ userError ("Addition of an empty value is not allowed. " ++ string)

throwIOBadKey :: String -> IO a
throwIOBadKey string =
    throwIO $ userError ("Addition of an empty key is not allowed. " ++ string)

throwIORecoveryFailure :: String -> IO a
throwIORecoveryFailure string =
    throwIO $ userError ("Recovery failed - " ++ string)

randomVersion :: IO String
randomVersion = tail . (++ extension) . show <$> (randomIO :: IO Int)
    where extension = "lsm.db"

readVersion :: LSM String
readVersion = do
    path <- asks dbName
    io $ readFile (fileNameCurrent path)

writeVersion :: String -> LSM ()
writeVersion ver = do
    lsmLog ("Writing version: " ++ ver ++ ".")
    currPath <- fileNameCurrent <$> asks dbName
    currExists <- io $ doesFileExist currPath

    -- throw exception if CURRENT does not exist
    io $ unless currExists (throwIODoesNotExist currPath)

    io $ renameFile currPath (currPath ++ ".old")
    io $ writeFile currPath ver

    content <- io $ readFile currPath -- hack to do strict IO
    lsmLog ("Writing version finished: " ++ content ++ ".")

nameAndVersion :: LSM FilePath
nameAndVersion = do
    path <- asks dbName
    version <- readVersion
    return (path </> version)

io :: MonadIO m => IO a -> m a
io = liftIO

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust p q =
    case p of
        Just _  -> p
        _       -> q

logStdErr :: String -> IO ()
logStdErr = hPutStrLn stderr

lsmLog :: String -> LSM ()
lsmLog str = asks debugLog >>= \x -> when x (io $ logStdErr str)

openTree :: FilePath -> IO (BT.LookupTree Bs Bs)
openTree fpath = do
    eTree <- BT.open fpath
    case eTree of
        Left m  -> throwIOBadBTree m
        Right t -> return t

mapToProducer :: Monad m => ImmutableTable -> Producer (BT.BLeaf Bs Bs) m ()
mapToProducer table = each (map (uncurry BT.BLeaf) (Map.toAscList table))

producerToTree :: Monad m => BT.Order -> BT.Size -> Producer (BT.BLeaf Bs Bs) m () -> m (BT.LookupTree Bs Bs)
producerToTree order size producer = do
    tree <- liftM BT.fromByteString (BT.fromOrderedToByteString order size producer)
    return $ case tree of
        Left m -> error m
        Right x -> x

mapToTree :: Monad m => BT.Order -> BT.Size -> ImmutableTable -> m (BT.LookupTree Bs Bs)
mapToTree order size = producerToTree order size . mapToProducer

merge :: BT.Order -> FilePath -> BT.LookupTree Bs Bs -> BT.LookupTree Bs Bs -> IO ()
merge order fpath t1 t2 = BT.mergeTrees (\a _ -> return a) order fpath [t1, t2]

