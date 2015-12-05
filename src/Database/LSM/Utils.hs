
module Database.LSM.Utils where

import qualified Data.ByteString as BS
import System.FilePath ((</>))
import Control.Monad (unless)
import Control.Exception (throwIO)
import Control.Monad.State (liftIO, MonadIO)
import System.Directory (doesFileExist)
import System.IO.Error (alreadyExistsErrorType, doesNotExistErrorType, mkIOError)

type Bs = BS.ByteString

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

io :: MonadIO m => IO a -> m a
io = liftIO

