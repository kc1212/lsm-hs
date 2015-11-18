
module Database.LevelDB.Utils where

import qualified Data.ByteString as BS
import Control.Monad
import Control.Exception
import System.Directory
import System.IO.Error

type Bs = BS.ByteString

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing name = doesFileExist name >>= \e -> unless e (writeFile name "")

fileNameCurrent :: String
fileNameCurrent = "CURRENT"

fileNameLock :: String
fileNameLock = "LOCK"

createFile :: FilePath -> IO ()
createFile name = do 
    exist <- doesFileExist name
    if (exist)
        then (throwIO $ mkIOError alreadyExistsErrorType "" Nothing (Just name))
        else writeFile name "" 

