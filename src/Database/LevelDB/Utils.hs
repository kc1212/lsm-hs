
module Database.LevelDB.Utils where

import qualified Data.ByteString as BS
import Control.Monad (unless)
import Control.Exception (throwIO)
import System.Directory (doesFileExist)
import System.IO.Error (alreadyExistsErrorType, mkIOError)

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

