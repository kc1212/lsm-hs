
module Database.LevelDB.Utils where

import qualified Data.ByteString as BS
import Control.Monad
import Control.Exception
import System.Directory

type Bs = BS.ByteString

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing name = doesFileExist name >>= \e -> unless e (writeFile name "")

fileNameCurrent :: String
fileNameCurrent = "CURRENT"

lockFileName :: String
lockFileName = "LOCK"

createFile :: FilePath -> Bool -> String -> IO ()
createFile fileName condition errorMessage  = do 
    when (condition) 
        (throwIO $ userError (errorMessage))
    writeFile fileName "" 

