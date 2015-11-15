
module Database.LevelDB.Utils where

import qualified Data.ByteString as BS
import Control.Monad
import System.Directory

type Bs = BS.ByteString

createFileIfMissing :: FilePath -> IO ()
createFileIfMissing name = doesFileExist name >>= \e -> unless e (writeFile name "")

fileNameCurrent :: String
fileNameCurrent = "CURRENT"

lock :: String
lock = "LOCK"

