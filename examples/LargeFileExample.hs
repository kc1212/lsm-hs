
module LargeFileExample where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Database.LSM

main :: IO ()
main =
    withLSM def { dbName = "/tmp/ExampleDB_Large"
                , errorIfExists = False } $ do
        let (key1, val1) = (C.pack "Key 1", C.replicate (2 * 1024 * 1024) 'a')
        let (key2, val2) = (C.pack "Key 2", C.replicate (4 * 1024 * 1024) 'b')
        add key1 val1
        add key2 val2
        res1 <- get key1
        res2 <- get key2
        liftIO $ print (C.take 1 (fromJust res1))
        liftIO $ print (C.take 1 (fromJust res2))

