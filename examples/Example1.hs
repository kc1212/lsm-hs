
module Example1 where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Database.LSM

main :: IO ()
main = do
    -- simple test
    withLSM def { dbName = "/tmp/ExampleDB"
                , errorIfExists = False } $ do
        let key1 = C.pack "Key 1"
        let val1 = C.pack "Value 1"
        let val2 = C.pack "Value 2"
        add key1 val1
        res1 <- get key1
        update key1 val2
        res2 <- get key1
        delete key1
        res3 <- get key1 -- TODO this should return Nothing
        liftIO $ print (fromJust res1)
        liftIO $ print (fromJust res2)
        liftIO $ print (fromJust res3)

    -- write large value
    withLSM def { dbName = "/tmp/ExampleDB_Large"
                , errorIfExists = False } $ do
        let (key1, val1) = (C.pack "Key 1", C.replicate (2 * 1024 * 1024) 'a')
        let (key2, val2) = (C.pack "Key 2", C.replicate (2 * 1024 * 1024) 'b')
        add key1 val1
        add key2 val2
        res1 <- get key1
        res2 <- get key2
        liftIO $ print (C.take 1 (fromJust res1))
        liftIO $ print (C.take 1 (fromJust res2))

