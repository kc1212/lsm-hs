
module Example where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Database.LSM

main = do
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
        res3 <- get key1
        liftIO $ print (fromJust res1)
        liftIO $ print (fromJust res2)
        liftIO $ print (fromJust res3)
        
