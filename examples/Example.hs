
module Example where

import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Database.LSM

main = do
    withLSM def { dbName = "/tmp/ExampleDB" 
                , errorIfExists = False } $ do
        add (B.pack "Key 1") (B.pack "Value 1")
        res1 <- get (B.pack "Key 1")
        update (B.pack "Key 1") (B.pack "Value 2")
        res2 <- get (B.pack "Key 1")
        delete (B.pack "Key 1")
        res3 <- get (B.pack "Key 1")
        liftIO $ print (fromJust res1)
        liftIO $ print (fromJust res2)
        liftIO $ print (fromJust res3)
        
