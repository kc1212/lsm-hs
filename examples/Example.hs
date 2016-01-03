
module Example where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

import Database.LSM

main = do
    withLSM def { dbName = "/tmp/ExampleDB" 
                , errorIfExists = False } $ do
        add (C.pack "Key 1") (C.pack "Value 1")
        res1 <- get (C.pack "Key 1")
        update (C.pack "Key 1") (C.pack "Value 2")
        res2 <- get (C.pack "Key 1")
        delete (C.pack "Key 1")
        res3 <- get (C.pack "Key 1")
        liftIO $ print (fromJust res1)
        liftIO $ print (fromJust res2)
        liftIO $ print (fromJust res3)
        
