
module RecoveryExample where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans (liftIO)
import Control.Exception (throwIO)

import Database.LSM

main :: IO ()
main =
    withLSM def { dbName = "/tmp/ExampleDB_Recovery"
                , errorIfExists = False
                , memtableThreshold = 2 * 1024 * 1024 } $ do
        let key1 = C.pack "Key 1"
        let key2 = C.pack "Key 2"
        let val1 = C.pack "Value 1"
        let val2 = C.pack "Value 2"
        add key1 val1
        add key2 val2
        -- throw error
        liftIO $ throwIO $ userError ""


