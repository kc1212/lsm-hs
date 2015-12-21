
module BTreeTest where

import qualified Data.ByteString.Char8 as BSC
import qualified BTree as BT
import Pipes

import Database.LSM.MemTable
import Database.LSM.Types
import Database.LSM.Utils

a :: MemTable
a = insert (BSC.pack "b") (BSC.pack "b") (
        insert (BSC.pack "a") (BSC.pack "a") new
    )

b :: MemTable
b = insert (BSC.pack "c") (BSC.pack "c") (
        insert (BSC.pack "d") (BSC.pack "d") new
    )

p :: Producer (BT.BLeaf Bs Bs) IO ()
p = fromMapToProducer a

q :: Producer (BT.BLeaf Bs Bs) IO ()
q = fromMapToProducer b

main :: IO ()
main = do
    s <- getTreeIO p
    t <- getTreeIO q
    _ <- merge s t
    lookupTree <- BT.open "/tmp/tree.tmp"
    case lookupTree of
        Left m -> error m
        Right x -> x
    return ()
