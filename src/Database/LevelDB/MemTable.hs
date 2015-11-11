
module Database.LevelDB.MemTable where

-- In the original LevelDB, memtable is implemented using skip list.
-- For this project we use Data.Map for now, then we will change it to skip list.
-- The interface should stay the same.
import qualified Data.Map as Map
import Database.LevelDB.Utils
import Data.Int

data ValueType = Deletion | Value deriving (Eq, Show)
instance Ord ValueType where
    compare Deletion Value = LT
    compare Value Deletion = GT
    compare _     _        = EQ

type SequenceNumber = Int32
type KeyTag = (SequenceNumber, ValueType)
type LookupKey = (Bs, KeyTag)
type MemTable = Map.Map LookupKey Bs

new :: MemTable
new = Map.empty

insert :: LookupKey -> Bs -> MemTable -> MemTable
insert = Map.insert

lookup :: LookupKey -> MemTable -> Maybe Bs
lookup = Map.lookup

-- TODO this is an inefficient way to check the memory usage
-- it will be improved when we switch to skip list
approxMemUsage :: MemTable -> Int
approxMemUsage = length . Map.showTree



