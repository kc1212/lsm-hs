
module Database.LevelDB.MemTable where

-- In the original LevelDB, memtable is implemented using skip list.
-- For this project we use Data.Map for now, then we will change it to skip list.
-- The interface should stay the same.
import qualified Data.Map as Map
import Database.LevelDB.Core (Bs)
import Data.Int

data ValueType = Deletion | Value deriving (Eq, Show)
instance Ord ValueType where
    compare Deletion Value = LT
    compare Value Deletion = GT
    compare _     _        = EQ

type SequenceNumber = Int32
type KeyTag = (SequenceNumber, ValueType)
type LookupKey = (Bs, KeyTag)

new :: Map.Map LookupKey Bs
new = Map.empty

insert :: LookupKey -> Bs -> Map.Map LookupKey Bs -> Map.Map LookupKey Bs
insert = Map.insert

lookup :: LookupKey -> Map.Map LookupKey Bs -> Maybe Bs
lookup = Map.lookup


