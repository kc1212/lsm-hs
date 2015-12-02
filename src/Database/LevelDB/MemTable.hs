
module Database.LevelDB.MemTable where

-- In the original LevelDB, memtable is implemented using skip list.
-- For this project we use Data.Map for now, then we will change it to skip list.
-- The interface should stay the same.
import qualified Data.Map as Map
import Database.LevelDB.Utils

type MemTable = Map.Map Bs Bs
type ImmutableTable = MemTable

new :: MemTable
new = Map.empty

insert :: Bs -> Bs -> MemTable -> MemTable
insert = Map.insert

lookup :: Bs -> MemTable -> Maybe Bs
lookup = Map.lookup

