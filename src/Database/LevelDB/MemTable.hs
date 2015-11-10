
module Database.LevelDB.MemTable where

-- In the original LevelDB, memtable is implemented using skip list.
-- For this project we use Data.Map for now, then we will change it to skip list.
-- The interface should stay the same.
import qualified Data.Map as Map
import Database.LevelDB.Core (Bs)

new :: Map.Map Bs Bs
new = Map.empty

insert :: Bs -> Bs -> Map.Map Bs Bs -> Map.Map Bs Bs
insert = Map.insert

delete :: Bs -> Map.Map Bs Bs -> Map.Map Bs Bs
delete = Map.delete

lookup :: Bs -> Map.Map Bs Bs -> Maybe Bs
lookup = Map.lookup


