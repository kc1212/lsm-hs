
module Database.LSM.MemTable where

-- In the original LSM paper, it was recommended to use (2-3) tree or AVL-tree.
-- For this project we use Data.Map, this may change later,
-- but interface should stay the same.
import qualified Data.Map as Map
import Data.ByteString.Lazy (empty)

import Database.LSM.Types (Bs, MemTable)

-- there must be at least one entry otherwise BTree fails
new :: MemTable
new = Map.insert empty empty Map.empty

insert :: Bs -> Bs -> MemTable -> MemTable
insert = Map.insert

lookup :: Bs -> MemTable -> Maybe Bs
lookup = Map.lookup

