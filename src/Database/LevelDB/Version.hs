
module Database.LevelDB.Version where

import Data.Int (Int64)
import Data.Bits (shift)

import Database.LevelDB.Types

data FileMetaData = FileMetaData
    { largestIKey   :: InternalKey
    , smallestIKey  :: InternalKey
    , fileSize      :: Int64
    , fileNumber    :: Int64
    , refCount      :: Int
    , allowedSeeks  :: Int
    }

initFileMetaData :: Int64 -> Int64 -> InternalKey -> InternalKey -> FileMetaData
initFileMetaData number filesize smallest largest =
    FileMetaData
        { largestIKey = largest
        , smallestIKey = smallest
        , fileSize = filesize
        , fileNumber = number
        , refCount = 0
        , allowedSeeks = 1 `shift` 30
        }

initOrRecoverVersion :: LevelDB a
initOrRecoverVersion = undefined
-- update the state - table cache and version
-- perform recovery
-- if recovery failed because CURRENT does not exist, then perform creation


