
import Data.Int (Int64)
import Data.Bits (shift)

type InternalKey = Int -- dummy

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
        , refCout = 0
        , allowedSeeks = 1 `shift` 30
        }



