
module Database.LSM.Recovery
    ( appendPairToFile
    , readPairsFromFile
    ) where

import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Crypto.Hash.SHA256 (hashlazy)

import Database.LSM.Types

lenSize, hashSize :: Int64
lenSize = 64 `div` 8
hashSize = 256 `div` 8

-- This function parses a single entry of the log file.
-- It returns Maybe ((key or value, checksum), the remaining data)
parseEntry :: Bs -> Maybe ((Bs, Bs), Bs)
parseEntry bs
    | B.null bs = Nothing
    | otherwise =
        parseLen bs >>=
        parseData >>=
        \(k, r) -> parseCheckSum r >>=
        \(ksum, rest) -> return ((k, ksum), rest)

-- Given some data, the first 8 bytes (Int64) should represent the length
-- returns Maybe (the length, the remaining data)
parseLen :: Bs -> Maybe (Int64, Bs)
parseLen bs
    | B.length bs < lenSize = Nothing
    | otherwise             = Just (decode n, rest)
    where (n, rest) = B.splitAt lenSize bs

-- Given length n and some data,
-- this function returns Maybe (first n bytes of the data, the remaining data)
parseData :: (Int64, Bs) -> Maybe (Bs, Bs)
parseData (n, bs)
    | B.length bs < n = Nothing
    | otherwise       = Just (B.splitAt n bs)

-- This is almost the same as `parseLen` except that it doesn't decode
-- It returns a tuple of the digest of length `hashSize` and the remaining data
parseCheckSum :: Bs -> Maybe (Bs, Bs)
parseCheckSum bs
    | B.length bs < hashSize = Nothing
    | otherwise              = Just (B.splitAt hashSize bs)

-- This is a recursive function that parses the log file in ByteString into
-- a list of tuples: [(key or value, checksum)], failure is represented by Left.
parseEntries :: Bs -> Either String [(Bs, Bs)]
parseEntries bs
    | B.null bs = Right []
    | otherwise = case parseEntry bs of
                    Just (e, rest) -> (e:) <$> parseEntries rest
                    _              -> Left "Parsing failed, parseEntry returned Nothing."

-- This function turns the results of `parseEntries`, i.e. [(key or value, checksum)]
-- into key value paris [(key, value)]. It also verifies the checksum.
parsePairs :: [(Bs, Bs)] -> Either String [(Bs, Bs)]
parsePairs [] = Right []
parsePairs [_] = Left "Parsing failed, odd number of pairs."
parsePairs ((x, xsum) : (y, ysum) : rest) =
    if hashlazy2 x == xsum && hashlazy2 y == ysum
        then ((x, y):) <$> parsePairs rest
        else Left "Parsing failed, checksum did not match."

encodePair :: (Bs, Bs) -> Bs
encodePair (k, v) = createEntry k `B.append` createEntry v
    where createEntry x = encode (B.length x) `B.append` x `B.append` hashlazy2 x

-- The B.appendFile function creates file if it does not exist.
appendPairToFile :: FilePath -> (Bs, Bs) -> IO ()
appendPairToFile fpath pair = B.appendFile fpath (encodePair pair)

readPairsFromFile :: FilePath -> IO (Either String [(Bs, Bs)])
readPairsFromFile fpath = do
    bs <- B.readFile fpath
    return $ parseEntries bs >>= parsePairs

hashlazy2 :: Bs -> Bs
hashlazy2 = B.fromStrict . hashlazy

