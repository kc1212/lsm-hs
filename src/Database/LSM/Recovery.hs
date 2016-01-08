
module Database.LSM.Recovery where

import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Crypto.Hash.SHA256 (hashlazy)

import Database.LSM.Types

lenSize, hashSize :: Int64
lenSize = 64 `div` 8
hashSize = 256 `div` 8

parseEntry :: Bs -> Maybe ((Bs, Bs), Bs)
parseEntry bs
    | B.null bs = Nothing
    | otherwise =
        parseLen bs >>=
        parseData >>=
        \(k, r) -> parseCheckSum r >>=
        \(ksum, rest) -> return ((k, ksum), rest)

parseLen :: Bs -> Maybe (Int64, Bs)
parseLen bs
    | B.length bs < lenSize = Nothing
    | otherwise             = Just (decode n, rest)
    where (n, rest) = B.splitAt lenSize bs

parseData :: (Int64, Bs) -> Maybe (Bs, Bs)
parseData (n, bs)
    | B.length bs < n = Nothing
    | otherwise       = Just (B.splitAt n bs)

parseCheckSum :: Bs -> Maybe (Bs, Bs)
parseCheckSum bs
    | B.length bs < hashSize = Nothing
    | otherwise              = Just (B.splitAt hashSize bs)

parseEntries :: Bs -> Either String [(Bs, Bs)]
parseEntries bs
    | B.null bs = Right []
    | otherwise = case parseEntry bs of
                    Just (e, rest) -> (e:) <$> parseEntries rest
                    _              -> Left "Parsing failed, parseEntry returned Nothing."

parsePairs :: [(Bs, Bs)] -> Either String [(Bs, Bs)]
parsePairs [] = Right []
parsePairs [_] = Left "Parsing failed, odd number of pairs."
parsePairs ((x, xsum) : (y, ysum) : rest) =
    if hashlazy2 x == xsum && hashlazy2 y == ysum
        then ((x, y):) <$> parsePairs rest
        else Left "Parsing failed, checksum did not match."

createPair :: (Bs, Bs) -> Bs
createPair (k, v) = createEntry k `B.append` createEntry v
    where createEntry x = encode (B.length x) `B.append` x `B.append` hashlazy2 x

-- The B.appendFile function creates file if it does not exist.
appendPairToFile :: FilePath -> (Bs, Bs) -> IO ()
appendPairToFile fpath pair = B.appendFile fpath (createPair pair)

readPairsFromFile :: FilePath -> IO (Either String [(Bs, Bs)])
readPairsFromFile fpath = do
    bs <- B.readFile fpath
    return $ parseEntries bs >>= parsePairs

hashlazy2 :: Bs -> Bs
hashlazy2 = B.fromStrict . hashlazy

