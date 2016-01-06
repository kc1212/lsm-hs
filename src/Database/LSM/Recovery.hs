
module Database.LSM.Recovery where

import Control.Exception (Exception, throw)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Data.Typeable (Typeable)

import Database.LSM.Types

data RecoveryException = ParsingException | ChecksumException | BadPairException
    deriving (Show, Typeable)

instance Exception RecoveryException

lenSize, sha256Size :: Int64
lenSize = 64 `div` 8
sha256Size = 256 `div` 8

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
    | B.length bs < sha256Size = Nothing
    | otherwise                = Just (B.splitAt sha256Size bs)

parseEntries :: Bs -> [(Bs, Bs)]
parseEntries bs
    | B.null bs = []
    | otherwise = case parseEntry bs of
                    Just (e, rest) -> e : parseEntries rest
                    _              -> throw ParsingException

parsePairs :: [(Bs, Bs)] -> [(Bs, Bs)]
parsePairs [] = []
parsePairs [_] = throw BadPairException
parsePairs ((x, xsum) : (y, ysum) : rest) =
    if sha256sum x == xsum && sha256sum y == ysum
        then (x, y) : parsePairs rest
        else throw ChecksumException

createPair :: (Bs, Bs) -> Bs
createPair (k, v) = createEntry k `B.append` createEntry v
    where createEntry x = encode (B.length x) `B.append` x `B.append` sha256sum x

-- The B.appendFile function creates file if it does not exist.
appendPairToFile :: FilePath -> (Bs, Bs) -> IO ()
appendPairToFile fpath pair = B.appendFile fpath (createPair pair)

readPairsFromFile :: FilePath -> IO [(Bs, Bs)]
readPairsFromFile fpath = (parsePairs . parseEntries) <$> B.readFile fpath

-- temporary place holder
sha256sum = undefined

