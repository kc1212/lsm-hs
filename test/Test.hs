
module Main where

import Test.QuickCheck
import Database.LevelDB
import Database.LevelDB.Core

prop_reverseReverse :: [Char] -> Bool
prop_reverseReverse s = (reverse . reverse) s == s

main = do
    quickCheck prop_reverseReverse

