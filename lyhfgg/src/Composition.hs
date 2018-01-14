module Composition where

import           Data.Char

capCount :: String -> Int
capCount =  length . filter (isUpper . head) . words
