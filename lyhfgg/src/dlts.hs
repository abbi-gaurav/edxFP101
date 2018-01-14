module DLTS where

import           Data.List (isPrefixOf)

dlts :: String -> [String]
dlts = foldr step [] . lines
  where step line accumulator
          | "#define DLT_" `isPrefixOf` line = secondWord line : accumulator
          | otherwise                        = accumulator
        secondWord = head . tail . words
