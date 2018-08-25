module DivBy1 where

divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)
