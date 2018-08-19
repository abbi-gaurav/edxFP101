module Collatz where

collatzSeq :: Int -> [Int]
collatzSeq n = reverse (loop n [])

loop :: Int -> [Int] -> [Int]
loop n xs
  | n <= 0 = xs
  | n == 1 = 1 : xs
  | even n = loop (quot n 2) (n : xs)
  | otherwise = loop (3 * n + 1) (n : xs)
