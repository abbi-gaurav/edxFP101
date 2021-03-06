module QCBasics where

import           Data.List
import           Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort []       = []
qsort (x : xs) = qsort l ++ [x] ++ qsort r
  where l = filter (< x) xs
        r = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered :: Ord a => [a] -> Bool
prop_ordered xs = ordered (qsort xs)
  where ordered []           = True
        ordered [x]          = True
        ordered (x : y:  xs) = x <= y && ordered (y : xs)

prop_permutation :: Ord a => [a] -> Bool
prop_permutation xs = permutation xs (qsort xs)
  where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum :: Ord a => [a] -> Property
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append :: Ord a => [a] -> [a] -> Property
prop_append xs ys =
  not (null xs) ==>
  not (null ys) ==>
  head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model :: Ord a => [a] -> Bool
prop_sort_model xs = sort xs == qsort xs
