module Helpers where

delNth :: Int -> [a] -> [a]
delNth _ [] = []
delNth idx (x:xs)
  | idx == 0  = xs
  | otherwise = x : delNth (idx -1) xs

updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth _ _ [] = []
updateNth idx f (x:xs)
  | idx == 0  = (f(x) : xs)
  | otherwise = x : updateNth (idx-1) f xs


showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "not set"
