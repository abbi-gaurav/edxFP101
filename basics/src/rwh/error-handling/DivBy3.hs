{-# LANGUAGE ScopedTypeVariables #-}

module DivBy3 where

divBy :: forall a . Integral a => a -> [a] -> [Maybe a]
divBy numerator denominators =
  map worker denominators
  where worker :: a -> Maybe a
        worker 0 = Nothing
        worker x = Just (numerator `div` x)
