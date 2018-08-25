module DivBy4 where

divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ []                   = Just []
divBy _ (0:_)                = fail "division by zero"
divBy numerator (denom : xs) = do
  next <- divBy numerator xs
  return ((numerator `div` denom) : next)
