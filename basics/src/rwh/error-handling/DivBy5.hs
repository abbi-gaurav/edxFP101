module DivBy5 where

divByGeneric :: (Integral a, Monad m) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = fail "division by zero"
divByGeneric numerator (denom : xs) = do
  next <- divByGeneric numerator xs
  return ((numerator `div` denom) : next)

divBy :: Integral a => a -> [a] -> Maybe [a]
divBy = divByGeneric
