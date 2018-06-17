{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module SupplyClass
  (
    MonadSupply(..)
  )
where

class (Monad m) => MonadSupply s m|m -> s where
  next :: m (Maybe s)


showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ "b: " ++  show b)
