module Monad_As_All_Types where

import           Control.Monad (liftM)

main = do
  print $ fmap  (*2)     (Just 10)    --Functor
  print $ pure  (*2) <*> (Just 10)    --Applicative
  print $ liftM (*2)     (Just 10)    --Monad
