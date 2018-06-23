{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SupplyInstance where

import           Control.Monad
import           SupplyClass

newtype Reader e a = R { runReader :: e -> a}

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  (<*>) = ap
  pure = return

instance Monad (Reader e) where
  return a = R $ \_ -> a
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a} deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
  next = MySupply $ do
    v <- ask
    return (Just v)

-- next = MySupply (liftM Just ask)

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)

runMS :: MySupply e a -> e -> a
runMS ms = (runReader . runMySupply) ms
