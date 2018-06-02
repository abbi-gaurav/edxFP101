{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoid where

newtype AInt = A { unA :: Int} deriving (Eq, Show, Num)

instance Monoid AInt where
  mempty = 0
  mappend = (+)


newtype MInt = M {unM :: Int} deriving (Eq, Show, Num)

instance Monoid MInt where
  mempty = 1
  mappend = (*)
