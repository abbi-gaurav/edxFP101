{-# LANGUAGE TemplateHaskell #-}

{-
to run
verboseCheck (prop_monoid_unit :: DList Char -> Bool)
-}

module DListSpec where

import           DList
import           Test.QuickCheck
import           Test.QuickCheck.All

instance (Arbitrary a) => Arbitrary (DList a) where
  arbitrary = do
    list <- arbitrary
    return $ fromList list --return (fromList list)

prop_monoid_unit :: (Monoid a, Eq a) => a -> Bool
prop_monoid_unit x =
  mempty `mappend` x == x
  &&
  x `mappend` mempty == x

prop_monoid_associative :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_monoid_associative x y z =
  (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
