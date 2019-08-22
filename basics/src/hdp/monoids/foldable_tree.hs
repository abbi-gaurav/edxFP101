{-# LANGUAGE InstanceSigs #-}

module Foldable_Tree where

import qualified Control.Monad as M
import qualified Data.Foldable as F
import           Data.Monoid

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

instance F.Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x l r) = (F.foldMap toMonoid l)
                                  `mappend` (toMonoid x)
                                  `mappend` (F.foldMap toMonoid r)

main = do
  print $ F.foldMap Sum             aTree
  print $ F.foldMap Product         aTree
  print $ F.foldMap (Any . (==5))   aTree
  print $ F.foldMap (All . (>0))    aTree
  print $ F.maximum                 aTree
    where aTree = Node 2 (Leaf 3) (Leaf 5)
