{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Mapping_Traversable where

import           Data.Traversable

doF :: (Num b, Show b) => b -> IO b
doF n = do print n; return (n * 2)


data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving (Functor, Show, Foldable, Traversable)
aTree = Node 2
        (Leaf 3)
        (Node 5
          (Leaf 7)
          (Leaf 11))

main :: IO (Tree Integer)
main = traverse doF aTree
