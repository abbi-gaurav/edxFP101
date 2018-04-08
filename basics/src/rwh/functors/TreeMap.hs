module TreeMap where

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

instance Functor Tree where
  fmap = treeMap
