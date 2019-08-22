module Deriving_Lens where

data Tree a = Node a (Tree a) (Tree a) | Leaf a
            deriving Show

intTree = Node 2 (Leaf 3)
                 (Node 5 (Leaf 7)
                 (Leaf 11))

listTree = Node [1 ,1 ]
           (Leaf [2,1 ])
           (Node [3,2]
            (Leaf [5,2])
            (Leaf [7,4]))

tupleTree = Node (1 ,1 )
            (Leaf (2,1 ))
            (Node (3,2)
              (Leaf (5,2))
              (Leaf (7,4)))

getRoot :: Tree a -> a
getRoot (Leaf x)     = x
getRoot (Node x _ _) = x

setRoot :: Tree a -> a -> Tree a
setRoot (Leaf x) y     = Leaf y
setRoot (Node x l r) y = Node y l r

main = do
  print $ getRoot intTree
  print $ setRoot intTree 11
  print $ getRoot (setRoot intTree 11)

fmapRoot :: (a -> a) -> Tree a -> Tree a
fmapRoot f tree = setRoot tree newRoot
  where newRoot = f (getRoot tree)

fmapRoot' :: (a -> a) -> Tree a -> Tree a
fmapRoot' f (Leaf z)     = Leaf (f z)
fmapRoot' f (Node z l r) = Node (f z) l r

setRoot' :: Tree a -> a -> Tree a
setRoot' tree x = fmapRoot' (\_ -> x) tree

main2 = do
  print $ setRoot' intTree 11
  print $ fmapRoot' (*2) intTree
