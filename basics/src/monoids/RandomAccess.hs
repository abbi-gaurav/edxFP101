{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandomAccess where

import           Data.Monoid ((<>))

data Tree v a = Leaf v a
              | Branch v (Tree v a) (Tree v a)

newtype Size = Size {getSize :: Int}

newtype Priority = Priority {getPriority :: Int}

class Tag t where
  tag :: Tree t a -> t

instance Tag (Tree Size a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v

instance Tag (Tree Priority a) where
  tag (Leaf v _)     = v
  tag (Branch v _ _) = v

instance Monoid Size where
  mempty = Size 0
  Size x `mappend` Size y = Size (x + y)

instance Monoid Priority where
  mempty = Priority (maxBound :: Int)
  Priority x `mappend` Priority y = Priority (min x y)

branch :: (Tag v, Monoid v) => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y

(!!!) :: Tag Size => Tree Size a -> Int -> a
(Leaf _ a) !!! 0 = a
(Branch _ x y) !!! n
  | n < getSize (tag x) = x !!! n
  | otherwise           = y !!! (n - getSize(tag x))

winner :: Tag Priority => Tree Priority a -> a
winner t = go t
  where
    go (Leaf _ a) = a
    go (Branch _ x y)
      | getPriority (tag x) == getPriority (tag t) = go x
      | getPriority (tag y) == getPriority (tag t) = go y

search :: (Monoid v, Tag v) => (v -> Bool) -> Tree v a -> Maybe a
search p t
  | p (tag t) = Just (go mempty p t)
  | otherwise = Nothing
  where
    go :: (Monoid t, Tag t) => t -> (t -> Bool) -> Tree t a -> a
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
      | p (i <> tag l) = go i p l
      | otherwise = go (i <> tag l) p r
