module MyList where

infixr 5 :-:
data MyList a = Empty | a :-: MyList a deriving (Show, Read, Eq, Ord)
