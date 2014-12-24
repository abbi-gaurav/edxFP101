module TypesNClasses where

data Shape = Circle Float|Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect n m ) = n * m

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just(x `div` y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just (x)

data Nat = Zero | Succ Nat
         deriving Show

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data Expr = Val Int
          | Add Expr Expr
          | Mult Expr Expr

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y
size (Mult x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y

data Tree = Leaf Int
          | Node Tree Int Tree

data Foo = Foo {x::Integer, str::String}
         deriving (Eq, Ord, Show)
