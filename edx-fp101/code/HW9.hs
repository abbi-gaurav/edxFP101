import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat
         deriving Show

natToInteger11 :: Nat -> Integer
natToInteger11 (Succ n ) = 1 + natToInteger11 n
natToInteger11 Zero = 0

--wrong
natToInteger15 :: Nat -> Integer
natToInteger15 (Succ n ) = (1 + natToInteger15 n) - 1
natToInteger15 Zero = 1

natToInteger16 :: Nat -> Integer
natToInteger16 = head . m
                 where m Zero = [0]
                       m (Succ n) = [sum[x|x <- (1 : m n)]]

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

integerToNat4 :: Integer -> Nat
integerToNat4 (n + 1) = Succ (integerToNat4 n)
integerToNat4 0 = Zero

integerToNat5 :: Integer -> Nat
integerToNat5 (n + 1) = let m = integerToNat5 n in Succ m
integerToNat5 0 = Zero

f a = natToInteger11 (a (integerToNat1 3) (integerToNat1 4))

add1 :: Nat -> Nat -> Nat
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

add2 :: Nat -> Nat -> Nat
add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

add7 :: Nat -> Nat -> Nat
add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add1 m (mult m n)

data Tree = Leaf Integer | Node Tree Integer Tree
          deriving Show
occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r) = case compare m n of
                           LT -> occurs1 m l
                           EQ -> True
                           GT -> occurs1 m r

data Tree2 = Leaf2 Integer | Node2 Tree2 Tree2
             deriving Show
leaves (Leaf2 _) = 1
leaves (Node2 l r) = leaves l + leaves r

balanced (Leaf2 _) = True
balanced (Node2 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf2 x
balance (xs) = Node2 (balance ys) (balance zs)
                 where (ys,zs) = halve xs
