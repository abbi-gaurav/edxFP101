module Records where

data Car = Car {company :: String,
                model :: String,
                 year :: Int} deriving Show

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProduct :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProduct` (Vector l m n) = (i*l) + (j*m) + (k*n)
