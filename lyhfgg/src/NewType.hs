module NewType where

data DataInt = D Int deriving (Eq, Ord, Show)

newtype NewInt = N Int deriving (Eq, Ord, Show)
