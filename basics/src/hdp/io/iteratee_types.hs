module Iteratee_Types where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

data Chunk = Chunk{ chunk :: String}
           | LineEnd { chunk :: String, remainder :: String}
           deriving Show

newtype Iter = Iter{runIter :: B8.ByteString -> IterResult}

data IterResult = HaveLine {line :: String, residual :: String}
                | NeedChunk Iter

instance Show IterResult where
  show (HaveLine l r) = "Haveline " ++ l ++ " | " ++ r
  show (NeedChunk _)  = "Need chunk"
