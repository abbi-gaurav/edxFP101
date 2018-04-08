module Common where

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char

data GreyMap = GreyMap {
  greyWidth    :: Int
  , greyHeight :: Int
  , greyMax    :: Int
  , greyData   :: L.ByteString
                       } deriving (Eq)

instance Show GreyMap where
  show (GreyMap w h m _) = "GreyMap " ++ show w ++ " x " ++ show h ++ " " ++ show m
