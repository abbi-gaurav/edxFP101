module PodTypes where

data Podcast = Podcast {
  castId  :: Integer,
  castURL :: String
  }
  deriving (Eq, Read, Show)

data Episode = Episode {
  epId   :: Integer,
  epCast :: Podcast,
  epURL  :: String,
  epDone :: Bool
  }
  deriving (Eq, Read, Show)
