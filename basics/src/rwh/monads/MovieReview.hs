module MovieReview where

import           Control.Monad

data MovieReview = MovieReview {
     title  :: String
  ,  user   :: String
  ,  review :: String
  }

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview aList = do
  title <- lookup1 "title" aList
  user <- lookup1 "user" aList
  review <- lookup1 "review" aList
  return (MovieReview title user review)

lookup1 :: String -> [(String, Maybe String)] -> Maybe String
lookup1 key aList = case lookup key aList of
                      Just (Just s) -> Just s
                      _             -> Nothing

liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview aList = liftM3 MovieReview
                     (lookup1 "title" aList)
                     (lookup1 "user" aList)
                     (lookup1 "review" aList)

apReview :: [(String, Maybe String)] -> Maybe MovieReview
apReview aList =
  MovieReview `liftM` lookup1 "title" aList -- Maybe(String -> String -> Moviereview)
              `ap` lookup1 "user" aList     -- Maybe(String -> Moviereview)
              `ap` lookup1 "review" aList   -- Maybe Moviereview
