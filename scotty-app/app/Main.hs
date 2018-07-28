{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Monoid  ((<>))
import           GHC.Generics
import           Web.Scotty

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

main :: IO ()
main = scotty 3000 routes

routes :: ScottyM()
routes = do
  get "/users" $ usersH
  get "/:word" $ wordHandler
  get "/hello/:name" $ hello

usersH :: ActionM ()
usersH = do
  json allUsers

wordHandler :: ActionM()
wordHandler = do
  beam <- param "word"
  html $ mconcat ["<h1>ScottY, ", beam, " me up!</h1>"]

hello :: ActionM()
hello = do
  name <- param "name"
  text ("hello " <> name <> "!")
