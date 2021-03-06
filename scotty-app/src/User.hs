{-# LANGUAGE DeriveGeneric #-}
module User where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]
