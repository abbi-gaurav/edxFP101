{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           GHC.Generics
import           Data.Time

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemDueBy = Maybe LocalTime
type ItemPriority = Maybe Priority

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data Item = Item {
  title :: ItemTitle
  , description :: ItemDescription
  , priority :: ItemPriority
  , dueBy ::  ItemDueBy
  } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

data ItemUpdate = ItemUpdate {
  titleUpdate :: Maybe ItemTitle
  , descriptionUpdate :: Maybe ItemDescription
  , priorityUpdate :: Maybe ItemPriority
  , dueByUpdate :: Maybe ItemDueBy
  } deriving Show

data Command =
  Info
  | Init
  | List
  | Add Item
  | View ItemIndex
  | Update ItemIndex ItemUpdate
  | Remove ItemIndex
  deriving Show

data Options = Options FilePath Command deriving Show
