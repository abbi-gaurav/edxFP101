{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.Safe ((!!))
import           Data.String.Utils
import           Data.Time
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative hiding(infoParser)
import           Prelude hiding ((!!))
import           System.Directory
import           System.IO.Error

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

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser::Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
  <$> itemTitleValueParser
  <*> optional itemDescriptionValueParser
  <*> optional itemPriorityValueParser
  <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
  <$> optional updateItemTitleParser
  <*> optional updateItemDescriptionParser
  <*> optional updateItemPriorityParser
  <*> optional updateDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
  Just <$> itemPriorityValueParser
  <|> flag' Nothing (long "clear-priority")

updateDueByParser :: Parser ItemDueBy
updateDueByParser =
  Just <$> itemDueByValueParser
  <|> flag' Nothing (long "clear-due-by")

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info" (info infoParser (progDesc "Show Info"))
  , command "init" (info initParser (progDesc "Initialize Items"))
  , command "list" (info listParser (progDesc "List Items"))
  , command "add" (info addParser (progDesc "Add Item"))
  , command "view" (info viewParser (progDesc "View Item"))
  , command "update" (info updateParser (progDesc "Update Item"))
  , command "remove" (info removeParser (progDesc "Remove Item"))
  ]

dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "data-path"
  <> short 'p'
  <> metavar "DATAPATH"
  <> help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption(long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

itemTitleValueParser :: Parser String
itemTitleValueParser =
  strOption(long "title" <> short 't' <> metavar "TITLE" <> help "title")

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser = option readPriority (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")
  where readPriority = eitherReader $ \arg ->
          case arg of
            "1" -> Right Low
            "2" -> Right Normal
            "3" -> Right High
            _ -> Left  $ "Invalid Priority Value" ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser = option readDateTime(long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by date/time")
  where
    readDateTime = eitherReader $ \arg ->
      case parseDateTimeMaybe arg of
        (Just dateTime) -> Right dateTime
        Nothing -> Left $ "Date time string must be in " ++ dateTimeFormat ++ "."
    parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
    dateTimeFormat = "%Y/%m/%d %H:%M:%S"

updateItemDescriptionaParser :: Parser ItemDescription
updateItemDescriptionaParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc")

optionsParser :: Parser Options
optionsParser = Options
  <$> dataPathParser
  <*> commandParser

main :: IO ()
main = do
  Options dataPath command <- execParser(info (optionsParser)(progDesc "To-do list manager"))
  homeDir <- getHomeDirectory
  let expandedDataPath = replace "~" homeDir dataPath
  run expandedDataPath command

run :: FilePath -> Command -> IO()
run dataPath Info = showInfo dataPath
run dataPath Init = initItems dataPath
run dataPath List = viewItems dataPath
run dataPath (Add item) = addItem dataPath item
run dataPath (View index) = viewItem dataPath index
run dataPath (Update idx itemUpdate) = updateItem dataPath idx itemUpdate
run dataPath (Remove idx) = removeItem dataPath idx

showInfo :: FilePath -> IO()
showInfo dataPath = do
  putStrLn $ "Data file path " ++ dataPath
  exists <- doesFileExist dataPath
  if exists
    then do
    s <- BS.readFile dataPath
    let mbToDoList = Yaml.decode s
    case mbToDoList of
      Nothing -> putStrLn "Status: file is invalid"
      Just (ToDoList items) -> putStrLn $ "Status: contains " ++ show(length items) ++ " items"
    else putStrLn "File does not exist"

initItems :: FilePath -> IO()
initItems dataPath = writeToDoList dataPath (ToDoList [])
    
writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath todoList = BS.writeFile dataPath (Yaml.encode todoList)

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
  mbToDoList <-  catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (BS.readFile dataPath >>= return . Yaml.decode)
    (\_ -> return $ Just(ToDoList []))
  case mbToDoList of
    Nothing -> error "YAML is corrupt"
    Just todoList -> return todoList

viewItem :: FilePath -> ItemIndex -> IO()
viewItem dataPath index = do
  ToDoList items <- readToDoList dataPath
  let mbItem = items !! index
  case mbItem of
    Nothing -> putStrLn "Invalid item index"
    Just item -> showItem index item

showItem :: ItemIndex -> Item -> IO()
showItem idx (Item title mbDescription mbPriority mbDueby) = do
  putStrLn $ "[" ++ show idx ++ "] " ++ title
  putStr " Description: "
  putStrLn $ showField id mbDescription
  putStr " Priority: "
  putStrLn $ showField show mbPriority
  putStr " Due by: "
  putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueby

showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "not set"

addItem :: FilePath -> Item -> IO()
addItem dataPath item = do
  ToDoList items <- readToDoList dataPath
  let newTodoList = ToDoList(item : items)
  writeToDoList dataPath newTodoList

removeItem :: FilePath -> ItemIndex -> IO()
removeItem dataPath index = do
  ToDoList items <- readToDoList dataPath
  let mbItems = items `removeAt` index
  case mbItems of
    Nothing -> putStrLn "invalid item index"
    Just items' -> writeToDoList dataPath (ToDoList items')

updateItem :: FilePath -> ItemIndex -> ItemUpdate -> IO()
updateItem dataPath index (ItemUpdate mbTitle mbDesc mbPriority mbDueby) = do
  ToDoList items <- readToDoList dataPath
  let update (Item title desc priority dueBy) = Item
        (updateField mbTitle title)
        (updateField mbDesc desc)
        (updateField mbPriority priority)
        (updateField mbDueby dueBy)
      updateField (Just value) _ = value
      updateField Nothing value = value
      mbItems = updateAt items index update
  case mbItems of
    Nothing -> putStrLn "invalid item index"
    Just items' -> writeToDoList dataPath (ToDoList items')

removeAt :: [a] -> Int -> Maybe [a]
removeAt list idx = case list !! idx of
                      Nothing -> Nothing
                      _ -> Just (delNth idx list)

updateAt :: [a] -> Int -> (a->a) -> Maybe [a]
updateAt list idx f = case list !! idx of
                        Nothing -> Nothing
                        _ -> Just (updateNth idx f list)

delNth :: Int -> [a] -> [a]
delNth _ [] = []
delNth idx (x:xs)
  | idx == 0  = xs
  | otherwise = x : delNth (idx -1) xs

updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth _ _ [] = []
updateNth idx f (x:xs)
  | idx == 0  = (f(x) : xs)
  | otherwise = x : updateNth (idx-1) f xs

viewItems :: FilePath -> IO()
viewItems dataPath = do
  ToDoList items <- readToDoList dataPath
  let z = zip [0..] items
  forM_ z (\(idx, item) -> showItem idx item)
