module Main (main) where

import           Options.Applicative hiding(infoParser)

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String

data Item = Item {
  title :: ItemTitle
  , description :: ItemDescription
  , priority :: ItemPriority
  , dueBy ::  ItemDueBy
  } deriving Show

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

itemPriorityValueParser :: Parser String
itemPriorityValueParser =
  strOption(long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")

itemDueByValueParser :: Parser String
itemDueByValueParser = strOption(long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by date/time")

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
  Options dataPath command <- execParser (info (optionsParser)(progDesc "To-do list manager"))
  run dataPath command

run :: FilePath -> Command -> IO()
run dataPath Info = putStrLn "Info"
run dataPath Init = putStrLn "Init"
run dataPath List = putStrLn "List"
run dataPath (Add item) = putStrLn $ "Add: " ++ show item
run dataPath (View index) = putStrLn $ "View idx: " ++ show index
run dataPath (Update idx itemUpdate) = putStrLn $ "Update : idx " ++ show idx ++ " item to update : " ++ show itemUpdate 
run dataPath (Remove idx) = putStrLn $ "Remove idx: " ++ show idx
