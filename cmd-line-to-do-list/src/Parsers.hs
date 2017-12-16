module Parsers where

import Data.Time
import Options.Applicative hiding (infoParser)
import Types

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser =
  Item <$> itemTitleValueParser <*> optional itemDescriptionValueParser <*>
  optional itemPriorityValueParser <*>
  optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser =
  ItemUpdate <$> optional updateItemTitleParser <*>
  optional updateItemDescriptionParser <*>
  optional updateItemPriorityParser <*>
  optional updateDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
  Just <$> itemPriorityValueParser <|> flag' Nothing (long "clear-priority")

updateDueByParser :: Parser ItemDueBy
updateDueByParser =
  Just <$> itemDueByValueParser <|> flag' Nothing (long "clear-due-by")

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser =
  subparser $
  mconcat
    [ command "info" (info infoParser (progDesc "Show Info"))
    , command "init" (info initParser (progDesc "Initialize Items"))
    , command "list" (info listParser (progDesc "List Items"))
    , command "add" (info addParser (progDesc "Add Item"))
    , command "view" (info viewParser (progDesc "View Item"))
    , command "update" (info updateParser (progDesc "Update Item"))
    , command "remove" (info removeParser (progDesc "Remove Item"))
    ]

dataPathParser :: Parser FilePath
dataPathParser =
  strOption $
  value defaultDataPath <> long "data-path" <> short 'p' <> metavar "DATAPATH" <>
  help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption
    (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

itemTitleValueParser :: Parser String
itemTitleValueParser =
  strOption (long "title" <> short 't' <> metavar "TITLE" <> help "title")

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser =
  option
    readPriority
    (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")
  where
    readPriority =
      eitherReader $ \arg ->
        case arg of
          "1" -> Right Low
          "2" -> Right Normal
          "3" -> Right High
          _ -> Left $ "Invalid Priority Value" ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser =
  option
    readDateTime
    (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by date/time")
  where
    readDateTime =
      eitherReader $ \arg ->
        case parseDateTimeMaybe arg of
          (Just dateTime) -> Right dateTime
          Nothing ->
            Left $ "Date time string must be in " ++ dateTimeFormat ++ "."
    parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
    dateTimeFormat = "%Y/%m/%d %H:%M:%S"

updateItemDescriptionaParser :: Parser ItemDescription
updateItemDescriptionaParser =
  Just <$> itemDescriptionValueParser <|> flag' Nothing (long "clear-desc")

optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> commandParser
