module Commands where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.Safe ((!!))
import           Data.String.Utils
import           Data.Time
import qualified Data.Yaml as Yaml
import           Helpers
import           Prelude hiding ((!!))
import           System.Directory
import           System.IO.Error
import           Types

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

viewItems :: FilePath -> IO()
viewItems dataPath = do
  ToDoList items <- readToDoList dataPath
  let z = zip [0..] items
  forM_ z (\(idx, item) -> showItem idx item)
