module Main (main) where

import           Commands
import           Data.String.Utils
import           Helpers
import           Options.Applicative hiding(infoParser)
import           Parsers
import           System.Directory
import           Types

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


