module PasswdMap where

import           Control.Monad      (when)
import           Data.List
import qualified Data.Map           as Map
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           Text.Printf        (printf)

data PasswdEntry = PasswdEntry {
  userName :: String,
  password :: String,
  uid      :: Integer,
  gid      :: Integer,
  gecos    :: String,
  homeDir  :: String,
  shell    :: String
  } deriving (Eq, Ord)

instance Show PasswdEntry where
  show pe = printf "%s:%s:%d:%d:%s:%s:%s"
            (userName pe) (password pe) (uid pe) (gid pe) (gecos pe)(homeDir pe) (shell pe)

instance Read PasswdEntry where
  readsPrec _ value =
    case split ':' value of
         [f1,f2,f3,f4,f5,f6,f7] -> [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
         x                      -> error $ "invalid number of fields in the input: " ++ show x

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim list = let (before, remainder) = span (/= delim) list
                   in
                     before : case remainder of
                                [] -> []
                                x  -> split delim (tail x)

type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidMap, userMap)
  where uidMap   = Map.fromList . map (\pe -> (uid pe, pe)) $ enteries
        userMap  = Map.fromList . map (\pe -> (userName pe, pe)) $ enteries
        enteries = map read (filter (\str -> head str /= '#') . lines  $ inp)

bringUpMenu fileName = do
  content <- readFile fileName
  let maps = inputToMaps content
  mainMenu maps

mainMenu :: (UIDMap, UserMap) -> IO ()
mainMenu maps@(uidMap, userMap) = do
  putStr optionText
  hFlush stdout
  sel <- getLine
  case sel of
    "1" -> lookupUserName >> mainMenu maps
    "2" -> lookupUID >> mainMenu maps
    "3" -> displayFile >> mainMenu maps
    "4" -> return ()
    _   -> putStrLn "Invalid selection" >> mainMenu maps
  where
    lookupUserName = do
      putStrLn "Username: "
      userName <- getLine
      case Map.lookup userName userMap of
        Nothing -> putStrLn "Not found."
        Just x  -> print x
    lookupUID = do
      putStrLn "UID: "
      uidStr <- getLine
      case Map.lookup (read uidStr) uidMap of
        Nothing -> putStrLn "Not found."
        Just x  -> print x
    displayFile =  putStr . unlines . map ( show . snd )  . Map.toList $ uidMap
    optionText =
      "\npasswdmap options:\n\
      \\n\
      \1 Lookup username\n\
      \2 Lookup uid\n\
      \3 show file\n\
      \4 Quit\n\
      \Your Selection: "
