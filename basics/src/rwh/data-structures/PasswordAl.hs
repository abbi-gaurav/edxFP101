module PasswordAl where

import           Control.Monad      (when)
import           Data.List
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Syntax : passwd-al filename uid"
    exitFailure
  res <- doFind (args !! 0) (read (args !! 1))
  putStrLn res

doFind :: FilePath -> Integer -> IO String
doFind fileName uid = do
  content <- readFile fileName
  let userName = findByUUID content uid
  case userName of
    Just x  -> return x
    Nothing -> return "Could not find the UUID"

findByUUID :: String -> Integer -> Maybe String
findByUUID content uid =
  let al = map parseline . filter (\line -> head line /= '#') . lines $ content
      in lookup uid al

parseline :: String -> (Integer, String)
parseline input =
  let fields = split ':' input
  in (read (fields !! 2), fields !! 0)

split :: Eq a => a -> [a] -> [[a]]
split _ []      = []
split delim str =
  let (before, remainder) = span (/= delim) str
  in
    before : case remainder of
               [] -> []
               x  -> split delim (tail x)
