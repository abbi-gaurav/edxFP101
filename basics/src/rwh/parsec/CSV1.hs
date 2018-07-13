module CSV1 where

import           Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}

eol :: GenParser Char st Char
eol = char '\n'

cellContent :: GenParser Char st String
cellContent = many(noneOf ",\n")

cells :: GenParser Char st [String]
cells =
  do first <- cellContent
     next <- remainingCells
     return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells)
                 <|> (return [])

line :: GenParser Char st [String]
line = do
  result <- cells
  eol
  return result

csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof
  return result

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown) " input
