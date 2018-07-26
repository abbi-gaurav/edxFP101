module CSV2 where

import           Text.ParserCombinators.Parsec

eol :: GenParser Char st Char
eol = char '\n'

cell :: GenParser Char st String
cell = many(noneOf ",\n")

line :: GenParser Char st [String]
line = sepBy cell (char ',')

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
