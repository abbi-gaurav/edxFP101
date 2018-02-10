import           Data.Char (toUpper)

toUpperLazy = do
  inpStr <- readFile "input.txt"
  writeFile "output.txt" (map toUpper inpStr)
