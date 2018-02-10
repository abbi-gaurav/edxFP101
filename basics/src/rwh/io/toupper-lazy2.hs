import           Data.Char (toUpper)
import           System.IO

main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  inputStr <- hGetContents inh
  hPutStr outh (map toUpper inputStr)
  hClose inh
  hClose outh
