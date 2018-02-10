import           Data.Char (toUpper)
import           System.IO

main :: IO()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  mainLoop inh outh
  hClose inh
  hClose outh

mainLoop :: Handle -> Handle -> IO()
mainLoop inh outh =
  do ineof <- hIsEOF inh
     if ineof
       then return()
       else do inputString <- hGetLine inh
               hPutStrLn outh (map toUpper inputString)
               mainLoop inh outh
