str2Message :: String -> String
str2Message = (++) "Data: "

str2Action :: String -> IO ()
str2Action = putStrLn . str2Message

numbers :: [Int]
numbers = [1..10]

main :: IO ()
main = do str2Action "Start of the programm"
          mapM_ (str2Action . show) numbers
          str2Action "Done"
