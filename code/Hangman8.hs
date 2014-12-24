import System.IO

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-'| x <- xs]

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

sGetLine :: IO String
sGetLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '-'
                     xs <- sGetLine
                     return (x:xs)

guess :: String -> IO ()
guess word = do putStr ">"
                xs <- getLine
                if (xs == word) then
                    putStrLn "you got it"
                else
                    do putStrLn (diff word xs)
                       guess word

hangman :: IO ()
hangman = do putStrLn "think of a word"
             word <- sGetLine
             putStrLn "guess it"
             guess word
