import Data.Char

a :: IO(Char,Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x,y)

myGetLine :: IO String
myGetLine = do x <- getChar
               if ( x == '\n') then
                   return []
               else
                   do xs <- myGetLine
                      return (x:xs)

strlen::IO ()
strlen = do putStr "enter a line: "
            xs <- getLine
            putStr "the string has "
            putStr (show (length xs))
            putStrLn " characters"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn1 :: String -> IO ()
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putStrLn1 ""

putStrLn2 :: String -> IO ()
putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >> putChar '\n'

putStrLn3 :: String -> IO ()
putStrLn3 [] = putChar '\n'
putStrLn3 xs = putStr' xs >>= \x -> putChar '\n'

--putStrLn4 :: String -> IO ()
--putStrLn4 [] = putChar '\n'
--putStrLn4 xs = putStr' xs >> \x -> putChar '\n'

putStrLn5 :: String -> IO ()
putStrLn5 [] = putChar '\n'
putStrLn5 xs = putStr' xs >> putStr' "\n"

putStrLn6 :: String -> IO ()
putStrLn6 [] = putChar '\n'
putStrLn6 xs = putStr' xs >> putStrLn6 "\n"

getLine31 ::  IO String
getLine31 = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

interact1 :: (String -> String) -> IO ()
interact1 f = do input <- getLine31
                 putStrLn1 (f input)

sequence_2 :: Monad m => [m a] -> m ()
sequence_2 [] = return ()
sequence_2 (m:ms) = (foldl (>>) m ms) >> return ()

sequence_4 :: Monad m => [m a] -> m ()
sequence_4 [] = return ()
sequence_4 (m:ms) = m >> sequence_4 ms

sequence_5 :: Monad m => [m a] -> m ()
sequence_5 [] = return ()
sequence_5 (m:ms) = m >>= \ _ -> sequence_5 ms

sequence_7 :: Monad m => [m a] -> m()
sequence_7 ms = foldr (>>) (return ()) ms

f str = map (\c -> take (ord c - ord 'a' + 1) (repeat c)) str

sequence61 :: Monad m => [m a] -> m [a]
sequence61 [] = return []
sequence61 (m:ms) = m >>= \ a -> do as <- sequence61 ms
                                    return (a:as)

sequence65 :: Monad m => [m a] -> m [a]
sequence65 ms = foldr func (return []) ms
                where
                  func :: (Monad m) => m a -> m [a] -> m [a]
                  func m acc = do x <- m
                                  xs <- acc
                                  return (x:xs)

sequence66 :: Monad m => [m a] -> m [a]
sequence66 [] = return []
sequence66 (m:ms) = do a <- m
                       as <- sequence66 ms
                       return (a:as)
f2 c = take 3 (repeat c)

mapM71 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM71 f as = sequence66 (map f as)

mapM72 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM72 f [] = return []
mapM72 f (a:as) = f a >>= \ b -> mapM72 f as >>= \ bs -> return (b:bs)

mapM76 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM76 f [] = return []
mapM76 f (a:as) = do b <- f a
                     bs <- mapM76 f as
                     return (b:bs)

mapM77 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM77 f [] = return []
mapM77 f (a:as) = f a >>= \b -> do bs <- mapM77 f as
                                   return (b:bs)

mapM78 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM78 f [] = return []
mapM78 f (a:as) = f a >>= \b -> do bs <- mapM78 f as
                                   return (bs ++ [b])

f3 a = Just(a)

f4 a = Just(odd a)

filterM82 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM82 _ [] = return []
filterM82 p (x:xs) = do flag <- p x
                        ys <- filterM82 p xs
                        if flag then return (x:ys) else return ys

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do b <- f a x
                          foldLeftM f b xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (x:xs) = do acc <- foldRightM f b xs
                           f x acc

liftM111 :: Monad m => (a -> b) -> m a -> m b
liftM111 f m = do x <- m
                  return (f x)

liftM113 :: Monad m => (a -> b) -> m a -> m b
liftM113 f m = m >>= \a -> return (f a)

liftM115 :: Monad m => (a -> b) -> m a -> m b
liftM115 f m = m >>= \a -> m >>= \b -> return (f a)

liftM118 :: Monad m => (a -> b) -> m a -> m b
liftM118 f m = m >> \ a -> return (f a)

