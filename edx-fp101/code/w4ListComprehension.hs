import Data.Char

myConcatnate :: [[a]] -> [a]
myConcatnate xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], isPrime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [x <= y| (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

perfects n = [x | x <- [1..n], isPerfect x]
             where isPerfect num = sum (init (factors num)) == num

prb4 = concat [[(x,y) | y <- [4,5,6]]| x <-[1,2,3]]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions2 x xs = find x (zip xs [0..n])
                  where n = length xs - 1

scalarproduct xs ys = sum [x*y| (x,y) <- xs `zip` ys]
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift  :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = toUpper (shift n (toLower c))
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle xs ys = concat [[x,y]|(x,y) <- xs `zip` ys]

divides x n = x `mod` n == 0
divisors x = [d | d<- [1..x], x `divides` d]
