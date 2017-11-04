primes ::  [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve[x|x <- xs, x `mod` p /= 0]

fibs = 0 : 1 : [(x+y)|(x,y) <- zip fibs (tail fibs)]
