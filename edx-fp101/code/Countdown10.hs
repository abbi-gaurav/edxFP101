import Data.List
data Op = Add|Sub|Mul|Div
        deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = y /= 1 && x `mod` y == 0


data Expr = Val Int | App Op Expr Expr
          deriving Show

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y|x <- eval l, y <- eval r, valid o x y]


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r ) = values l ++ values r

choices :: [a] -> [[a]]
choices xs = (subsequences xs) >>= permutations

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs)|(ls,rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e| (ls, rs) <- split ns,
                           l <- exprs ls,
                           r <- exprs rs,
                           e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                             e <- exprs ns',
                             eval e == [n]]

-- lect 2
type Result = (Expr, Int)

combine2 :: Result -> Result -> [Result ]
combine2 (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Mul, Div, Sub], valid o x y]
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res| (ls, rs) <- split ns,
                               lx <- results ls,
                               ry <- results rs,
                               res <- combine2 lx ry
             ]

solutions2 :: [Int] -> Int -> [Expr]
solutions2 ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]




removeOne :: Eq a => a -> [a] -> [a]
removeOne x [] = []
removeOne x (y:ys) 
    | x == y = ys
    | otherwise = y : removeOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool 
isChoice [] _ = True
isChoice (x:xs) ys = elem x ys && isChoice xs (removeOne x ys)
