double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average xs = sum xs `div` length xs

product' [] = 1
product' (x:xs) = x * product' xs

qsort1 [] = []
qsort1 (x:xs) = reverse (reverse (qsort1 smaller) ++ [x] ++ reverse(qsort1 larger))
       where larger = [a | a <- xs , a > x ]
             smaller = [b | b <- xs, b <= x]
