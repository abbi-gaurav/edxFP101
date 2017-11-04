map1 f xs = [f x | x <- xs]

map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

filter1 p xs = [x | x <- xs, p x]

filter2 p [] = []
filter2 p (x:xs)
        | p x = x : rest
        | otherwise = rest
                      where rest = filter p xs
foldr1g :: (a -> b -> b) -> b -> [a] -> b
foldr1g f z [] = z
foldr1g f z (x:xs) = f x (foldr1g f z xs)

reverse1g = foldr1g (\x xs -> xs ++ [x]) []

--(+& ys) = foldr1g (:) ys
