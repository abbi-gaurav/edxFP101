curry3::((a,b)->c) -> a -> b -> c
curry3 f = \x y -> f (x,y)

uncurry1 :: (a -> b -> c) -> (a, b) -> c
uncurry1 f = \(x,y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
    | p x = []
    | otherwise =  h x : unfold p h t (t x)

int2bin = unfold (==0) (`mod` 2) (`div` 2)

map2 f = unfold null (f . head) tail
