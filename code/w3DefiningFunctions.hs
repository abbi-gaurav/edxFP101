signum' :: Int -> Int
signum' n = if n < 0 then -1 else
               if n == 0 then 0 else 1


abs' n | n >= 0 = n
       | otherwise = -n

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False


(|&) :: Bool -> Bool -> Bool
True |& b = b
False |& _ = False

odds1 n = map f [0..n-1]
    where
      f x = x * 2 + 1

odds2 n = map (\x -> x*2 + 1) [0..n-1]
