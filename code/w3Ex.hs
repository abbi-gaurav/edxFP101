--halve1 xs = (take n xs, drop n xs)
--           where n = (length xs) / 2

halve2 xs = splitAt(length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
           where n = length xs

halve5 xs = (take n xs, drop n xs)
            where n = length xs `div` 2


safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3  xs
          | null xs = []
          | otherwise = tail xs

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_ : xs) = xs

safetail8 = \ xs ->
              case xs of
                [] -> []
                (_ : xs) -> xs

b||& c 
 | b == c = c
 | otherwise = True
