module FuncRecs2 where
data FuncRec = FuncRec { name      :: String,
                         calc      :: Int -> Int,
                         namedCalc :: Int -> (String, Int)
                       }

mkFuncRec :: String -> (Int -> Int) -> FuncRec
mkFuncRec name f = FuncRec{ name = name,
                            calc = f,
                            namedCalc = \x -> (name, f x)}

plus5 = mkFuncRec "plus5" (+ 5)

always0 = mkFuncRec "always0" (\x -> 0)
