-- lolol halt so geil hier 

posifyer :: [String] -> [(String, Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk+1)