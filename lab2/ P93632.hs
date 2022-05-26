eql :: [Int] -> [Int] -> Bool

eql as bs = all id (zipWith (==) as bs) && (length as == length bs)


prod :: [Int] -> Int

prod xs = foldl (*) 1 xs


prodOfEvens :: [Int] -> Int

prodOfEvens xs = prod $ filter even xs


powersOf2 :: [Int]

powersOf2 = iterate (* 2) 1


scalarProduct :: [Float] -> [Float] -> Float

scalarProduct as bs = foldl (+) 0 (zipWith (*) as bs)