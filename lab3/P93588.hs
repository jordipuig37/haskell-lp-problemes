myMap :: (a -> b) -> [a] -> [b]

myMap fun xs = [fun i | i <- xs]


myFilter :: (a -> Bool) -> [a] -> [a]

myFilter pred xs = [i | i <- xs, pred i]


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith fun xs ys = [fun (fst t) (snd t) | t <- zip xs ys]


thingify :: [Int] -> [Int] -> [(Int, Int)]

thingify xs ys = [(i, j) | i <- xs, j <- ys, mod i j == 0]


factors :: Int -> [Int]

factors n = [i | i <- [1..n], mod n i == 0]