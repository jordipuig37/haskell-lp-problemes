countIf :: (Int -> Bool) -> [Int] -> Int

countIf pred xs = foldl (\x y -> x + fromEnum (pred y)) 0 xs


pam :: [Int] -> [Int -> Int] -> [[Int]]

pam xs funcs = [map fun xs | fun <- funcs]


pam2 :: [Int] -> [Int -> Int] -> [[Int]]

pam2 xs funcs = [(zipWith (\x f -> f x) (take n [i,i..]) funcs) | i <- xs]
    where n = length funcs


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int

filterFoldl pred func x0 xs = foldl func x0 (filter pred xs)


insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]

insert relation xs k = (takeWhile (flip relation k) xs) ++ [k] ++ (dropWhile (flip relation k) xs)


insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]

insertionSort relation xs = foldl (insert relation) [] xs

