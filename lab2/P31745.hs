flatten :: [[Int]] -> [Int]

flatten as = foldl (++) [] as


myLength :: String -> Int

myLength word = foldl (+) 0 (map (const 1) word)


myReverse :: [Int] -> [Int]

myReverse xs = map (last) (take n $ iterate (init) xs)
    where n = length xs


myCount :: [Int] -> Int -> Int

myCount xs n = length $ filter (==n) xs


countIn :: [[Int]] -> Int -> [Int]
countIn xs n = map (flip myCount n) xs


firstWord :: String -> String

firstWord frase = takeWhile (/=' ') (dropWhile (==' ') frase)