myLength :: [Int] -> Int

myLength [] = 0
myLength (_:xs) = 1 + myLength xs


myMaximum :: [Int] -> Int

myMaximum (n : []) = n
myMaximum (n : xr)
    | n >= rightMax = n
    | n < rightMax  = rightMax
        where
            rightMax = myMaximum xr


average :: [Int] -> Float

average (x :[])  = fromIntegral x
average (x : xs) = (fromIntegral x + (average xs) * fromIntegral (myLength xs)) / fromIntegral (myLength xs + 1)


buildPalindrome :: [Int] -> [Int]

buildPalindrome [] = []
buildPalindrome xs = [last xs] ++ (buildPalindrome (init xs)) ++ [last xs]


remove :: [Int] -> [Int] -> [Int]

remove x [] = x
remove x (y:ys) = remove [i | i <-x, i /= y] ys

flatten :: [[Int]] -> [Int]

flatten [] = []
flatten (elem:rest) = elem ++ (flatten rest)

oddsNevens :: [Int] -> ([Int],[Int])

oddsNevens x = ([i | i<-x, not (even i)], [j | j<-x, even j])


isPrime :: Int -> Bool

isPrime k = if k > 1
            then null [ x | x <- [2..floor (sqrt (fromIntegral (k)))], k `mod` x == 0]
            else False


primeDivisors :: Int -> [Int]

primeDivisors n = [i | i <- [2..n],
                       isPrime i, mod n i == 0]