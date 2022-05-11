absValue :: Int -> Int

absValue n
    | n >= 0    =  n 
    | otherwise = -n 


power :: Int -> Int -> Int

power _ 0 = 1
power x n
    | even n = y * y
    | odd  n = y * y * x
    where
        y = power x (div n 2)



isPrime :: Int -> Bool

isPrime k = if k > 1
            then null [ x | x <- [2..floor (sqrt (fromIntegral (k)))], k `mod` x == 0]
            else False


slowFib :: Int -> Int

slowFib 0 = 0
slowFib 1 = 1
slowFib n = (slowFib (n-1)) + (slowFib (n-2))


quickFib :: Int -> Int

quickFib n = sum (fibo (n))
    where
        fibo 0 = (0, 0)
        fibo 1 = (0, 1)
        fibo n = (f2, f1+f2)
            where
                (f1, f2) = fibo (n-1)
