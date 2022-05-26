-- Nombres molt compostos

-- Here we avoid iterating for all numbers greaters than x/2 as we know they can't divide x
-- we append x at the end (for this reason the case divisors 1 has to be special because
-- it would yield [1,1]).
divisors :: Int -> [Int]

divisors 1 = [1] 
divisors x = (filter (\k -> x `mod` k == 0) [1..(ceiling $ fromIntegral x / 2.0)]) ++ [x]


nbDivisors :: Int -> Int

nbDivisors x = length $ divisors $ x


-- This function should be as efficient as possible. In this solution we take
-- advantage of lazy evaluation with function fold called with argument (&&),
-- when it sees a false, it automatically breakes and returns false.
-- Other aproaches may be valid for jutge.org
moltCompost :: Int -> Bool

moltCompost x = foldl (&&) True [nbDivisors i < xDivisors | i <- [x-1, x-2..1]]
    where xDivisors = nbDivisors x
