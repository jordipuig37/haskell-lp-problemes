insert :: [Int] -> Int -> [Int] 

insert [] new = [new]
insert (a : as) new
    | a > new   = [new] ++ [a] ++ as
    | otherwise = [a] ++ (insert as new)


isort :: [Int] -> [Int]

isort [] = []
isort (a : as) = insert (isort as) a


myMaximum :: [Int] -> Int

myMaximum (n : []) = n
myMaximum (n : xr)
    | n >= rightMax = n
    | n < rightMax  = rightMax
        where
            rightMax = myMaximum xr


remove :: [Int] -> Int -> [Int]

remove [] n = []
remove (a: as) n
    | a == n    = as
    | otherwise = [a] ++ (remove as n)


ssort :: [Int] -> [Int]

ssort [] = []
ssort as = ssort (remove as a) ++ [a]
    where a = myMaximum as


merge :: [Int] -> [Int] -> [Int]

merge [] bs = bs 
merge as [] = as
merge (a : as) (b : bs)
    | a >  b = [b] ++ (merge ([a]++as) bs)
    | a <= b = [a] ++ (merge as ([b]++bs))


splitHalf :: [Int] -> ([Int], [Int])

splitHalf xs = splitAt (floor (fromIntegral (length xs) / 2)) xs


msort :: [Int] -> [Int]

msort [] = []
msort (a : []) = [a]
msort xs = merge (msort xl) (msort xr)
    where (xl, xr) = splitHalf xs


qsort :: [Int] -> [Int]

qsort [] = []
qsort (a: as) = (qsort [i | i<- as, i<a]) ++ [a] ++ (qsort [j | j<- as, j>=a])


genQsort :: Ord a => [a] -> [a]

genQsort [] = []
genQsort (a: as) = (genQsort [i | i<- as, i<a]) ++ [a] ++ (genQsort [j | j<- as, j>=a])
