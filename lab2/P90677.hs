myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl func x0 [] = x0
myFoldl func x0 (xi:xs) = myFoldl func (func x0 xi) xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr func x0 [] = x0
myFoldr func x0 (xi : xs) = func xi (myFoldr func x0 xs)


myIterate :: (a -> a) -> a -> [a]

myIterate func x0 = [x0] ++ myIterate func (func x0)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a

myUntil pred func x0
    | pred x0   = x0
    | otherwise = myUntil pred func (func x0)


myMap :: (a -> b) -> [a] -> [b]

myMap func xs = [func i| i <- xs]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred xs = [i | i <- xs, pred i]


myAll :: (a -> Bool) -> [a] -> Bool

myAll pred xs = foldl (\x y -> x && (pred y)) True xs


myAny :: (a -> Bool) -> [a] -> Bool

myAny pred xs = foldl (\x y -> x || (pred y)) False xs


myZip :: [a] -> [b] -> [(a, b)]

myZip as [] = []
myZip [] as = []
myZip (a : as) (b : bs) = [(a, b)] ++ myZip as bs


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith func as bs = [func (fst x) (snd x) | x <- myZip as bs]