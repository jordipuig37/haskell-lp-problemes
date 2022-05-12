-- EXERCISE 1

-- This function transforms single character roman numbers to their respective
-- integer value. Will be very useful for exercises 1 & 2
romanDigit :: Char -> Int

romanDigit 'I' = 1
romanDigit 'V' = 5
romanDigit 'X' = 10
romanDigit 'L' = 50
romanDigit 'C' = 100
romanDigit 'D' = 500
romanDigit 'M' = 1000
romanDigit _ = 0 -- an edge case to avoid problems


roman2int :: String -> Int

roman2int [] = 0
roman2int (d1:[]) = romanDigit d1
roman2int (d1:d2:digits)
    | n1 < n2 = n2 - n1 + roman2int digits
    | otherwise = n1 + roman2int (d2:digits)
    where
        n1 = romanDigit d1
        n2 = romanDigit d2


-- EXERCISE 2

-- This auxiliary function takes two integers and returns the second multiplied
-- by minus one if the absolute value of the first is greater than the second,
-- unaltered otherwise. Useful to decide if a digit (second argument) from a 
-- roman number should be added or subtracted knowing the previous one (first argument)
aux :: Int -> Int -> Int

aux y x
    | abs x > y = - y
    | otherwise =   y


roman2int' :: String -> Int

roman2int' number = sum $ scanr aux 0 digits
    where digits = map romanDigit number

-- EXERCISE 3

arrels :: Float -> [Float]

arrels x = iterate (taylor) x
    where taylor n = 1 / 2 * (n + x / n)

-- EXERCISE 4

-- This function returns the absolute distance between two floats, and the
-- second in a tuple
errorAndsecond :: Float -> Float -> (Float, Float)

errorAndsecond x y = (abs(x - y), y)


arrel :: Float -> Float -> Float

arrel x epsilon = snd $ head $ dropWhile (\tup -> fst tup >= epsilon) $ zipWith errorAndsecond (arrels x) (tail (arrels x))

-- EXERCISE 5

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance Show a => Show (LTree a) where
    show (Leaf x) = "{" ++ show x ++ "}"
    show (Node lt rt) = "<" ++ show lt ++ "," ++ show rt ++ ">"

-- EXERCISE 6

build :: [a] -> LTree a

build (x:[]) = Leaf x
build xs = Node (build $ take n xs) (build $ drop n xs)
    where n = length xs `div` 2 + length xs `mod` 2

-- EXERCISE 7

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))

zipLTrees (Leaf x) (Leaf y) = Just $ Leaf (x, y)
zipLTrees (Leaf x) (Node lt2 rt2) = Nothing
zipLTrees (Node lt1 rt1) (Leaf x) = Nothing
zipLTrees (Node lt1 rt1) (Node lt2 rt2) = do
    leftZipped <- zipLTrees lt1 lt2
    rightZipped <- zipLTrees rt1 rt2
    Just $ Node leftZipped rightZipped
