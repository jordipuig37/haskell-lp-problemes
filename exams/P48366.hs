-- EXERCISE 1:
-- this function reads a string and casts it to integer type
s2i x = (read x) :: Int


-- this function indicates whether the input is an operator
isOperator :: String -> Bool
isOperator x = (x == "+") || (x == "-") || (x == "*") || (x == "/")


-- this function takes the queue of numbers, an operator and applies the operator
-- to the first two numbers. Then pushes the result back in.
resolveOperator :: [Int] -> String -> [Int]

resolveOperator (x:y:nums) op
    | op == "+" = (x + y) : nums
    | op == "-" = (y - x) : nums
    | op == "*" = (x * y) : nums
    | op == "/" = (y `div` x) : nums


-- this function evaluates in a recursive way the ...
evalRecursive :: [Int] -> [String] -> ([Int], [String])

evalRecursive (x:[]) [] = ([x], [])
evalRecursive nums (x: xs)
    | isOperator x = evalRecursive (resolveOperator nums x) (xs)
    | otherwise    = evalRecursive (s2i x: nums) (xs)


eval1 :: String -> Int

eval1 expr = head result
    where (result, _) = evalRecursive [] (words expr)

-- EXERCISE 2:

-- this function takes a list of ints and the next symbol of the expression.
-- if the next symbol is an operator it applies it to the two first elements of
-- the list, otherwise it puts the number at the beginning of the list
resolveIterative :: [Int] -> String -> [Int]

resolveIterative nums next
    | isOperator next = resolveOperator nums next
    | otherwise       = (s2i next) : nums


-- now do it without recursivity
eval2 :: String -> Int

eval2 expr = head $ foldl resolveIterative [] (words expr)


-- EXERCISE 3:

fsmap :: a -> [a -> a] -> a

fsmap x0 fs = foldl (flip ($)) x0 fs


-- EXERCISE 4:

divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b

divideNconquer base divide conquer x =
    case base x of
        Just x -> x
        Nothing -> conquer x (x1, x2) (y1, y2)
            where
                (x1, x2) = divide x
                y1 = divideNconquer base divide conquer x1
                y2 = divideNconquer base divide conquer x2

quickSort :: [Int] -> [Int]

quickSort = divideNconquer quickBase quickDivide quickConquer
    where
        quickBase [] = Just []
        quickBase (x:[]) = Just [x]
        quickBase (x:xs) = Nothing
        quickDivide (x:xs) = ([i | i <- xs, i <= x], [j | j <- xs, j > x])
        quickConquer (x:xs) (_, _) (smallers, greaters) = smallers ++ [x] ++ greaters


-- EXERCISE 5:

data Racional = Racional Integer Integer

instance Show Racional where
    show (Racional x y) = show x ++ "/" ++ show y

instance Eq Racional where
    (Racional x1 y1) == (Racional x2 y2) = x1 == x2 && y1 == y2


racional :: Integer -> Integer -> Racional
racional x y = Racional (x `div` n) (y `div` n)
    where n = gcd x y

numerador :: Racional -> Integer
numerador (Racional x y) = x

denominador :: Racional -> Integer
denominador (Racional x y) = y


-- EXERCISE 6:

data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

-- this function generates recursively a subtree of the calkin-wilf tree, given the root
calkinWilf :: Racional -> Tree Racional

calkinWilf (Racional x y) = Node (racional x y) (calkinWilf (racional x (x+y))) (calkinWilf (racional (x+y) y))


racionals :: [Racional]
racionals = recXnivells $ calkinWilf $ racional 1 1