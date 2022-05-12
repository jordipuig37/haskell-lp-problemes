import Control.Monad -- for exercise
-- EXERCISE 1

multEq :: Int -> Int -> [Int]
multEq x y = iterate (* (x*y)) 1

-- Another possible solution: multEq x y = [x**i * y**i| i <- [0..]]


-- EXERCISE 2

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 = filter ofirst l1
    where
        n2 = length l2
        n3 = length l3
        ofirst x = t2 < n2 && (n3==t3 || t2 < t3)
            where 
                t2 = length (takeWhile (/= x) l2)
                t3 = length (takeWhile (/= x) l3)


-- EXERCISE 3
-- Comment on solutions: three solutions are provided, they all do the same and
-- are accepted by the jutge. Some of them are harder to get your head around
-- but are more elegant.

myIterate :: (a -> a) -> a -> [a]

myIterate func x0 = scanl (const . func) x0 (repeat x0)

-- other solutions:
-- myIterate func x0 = scanl (\x _ -> func x) x0 (repeat 0)
-- myIterate func x0 = scanl (flip $ const func) x0 (repeat x0)


-- EXERCISE 4

type SymTab a = String -> Maybe a

empty :: SymTab a
empty = const Nothing

get :: SymTab a -> String -> Maybe a
get = ($)

set :: SymTab a -> String -> a -> SymTab a
set st key value x
    | x == key = Just value
    | otherwise = st x


-- EXERCISE 5

data Expr a
    = Val a
    | Var String
    | Sum (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving Show


eval :: (Num a) => SymTab a -> Expr a -> Maybe a

eval st (Val x) = Just x
eval st (Var n) = get st n
eval st (Sum x y) = liftM2 (+) (eval st x) (eval st y)
eval st (Sub x y) = liftM2 (-) (eval st x) (eval st y)
eval st (Mul x y) = liftM2 (*) (eval st x) (eval st y)
