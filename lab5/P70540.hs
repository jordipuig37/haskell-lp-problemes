import Control.Monad

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr


eval1 :: Expr -> Int

eval1 (Val x) = x
eval1 (Add x1 x2) = eval1 x1 + eval1 x2
eval1 (Sub x1 x2) = eval1 x1 - eval1 x2
eval1 (Mul x1 x2) = eval1 x1 * eval1 x2
eval1 (Div x1 x2) = div (eval1 x1) (eval1 x2)


intDivide :: Int -> Int -> Maybe Int

intDivide _ 0 = Nothing
intDivide x y = Just (div x y)


eval2 :: Expr -> Maybe Int

eval2 (Val x) = Just x
eval2 (Add x1 x2) = liftM2 (+) (eval2 x1) (eval2 x2)
eval2 (Sub x1 x2) = liftM2 (-) (eval2 x1) (eval2 x2)
eval2 (Mul x1 x2) = liftM2 (*) (eval2 x1) (eval2 x2)

eval2 (Div x1 x2) = do
    divisor <- eval2 x2
    dividend <- eval2 x1
    intDivide dividend divisor


intDivide2 :: Int -> Int -> Either String Int

intDivide2 _ 0 = Left "div0"
intDivide2 x y = Right (div x y)


eval3 :: Expr -> Either String Int

eval3 (Val x) = Right x
eval3 (Add x1 x2) = liftM2 (+) (eval3 x1) (eval3 x2)
eval3 (Sub x1 x2) = liftM2 (-) (eval3 x1) (eval3 x2)
eval3 (Mul x1 x2) = liftM2 (*) (eval3 x1) (eval3 x2)
eval3 (Div x1 x2) = do
    divisor <- eval3 x2
    dividend <- eval3 x1
    intDivide2 dividend divisor
