nats :: [Int]

nats = iterate (+1) 0


fizzAux :: Int -> Either Int String

fizzAux x
    | (x `mod` 5 == 0) && (x `mod` 3 == 0) = Right "FizzBuzz"
    | x `mod` 5 == 0 = Right "Buzz"
    | x `mod` 3 == 0 = Right "Fizz"
    | otherwise = Left x


fizzBuzz :: [Either Int String]

fizzBuzz = map fizzAux nats
        