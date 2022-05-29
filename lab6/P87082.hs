s2f x = (read x) :: Float


interp :: Float -> Float -> String
interp m h
    | imc < 18 = "magror"
    | imc < 25 = "corpulencia normal"
    | imc < 30 = "sobrepes"
    | imc < 40 = "obesitat"
    | otherwise = "obesitat morbida"
    where imc = m / (h*h)


resp :: String -> String
resp s = nom ++ ": " ++ interp (s2f m) (s2f h)
    where [nom, m, h] = words s

main = do
    line <- getLine
    if line /= "*" then do
        putStrLn $ resp line
        main
    else
        return ()
