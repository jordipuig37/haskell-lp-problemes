resposta :: [Char] -> String
resposta x
    | final == 'a' = "Hola maca!"
    | final == 'A' = "Hola maca!"
    | otherwise = "Hola maco!"
    where final = last x

main = do
    nom <- getLine
    putStrLn $ resposta nom
