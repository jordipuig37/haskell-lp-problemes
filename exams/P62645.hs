s2i x = (read x) :: Int

main = do
    listOfStr <- fmap words getContents
    print (sum (map s2i listOfStr))