import Data.List 


degree :: Eq a => [(a, a)] -> a -> Int

degree [] _ = 0
degree (edge:edges) x = connected + degree edges x
    where
        connected
            | fst edge == x || snd edge == x = 1
            | otherwise = 0


degree' :: Eq a => [(a, a)] -> a -> Int

degree' edges node = length $ filter (\x -> (fst x == node) || (snd x == node)) edges


-- the solution provided builds the list comprehension of the neighbours considering
-- the edges where the node is the first, and when the node is the second of the tuple
-- we don't need to remove duplicates for the assumtions given in the directions
neighbors :: Ord a => [(a, a)] -> a -> [a]

neighbors edges node = sort ([snd tup | tup <- edges, fst tup==node] ++ [fst tup | tup <- edges, snd tup ==node])
