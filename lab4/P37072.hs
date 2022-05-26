import Data.List

data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving (Show)


size :: Tree a -> Int 

size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt


height :: Tree a -> Int

height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)


equal :: Eq a => Tree a -> Tree a -> Bool

equal Empty Empty = True
equal Empty notempty = False
equal notempty Empty = False
equal (Node x left1 right1) (Node y left2 right2) = x == y && (equal left1 left2) && (equal right1 right2)


isomorphic :: Eq a => Tree a -> Tree a -> Bool

isomorphic Empty Empty = True
isomorphic Empty notempty = False
isomorphic notempty Empty = False
isomorphic (Node x left1 right1) (Node y left2 right2) = x == y &&
    ((isomorphic left1 left2) && (isomorphic right1 right2) || (isomorphic left1 right2) && (isomorphic right1 left2))


preOrder :: Tree a -> [a]

preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right


inOrder :: Tree a -> [a]

inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right


postOrder :: Tree a -> [a]

postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

-- This function returns a list of pairs (leve, data) for each node in the
-- given tree. Needs to be called as: getLevels tree 0.
getLevels :: Tree a -> Int -> [(Int, a)]

getLevels Empty _ = []
getLevels (Node x left right) level = [(level, x)] ++ getLevels left (level+1) ++ getLevels right (level+1)


breadthFirst :: Tree a -> [a]

breadthFirst tree = map (snd) $ (sortBy (\a b -> compare (fst a) (fst b)) (getLevels tree 0))


build :: Eq a => [a] -> [a] -> Tree a

build [] [] = Empty
build (x:xs) ys = Node x (build leftPre leftIn) (build rightPre rightIn)
    where
        leftIn = takeWhile (/=x) ys
        rightIn = tail $ dropWhile (/=x) ys
        n = length leftIn
        leftPre = take n xs
        rightPre = drop n xs


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a

overlap func Empty Empty = Empty
overlap func notempty Empty = notempty
overlap func Empty notempty = notempty
overlap func (Node x left1 right1) (Node y left2 right2) = 
    Node (func x y) (overlap func left1 left2) (overlap func right1 right2)
