data Tree a = Empty | Node a (Tree a) (Tree a)

data Forest a = Forest [Tree a]
    deriving (Show)

instance Show a => Show (Tree a) where
    show Empty = "()"
    show (Node x lt rt) = "(" ++ show lt ++ "," ++ show x ++ "," ++ show rt ++ ")"


instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node a lt rt) = Node (f a) (fmap f lt) (fmap f rt)


doubleT :: Num a => Tree a -> Tree a

doubleT tree = fmap (*2) tree


instance Functor Forest where
    fmap func (Forest listOfTrees) = Forest (map (fmap func) listOfTrees)


doubleF :: Num a => Forest a -> Forest a

doubleF (Forest listOfTrees) = Forest (fmap doubleT listOfTrees)
