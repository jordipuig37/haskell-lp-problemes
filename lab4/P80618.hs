data Queue a = Queue [a] [a]
    deriving (Show)


create :: Queue a

create = Queue [] []


push :: a -> Queue a -> Queue a

push new (Queue cap cua) = Queue cap (new:cua)


pop :: Queue a -> Queue a

pop (Queue [] ys) = Queue (reverse (init ys)) []
pop (Queue xs ys) = Queue (tail xs) ys


top :: Queue a -> a

top (Queue [] xs) = last xs
top (Queue xs _)  = head xs


empty :: Queue a -> Bool

empty (Queue [] []) = True
empty (Queue _ _) = False


instance Eq a => Eq (Queue a) where
    (Queue xcap xcua) == (Queue ycap ycua) = (length xcap + length xcua) == (length ycap + length ycua) && 
        (all  id (zipWith (==) (xcap ++ (reverse xcua)) (ycap ++ (reverse ycua))))
