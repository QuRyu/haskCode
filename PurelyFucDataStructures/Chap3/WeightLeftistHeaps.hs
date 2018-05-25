module WeightLeftistHeaps where 

data Heap a = Empty 
            | Node Int a (Heap a) (Heap a)

weight :: Heap a -> Int 
weight Empty          = 0 
weight (Node n _ _ _) = n 

merge :: (Ord a) => Heap a -> Heap a -> Heap a 
merge Empty r@(Node _ _ _ _) = r
merge l@(Node _ _ _ _) Empty = l 
merge l@(Node _ l' ll lr) r@(Node _ r' rl rr) 
        | l' <= r' = mkH l' ll (merge lr r)
        | otherwise = mkH r' rl (merge l rr)

mkH :: a -> Heap a -> Heap a -> Heap a 
mkH root l r | weight l >= weight r = Node (weight l+1) root l r 
             | otherwise = Node (weight r+1) root r l 

insert :: (Ord a) => a -> Heap a -> Heap a 
insert elem hp = merge hp (Node 1 elem Empty Empty) 

findMin :: Heap a -> Maybe a 
findMin Empty          = Nothing 
findMin (Node _ x _ _) = Just x 

deleteMin :: (Ord a) => Heap a -> Heap a 
deleteMin Empty          = Empty 
deleteMin (Node _ _ l r) = merge l r 


