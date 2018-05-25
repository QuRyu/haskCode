module LeftistHeaps where 

data LeftistHeap a = Empty
                   | LH Int a (LeftistHeap a) (LeftistHeap a)

merge :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a 
merge Empty r = r 
merge l     Empty = l 
merge l@(LH _ x xl xr) r@(LH _ y yl yr) | x <= y    = mkLH x xl (merge xr r)
                                    | otherwise     = mkLH y yl (merge l yr)

mkLH :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a 
mkLH x l r | rank l >= rank r = LH (rank r + 1) x l r 
           | otherwise        = LH (rank l + 1) x r l 

rank :: LeftistHeap a -> Int 
rank Empty        = 0
rank (LH r _ _ _) = r 

insert :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a 
insert x lh = merge lh (LH 1 x Empty Empty)

insert' :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a 
insert' x Empty        = LH 1 x Empty Empty 
insert' x lt@(LH rk a l r) | x > a  = LH (rk+1) a l (insert' x r)
                           | x <= a = LH 1      x lt Empty


findMin :: LeftistHeap a -> Maybe a 
findMin Empty        = Nothing 
findMin (LH _ x _ _) = Just x 

deleteMin :: (Ord a) =>  LeftistHeap a -> LeftistHeap a 
deleteMin Empty = Empty 
deleteMin (LH _ _ l r) = merge l r 

fromList :: (Ord a) => [a] -> LeftistHeap a 
fromList xs = go $ map (\x -> LH 1 x Empty Empty) xs 
        where 
          go []      = Empty 
          go [a]     = a  
          go (a:b:s) = merge (merge a b) (go s)

