module BinomialHeaps where 


data Tree a = Node a [Tree a]

minNode :: Tree a -> a 
minNode (Node x _) = x 

nodes :: Tree a -> [Tree a]
nodes (Node _ xs) = xs 

type Rank = Int 
newtype BinomialHeap a = BH [(Rank, Tree a)]

rank :: (Rank, Tree a) -> Rank 
rank = fst 

tree :: (Rank, Tree a) -> Tree a 
tree = snd 

merge :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a 
merge (BH []) bh      = bh 
merge bh      (BH []) = bh
merge l@(BH (l':ls)) r@(BH (r':rs))
    | rank l' < rank r' = let (BH hp) = merge (BH ls) r 
                          in BH (l' : hp)
    | rank l' > rank r' = let (BH hp) = merge l (BH rs) 
                          in BH (r' : hp) 
    | otherwise         = insTree (rank l' + 1, linkTree (tree l') (tree r'))
                                  (merge (BH ls) (BH rs))

linkTree :: (Ord a) => Tree a -> Tree a -> Tree a 
linkTree l'@(Node l ls) r'@(Node r rs) 
    | l <= r = Node l (r':ls)
    | otherwise = Node r (l':rs)

insTree :: (Rank, Tree a) -> BinomialHeap a -> BinomialHeap a 
insTree n@(r, t) (BH (x:xs)) | r < rank x = BH (n:x:xs)
                           | otherwise  = let (BH ins) = insTree n (BH xs)
                                          in BH (x:ins)

findMin :: (Ord a) => BinomialHeap a -> a 
findMin (BH [t]) = minNode (tree t)
findMin (BH (x:xs)) = let min' = findMin (BH xs)
                      in if minNode (tree x) < min' 
                            then minNode (tree x) 
                            else min'

removeMinTree :: (Ord a) => BinomialHeap a -> (Tree a, BinomialHeap a)
removeMinTree (BH [x])    = ((tree x), BH [])
removeMinTree (BH (x:xs)) = let (t, BH bh) = removeMinTree (BH xs)
                            in if minNode (tree x) <= minNode t 
                                 then (tree x, BH xs) 
                                 else (t, BH (x:bh))

-- TODO: zip the rank into "reverse $ nodes t"
deleteMin :: (Ord a) => BinomialHeap a -> (a, BinomialHeap a)
deleteMin heap = let (t, bh) = removeMinTree heap
                    in (minNode t, 
                            merge (BH . reverse $ nodes t) bh)
