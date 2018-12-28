{-# LANGUAGE Rank2Types, 
             MultiParamTypeClasses, 
             FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables #-}

module CodensityTransform ( 
    ) where 

-- Problems adapted from
-- http://blog.ezyang.com/2012/01/problem-set-the-codensity-transformation/
-- Illustrates the codensity technique to speed up execution of certain monads 

import Prelude hiding (abs)






-----------------------------------------------------------------------------
---- Warmup: Hughes lists
-------------------------------------------------------------------------------
--
---- Experienced Haskellers should feel free to skip this section.
--
---- We first consider the problem of left-associative list append.  In
---- order to see the difficulty, we will hand-evaluate a lazy language.
---- For the sake of being as mechanical as possible, here are the
---- operational semantics, where e1, e2 are expressions and x is a
---- variable, and e1[e2/x] is replace all instances of x in e1 with e2.
----
----        e1 ==> e1'
----   ---------------------
----     e1 e2 ==> e1' e2
----
----   (\x -> e1[x]) e2 ==> e1[e2/x]
----
---- For reference, the definition of append is as follows:
----
----      a ++ b = foldr (:) b a
----
---- Assume that, on forcing a saturated foldr, its third argument is
---- forced, as follows:
----
----                e1 ==> e1'
----    -----------------------------------
----      foldr f e2 e1 ==> foldr f e2 e1'
----
----  foldr f e2 (x:xs) ==> f x (foldr f e2 xs)
----
---- Hand evaluate this implementation by forcing the head constructor,
---- assuming 'as' is not null:
--

listsample as bs cs = (as ++ bs) ++ cs

-- Solution:
-- --
-- --        (as ++ bs) ++ cs
-- --      = foldr (:) cs (as ++ bs)
-- --      = foldr (:) cs (foldr (:) bs as)
-- --      = foldr (:) cs (foldr (:) bs (a:as'))
-- --      = foldr (:) cs (a : foldr (:) b as')
-- --      = a : foldr (:) cs (foldr (:) bs as')
-- --
-- -- Convince yourself that this takes linear time per append, and that
-- -- processing each element of the resulting tail of the list will also
-- -- take linear time.
--
-- -- We now present Hughes lists:


type Huges a = [a] -> [a] 

listrep :: Huges a -> [a] 
listrep f = f [] 

append :: Huges a -> Huges a -> Huges a 
append f g = f . g 


listsample' a b c = listrep ((a `append` b) `append` c)


-- Solution:
-- 
--         listrep (append (append a b) c)
--       = (\l -> l []) (append (append a b) c)
--       = (append (append a b) c) []
--       = (\z -> (append a b) (c z)) []
--       = (append a b) (c [])
--       = (\z -> a (b z)) (c [])
--       = a (b (c []))
-- 
-- Convince yourself that the result requires only constant time per
-- element, assuming a, b and c are of the form (\z -> a1:a2:...:z).
-- Notice the left-associativity has been converted into
-- right-associative function application.
--
-- The codensity transformation operates on similar principles.  This
-- ends the warmup.
--
-----------------------------------------------------------------------------
-- Case for leafy trees
-----------------------------------------------------------------------------
--
-- Some simple definitions of trees
--
data Tree a = Leaf a | Node (Tree a) (Tree a)


fullTree :: Int -> Tree Int 
fullTree 1 = Leaf 1 
fullTree n = do i <- fullTree (n-1) 
                Node (Leaf $ n-i) (Leaf $ i+1)

-- Here is the obvious monad definition for trees, where each leaf
-- is substituted with a new tree.

instance Functor Tree where 
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node t1 t2) = Node (f <$> t1) (f <$> t2) 

instance Applicative Tree where 
    pure = Leaf  
    (Leaf f) <*> (Leaf x) = Leaf (f x)
    (Leaf f) <*> (Node t1 t2) = Node (f <$> t1) (f <$> t2) 

instance Monad Tree where 
    return = Leaf 
    Leaf a >>= f = f a 
    (Node t1 t2) >>= f = Node (t1 >>= f) (t2 >>= f)



-- You should convince yourself of the performance problem with this
-- code by considering what happens if you force it to normal form.
--
sample = (Leaf 0 >>= f) >>= f
    where f n = Node (Leaf (n + 1)) (Leaf (n + 1))


-- Let's fix this problem.  Now abstract over the /leaves/ of the tree

newtype CTree a = CTree { 
    unCTree :: forall r. (a -> Tree r) -> Tree r 
  }

-- Please write functions which witness the isomorphism between the
-- abstract and concrete versions of trees.

treerep :: Tree a -> CTree a 
treerep (Leaf a) = CTree $ \f -> f a 
treerep (Node l r) = let (CTree f) = treerep l 
                         (CTree g) = treerep r 
                     in CTree (\h -> Node (f h) (g h))


treeabs :: CTree a -> Tree a 
treeabs (CTree f) = f Leaf 


instance Functor CTree where 
    fmap f (CTree g) = CTree $ \h -> g (h . f)

instance Applicative CTree where 
    pure x = CTree $ \f -> f x 
    (<*>) = undefined 
    --CTree f <*> CTree g = CTree $ \h -> 
                    --(($ f) :: _) undefined 

instance Monad CTree where 
    return = pure 
    (>>=) :: CTree a -> (a -> CTree b) -> CTree b 
    CTree f >>= g = CTree $ \h -> 
                      f (\a -> case g a of 
                                 CTree j -> j h)



-- How do you construct a node in the case of the abstract version?
-- It is trivial for concrete trees.

class Monad m => TreeLike m where 
    node :: m a -> m a -> m a 
    leaf :: a -> m a 
    leaf = return 


instance TreeLike Tree where 
    node = Node 

instance TreeLike CTree where 
    node (CTree l) (CTree r) = CTree $ \h -> Node (l h) (r h)

-- We now gain efficiency by operating on the /abstracted/ version as
-- opposed to the ordinary one.

treeImprove :: (forall m. TreeLike m => m a) -> Tree a 
treeImprove = treeabs 

-- You should convince yourself of the efficiency of this code.
-- Remember that expressions inside lambda abstraction don't evaluate
-- until the lambda is applied.

sample' = treeabs ((leaf 0 >>= f) >>= f)
    where f n = node (leaf (n + 1)) (leaf (n + 1))


---------------------------------------------------------------------------
-- General case
-----------------------------------------------------------------------------

-- Basic properties about free monads

data Free f a = Return a | Wrap (f (Free f a))

instance Functor f => Functor (Free f) where 
    fmap f (Return a) = Return (f a) 
    fmap f (Wrap m) = Wrap (fmap (fmap f) m)

instance Functor f => Applicative (Free f) where 
    pure = Return 

    Return f <*> Return a = Return (f a)
    Return f <*> Wrap m   = Wrap (fmap (fmap f) m)


instance Functor f => Monad (Free f) where
    return = Return 

    Return x >>= f = f x 
    Wrap m >>= f = Wrap (fmap (>>= f) m)


-- Leafy trees are a special case, with F as the functor. Please write
-- functions which witness this isomorphism.

data F a = N a a 

freeFToTree :: Free F a -> Tree a 
freeFToTree (Return x) = Leaf x 
freeFToTree (Wrap (N l r)) = Node (freeFToTree l) (freeFToTree r)

treeToFreeF :: Tree a -> Free F a 
treeToFreeF (Leaf x) = Return x 
treeToFreeF (Node l r) = Wrap $ N (treeToFreeF l) (treeToFreeF r)


-- We now define an abstract version of arbitrary monads, analogous to
-- abstracted trees.  Witness an isomorphism.

newtype C m a = C { 
    unC :: forall r. (a -> m r) -> m r
  } 

rep :: Monad m => m a -> C m a 
rep m = C $ \f -> m >>= f 

abs :: Monad m => C m a -> m a 
abs (C f) = f return 

instance Functor (C m) where 
    fmap f (C g) = C $ \h -> g (h . f)

instance Applicative (C m) where 
    pure x = C $ \f -> f x 
    C f <*> C g = undefined

instance Monad (C m) where 
    return = pure 
    (C f) >>= g = C $ \h -> 
                f (\a -> case g a of 
                           C j -> j h)

class (Functor f, Monad m) => FreeLike f m where 
    wrap :: f (m a) -> m a 

instance Functor f => FreeLike f (Free f) where 
    wrap :: f (Free f a) -> Free f a 
    wrap = Wrap


instance FreeLike f m => FreeLike f (C m) where 
    wrap :: f (C m a) -> C m a 
    wrap m = C $ \f -> wrap (fmap (\(C g) -> g f) m)









