{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}

module Predicates where 

import GHC.Exts (Constraint)

type Name = String 
data Pred a = Leaf Name (a -> Bool) 
            | And (Pred a) (Pred a)
            | Or (Pred a) (Pred a)
            | Not (Pred a)

data Pred' (c :: * -> Constraint) where 
    Leaf' :: Name -> (forall a . c a => a -> Bool) -> Pred' c 
    And' :: Pred' c1 -> Pred' c2 -> Pred' (c1 /\ c2) 
    Or' :: Pred' c1 -> Pred' c2 -> Pred' (c1 /\ c2) 
    Not' :: Pred' c -> Pred' c 

class (c1 a, c2 a ) => (/\) c1 c2 a
instance (c1 a, c2 a) => (/\) c1 c2 a 


data User = U { 
    userName :: String, 
    isRegistered :: Bool
    } deriving (Eq, Show)

data Post = P { 
    isPublic :: Bool, 
    canCommentAnonymously :: Bool,
    author :: User
    } deriving (Eq, Show)

data Comment = C { 
    commentAuthor :: User, 
    commentText :: String
    } deriving (Eq, Show)

--data World = W {
--    user :: User, 
--    post :: Post
--    }

class HasUser a where 
    user :: a -> User 

class HasPost a where 
    post :: a -> Post 

class HasComment a where 
    comment :: a -> Comment

class Get r a where 
    get :: a -> r 

-- userIsRegistered :: Pred World 
-- userIsRegistered :: (HasUser a) => Pred a 
userIsRegistered :: Pred' (Get User) 
userIsRegistered = Leaf "userIsRegistered" (isRegistered . user)

-- postIsPublic :: Pred World 
-- postIsPublic :: (HasPost a) => Pred a 
postIsPublic :: Pred' (Get Post) 
postIsPublic = Leaf "postIsPublic" (isPublic . post)

-- userIsAuthor :: Pred World 
-- userIsAuthor :: (HasPost a, HasUser a) => Pred a 
userIsAuthor :: Pred' (Get User /\ Get Post)
userIsAuthor = Leaf "userIsAuthor" (\w -> user w == author (post w))

-- postAllowsAnonComments :: Pred World 
-- postAllowsAnonComments :: (HasPost a) => Pred a 
postAllowsAnonComments :: Pred' (Get Post)
postAllowsAnonComments = Leaf "postAllowsAnonComments" (canCommentAnonymously . post) 

-- userCanComment :: Pred World 
-- userCanComment :: (HasPost a, HasUser a) => Pred a 
userCanComment :: Pred' (Get User /\ Get Post)
userCanComment = (userIsRegistered `Or` postAllowsAnonComments) `And` postIsPublic

-- userCanEditPost :: Pred World 
-- userCanEditPost :: (HasUser a, HasPost a) => Pred a 
userCanEditPost :: Pred' (Get User /\ Get Post)
userCanEditPost = userIsAuthor `And` userIsRegistered 

fold :: (Name -> (a -> Bool) -> b) ->  -- map leaves 
        (b -> b -> b) ->               -- combine And nodes 
        (b -> b -> b) ->               -- combine Or nodes 
        (b -> b) ->                    -- transform Not nodes 
        Pred a -> b
fold leaf and or not (Leaf name f) = leaf name f 
fold leaf and or not (And l r) = fold leaf and or not l `and` fold leaf and or not r 
fold leaf and or not (Or l r) = fold leaf and or not l `or` fold leaf and or not r 
fold leaf and or not (Not p) = not $ fold leaf and or not p

eval :: Pred a -> a -> Bool 
eval p x = fold (\name f -> f x) (&&) (||) (not) p  

prettyPrint :: Pred a -> String 
prettyPrint = fold leaf and or not 
    where 
      leaf name f = name 
      and l r = "(" ++ l ++ " AND " ++ r ++ ")"
      or l r = "(" ++ l ++ " OR " ++ r ++ ")"
      not x = "(NOT " ++ x ++ ")"


type f ~> g = forall a . f a -> g a 
fold' :: 
    (forall c . String -> (forall a . c a => a -> Bool) -> f c) -> -- map leaves 
    (forall c1 c2 . f c1 -> f c2 -> f (c1 /\ c2)) ->               -- combine And nodes 
    (forall c1 c2 . f c1 -> f c2 -> f (c1 /\ c2)) -> 
    (forall c . f c -> f c) -> 
    Pred' ~> f 
