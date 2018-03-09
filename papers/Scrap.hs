module Scrap ( 
    ) where 

{-# LANGUAGE RankNTypes #-}

import Data.Typeable 

data Company    = C [Department] deriving (Typeable, Show)
data Department = D Manager [Subunit] deriving (Typeable, Show)
data Subunit    = P Employee | PU Department deriving (Typeable, Show)
data Employee   = E Person deriving (Typeable, Show) 
data Person     = P Name Address 
data Salary     = S Float 

type Manager = Employee  
type Name    = String 
type Address = String 

class Typeable a => Term a where 
    gmapT :: (forall b. Term b => b -> b) -> a -> a 
    
instance Term Company where 
    gmapT f (C dept) = C (map f dept)

instance Term Department where 
    gmapT f (D m u) = D (f m) (f u)

instance Term Subunit where 
    gmapT f (P e) = P (f e) 
    gmapT f (PU d) = PU (f d)

instance Term Employee where 
    gmapT f (E n s) = E (f n) (f s)

mkT :: (Typeable a, Typeable b) => 
        (b -> b) -> a -> a 
mkT f = case cast T of 
            Just g  -> g 
            Nothing -> id 

everywhere :: Term a => 
              (forall b. Term b => b -> b) ->
              a -> a 
everywhere f x = f (gmapT (everywhere f) x)


