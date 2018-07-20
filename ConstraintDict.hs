{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
 
module ConstraintDict ( ) where 

data Dict p where 
    D :: p => Dict p 

instance Show (Dict p) where 
    showsPrec _ _ = showString "D"

newtype p :- q = Sub (p => Dict q)

instance Show (p :- q) where 
    showsPrec d _ = showParen (d > 10) $ showString "Sub"

infixr 8 <&> 
-- merge two dictionaries 
(<&>) :: Dict p -> Dict q -> Dict (p, q) 
(<&>) D D = D 

infixr 7 <-> 
-- cast the content of dictionary 
(<->) :: Dict p -> (p => Dict q) -> Dict q 
(<->) D dict = dict 

data Handler p a r = Handler (Dict p -> a -> r)

mkHandler :: (p => a -> r) -> Handler p a r 
mkHandler f = Handler $ \D -> f 

callHandler :: Handler p a r -> Dict p -> a -> r 
callHandler (Handler f) = f 

castHandler :: Handler q a r -> (p => Dict q) -> Handler p a r 
castHandler h cast = Handler $ \dict -> callHandler h (dict <-> cast)  

-- to recover the power of implicits, what is needed is a selector that chooses
-- different constraints and types based on the method 
-- the handler essentially needs to become a map with typeRep as the key 
--
-- The signature of the handler needs to instead change to 
-- Map TypeRep (Dict p -> a -> r) 
-- Problem 1: how to make a and TypeRep have the same type? 



