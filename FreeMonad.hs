module FreeMonad where 

import Data.Function (fix)

data Toy b next = 
    Output b next 
  | Bell next 
  | Done 

newtype Fix f = Fix (f (Fix f))
