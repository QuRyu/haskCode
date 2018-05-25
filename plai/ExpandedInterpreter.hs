module ExpandedInterpreter (
    ) where

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.Maybe

type Symbol = String 
type Number = Int 

data ExprC = NumC Int 
           | IdC Symbol

           | AppC ExprC ExprC -- the function applied and argument 
           | LamC Symbol ExprC -- function definition in lambda form 

           | PlusC ExprC ExprC 
           | MultC ExprC ExprC 

           -- core extension for mutable structures 
           | BoxC ExprC           -- equivalent to a constructor 
           | UnboxC ExprC         -- equivalent to a getter 
           | SetboxC ExprC ExprC  -- equivalent to a setter
                             -- first argument is the position of the box 
           | SeqC ExprC ExprC     -- equivalent to do notation in Haskell 


data Value = NumV Number 
           | ClosV Symbol ExprC Env -- closure: arg, body and env
           | BoxV Value 

numPlus :: Value -> Value -> Value 
numPlus (NumV l) (NumV r) = NumV (l+r)
numPlus _        _        = error "types do not match"

numMult :: Value -> Value -> Value
numMult (NumV l) (NumV r) = NumV (l*r)
numMult _        _        = error "types do not match"

newtype Env = Env [(Symbol, Value)]

extendEnv :: (Symbol, Value) -> Env -> Env 
extendEnv s (Env e) = Env (s:e)

lookupEnv :: Symbol -> Env -> Maybe Value 
lookupEnv s (Env e) = lookup s e 

bind :: Symbol -> Value -> (Symbol, Value) 
bind = (,)

class HasEnv a where
    getEnv :: a -> Env 

interp :: (MonadState Env m) => ExprC -> m Value 
interp (NumC n) = return (NumV n)
interp (IdC s)  = do env <- get 
                     return (fromMaybe (error "variable not found") $ lookupEnv s env)
interp (AppC f a) = do env <- get  
                       (ClosV arg body e) <- interp f 
                       appArg <- interp a 
                       let env' = extendEnv (bind arg appArg) e 
                       put env' 
                       interp body 
interp (LamC s e) = do env <- get  
                       return (ClosV s e env)
interp PlusC l r) = do l' <- interp l 
                       r' <- interp r 
                       return (numPlus l' r')
interp (MultC l r) = do l' <- interp l 
                        r' <- interp r 
                        return (numMult l' r')
                           

