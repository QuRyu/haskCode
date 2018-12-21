module Transformers where 

import Control.Monad.Identity 
import Control.Monad.Except 
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer
import Data.Maybe 
import qualified Data.Map as M


type Name = String 


data Exp = Lit Int 
         | Var Name 
         | Plus Exp Exp 
         | Abs Name Exp 
         | App Exp Exp 
  deriving (Show)


data Value = IntVal Int 
           | FunVal Env Name Exp 
  deriving (Show)

type Env = M.Map Name Value

exampleExp = Lit 12 `Plus` (App (Abs "x" ((Lit 2) `Plus` (Var "x"))) 
                                (Lit 4 `Plus` Lit 2))


-- refernece implementation 

eval0 :: Env -> Exp -> Value 
eval0 env (Lit i) = IntVal i 
eval0 env (Var n) = fromJust $ M.lookup n env 
eval0 env (Plus e1 e2) = let IntVal val1 = eval0 env e1 
                             IntVal val2 = eval0 env e2 
                         in IntVal (val1 + val2)
eval0 env (Abs n e) = FunVal env n e 
eval0 env (App e1 e2) = let val1 = eval0 env e1 
                            val2 = eval0 env e2 
                        in case val1 of 
                             (FunVal env' n body) 
                                -> eval0 (M.insert n val2 env') body


-- basic monadic style, the same functionality as the reference one 

type Eval1 a = Identity a 

runEval1 :: Eval1 a -> a 
runEval1 = runIdentity
                          

eval1 :: Env -> Exp -> Eval1 Value 
eval1 env (Lit i)      = return $ IntVal i 
eval1 env (Var n)      = return . fromJust $ M.lookup n env 
eval1 env (Plus e1 e2) = do IntVal val1 <- eval1 env e1 
                            IntVal val2 <- eval1 env e2 
                            return $ IntVal (val1 + val2)
eval1 env (Abs n e)    = return $ FunVal env n e 
eval1 env (App e1 e2)  = do val1 <- eval1 env e1 
                            val2 <- eval1 env e2 
                            case val1 of 
                              (FunVal env' n body) 
                                 -> eval1 (M.insert n val2 env') body
                             

-- add error handling 

type Eval2 a = ExceptT String Identity a 

runEval2 :: Eval2 a -> Either String a  
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> Eval2 Value 
eval2 env (Lit i)      = return $ IntVal i 
eval2 env (Var n)      = case M.lookup n env of 
                           Just v -> return v 
                           Nothing -> throwError ("unbound variable: " ++ n)
eval2 env (Plus e1 e2) = do val1 <- eval2 env e1 
                            val2 <- eval2 env e2 
                            case (val1, val2) of 
                              (IntVal v1, IntVal v2) -> return $ IntVal (v1+v2)
                              _         -> throwError "type errror in addition"
eval2 env (Abs n e)    = return $ FunVal env n e 
eval2 env (App e1 e2)  = do val1 <- eval2 env e1 
                            val2 <- eval2 env e2 
                            case val1 of 
                              (FunVal env' n body) 
                                 -> eval2 (M.insert n val2 env') body
                              _  -> throwError "type errror in application"
        

-- add ReaderT to hide explicit environemnt passing

type Eval3 a = ReaderT Env (ExceptT String Identity) a 

runEval3 :: Eval3 a -> Either String a 
runEval3 = runIdentity . runExceptT . flip runReaderT M.empty 

eval3 ::  Exp -> Eval3 Value 
eval3 (Lit v)      = return $ IntVal v 
eval3 (Var n)      = do env <- ask 
                        case M.lookup n env of 
                          Just v -> return v 
                          Nothing -> throwError ("unbound variable: " ++ n)
eval3 (Plus e1 e2) = do env <- ask 
                        val1 <- eval3 e1 
                        val2 <- eval3 e2 
                        case (val1, val2) of 
                          (IntVal v1, IntVal v2) -> return $ IntVal (v1+v2)
                          _         -> throwError "type errror in addition"
eval3 (Abs n e)    = do env <- ask 
                        return $ FunVal env n e 
eval3 (App e1 e2)  = do env <- ask 
                        val1 <- eval3 e1 
                        val2 <- eval3 e2 
                        case val1 of 
                          (FunVal env' n body) 
                            -> local (const $ M.insert n val2 env') 
                                     (eval3 body)
                          _ -> throwError "type errror in application"



-- add state to count how many operations we have performed 

type Eval4 a = ReaderT Env (ExceptT String (StateT Int Identity)) a 

runEval4 :: Env -> Int -> Eval4 a -> (Either String a, Int) 
runEval4 env count ev = runIdentity . flip runStateT count 
                          . runExceptT $ runReaderT ev env 

tick :: (Num s, MonadState s m) => m () 
tick = do count <- get 
          put (count + 1) 

eval4 :: Exp -> Eval4 Value 
eval4 (Lit i)      = do tick
                        return $ IntVal i 
eval4 (Var n)      = do tick 
                        env <- ask 
                        case M.lookup n env of 
                          Just v -> return v 
                          Nothing -> throwError ("unbound variable: " ++ n)
eval4 (Plus e1 e2) = do tick
                        env <- ask 
                        val1 <- eval4 e1 
                        val2 <- eval4 e2 
                        case (val1, val2) of 
                          (IntVal v1, IntVal v2) -> return $ IntVal (v1+v2)
                          _         -> throwError "type errror in addition"
eval4 (Abs n e)    = do tick 
                        env <- ask 
                        return $ FunVal env n e 
eval4 (App e1 e2)  = do env <- ask 
                        val1 <- eval4 e1 
                        val2 <- eval4 e2 
                        case val1 of 
                          (FunVal env' n body) 
                            -> local (const $ M.insert n val2 env') 
                                     (eval4 body)
                          _ -> throwError "type errror in application"


-- add logging information 

type Eval5 a = ReaderT Env (ExceptT String 
                                (WriterT [String] (StateT Int Identity))) a
 
runEval5 :: Env -> Int -> Eval5 a -> ((Either String a, [String]), Int)
runEval5 env count ev = runIdentity . flip runStateT count . runWriterT 
                            . runExceptT $ runReaderT ev env 

eval5 :: Exp -> Eval5 Value 
eval5 (Lit i)   = do tick 
                     return $ IntVal i 
eval5 (Var n)   = do tick 
                     tell [n] 
                     env <- ask 
                     case M.lookup n env of 
                       Just v  -> return v 
                       Nothing -> throwError ("unbound variable" ++ n) 
eval5 (Plus e1 e2) = do tick 
                        val1 <- eval5 e1 
                        val2 <- eval5 e2 
                        case (val1, val2) of 
                          (IntVal v1, IntVal v2) -> return $ IntVal (v1+v2) 
                          _     -> throwError "type error in addition" 
eval5 (Abs n e) = do tick 
                     env <- ask 
                     return $ FunVal env n e 
eval5 (App e1 e2) = do tick 
                       val1 <- eval5 e1 
                       val2 <- eval5 e2 
                       case val1 of 
                         FunVal env' n body -> 
                            local (const (Map.insert n val2 env'))
                              (eval5 body) 
                         _ -> throwError "type error in application" 




            
