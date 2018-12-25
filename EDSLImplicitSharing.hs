module EDSLImplicitSharing (
    ) where 

import Control.Monad.State

----------------------------------------------
-- A DSL 
----------------------------------------------

data Exp = Add Exp Exp 
         | Var String 
         | Const Int 
    deriving Show


----------------------------------------------
-- A first attempt to solve sharing 
----------------------------------------------

type Label = Int 

data ExpL = AddL Label ExpL ExpL 
          | VarL Label String 
          | ConstL Label Int 
    deriving Show 

instance Eq ExpL where 
    e1 == e2 = label e1 == label e2 
        where 
          label (AddL l _ _) = l 
          label (VarL l _)   = l 
          label (ConstL l _) = l 

type ExpM = State Label ExpL

newLabelM :: State Label Label
newLabelM = do l <- get 
               put (l+1) 
               return l 

runExpM :: ExpM -> ExpL 
runExpM = fst . flip runState 0 

varM :: String -> ExpM 
varM s = newLabelM >>= \l -> return $ VarL l s 

addM :: ExpL -> ExpL -> ExpM
addM e1 e2 = newLabelM >>= \l -> return $ AddL l e1 e2 

constM :: Int -> ExpM
constM x = newLabelM >>= \l -> return $ ConstL l x

--mulM :: Int -> ExpL -> ExpM 
mulM 0 _ = constM 0 
mulM 1 x = return x  
mulM n x | n `mod` 2 == 0 = mulM (n `div` 2) =<< addM x x 
         | otherwise = addM x =<< mulM (n-1) x 

expM_mul4 mul = mulM 4 =<< varM "i1"

addM' :: ExpM -> ExpM -> ExpM 
addM' e1 e2 = do x <- e1 
                 y <- e2 
                 l <- newLabelM 
                 return $ AddL l x y  

mulM' :: Int -> ExpM -> ExpM 
mulM' 0 _ = constM 0 
mulM' 1 x = x  
mulM' n x | n `mod` 2 == 0 = mulM' (n `div` 2) $ addM' x x 
          | otherwise = addM' x $ mulM' (n-1) x 

expM_a' = addM' (constM 10) (varM "i1")

-- try it 
expM_mul4' = mulM' 4 expM_a'


---------------------------------------------
-- Finally tagless 
---------------------------------------------

class CExp repr where 
    constant :: Int -> repr Int 
    variable :: String -> repr Int 
    add :: repr Int -> repr Int -> repr Int 

newtype Expl t = Expl Exp

instance CExp Expl where 
    constant = Expl . Const
    variable = Expl . Var
    add (Expl e1) (Expl e2) = Expl $ Add e1 e2 

mul :: CExp repr => Int -> repr Int -> repr Int 
mul 0 _ = constant 0 
mul 1 x = x 
mul n x | n `mod` 2 == 0 = mul (n `div` 2) $ add x x 
        | otherwise      = add x $ mul (n-1) x 

---------------------------------------------
-- Detect implicit sharing 
---------------------------------------------

type NodeId = Int 

data Node = NConst Int 
          | NVar String 
          | NAdd NodeId NodeId 

-- use a data type BiMap to store hashed values and 
-- the key 



