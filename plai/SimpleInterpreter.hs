module SimpleInterpreter (
    ) where 

-- this is the core language of this interpreter 
--data ArithC = NumC Int 
--            | PlusC ArithC ArithC 
--            | MultC ArithC ArithC
--
--interp :: ArithC -> Int 
--interp (NumC n) = n 
--interp (PlusC l r) = (interp l) + (interp r) 
--interp (MultC l r) = (interp l) * (interp r)


-- | an example of desugaring 
-- the exapanded surface language, ArithS, will be 
-- desugared into ArithC to preserve the stability of 
-- core language and core interpreter 
data ArithS = NumS Int 
            | PlusS ArithS ArithS 
            | MinusS ArithS ArithS 
            | MultS ArithS ArithS 
            | UMinusS ArithS 


desugar :: ArithS -> ExprC
desugar (NumS n) = NumC n 
desugar (PlusS l r) = PlusC (desugar l) (desugar r)
desugar (MultS l r) = MultC (desugar l) (desugar r)
desugar (MinusS l r) = PlusC (desugar l) (MultC (NumC $ -1) (desugar r))
desugar (UMinusS n) = MultC (NumC $ -1) (desugar n)


-- | Add function constructs to this interpreter 
-- To do this we need another new surface language 
type Symbol = String 
type Environ = [FunDef]

data FunDef = FDC {
    name :: Symbol 
  , arg  :: Symbol 
  , body :: ExprC 
  }

data ExprC = NumC Int 
           | PlusC ExprC ExprC 
           | MultC ExprC ExprC 
           | IdC Symbol 
           | AppC Symbol ExprC -- function application: function name and argument

interp :: ExprC -> Environ -> Int 
interp (NumC n) e = n 
interp (PlusC l r) e = (interp l e) + (interp r e) 
interp (MultC l r) e = (interp l e) * (interp r e)
interp (AppC f a)  e = let fd@(FDC _ arg body) = getFunDef e f
                       in interp (subst a arg body) e 
                                    

-- subst substitues 'what' with the identifier 
-- if 'for' matches identifier we are matching against, 
-- then return 'what' 
subst :: ExprC -> Symbol -> ExprC -> ExprC 
subst _    _   i@(NumC n) = i 
subst what for i@(IdC s) | s == for = what 
                         | otherwise = i
subst what for (AppC s e) = AppC s (subst what for e)
subst what for (PlusC l r) = PlusC (subst what for l) (subst what for r)
subst what for (MultC l r) = MultC (subst what for l) (subst what for r)

getFunDef :: Environ -> Symbol -> FunDef 
getFunDef (x:xs) s | s == name x = x 
                   | otherwise   = getFunDef xs s 
