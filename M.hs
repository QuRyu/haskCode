module M ( ) where 

{-# LANGUAGE FlexibleContexts #-} 

import Control.Applicative 
import Control.Monad.Trans.State 
import Control.Monad.Trans.Control

foo :: (MonadBaseControl IO) m => m String -> m String 
foo f = do 
    x <- f 
    y' <- liftBaseWith $ \run -> do 
              print $ "x = " ++ show x 
              x' <- run f 
              return x' 
    restoreM y'
     



f :: StateT Int IO String 
f = do modify (+1) ; show <$> get 

