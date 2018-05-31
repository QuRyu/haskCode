{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Coroutine (
    ) where 

import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative 

---------------------------------------
-- Simple pause mechanism 
---------------------------------------

data Pause m 
    = RunP (m (Pause m))
    | DoneP 

runN :: Monad m => Int -> Pause m -> m (Pause m)
runN _ DoneP    = return DoneP 
runN 0 p       = return p  
runN n (RunP r) = r >>= runN (n-1)

fullRun :: Monad m => Pause m -> m () 
fullRun DoneP    = return () 
fullRun (RunP r) = r >>= fullRun

pauseExample1 :: Pause IO 
pauseExample1 = RunP $ do 
    putStrLn "First round" 
    return $ RunP $ do 
        putStrLn "Second round" 
        return $ RunP $ do 
            putStrLn "Third round" 
            return DoneP 
    
data PauseT m r 
    = RunT (m (PauseT m r))
    | DoneT r 

instance (Functor m) => Functor (PauseT m) where 
    fmap f (DoneT r) = DoneT (f r) 
    fmap f (RunT m) = RunT $ fmap (fmap f) m

instance (Applicative m) => Applicative (PauseT m) where 
    pure r = DoneT r 
    DoneT f <*> DoneT r = DoneT (f r)
    DoneT f <*> RunT m = RunT $ liftA (pure f <*>) m 
    

instance (Monad m) => Monad (PauseT m) where 
    return r = DoneT r 
    RunT m  >>= f = RunT $ liftM (>>= f) m 
    DoneT r >>= f = f r 


instance MonadTrans PauseT where 
    lift m = RunT $ liftM DoneT m 


---------------------------------------
-- now going to the monad-coroutine package 
---------------------------------------

newtype Coroutine s m r = Coroutine {
    resume :: m (CoroutineState s m r)
    } 

data CoroutineState s m r 
    = Run (s (Coroutine s m r))
    | Done r 

instance (Functor s, Functor m) => Functor (Coroutine s m) where 
    fmap f t = Coroutine $ fmap (apply f)  (resume t)
        where apply fc (Run s)  = Run (fmap (fmap fc) s)
              apply fc (Done r) = Done (fc r)

instance (Functor s, Monad m) => Applicative (Coroutine s m) where 
    pure r = Coroutine $ pure (Done r)
    (<*>) = ap 

instance (Functor s, Monad m) => Monad (Coroutine s m) where 
    return r = Coroutine $ return (Done r)
    t >>= f = Coroutine (resume t >>= apply f)
        where apply fc (Done r) = resume (fc r) 
              apply fc (Run s)  = return $ Run (fmap (>>= fc) s)

instance Functor s => MonadTrans (Coroutine s) where 
    lift = Coroutine . liftM Done 

suspend :: (Monad m, Functor s) => s (Coroutine s m r) -> Coroutine s m r 
suspend = Coroutine . return . Run 

data Interface i o x = Produced o (i -> x)

instance Functor (Interface i o) where 
    fmap f (Produced o k) = Produced o (f . k)

type Producing o i = Coroutine (Interface i o) 
type Consuming r m i o = i -> Producing o i m r 

yield :: Monad m => o -> Producing o i m i 
yield o = suspend $ Produced o return 

infixl 0 $$
($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r 
producing $$ consuming = resume producing >>= \co -> 
                            case co of 
                                Done r -> return r 
                                Run (Produced a k) -> consuming a $$ k  

example1 :: Producing String String IO () 
example1 = do 
    name <- yield "What is your name?" 
    lift $ putStrLn ("hello, " ++ name)
    color <- yield "what is your favorite color?" 
    lift $ putStrLn ("I like " ++ color ++ " too!")
    return ()

foreverK :: Monad m => (a -> m a) -> a -> m r 
foreverK (f :: a -> m a) = go 
    where go a = f a >>= go 

stdOutIn :: Consuming () IO String String 
stdOutIn = foreverK $ \str -> do 
                lift $ putStrLn str 
                lift getLine >>= yield 
    
connect = example1 $$ stdOutIn 

