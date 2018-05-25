module CPS () where 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.Cont 
import Control.Monad.State

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f x = \k -> 
    f x $ \fx -> 
    f fx $ \fxx -> 
    f fxx $ k 

tail_cps :: [a] -> (([a] -> r) -> r)
tail_cps x = \k -> k $ tail x 

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) ->
                ((b -> r) -> r)
chainCPS m f = \k -> m $ \x -> f x $ k 
            

newtype CoroutineT r m a = CoroutineT {
    runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a
    } deriving (Functor, Applicative, Monad, MonadCont, MonadIO)


getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get 
    
putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m () 
putCCs = CoroutineT . lift . put 

dequeue :: Monad m => CoroutineT r m () 
dequeue = do 
    current_ccs <- getCCs 
    case current_ccs of 
        []     -> return () 
        (x:xs) -> do 
            putCCs xs 
            x

queue :: Monad m => CoroutineT r m () -> CoroutineT r m () 
queue c = do 
    ccs <- getCCs 
    putCCs (ccs ++ [c])

yield ::Monad m => CoroutineT r m () 
yield = callCC $ \k -> do 
    queue (k ())
    dequeue 

fork :: Monad m => CoroutineT r m () -> CoroutineT r m () 
fork c = callCC $ \k -> do 
    queue (k ())
    c 
    dequeue 

exhaust :: Monad m => CoroutineT r m ()
exhaust = do 
    exhausted <- null <$> getCCs 
    if not exhausted 
        then yield >> exhaust 
        else return ()

runCoroutineT :: Monad m => CoroutineT r m r -> m r 
runCoroutineT = flip evalStateT [] . flip runContT return . 
                    runCoroutineT' . (<* exhaust)

-- example of coroutine 
printOne n = do 
    liftIO (print n)
    yield 

example = runCoroutineT $ do 
    fork $ replicateM_ 3 (printOne 3) 
    fork $ replicateM_ 4 (printOne 4)
    replicateM_ 2 (printOne 2)
