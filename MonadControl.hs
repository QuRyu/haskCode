module MonadControl (
    ) where 

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import System.IO 
import Control.Monad.Error 
import Control.Exception
import Control.Monad 
import Data.Functor.Identity 


withMyFile :: (Handle -> IO a) -> IO a 
withMyFile = withFile "test.txt" WriteMode 

sayHi :: Handle -> IO a 
sayHi handle = hPutStrLn handle "Hello!" 

useMyFile :: IO () 
useMyFile = withMyFile sayHi


sayHiError :: (Exception e) => Handle -> ErrorT e IO ()
sayHiError handle = do 
    lift $ hPutStrLn handle "hi there" 
    throwError IOException


useMyFileError :: ErrorT IOException IO ()
useMyFileError = 
    let unwrapped :: Handle -> IO (Either IOException ()) 
        unwrapped handle = runErrorT $ sayHiError handle 
        applied :: IO (Either IOException ()) 
        applied = withMyFile unwrapped 
        wrapped :: ErrorT IOException IO () 
        wrapped = ErrorT applied 
    in wrapped 


type Run t = forall n o b. (Monad n, Monad o, Monad (t o)) => 
                t n b -> n (t o b)

--errorRun :: Run (ErrorT IOException)
--errorRun = undefined 
--
--useMyFileError2 :: ErrorT IOException IO () 
--useMyFileError2 = 
--    let afterRun :: Handle -> IO (ErrorT IOException Identity ())
--        afterRun handle = errorRun $ saiHiError handle 
--        applied :: IO (ErrorT IOException Identity ())
--        applied = withMyFile applied 
--    in applied 

useMyFileError3 :: (Monad m) => ErrorT IOException IO (ErrorT IOException m ()) 
useMyFileError3 = liftControl inside 
    where 
      inside :: (Monad m) => Run (ErrorT IOException) -> IO (ErrorT IOException m ())
      inside run = withMyFile $ helper run 
      helper :: (Monad m) => Run(ErrorT IOException) -> Handle -> IO (ErrorT IOException m ())
      helper run handle = run (sayHiError handle :: ErrorT IOException IO ())


useMyFileError4 :: ErrorT IOException IO ()
useMyFileError4 = control inside 
    where 
      inside :: (Monad m) => Run (ErrorT IOException) -> IO (ErrorT IOException m ())
      inside run = withMyFile $ helper run 
      helper :: (Monad m) => Run(ErrorT IOException) -> Handle -> IO (ErrorT IOException m ())
      helper run handle = run (sayHiError handle :: ErrorT IOException IO ())

useMyFileError5 :: ErrorT IOException IO ()
useMyFileError5 = control $ \run -> withMyFile $ run . sayHiError  
