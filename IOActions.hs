{-# LANGUAGE BangPatterns #-}
module IOActions (
    ) where 

import System.IO.Unsafe 
import Control.Parallel 

-- a function to print out the result 
helper :: (Show a) => a -> IO a 
helper i = print i >> return i 

-- this function on the surface behaves the same as helper function 
-- but under the hood print the value 
helper' :: (Show a) -> a -> a 
helper' i = unsafePerformIO $ print i >> return i 

-- the reference program 
-- will print 1, then print 2 and print 3 

normal :: IO () 
normal = do 
    one <- helper 1 
    two <- helper 2 
    print $ one + two 


-- the order of evaluation is not guaranteed, as GHC is free to choose 
-- which of `one` and `two` to evaluate first 
unordered :: IO () 
unordered = do 
    let one = helper' 1 
        two = helper' 2 
    print $ one + two 

-- the evaluation order of this function is unorderd too! 
-- the problem is `seq` gives only gurantee that for an expression 
-- `a `seq` b`, both `a` and `b` are evaluated before the result returns, 
-- but says nothing about if `a` is necessarily evaluated before `b` 
unordered_with_seq :: IO () 
unordered_with_seq = do 
    let one = helper' 1 
        two = helper' 2 
    print $ one `seq` one + two 

-- `pseq` differs from `seq` in that it promises to evalute `a` before `b`
normal_with_pseq :: IO ()
normal_with_pseq = do 
    let one = helper' 1 
        two = helper' 2 
    print $ one `pseq` one + two 
        

-- same problem as the `seq` one
unordered_strict :: IO ()
unordered_strict = do 
    let one = helper' 1 
        two = helper' 2 
    print $ add one two 
  where 
    add !x y = x + y 

-- main s0 = 
--  case helper' 1 s0 of 
--      (# _s1, one #) -> 
--          case helper 2 s0 of 
--              (# _s2, two #) -> 
--                  print (one + two) s0 


    
