module Chap2 () where 

suffixes :: [a] -> [[a]]
suffixes [] = [[]] 
suffixes s@(x:xs) = s : suffixes xs 
