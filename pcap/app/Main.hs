module Main where

import System.Environment 
import Control.Monad 

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get  

import Lib

main :: IO () 
main = do 
    args <- getArgs 
    let ordering = "-r" `elem` args 
    let args' = act ordering removeR args 
    if null args' || length args' > 1 
        then putStrLn "Expecting one argument!"
        else do pcap <- readPcap args' 
                let mData = act ordering sortMarketData (getMarketData pcap)
                forM_ mData print 
        

  where 
    act True f = f  
    act _    _ = id 

    removeR [] = [] 
    removeR (x:xs) | x == "-r" = xs
                   | otherwise = x : removeR xs 

readPcap :: [String] -> IO Pcap 
readPcap (path:_) = do
    content <- BL.readFile path 
    return $ parsePCAP content 
                
    
