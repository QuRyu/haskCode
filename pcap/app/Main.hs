{-# LANGUAGE BangPatterns #-} 

module Main (main) where

import System.IO 
import System.Environment 
import Control.Monad 
import Control.Exception 

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Binary.Get  

import Lib

main :: IO () 
main = do 
    args <- getArgs 
    let ordering = "-r" `elem` args 
    let args' = act ordering removeR args 
    if null args' || length args' > 1 
        then putStrLn "Expecting pcap file path!"
        else do pcap <- readPcap args' 
                let pcap' = act ordering sortPcap pcap
                hPutBuilder stdout (pcapBuilder pcap')
                return ()
        --else do header <- Main.read args'
                --hPutBuilder stdout (string8 "hello\n" `mappend`  
                                        --headerBuilder header)
                --return ()
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
                
read :: [String] -> IO PGlobalHeader 
read (path:_) = do 
    content <- BL.readFile path 
    header <- evaluate $ runGet parseGHeader content 
    return header 
    


     
