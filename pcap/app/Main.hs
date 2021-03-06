module Main (main) where

import System.IO 
import System.Environment 

import Data.ByteString.Builder
import qualified Data.Vector as V
import System.Directory 

import Lib

main :: IO () 
main = do 
    args <- getArgs 
    let ordering = "-r" `elem` args 
    let args' = act ordering removeR args 
    if null args' || length args' > 1 
        then putStrLn "Expecting pcap file path!"
        else do pcap <- readPcap args'  
                let !pcap' = act ordering sortPcap pcap
                hPutBuilder stdout (pcapBuilder pcap')
                return () 
  where 
    act True f = f  
    act _    _ = id 

    removeR [] = [] 
    removeR (x:xs) | x == "-r" = xs
                   | otherwise = x : removeR xs 


--readPcap :: [String] -> IO Pcap 
readPcap (path:_) = do
    (header, mdata) <- pipeline path 
    return $ mkPcapVec header (V.mapMaybe id mdata) 
                
pcapFilePath :: IO FilePath 
pcapFilePath = fmap (flip (++) "/test/test.pcap") getCurrentDirectory 



