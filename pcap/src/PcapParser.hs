{-# LANGUAGE BangPatterns #-} 

module PcapParser (
      PGlobalHeader
    , MarketData
    , Pcap
    
    --, parsePCAP 
    , parseB6034
    , parseGHeader
    ) where 

import Data.Int 
import Data.Word 
import Data.Maybe 

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as Char8
import Data.Serialize.Get 

import PcapData 

packetLen :: Word32 
packetLen = 42 
      
parseGHeader :: Get PGlobalHeader 
parseGHeader = mkPGlobalHeader <$> 
               getWord32le <*> 
               getWord16le <*> 
               getWord16le <*>
               getInt32le <*>
               getWord32le <*> 
               getWord32le <*> 
               getWord32le 


parsePPacket :: (BS.ByteString -> Bool) -> -- discard the packet?
                (Word32 -> Get a) -> -- packet parser 
                Get (Maybe a) 
parsePPacket f p = do 
    pacc <- getWord32le 
    skip 8 
    plen <- getWord32le -- length of pcap packet 
    if plen < 47
        then do skip' plen 
                return Nothing 
        else do skip 42 -- skip the IP/UDP header 
                code <- getByteString 5 
                if f code 
                    then do r <- p pacc 
                            return (Just r)
                    else do let skipLen = abs (plen - 47)
                            skip' skipLen
                            return Nothing 
  where 
    skip' = skip . fromIntegral 
    getByteString' = getByteString . fromIntegral


parseB6034 :: Get (Maybe MarketData)
parseB6034 = parsePPacket (BS.isPrefixOf quote) parseB6034' 
    where 
      quote :: BS.ByteString
      quote = Char8.pack "B6034"

      parseB6034' :: Word32 -> Get MarketData 
      parseB6034' pacc = do 
        readB <- bytesRead 
        content <- getByteString 209 
        skip 1 
        return (mkMarketData pacc content)

parsePCAP :: Get Pcap
parsePCAP = do 
    gHeader <- parseGHeader 
    packets <- parsePPackets [] 
    let packets' = catMaybes packets 
    return (mkPcap gHeader packets')
  where 
    parsePPackets xs = do 
        empty <- isEmpty 
        if empty 
            then return xs 
            else do p <- parseB6034 
                    parsePPackets (p:xs)




