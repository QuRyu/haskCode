module Header (
      PGlobalHeader 
    , PPacketHeader 
    ) where 

import Data.Int 
import Data.Word 

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Char8 as Char8
import Data.Binary.Get 

import Lib (source)

data PGlobalHeader = PGHeader { 
      magic_number  :: Word32 
    , version_major :: Word16 
    , version_minor :: Word16 
    , timezone      :: Int32  -- GMT to local correction 
    , sigfigs       :: Word32 -- accuracy of timestamps 
    , snaplen       :: Word32 -- max length of captured packets 
    , network       :: Word32 -- data link type 
    } deriving (Show, Eq)

data PPacketHeader = PPHeader { 
      ts_sec   :: Word32 -- timestamp seconds 
    , ts_usec  :: Word32 -- timestamp microseconds 
    , incl_len :: Word32 -- number of octets of packet saved in a file 
    , orig_len :: Word32 -- actual length of packet 
    } deriving (Show, Eq)


data Transaction = Trans { 
      qty   :: BS.ByteString 
    , price :: BS.ByteString
    } deriving (Show, Eq)

data MarketData = MD { 
      quote   :: BS.ByteString
    , issTIme :: BS.ByteString -- issue time 
    , accTime :: BS.ByteString -- accepted time 
    , bids    :: [Transaction] 
    , asks    :: [Transaction]
    } deriving (Show, Eq)

data PPacket = PPacket PPacketHeader MarketData 
    deriving Show

data Pcap = Pcap PGlobalHeader [PPacket] 
    deriving Show
      
parseGHeader :: Get PGlobalHeader 
parseGHeader = PGHeader <$> 
               getWord32le <*> 
               getWord16le <*> 
               getWord16le <*>
               getInt32le <*>
               getWord32le <*> 
               getWord32le <*> 
               getWord32le 


parsePHeader :: Get PPacketHeader 
parsePHeader = PPHeader <$> 
               getWord32le <*>
               getWord32le <*>
               getWord32le <*>
               getWord32le 

parseMarketData :: Get MarketData 
parseMarketData = do quote <- getByteString 5 
                     issTime <- getByteString 12 
                     skip 12 
                     bids <- go 5  
                     skip 7
                     asks <- go 5  
                     skip 50 
                     accTime <- getByteString 8
                     skip 1 
                     return (MD quote issTime accTime bids asks)
    where 
      go 0 = return [] 
      go n = do price <- getByteString 5  
                qty <- getByteString 7
                remains <- go (n-1)  
                return $ (Trans qty price) : remains 


parsePPacket :: Get PPacket 
parsePPacket = PPacket 
            <$> parsePHeader 
            <*> parseMarketData 

parsePCAP :: Get Pcap 
parsePCAP = do 
    gHeader <- parseGHeader
    packets <- parsePackets 
    return (Pcap gHeader packets)
  where 
    parsePackets :: Get [PPacket]
    parsePackets = do 
        empty <-isEmpty 
        if empty 
            then return [] 
            else (:) <$> parsePPacket <*> parsePackets 
