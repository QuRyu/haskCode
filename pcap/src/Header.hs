module Header (
      PGlobalHeader 
    , PPacketHeader 
    ) where 

import Data.Int 
import Data.Word 
import Control.Monad

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

data MarketData = B6034 { 
      quote   :: BS.ByteString
    , issCode :: BS.ByteString -- issue code (ISIN code) 
    , accTime :: BS.ByteString -- accepted time 
    , bids    :: [Transaction] 
    , asks    :: [Transaction]
    } deriving (Show, Eq)

data PPacket = PPacket PPacketHeader MarketData 
    deriving Show

data Pcap = Pcap PGlobalHeader [PPacket] 
    deriving Show

packetLen :: Word32 
packetLen = 42 
      
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

parsePPacket :: (BS.ByteString -> Bool) -> -- discard the packet?
                (BS.ByteString -> Get a) -> -- packet parser 
                Get (Maybe a) 
parsePPacket f p = do 
    skip' 12
    plen <- getWord32le -- length of pcap packet 
    skip 42 -- skip the IP/UDP header 
    result <- lookAheadM try
    case result of 
        Nothing -> do skip' (plen - packetLen)
                      return Nothing 
        Just _ -> do content <- getByteString' (plen - packetLen - 5)
                     parsed <- p content 
                     return (Just parsed)

  where 
    skip' = skip . fromIntegral 
    getByteString' = getByteString . fromIntegral
    try = do discard <- getByteString 5 
             if f discard 
               then return (Just ())
               else return Nothing 



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
                     return (B6034 quote issTime accTime bids asks)
    where 
      go 0 = return [] 
      go n = do price <- getByteString 5  
                qty <- getByteString 7
                remains <- go (n-1)  
                return $ (Trans qty price) : remains 


