module PcapParser (
      PGlobalHeader
    , Transaction
    , MarketData
    , Pcap
    
    , parsePCAP 
    , getMarketData
    , accTime 
    ) where 

import Data.Int 
import Data.Word 
import Control.Monad
import Data.Maybe 
import Control.Monad.IO.Class
import Debug.Trace

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import Data.Binary.Get 
import Data.Text (Text)
import Data.Text.Encoding 

import Time 

data PGlobalHeader = PGHeader { 
      magic_number  :: Word32 
    , version_major :: Word16 
    , version_minor :: Word16 
    , timezone      :: Int32  -- GMT to local correction 
    , sigfigs       :: Word32 -- accuracy of timestamps 
    , snaplen       :: Word32 -- max length of captured packets 
    , network       :: Word32 -- data link type 
    } deriving (Show, Eq)

data PPacket = PPacket BS.ByteString MarketData 

data Transaction = Trans { 
      qty   :: BS.ByteString 
    , price :: BS.ByteString 
    } deriving Eq

data MarketData = B6034 {
      issCode :: BS.ByteString -- issue code (ISIN code) 
    , accTime :: Time  -- accepted time 
    , bids    :: [Transaction] -- from 1st to 5th
    , asks    :: [Transaction]
    } deriving Eq

data Pcap = Pcap PGlobalHeader [MarketData] 
    deriving Show

instance Show PPacket where 
    show (PPacket a m) = show a ++ ' ' : show m 

instance Show Transaction where 
    show (Trans q p) = show q ++ '@' : show p 

instance Show MarketData where 
    show (B6034 i t b a) = 
         show t ++ ' ' : show i ++ ' ' : go (reverse b) ++ ' ' : go a 
        where 
          go [] = ""
          go (x:xs) = show x ++ ' ' : go xs 


getMarketData :: Pcap -> [MarketData]
getMarketData (Pcap _ ms) = ms 


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


parsePPacket :: (BS.ByteString -> Bool) -> -- discard the packet?
                Get a -> -- packet parser 
                Get (Maybe a) 
parsePPacket f p = do 
    skip 12
    plen <- getWord32le -- length of pcap packet 
    if plen < 47
        then do skip' plen 
                return Nothing 
        else do skip 42 -- skip the IP/UDP header 
                code <- getByteString 5 
                if f code 
                    then do r <- p 
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

      parseB6034' :: Get MarketData 
      parseB6034' = do
          issCode <- getByteString 12 
          skip 12 
          bids <- go 5  
          skip 7
          asks <- go 5  
          skip 50 
          accTime <- getTime 
          skip 1 
          return (B6034 issCode accTime bids asks)
        where 
          getTime = mkTime 
                 <$> getWord8 
                 <*> getWord8 
                 <*> getWord8
          go 0 = return [] 
          go n = do price <- getByteString 5  
                    qty <- getByteString 7
                    remains <- go (n-1)  
                    return $ (Trans qty price) : remains 


parsePCAP :: Get Pcap
parsePCAP = do 
    gHeader <- parseGHeader 
    packets <- parsePPackets [] 
    let packets' = catMaybes packets 
    return (Pcap gHeader packets')
  where 
    parsePPackets xs = do 
        empty <- isEmpty 
        if empty 
            then return xs 
            else do p <- parseB6034 
                    parsePPackets (p:xs)




