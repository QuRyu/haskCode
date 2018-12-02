{-# LANGUAGE BangPatterns #-} 

module PcapParser (
      PGlobalHeader
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

import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import Data.Binary.Get 
import Data.Text (Text)
import Data.Text.Encoding 

import Time 

data PGlobalHeader = PGHeader { 
      magic_number  :: {-# UNPACK #-} !Word32 
    , version_major :: {-# UNPACK #-} !Word16 
    , version_minor :: {-# UNPACK #-} !Word16 
    , timezone      :: {-# UNPACK #-} !Int32  -- GMT to local correction 
    , sigfigs       :: {-# UNPACK #-} !Word32 -- accuracy of timestamps 
    , snaplen       :: {-# UNPACK #-} !Word32 -- max length of captured packets
    , network       :: {-# UNPACK #-} !Word32 -- data link type 
    } deriving (Show, Eq)

data PPacket = PPacket BS.ByteString MarketData 

type Qty = BS.ByteString
type Price = BS.ByteString

data MarketData = B6034 {
      paccTime :: {-# UNPACK #-} !Word32 -- packet accpet time 
    , issCode :: {-# UNPACK #-} !BS.ByteString -- issue code (ISIN code) 
    , accTime :: {-# UNPACK #-} !(Word16, Word16, Word16, Word16)  
            -- accepted time, in format of (hour, minute, second, ms)
    , bids    :: ![(Qty, Price)] -- from 1st to 5th
    , asks    :: ![(Qty, Price)]
    } deriving Eq

data Pcap = Pcap !PGlobalHeader ![MarketData] 
    deriving Show

instance Show PPacket where 
    show (PPacket a m) = show a ++ ' ' : show m 

instance Show MarketData where 
    show (B6034 p i t b a) = 
         show p ++ ' ' : showTime t ++ ' ' : show i ++ ' ' : go (reverse b) 
         ++ ' ' : go a 
        where 
          go [] = ""
          go (x:xs) = showTrans x ++ ' ' : go xs 

          showTrans (q, p) = show q ++ '@' : show p 
          showTime (h, m, s, ms) = show h ++ ':' : show m ++ ':' : show s
                                   ++ ':' : show ms 

instance Ord MarketData where 
    compare (B6034 _ _ l _ _) (B6034 _ _ r _ _) = compare l r 

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
        issCode <- getByteString 12 
        skip 12 
        bids <- go 5  
        skip 7
        asks <- go 5  
        skip 50 
        accTime <- getTime 
        skip 1 
        return (B6034 pacc issCode accTime bids asks)
        where 
          getTime = (,,,) 
                 <$> getWord16le
                 <*> getWord16le 
                 <*> getWord16le
                 <*> getWord16le
          go 0 = return [] 
          go n = do !price <- getByteString 5  
                    !qty <- getByteString 7
                    !remains <- go (n-1)  
                    let trans = (qty, price)
                    return $ trans : remains 


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




