module Header (
      PGlobalHeader(..)
    , Transaction(..)
    , MarketData(..)
    , Pcap(..)
    
    , parsePCAP 
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

data Transaction = Trans { 
      qty   :: Text
    , price :: Text
    } deriving (Show, Eq)

data MarketData = B6034 {
      issCode :: Text -- issue code (ISIN code) 
    , accTime :: Text -- accepted time 
    , bids    :: [Transaction] -- from 1st to 5th
    , asks    :: [Transaction]
    } deriving (Show, Eq)

data Pcap = Pcap PGlobalHeader [MarketData] 
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


parsePPacket :: (BS.ByteString -> Bool) -> -- discard the packet?
                Get a -> -- packet parser 
                Get (Maybe a) 
parsePPacket f p = do 
    skip 12
    plen <- getWord32le -- length of pcap packet 
    skip 42 -- skip the IP/UDP header 
    code <- getByteString 5 
    if f code 
        then do r <- p 
                return (Just r)
        else do skip' (plen - 47)
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
          issCode <- mkText 12 
          skip 12 
          bids <- go 5  
          skip 7
          asks <- go 5  
          skip 50 
          accTime <- mkText 8
          skip 1 
          return (B6034 issCode accTime bids asks)
        where 
          go 0 = return [] 
          go n = do price <- mkText 5  
                    qty <- mkText 7
                    remains <- go (n-1)  
                    return $ (Trans qty price) : remains 

mkText :: Int -> Get Text 
mkText = fmap decodeLatin1 . getByteString 


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


firstPacket :: IO BL.ByteString 
firstPacket = liftM (BL.take 273 . BL.drop 270 . BL.drop 24) source 


