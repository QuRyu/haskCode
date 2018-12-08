{-# LANGUAGE ViewPatterns #-}

module PcapData ( 
      --PGlobalHeader
    --, Pcap
    ) where 

import Data.Word 
import Data.Char (ord) 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 
import Data.ByteString.Builder 

data MarketData = B6034 {
      paccTime :: {-# UNPACK #-} !Word32     -- packet accept time  
    , quotes   :: {-# UNPACK #-} !ByteString 
    } deriving Eq

mkMarketData :: Word32 -> ByteString -> MarketData 
mkMarketData = B6034

packStr = BS.pack . map (fromIntegral . ord) 

-- (split i e bs) extracts the piece of bytestring that starts at position 
-- i and has length e. The index starts at position 0. 
split :: Int -> Int -> ByteString -> ByteString 
split i e bs | e < 0     = BS.empty 
             | otherwise = BS.take e $ BS.drop i bs 

issueCode :: MarketData -> ByteString 
issueCode (quotes -> bs) = split 5 12 bs    

-- quote accept time, in format HHMMSSuu 
accTime :: MarketData -> ByteString  
accTime (quotes -> bs) = split 206 8 bs 

type Qty = ByteString 
type Price = ByteString 

readTrans :: (Int, Int) -> MarketData -> [(Qty, Price)]
readTrans (i, e) (quotes -> bs) = go 5 chunks [] 
  where 
    chunks = split i e bs 
    go :: Int -> ByteString -> [(Qty, Price)] -> [(Qty, Price)]
    go 0 _     xs = xs 
    go n chunk xs = let (qty, chunk')  = BS.splitAt 5 chunk 
                        (pri, chunk'') = BS.splitAt 7 chunk' 
                    in go (n-1) chunk'' ((qty, pri) : xs)

asks, bids :: MarketData -> [(Qty, Price)] 
asks = readTrans (96, 60)  
bids = reverse .  readTrans (29, 60)  

                       
marketDataBuilder :: MarketData -> Builder 
marketDataBuilder mdata  = word32LE (paccTime mdata) <> space  
                       <> byteString (accTime mdata) <> space 
                       <> byteString (issueCode mdata) <> space 
                       <> mapBuild (bids mdata) 
                       <> mapBuild (asks mdata) 
    where (<>) = mappend 
          space = charUtf8 ' ' 
          at = charUtf8 '@' 
          mapBuild = mconcat . map (\(q, p) -> 
                           byteString q <> at <> byteString p <> space)
                                        



