{-# LANGUAGE ViewPatterns #-}

module PcapData ( 
      MarketData
    , PGlobalHeader
    , Pcap


    , mkMarketData
    , mkPGlobalHeader
    , mkPcap


    , marketDataBuilder
    , pcapBuilder
    , sortPcap

    ) where 

import Data.Int
import Data.Word 
import Data.Char (ord) 
import Control.Monad.ST 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 
import Data.ByteString.Builder 
import qualified Data.Vector as V 
import qualified Data.Vector.Algorithms.Intro as V

data MarketData = B6034 {
      paccTime :: {-# UNPACK #-} !Word32     -- packet accept time  
    , quotes   :: {-# UNPACK #-} !ByteString 
    } deriving Eq

mkMarketData :: Word32 -> ByteString -> MarketData 
mkMarketData = B6034

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

instance Ord MarketData where 
    compare l r = compare (accTime l) (accTime r) 


-- | Global header of the netwke packet 
data PGlobalHeader = PGHeader { 
      magic_number  :: {-# UNPACK #-} !Word32 
    , version_major :: {-# UNPACK #-} !Word16 
    , version_minor :: {-# UNPACK #-} !Word16 
    , timezone      :: {-# UNPACK #-} !Int32  -- GMT to local correction 
    , sigfigs       :: {-# UNPACK #-} !Word32 -- accuracy of timestamps 
    , snaplen       :: {-# UNPACK #-} !Word32 -- max length of captured packets
    , network       :: {-# UNPACK #-} !Word32 -- data link type 
    } deriving (Show, Eq)

mkPGlobalHeader :: Word32 -> -- | magic number 
                   Word16 -> -- | major version 
                   Word16 -> -- | minor version
                   Int32 ->  -- | timezone 
                   Word32 -> -- | accuracy of timestamps 
                   Word32 -> -- | max length of captured packets 
                   Word32 -> -- | network  
                   PGlobalHeader
mkPGlobalHeader = PGHeader


data Pcap = Pcap !PGlobalHeader (V.Vector MarketData)

mkPcap :: PGlobalHeader -> [MarketData] -> Pcap
mkPcap header mdata = Pcap header (V.fromList mdata) 

pcapBuilder :: Pcap -> Builder 
pcapBuilder (Pcap _ mdata) = builders (charUtf8 ' ') mdata
 where builders = V.foldl' (\acc x -> 
                               acc <> charUtf8 '\n' <> marketDataBuilder x)
       (<>) = mappend

                                        

sortPcap :: Pcap -> Pcap 
sortPcap (Pcap header mdata) = 
    Pcap header $ runST $ 
             do v <- (V.thaw mdata) :: ST s (V.MVector s MarketData)
                V.sort v 
                sorted <- V.freeze v 
                return sorted
                             
         
 
