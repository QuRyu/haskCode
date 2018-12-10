module Lib
    ( PGlobalHeader
    , MarketData 
    , Pcap 

    , parsePCAP
    , sortPcap
    , pcapBuilder
    , parseGHeader
    , headerBuilder
    ) where


import Data.List 

import Data.Binary.Get 
import Data.Text.Encoding
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BL

import PcapData
import PcapParser hiding (parsePCAP) 
import qualified PcapParser as PCAP (parsePCAP)

parsePCAP :: BL.ByteString -> Pcap  
parsePCAP = runGet PCAP.parsePCAP 


