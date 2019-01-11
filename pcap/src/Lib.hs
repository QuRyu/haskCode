{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( PGlobalHeader
    , MarketData 
    , Pcap 

    , mkPcap
    , mkPcapVec

    , parseB6034
    , parseGHeader

    , sortPcap
    , pcapBuilder
    , headerBuilder
    , marketDataBuilder
    , getMarketData 

    , pipeline
    ) where


import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Vector.Generic as VG
import Control.Monad.Trans.Resource

import PcapData
import PcapParser
import ConduitParse 

-- | Stream  a file given the file path and parse it 
pipeline :: (VG.Vector v (Maybe MarketData)) => 
            FilePath 
         -> IO (PGlobalHeader, v (Maybe MarketData)) 
pipeline fp = do 
    runResourceT . runConduit $ (C.sourceFile fp) .| parser
    where 
      parser = fuseGet parseGHeader parseB6034
