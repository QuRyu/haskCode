{-# LANGUAGE RecordWildCards #-}

module ConduitParse (
      conduitParseOne
    , conduitParseMany
    , DecodeError
    , fuseGet
    ) where 

import Data.Serialize.Get 
import Data.Dynamic 
import Data.Maybe

import Control.Exception 
import Control.Monad.Catch 
import Data.Conduit 
import qualified Data.Conduit.Combinators as C 
import qualified Data.Vector.Generic as VG
import qualified Data.ByteString as BS
import Control.Monad.Trans.Resource
import Control.Monad.Primitive

import PcapData
import PcapParser

data DecodeError = DecodeError {
      unconsumed :: BS.ByteString -- bytes left unconsumed  
    --, nconsumed  :: ByteOffset    -- number of bytes consumed 
    , errMsg     :: String        -- error message from decoder 
    , fun        :: String        -- at which function the error occured
  } deriving (Typeable) 

instance Show DecodeError where 
    show (DecodeError {..}) = "Decode error :\"" ++ errMsg 
                            ++ "\", at function " ++ fun 
                            ++ "\nBytes left: " ++ show unconsumed 

instance Exception DecodeError 

-- | Run the given `Get` only once and push unconsumed bytes from upstream to 
-- downstream 
conduitParseOne :: (MonadThrow m, MonadResource m) => 
                        Get a -> ConduitT BS.ByteString BS.ByteString m a 
conduitParseOne g = go0 
    where 
      go0 = do x <- await 
               case x of 
                 Nothing -> undefined -- throwM SomeException 
                 Just bs -> go (runGetChunk g Nothing bs)

      go (Fail msg bs) = throwM (DecodeError bs msg "conduitParseOnce")
      go (Partial f)  = await >>= maybe (go $ f mempty) (go . f)
      go (Done v bs) = yield bs >> goFlush v
 
      goFlush v = do x <- await 
                     case x of 
                       Nothing -> return v 
                       Just bs -> do yield bs 
                                     goFlush v

-- | Run the given `Get` moand as many times as possible until there is 
-- no input remaining from upstream. 
conduitParseMany :: (MonadThrow m, MonadResource m, Show o) => 
                        Get o -> ConduitT BS.ByteString o m () 
conduitParseMany g = go0 
    where 
      go0 = do x <- await 
               case x of 
                 Nothing -> return () 
                 Just bs -> go (runGetChunk g Nothing bs) 

      go (Fail msg bs)  = throwM (DecodeError bs msg "conduitParseMany")
      go (Partial f)   = await >>= maybe (go $ f mempty) (go . f)
      go (Done v bs) = do yield v 
                          if BS.null bs 
                             then go0 
                             else go (runGetChunk g Nothing bs) 


-- | Run the first `Get` only once and run the second `Get` 
-- as much as possible until there is no input from upstream left. 
-- Then collect the results from the second `Get` into a vector and 
-- return it with the result fromt the first `Get`.
fuseGet :: (MonadThrow m, PrimMonad m, MonadResource m, VG.Vector v b, Show b) 
           => Get a  -- ^ Only run once
           -> Get b  -- ^ Run as many times as possible 
           -> ConduitT BS.ByteString Void m (a, v b) 
fuseGet gOnce gMany =  
    (fmap fst $ (conduitParseOne gOnce) `fuseBoth` (conduitParseMany gMany))
        `fuseBoth` C.sinkVector 
     
