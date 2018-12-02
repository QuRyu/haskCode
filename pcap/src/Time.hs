module Time ( 
      Time

    , hour 
    , minute 
    , second
    , mkTime 
    ) where 

import Data.Word 

data Time = Time { 
    hour    :: {-# UNPACK #-} Word16
  , minute  :: {-# UNPACK #-} Word16
  , second  :: {-# UNPACK #-} Word16
  , ms      :: {-# UNPACK #-} Word16 
  } deriving (Eq, Ord)

instance Show Time where 
    show (Time h m s u) = show h ++ ':' : show m ++ ':' : show s 
                            ++ ':' : show u

mkTime :: Word16 -> Word16 -> Word16 -> Word16 -> Time 
mkTime = Time 
