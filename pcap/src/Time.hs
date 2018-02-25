module Time ( 
      Time

    , hour 
    , minute 
    , second
    , mkTime 
    ) where 

import Data.Word 

data Time = Time { 
    hour    :: Word8
  , minute  :: Word8
  , second  :: Word8
  } deriving (Eq, Ord)

instance Show Time where 
    show (Time h m s) = show h ++ ':' : show m ++ ':' : show s

mkTime :: Word8 -> Word8 -> Word8 -> Time 
mkTime = Time 
