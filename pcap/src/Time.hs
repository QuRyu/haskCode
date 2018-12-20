module Time ( 
      Time

    , hour 
    , minute 
    , second
    , mkTime 
    ) where 

import Data.Word 

data Time = Time { 
    hour    :: Word16
  , minute  :: Word16
  , second  :: Word16
  , ms      :: Word16 
  } deriving (Eq, Ord)

instance Show Time where 
    show (Time h m s u) = show h ++ show m ++ show s ++ show u 

mkTime :: Word16 -> Word16 -> Word16 -> Word16 -> Time 
mkTime = Time 
