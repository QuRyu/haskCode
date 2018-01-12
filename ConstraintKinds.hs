{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Shape where 

import Control.Applicative ((<$>), (<|>))
import Data.Maybe (mapMaybe)
import Data.Typeable
import GHC.Exts (Constraint)

-- | reflextive, generatic, heterogenous container for instances 
-- of a type class 
data Object (constraint :: * -> Constraint) where 
    Obj :: (Typeable a, constraint a) => a -> Object constraint
        deriving Typeable

-- | Downcast an object to any type that satisfies constraint 
downcast :: forall a constraint. (Typeable a, constraint a) => 
            Object constraint -> Maybe a 
downcast (Obj (value :: b)) = 
    case eqT :: Maybe (a :~: b) of 
        Just Refl -> Just value 
        Nothing -> Nothing 

class Shape shape where 
    getArea :: shape -> Double 

instance Shape (Object Shape) where 
    getArea (Obj o) = getArea o 

data Circle = Circle { radius :: Double }
    deriving Typeable 

instance Shape Circle where 
    getArea (Circle r) = pi * r^2

data Rectangle = Rectangle {
    height :: Double
  , width :: Double 
  } deriving Typeable

instance Shape Rectangle where 
    getArea (Rectangle h w) = h * w

-- Now not only can we use Object as a container for objects, 
-- we can even downcast the data stored inside the Object 
exampleData :: [Object Shape]
exampleData = [Obj (Circle 2.0), Obj (Rectangle 3.0 1.0)]

example :: [Object Shape] -> [String] 
example objs = mapMaybe step objs 
    where step shape = describeCircle <$> (downcast shape)
                        <|> Just (describeShape shape)

describeCircle :: Circle -> String
describeCircle (Circle r) = "A circle of radius" ++ show r 

describeShape :: (Shape a) => a -> String 
describeShape s = "A shape with area " ++ show (getArea s)

