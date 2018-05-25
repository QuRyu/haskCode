module ImplicitConfig (
    ) where 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

newtype Modulus s a = Modulus a 
    deriving (Eq, Show)

newtype M s a = M a 
    deriving (Eq, Show)

unM :: M s a -> a 
unM (M a) = a 

add :: Integral a => Modulus s a -> M s a -> M s a -> M s a 
add (Modulus m) (M a) (M b) = M (mod (a+b) m)

mul :: Integral a => Modulus s a -> M s a -> M s a -> M s a 
mul (Modulus m) (M a) (M b) = M (mod (a*b) m)

test1 :: Integral a => Modulus s a -> M s a -> M s a -> M s a 
test1 m a b = add m (mul m a a) (mul m b b)


data AnyModulus a = forall s. AnyModulus (Modulus s a)

mkModulus :: a -> AnyModulus a
mkModulus a = AnyModulus (Modulus a)

withModulus :: a -> (forall s. Modulus s a -> w) -> w
withModulus m k = k (Modulus m)

test2 = withModulus 4 (\m -> 
            let a = M 3 
                b = M 5
            in unM $ add m (mul m a a) (mul m b b))

class Modular s a | s -> a where
    modulus :: s -> a

normalize :: forall s a. (Modular s a, Integral a) => a -> M s a
normalize a = M (mod a (modulus (undefined :: s)))

instance (Modular s a, Integral a) => Num (M s a) where
    (M a) + (M b) = normalize (a + b)
    (M a) * (M b) = normalize (a * b)
    (M a) - (M b) = normalize (a - b)
    negate (M a)  = normalize (negate a)
    fromInteger i = normalize (fromInteger i)
    signum        = error "modular numbers are not signed"
    abs           = error "modular numbers are not signed"



data Zero
data Twice s
data Succ  s
data Pred  s

class ReflectNum s where
    reflectNum :: Num a => s -> a

instance ReflectNum Zero where
    reflectNum _ = 0

instance ReflectNum s => ReflectNum (Twice s) where
    reflectNum _ = reflectNum (undefined :: s) * 2

instance ReflectNum s => ReflectNum (Succ s) where
    reflectNum _ = reflectNum (undefined :: s) + 1

instance ReflectNum s => ReflectNum (Pred s) where
    reflectNum _ = reflectNum (undefined :: s) - 1

reifyIntegral :: Integral a => a -> (forall s. ReflectNum s => s -> w) -> w
reifyIntegral i k = case quotRem i 2 of
    (0, 0) -> k (undefined :: Zero)
    (j, 0) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Twice s))
    (j, 1) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Succ (Twice s)))
    (j, -1) -> reifyIntegral j (\(_ :: s) -> k (undefined :: Pred (Twice s)))

data ModulusNum s a 

instance (ReflectNum s, Num a) => Modular (ModulusNum s a) a where 
    modulus _ = reflectNum (undefined :: s)

withIntegralModulus :: Integral a => a -> (forall s. Modular s a => s -> w) -> w
withIntegralModulus i k = reifyIntegral i (\(_ :: s) -> k (undefined :: ModulusNum s a))


data ModulusArray s a

