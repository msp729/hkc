module Calc.N (N (I, D)) where

data N = I Integer | D Double

d :: N -> Double
d (I n) = fromInteger n
d (D x) = x

instance Num N where
    I a + I b = I (a + b)
    a + b = D (d a + d b)
    I a * I b = I (a * b)
    a * b = D (d a * d b)
    abs (I n) = I (abs n)
    abs (D x) = D (abs x)
    signum (I n) = I (signum n)
    signum (D x) = D (signum x)
    fromInteger = I
    negate (I n) = I (negate n)
    negate (D x) = D (negate x)

instance Fractional N where
    fromRational = D . fromRational
    recip (I 1) = I 1
    recip (I (-1)) = I (-1)
    recip x = D $ recip $ d x

instance Floating N where -- for this, we just work in the D wrapper
    pi = D pi
    log = D . log . d
    exp = D . exp . d
    sin = D . sin . d
    cos = D . cos . d
    asin = D . asin . d
    acos = D . acos . d
    atan = D . atan . d
    sinh = D . sinh . d
    cosh = D . cosh . d
    asinh = D . asinh . d
    acosh = D . acosh . d
    atanh = D . atanh . d
    I x ** I y | y >= 0 = I (x ^ y)
    x ** y = D (d x ** d y)

instance Show N where
    show (I n) = show n
    show (D x) = show x
