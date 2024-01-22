{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

module Calc.N (N (I, D, C), pattern R, pattern Z) where

import Data.Complex

data N = I Integer | D Double | C (Complex Double)

nm :: (Integer -> a) -> (Double -> a) -> (Complex Double -> a) -> N -> a
nm f _ _ (I k) = f k
nm _ f _ (R x) = f x
nm _ _ f (Z z) = f z

nm' :: (Integer -> Integer) -> (Double -> Double) -> (Complex Double -> Complex Double) -> N -> N
nm' f _ _ (I k) = I (f k)
nm' _ f _ (R x) = D (f x)
nm' _ _ f (Z z) = C (f z)

nf :: (Double -> Bool) -> (Double -> Double) -> (Complex Double -> Complex Double) -> N -> N
nf p f g (R x) = if p x then R (f x) else Z (g (x :+ 0))
nf p f g (C z) = C (g z)

c :: N -> Complex Double
c (I n) = fromInteger n :+ 0
c (D x) = x :+ 0
c (C z) = z

r :: N -> Maybe Double
r (I n) = Just $ fromInteger n
r (D x) = Just x
r (C (x :+ 0)) = Just x
r _ = Nothing

{-# INLINE nm #-}
{-# INLINE nm' #-}
{-# INLINE c #-}
{-# INLINE r #-}
{-# INLINE R #-}
{-# INLINE Z #-}

pattern R :: Double -> N
pattern R x <- (r -> Just x) where R x = D x

pattern Z :: Complex Double -> N
pattern Z z <- (c -> z) where Z z = C z

{-# COMPLETE Z #-}
{-# COMPLETE R, C #-}

instance Num N where
    I a + I b = I (a + b)
    R a + R b = R (a + b)
    a + b = C (c a + c b)
    I a * I b = I (a * b)
    R a * R b = R (a * b)
    a * b = C (c a * c b)
    abs = nm (I . abs) (R . abs) (D . magnitude)
    signum = nm (I . signum) (R . signum) (C . signum)
    fromInteger = I
    negate = nm' negate negate negate

instance Fractional N where
    I n / I m = case divMod n m of
        (x, 0) -> I x
        (_, _) -> R (fromInteger n / fromInteger m)
    R x / R y = R (x / y)
    Z a / Z b = C (a / b)
    fromRational = R . fromRational

instance Floating N where
    pi = R pi
    exp = nm (R . exp . fromInteger) (D . exp) (C . exp)
    log = nf (> 0) log log
    sin = nm (R . sin . fromInteger) (D . sin) (C . sin)
    cos = nm (R . cos . fromInteger) (D . cos) (C . cos)
    asin = nf ((<= 1) . abs) asin asin
    acos = nf ((<= 1) . abs) acos acos
    atan = nm (R . atan . fromInteger) (D . atan) (C . atan)
    sinh = nm (R . sinh . fromInteger) (D . sinh) (C . sinh)
    cosh = nm (R . cosh . fromInteger) (D . cosh) (C . cosh)
    asinh = nm (R . asinh . fromInteger) (D . asinh) (C . asinh)
    acosh = nf (>= 1) acosh acosh
    atanh = nf ((<= 1) . abs) atanh atanh
    I a ** I b | b >= 0 = I (a ^ b)
    R a ** R b = let out = a ** b in if isNaN out then C ((a :+ 0) ** (b :+ 0)) else D out
    Z a ** Z b = Z (a ** b)

instance Show N where
    show = nm show show (\(a :+ b) -> show a ++ " + " ++ show b ++ "i")
