{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

{- |

Module      : Calc.N
Description : A numeric type built for precision and generality.
-}
module Calc.N (
    N (),
    unN,
    unZ,
    unQ,
    unR,
    unC,
    pattern MkZ,
    pattern MkQ,
    pattern MkR,
    pattern MkC,
    ℤ,
    ℚ,
    ℝ,
    ℂ,
) where

import Control.Spoon (teaspoon)
import Data.Complex (Complex ((:+)), magnitude)
import Data.Number.BigFloat (BigFloat, Prec50)
import Data.Ord (comparing)
import Data.Ratio ((%), numerator, denominator)

-- | A fun type alias.
type ℤ = Integer

-- | Another fun type alias.
type ℚ = Rational

-- | Not quite true, but a decent approximation.
type ℝ = BigFloat Prec50

-- | Again, not quite true, but so be it.
type ℂ = Complex ℝ

{- | A datatype that is either:
- an integer (`ℤ`)
- a rational number (`ℚ`)
- a real number (`ℝ`)
- or a complex number (`ℂ`)
-}
data N = Z ℤ | Q ℚ | R ℝ | C ℂ

-- | Pattern matching on 'N'.
unN :: (ℤ -> a) -> (ℚ -> a) -> (ℝ -> a) -> (ℂ -> a) -> (N -> a)
unN f _ _ _ (Z n) = f n
unN _ f _ _ (Q q) = f q
unN _ _ f _ (R x) = f x
unN _ _ _ f (C z) = f z

class IN a where insert :: a -> N
instance IN ℤ where insert = Z
instance IN ℚ where insert = Q
instance IN ℝ where insert = R
instance IN ℂ where insert = C

unN' :: (IN a, IN b, IN c, IN d) => (ℤ -> a) -> (ℚ -> b) -> (ℝ -> c) -> (ℂ -> d) -> (N -> N)
unN' z q r c = unN (insert . z) (insert . q) (insert . r) (insert . c)

unNF :: (IN a, IN b) => (ℝ -> a) -> (ℂ -> b) -> N -> N
unNF r = unN' (r . fromInteger) (r . fromRational) r

unNF' :: (IN a, IN b) => (ℝ -> a) -> (ℂ -> b) -> N -> N
unNF' r c = unN (r' . fromInteger) (r' . fromRational) r' (insert . c)
  where
    r' :: ℝ -> N
    r' x = maybe (insert $ c $ x :+ 0) insert $ teaspoon $ r x

unZ :: N -> Maybe ℤ
unZ = unN Just n n n where n = const Nothing

unQ :: N -> Maybe ℚ
unQ = unN (Just . (% 1)) Just n n where n = const Nothing

unR :: N -> Maybe ℝ
unR = unN (Just . fromInteger) (Just . fromRational) Just n where n = const Nothing

unC :: N -> ℂ
unC = unN fromInteger fromRational (:+ 0) id

pattern MkZ :: ℤ -> N
pattern MkZ n = Z n

-- | A pattern that matches any integers or rationals
pattern MkQ :: ℚ -> N
pattern MkQ q <- (unQ -> Just q) where MkQ q = Q q

-- | A pattern that matches anything except complex numbers
pattern MkR :: ℝ -> N
pattern MkR x <- (unR -> Just x) where MkR x = R x

-- | A pattern that matches everything, but casts to 'ℂ'.
pattern MkC :: ℂ -> N
pattern MkC z <- (unC -> z) where MkC z = C z

{-# COMPLETE MkC #-}
{-# COMPLETE MkR, C #-}
{-# COMPLETE MkQ, R, C #-}
{-# COMPLETE MkZ, Q, R, C #-}

instance Num N where
    MkZ a + MkZ b = MkZ (a + b)
    MkQ a + MkQ b = MkQ (a + b)
    MkR a + MkR b = MkR (a + b)
    MkC a + MkC b = MkC (a + b)
    MkZ a * MkZ b = MkZ (a * b)
    MkQ a * MkQ b = MkQ (a * b)
    MkR a * MkR b = MkR (a * b)
    MkC a * MkC b = MkC (a * b)
    abs = unN' abs abs abs magnitude
    signum = unN' abs abs abs abs
    fromInteger = MkZ
    negate = unN' negate negate negate negate

instance Fractional N where
    fromRational = MkQ
    MkZ a / MkZ b = case divMod a b of
        (c, 0) -> MkZ c
        _ -> MkQ (a % b)
    MkQ a / MkQ b = MkQ (a / b)
    MkR a / MkR b = MkR (a / b)
    MkC a / MkC b = MkC (a / b)

instance Floating N where
    pi = R pi
    exp = unNF exp exp
    log = unNF' log log
    sin = unNF sin sin
    cos = unNF cos cos
    asin = unNF' asin asin
    acos = unNF' acos acos
    atan = unNF' atan atan
    sinh = unNF sinh sinh
    cosh = unNF cosh cosh
    asinh = unNF asinh asinh
    acosh = unNF' acosh acosh
    atanh = unNF' atanh atanh

instance Show N where
    show = unN show (\q -> show (numerator q) ++ " / " ++ show (denominator q)) show' (\(a :+ b) -> show' a ++ " + " ++ show' b ++ "i")
      where
        s :: Int
        s = 20

        show' :: ℝ -> String
        show' x =
            let (m, e) = decodeFloat x
             in let (m', e') = (div m (10 ^ (s :: Int)), e + s)
                 in let unscaled = encodeFloat m' 0 :: Double
                     in show $ unscaled * (10 ** fromInteger (toInteger e'))

instance Eq N where
    MkC a == MkC b = a == b

instance Ord N where
    compare = comparing (magnitude . unC)
