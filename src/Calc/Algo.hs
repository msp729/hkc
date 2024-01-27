{-# LANGUAGE OverloadedStrings #-}

module Calc.Algo (deriv) where

import Calc.Common
import Calc.Expr

replaceX :: Expr -> Expr -> Expr
replaceX x = runExpr p (const x)
  where
    p :: Expr -> Bool
    p (Variable "x") = True
    p _ = False

replaceY :: Expr -> Expr -> Expr -> Expr
replaceY x y = runExpr p d
  where
    p :: Expr -> Bool
    p (Variable "x") = True
    p (Variable "y") = True
    p _ = False

    d (Variable "x") = x
    d (Variable "y") = y
    d _ = 0

replaceZ :: Expr -> Expr -> Expr -> Expr -> Expr
replaceZ x y z = runExpr p d
  where
    p :: Expr -> Bool
    p (Variable "x") = True
    p (Variable "y") = True
    p (Variable "z") = True
    p _ = False

    d (Variable "x") = x
    d (Variable "y") = y
    d (Variable "z") = z
    d _ = 0

deriv :: Ctx -> Expr -> Expr
deriv ctx (F x) = deriv ctx $ replaceX x $ f ctx
deriv ctx (G x y) = deriv ctx $ replaceY x y $ g ctx
deriv ctx (H x y z) = deriv ctx $ replaceZ x y z $ h ctx
deriv ctx (Add x y) = deriv ctx x + deriv ctx y
deriv ctx (Sub x y) = deriv ctx x - deriv ctx y
deriv ctx (Mul x y) = deriv ctx x * y + x * deriv ctx y
deriv ctx (Div x y) = (deriv ctx x * y - deriv ctx y * x) / (y * y)
deriv ctx (Neg x) = Neg (deriv ctx x)
deriv ctx (Pow x y) = Pow x y * (deriv ctx y * Ln x + y * deriv ctx x / x)
deriv ctx (Rt x y) = (deriv ctx x * y / x - deriv ctx y * Ln y) * Rt x y / (y * y)
deriv ctx (Abs x) = Neg (deriv ctx x)
deriv ctx (Signum x) = Neg (deriv ctx x)
deriv ctx (Exp x) = deriv ctx x * Exp x
deriv ctx (Ln x) = deriv ctx x / x
deriv ctx (Lg x) = Literal (logBase 10 (exp 1)) * deriv ctx x / x
deriv ctx (Lb x) = Literal (logBase 2 (exp 1)) * deriv ctx x / x
deriv ctx (Sin x) = deriv ctx x * Cos x
deriv ctx (Cos x) = deriv ctx x * Neg (Sin x)
deriv ctx (Tan x) = deriv ctx x * (1 + Tan x * Tan x)
deriv ctx (Asin x) = deriv ctx x / Rt (1 - x * x) 2
deriv ctx (Acos x) = Neg $ deriv ctx x / Rt (1 - x * x) 2
deriv ctx (Atan x) = deriv ctx x / (1 + x * x)
deriv ctx (Sinh x) = deriv ctx x * Cosh x
deriv ctx (Cosh x) = deriv ctx x * Sinh x
deriv ctx (Tanh x) = deriv ctx x / (Cosh x * Cosh x)
deriv ctx (Asinh x) = deriv ctx x / Rt (x * x + 1) 2
deriv ctx (Acosh x) = deriv ctx x / Rt (x * x - 1) 2
deriv ctx (Atanh x) = deriv ctx x / (1 - x * x)
deriv ctx (Variable "x") = 1
deriv _ _ = 0 -- non-X variables
