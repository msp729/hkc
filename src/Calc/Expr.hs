module Calc.Expr (Expr (..)) where

import qualified Data.Text as T

type Text = T.Text

data Expr
    = Literal Double
    | F Expr
    | G Expr Expr
    | H Expr Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Neg Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Rt Expr Expr
    | Abs Expr
    | Signum Expr
    | Exp Expr
    | Ln Expr
    | Lg Expr
    | Lb Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | Asin Expr
    | Acos Expr
    | Atan Expr
    | Sinh Expr
    | Cosh Expr
    | Tanh Expr
    | Asinh Expr
    | Acosh Expr
    | Atanh Expr
    | Variable Text
