module Calc.Expr (Expr (..)) where

import qualified Data.Text as T

type Text = T.Text

data Expr -- ^ The type of expressions
    = Literal Double -- ^ just a number
    | F Expr -- ^ custom unary function
    | G Expr Expr -- ^ custom binary function
    | H Expr Expr Expr -- ^ custom ternary function
    | Add Expr Expr -- ^ addition
    | Sub Expr Expr -- ^ subtraction
    | Neg Expr -- ^ negation
    | Mul Expr Expr -- ^ multiplication
    | Div Expr Expr -- ^ division
    | Pow Expr Expr -- ^ exponentiation
    | Rt Expr Expr -- ^ roots: rt a b = ^ a / 1 b
    | Abs Expr -- ^ absolute value
    | Signum Expr -- ^ signum
    | Exp Expr -- ^ exponential function, base e
    | Ln Expr -- ^ natural logarithm
    | Lg Expr -- ^ common log
    | Lb Expr -- ^ binary log
    | Sin Expr -- ^ sine
    | Cos Expr -- ^ cosine
    | Tan Expr -- ^ tangent
    | Asin Expr -- ^ arcsine
    | Acos Expr -- ^ arccosine
    | Atan Expr -- ^ arctangent
    | Sinh Expr -- ^ hyperbolic sine
    | Cosh Expr -- ^ hyperbolic cosine
    | Tanh Expr -- ^ hyperbolic tangent: tanh x = / sinh x cosh x
    | Asinh Expr -- ^ area hyperbolic sine
    | Acosh Expr -- ^ area hyperbolic cosine
    | Atanh Expr -- ^ area hyperbolic tangent
    | Variable Text -- ^ variable reference
