module Calc.Expr (Expr (..)) where

import Calc.N
import qualified Data.Text as T

type Text = T.Text

data Expr
    = -- \^ The type of expressions

      -- | just a number
      Literal N
    | -- | custom unary function
      F Expr
    | -- | custom binary function
      G Expr Expr
    | -- | custom ternary function
      H Expr Expr Expr
    | -- | addition
      Add Expr Expr
    | -- | subtraction
      Sub Expr Expr
    | -- | negation
      Neg Expr
    | -- | multiplication
      Mul Expr Expr
    | -- | division
      Div Expr Expr
    | -- | exponentiation
      Pow Expr Expr
    | -- | roots: rt a b = ^ a / 1 b
      Rt Expr Expr
    | -- | absolute value
      Abs Expr
    | -- | signum
      Signum Expr
    | -- | exponential function, base e
      Exp Expr
    | -- | natural logarithm
      Ln Expr
    | -- | common log
      Lg Expr
    | -- | binary log
      Lb Expr
    | -- | sine
      Sin Expr
    | -- | cosine
      Cos Expr
    | -- | tangent
      Tan Expr
    | -- | arcsine
      Asin Expr
    | -- | arccosine
      Acos Expr
    | -- | arctangent
      Atan Expr
    | -- | hyperbolic sine
      Sinh Expr
    | -- | hyperbolic cosine
      Cosh Expr
    | -- | hyperbolic tangent: tanh x = / sinh x cosh x
      Tanh Expr
    | -- | area hyperbolic sine
      Asinh Expr
    | -- | area hyperbolic cosine
      Acosh Expr
    | -- | area hyperbolic tangent
      Atanh Expr
    | -- | variable reference
      Variable Text
