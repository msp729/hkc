module Calc.Expr (Expr (..), runExpr) where

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

runExpr :: (Expr -> Bool) -> (Expr -> Expr) -> (Expr -> Expr)
runExpr p f e@(F x) = if p e then f e else F (runExpr p f x)
runExpr p f e@(G x y) = if p e then f e else G (runExpr p f x) (runExpr p f y)
runExpr p f e@(H x y z) = if p e then f e else H (runExpr p f x) (runExpr p f y) (runExpr p f z)
runExpr p f e@(Add x y) = if p e then f e else Add (runExpr p f x) (runExpr p f y)
runExpr p f e@(Sub x y) = if p e then f e else Sub (runExpr p f x) (runExpr p f y)
runExpr p f e@(Mul x y) = if p e then f e else Mul (runExpr p f x) (runExpr p f y)
runExpr p f e@(Div x y) = if p e then f e else Div (runExpr p f x) (runExpr p f y)
runExpr p f e@(Pow x y) = if p e then f e else Pow (runExpr p f x) (runExpr p f y)
runExpr p f e@(Rt x y) = if p e then f e else Rt (runExpr p f x) (runExpr p f y)
runExpr p f e@(Neg x) = if p e then f e else Neg (runExpr p f x)
runExpr p f e@(Signum x) = if p e then f e else Signum (runExpr p f x)
runExpr p f e@(Ln x) = if p e then f e else Ln (runExpr p f x)
runExpr p f e@(Lb x) = if p e then f e else Lb (runExpr p f x)
runExpr p f e@(Cos x) = if p e then f e else Cos (runExpr p f x)
runExpr p f e@(Asin x) = if p e then f e else Asin (runExpr p f x)
runExpr p f e@(Atan x) = if p e then f e else Atan (runExpr p f x)
runExpr p f e@(Cosh x) = if p e then f e else Cosh (runExpr p f x)
runExpr p f e@(Asinh x) = if p e then f e else Asinh (runExpr p f x)
runExpr p f e@(Abs x) = if p e then f e else Abs (runExpr p f x)
runExpr p f e@(Exp x) = if p e then f e else Exp (runExpr p f x)
runExpr p f e@(Lg x) = if p e then f e else Lg (runExpr p f x)
runExpr p f e@(Sin x) = if p e then f e else Sin (runExpr p f x)
runExpr p f e@(Tan x) = if p e then f e else Tan (runExpr p f x)
runExpr p f e@(Acos x) = if p e then f e else Acos (runExpr p f x)
runExpr p f e@(Sinh x) = if p e then f e else Sinh (runExpr p f x)
runExpr p f e@(Tanh x) = if p e then f e else Tanh (runExpr p f x)
runExpr p f e@(Acosh x) = if p e then f e else Acosh (runExpr p f x)
runExpr p f e@(Atanh x) = if p e then f e else Atanh (runExpr p f x)
runExpr p f e = if p e then f e else e

instance Num Expr where
    (+) = Add
    (*) = Mul
    abs = Abs
    signum = Signum
    fromInteger = Literal . fromInteger
    negate = Neg

instance Fractional Expr where
    fromRational = Literal . fromRational
    (/) = Div

instance Show Expr where
    show (Variable v) = T.unpack v
    show (Literal x) = show x
    show (F e1) = "f " ++ arg e1
    show (G e1 e2) = "g " ++ arg e1 ++ " " ++ arg e2
    show (H e1 e2 e3) = "h " ++ arg e1 ++ " " ++ arg e2
    show (Add e1 e2) = "add " ++ arg e1 ++ " " ++ arg e2
    show (Sub e1 e2) = "sub " ++ arg e1 ++ " " ++ arg e2
    show (Neg e1) = "neg " ++ arg e1
    show (Mul e1 e2) = "mul " ++ arg e1 ++ " " ++ arg e2
    show (Div e1 e2) = "div " ++ arg e1 ++ " " ++ arg e2
    show (Pow e1 e2) = "pow " ++ arg e1 ++ " " ++ arg e2
    show (Rt e1 e2) = "rt " ++ arg e1 ++ " " ++ arg e2
    show (Abs e1) = "abs " ++ arg e1
    show (Signum e1) = "signum " ++ arg e1
    show (Exp e1) = "exp " ++ arg e1
    show (Ln e1) = "ln " ++ arg e1
    show (Lg e1) = "lg " ++ arg e1
    show (Lb e1) = "lb " ++ arg e1
    show (Sin e1) = "sin " ++ arg e1
    show (Cos e1) = "cos " ++ arg e1
    show (Tan e1) = "tan " ++ arg e1
    show (Asin e1) = "asin " ++ arg e1
    show (Acos e1) = "acos " ++ arg e1
    show (Atan e1) = "atan " ++ arg e1
    show (Sinh e1) = "sinh " ++ arg e1
    show (Cosh e1) = "cosh " ++ arg e1
    show (Tanh e1) = "tanh " ++ arg e1
    show (Asinh e1) = "asinh " ++ arg e1
    show (Acosh e1) = "acosh " ++ arg e1
    show (Atanh e1) = "atanh " ++ arg e1

arg :: Expr -> String
arg e@(Variable _) = show e
arg e@(Literal _) = show e
arg e = "(" ++ show e ++ ")"
