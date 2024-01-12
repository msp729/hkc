{-# LANGUAGE OverloadedStrings #-}

module Calc.Parse.Expr (expr, fval, gval, hval, eval) where

import Calc.Expr (Expr (..))
import Calc.Common (Ctx (..))
import Calc.Parse.Common (binary, opts, ternary, unary)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space, letterChar)
import Text.Megaparsec.Char.Lexer (float, decimal, lexeme, symbol)
import Calc.N

expr :: Parsec Void Text Expr
expr =
    label "An expression" $
        lexeme space $
            opts
                [ Literal . D <$> float
                , Literal . I <$> decimal
                , unary "f" F expr
                , binary "g" G expr expr
                , ternary "h" H expr expr expr
                , binary "+" Add expr expr
                , binary "-" Sub expr expr
                , binary "*" Mul expr expr
                , binary "/" Div expr expr
                , binary "^" Pow expr expr
                , binary "rt" Rt expr expr
                , unary "_" Neg expr
                , unary "exp" Exp expr
                , unary "ln" Ln expr
                , unary "sin" Sin expr
                , unary "cos" Cos expr
                , unary "tan" Tan expr
                , unary "asin" Asin expr
                , unary "acos" Acos expr
                , unary "atan" Atan expr
                , unary "sinh" Sinh expr
                , unary "cosh" Cosh expr
                , unary "tanh" Tanh expr
                , unary "asinh" Asinh expr
                , unary "acosh" Acosh expr
                , unary "atanh" Atanh expr
                , unary "p" (Neg . Lg) expr
                , unary "P" (Pow (Literal 10) . Neg) expr
                , Variable "ans" <$ symbol space "ans"
                , Variable . pack . (:[]) <$> letterChar
                ]

fval :: Expr -> Expr -> Expr
fval (Literal x) _ = Literal x
fval (F e1) x = F (fval e1 x)
fval (G e1 e2) x = G (fval e1 x) (fval e2 x)
fval (H e1 e2 e3) x = H (fval e1 x) (fval e2 x) (fval e3 x)
fval (Neg e1) x = Neg (fval e1 x)
fval (Abs e1) x = Abs (fval e1 x)
fval (Signum e1) x = Signum (fval e1 x)
fval (Exp e1) x = Exp (fval e1 x)
fval (Ln e1) x = Ln (fval e1 x)
fval (Lg e1) x = Lg (fval e1 x)
fval (Lb e1) x = Lb (fval e1 x)
fval (Sin e1) x = Sin (fval e1 x)
fval (Cos e1) x = Cos (fval e1 x)
fval (Tan e1) x = Tan (fval e1 x)
fval (Asin e1) x = Asin (fval e1 x)
fval (Acos e1) x = Acos (fval e1 x)
fval (Atan e1) x = Atan (fval e1 x)
fval (Sinh e1) x = Sinh (fval e1 x)
fval (Cosh e1) x = Cosh (fval e1 x)
fval (Tanh e1) x = Tanh (fval e1 x)
fval (Asinh e1) x = Asinh (fval e1 x)
fval (Acosh e1) x = Acosh (fval e1 x)
fval (Atanh e1) x = Atanh (fval e1 x)
fval (Add e1 e2) x = Add (fval e1 x) (fval e2 x)
fval (Sub e1 e2) x = Sub (fval e1 x) (fval e2 x)
fval (Mul e1 e2) x = Mul (fval e1 x) (fval e2 x)
fval (Div e1 e2) x = Div (fval e1 x) (fval e2 x)
fval (Pow e1 e2) x = Pow (fval e1 x) (fval e2 x)
fval (Rt e1 e2) x = Rt (fval e1 x) (fval e2 x)
fval (Variable "x") x = x
fval x _ = x

gval :: Expr -> Expr -> Expr -> Expr
gval (Literal x) _ _ = Literal x
gval (F e1) x y = F (gval e1 x y)
gval (G e1 e2) x y = G (gval e1 x y) (gval e2 x y)
gval (H e1 e2 e3) x y = H (gval e1 x y) (gval e2 x y) (gval e3 x y)
gval (Neg e1) x y = Neg (gval e1 x y)
gval (Abs e1) x y = Abs (gval e1 x y)
gval (Signum e1) x y = Signum (gval e1 x y)
gval (Exp e1) x y = Exp (gval e1 x y)
gval (Ln e1) x y = Ln (gval e1 x y)
gval (Lg e1) x y = Lg (gval e1 x y)
gval (Lb e1) x y = Lb (gval e1 x y)
gval (Sin e1) x y = Sin (gval e1 x y)
gval (Cos e1) x y = Cos (gval e1 x y)
gval (Tan e1) x y = Tan (gval e1 x y)
gval (Asin e1) x y = Asin (gval e1 x y)
gval (Acos e1) x y = Acos (gval e1 x y)
gval (Atan e1) x y = Atan (gval e1 x y)
gval (Sinh e1) x y = Sinh (gval e1 x y)
gval (Cosh e1) x y = Cosh (gval e1 x y)
gval (Tanh e1) x y = Tanh (gval e1 x y)
gval (Asinh e1) x y = Asinh (gval e1 x y)
gval (Acosh e1) x y = Acosh (gval e1 x y)
gval (Atanh e1) x y = Atanh (gval e1 x y)
gval (Add e1 e2) x y = Add (gval e1 x y) (gval e2 x y)
gval (Sub e1 e2) x y = Sub (gval e1 x y) (gval e2 x y)
gval (Mul e1 e2) x y = Mul (gval e1 x y) (gval e2 x y)
gval (Div e1 e2) x y = Div (gval e1 x y) (gval e2 x y)
gval (Pow e1 e2) x y = Pow (gval e1 x y) (gval e2 x y)
gval (Rt e1 e2) x y = Rt (gval e1 x y) (gval e2 x y)
gval (Variable "x") x _ = x
gval (Variable "y") _ y = y
gval x _ _ = x

hval :: Expr -> Expr -> Expr -> Expr -> Expr
hval (Literal x) _ _ _ = Literal x
hval (F e1) x y z = F (hval e1 x y z)
hval (G e1 e2) x y z = G (hval e1 x y z) (hval e2 x y z)
hval (H e1 e2 e3) x y z = H (hval e1 x y z) (hval e2 x y z) (hval e3 x y z)
hval (Neg e1) x y z = Neg (hval e1 x y z)
hval (Abs e1) x y z = Abs (hval e1 x y z)
hval (Signum e1) x y z = Signum (hval e1 x y z)
hval (Exp e1) x y z = Exp (hval e1 x y z)
hval (Ln e1) x y z = Ln (hval e1 x y z)
hval (Lg e1) x y z = Lg (hval e1 x y z)
hval (Lb e1) x y z = Lb (hval e1 x y z)
hval (Sin e1) x y z = Sin (hval e1 x y z)
hval (Cos e1) x y z = Cos (hval e1 x y z)
hval (Tan e1) x y z = Tan (hval e1 x y z)
hval (Asin e1) x y z = Asin (hval e1 x y z)
hval (Acos e1) x y z = Acos (hval e1 x y z)
hval (Atan e1) x y z = Atan (hval e1 x y z)
hval (Sinh e1) x y z = Sinh (hval e1 x y z)
hval (Cosh e1) x y z = Cosh (hval e1 x y z)
hval (Tanh e1) x y z = Tanh (hval e1 x y z)
hval (Asinh e1) x y z = Asinh (hval e1 x y z)
hval (Acosh e1) x y z = Acosh (hval e1 x y z)
hval (Atanh e1) x y z = Atanh (hval e1 x y z)
hval (Add e1 e2) x y z = Add (hval e1 x y z) (hval e2 x y z)
hval (Sub e1 e2) x y z = Sub (hval e1 x y z) (hval e2 x y z)
hval (Mul e1 e2) x y z = Mul (hval e1 x y z) (hval e2 x y z)
hval (Div e1 e2) x y z = Div (hval e1 x y z) (hval e2 x y z)
hval (Pow e1 e2) x y z = Pow (hval e1 x y z) (hval e2 x y z)
hval (Rt e1 e2) x y z = Rt (hval e1 x y z) (hval e2 x y z)
hval (Variable "x") x _ _ = x
hval (Variable "y") _ y _ = y
hval (Variable "z") _ _ z = z
hval x _ _ _ = x

eval :: Ctx -> Expr -> Maybe N
eval _ (Literal d) = Just d
eval ctx (F x) = eval ctx $ fval (f ctx) x
eval ctx (G x y) = eval ctx $ gval (g ctx) x y
eval ctx (H x y z) = eval ctx $ hval (h ctx) x y z
eval ctx (Neg x) = negate <$> eval ctx x
eval ctx (Abs x) = abs <$> eval ctx x
eval ctx (Signum x) = signum <$> eval ctx x
eval ctx (Exp x) = exp <$> eval ctx x
eval ctx (Ln x) = log <$> eval ctx x
eval ctx (Lg x) = logBase 10 <$> eval ctx x
eval ctx (Lb x) = logBase 2 <$> eval ctx x
eval ctx (Sin x) = sin <$> eval ctx x
eval ctx (Cos x) = cos <$> eval ctx x
eval ctx (Tan x) = tan <$> eval ctx x
eval ctx (Asin x) = asin <$> eval ctx x
eval ctx (Acos x) = acos <$> eval ctx x
eval ctx (Atan x) = atan <$> eval ctx x
eval ctx (Sinh x) = sinh <$> eval ctx x
eval ctx (Cosh x) = cosh <$> eval ctx x
eval ctx (Tanh x) = tanh <$> eval ctx x
eval ctx (Asinh x) = asinh <$> eval ctx x
eval ctx (Acosh x) = acosh <$> eval ctx x
eval ctx (Atanh x) = atanh <$> eval ctx x
eval ctx (Add x y) = (+) <$> eval ctx x <*> eval ctx y
eval ctx (Sub x y) = (-) <$> eval ctx x <*> eval ctx y
eval ctx (Mul x y) = (*) <$> eval ctx x <*> eval ctx y
eval ctx (Div x y) = (/) <$> eval ctx x <*> eval ctx y
eval ctx (Pow x y) = (**) <$> eval ctx x <*> eval ctx y
eval ctx (Rt x y) = (**) <$> eval ctx x <*> fmap recip (eval ctx y)
eval ctx (Variable "a") = Just $ a ctx
eval ctx (Variable "b") = Just $ b ctx
eval ctx (Variable "c") = Just $ c ctx
eval ctx (Variable "ans") = Just $ ans ctx
eval _ _ = Nothing
