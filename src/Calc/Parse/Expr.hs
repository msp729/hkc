{-# LANGUAGE OverloadedStrings #-}

module Calc.Parse.Expr () where

import Calc.Expr (Expr (..))
import Calc.Parse.Common (binary, opts, ternary, unary)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space, letterChar)
import Text.Megaparsec.Char.Lexer (decimal, lexeme, symbol)

expr :: Parsec Void Text Expr
expr =
    label "An expression" $
        lexeme space $
            opts
                [ Literal <$> decimal
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
