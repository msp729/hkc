{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Calc.Parse.Command (command) where

import Calc.Parse.Expr (expr)
import Calc.Command (Command(..), Var(..), Fun(..))
import Calc.Parse.Common (opts, unary)
import Data.Void (Void)
import Text.Megaparsec (Parsec, label)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Char (space)
import Data.Text (Text)

command :: Parsec Void Text Command
command = label "Valid command" $ opts [
    unary ":a" (Assign A) expr,
    unary ":b" (Assign B) expr,
    unary ":c" (Assign C) expr,
    unary ":f" (Define F) expr,
    unary ":g" (Define G) expr,
    unary ":h" (Define H) expr,
    Quit <$ symbol space ":q",
    Calculate <$> expr
    ]
