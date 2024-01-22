{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Calc.Parse.Command (command) where

import Calc.Command (Command (..), Fun (..), Var (..))
import Calc.Parse.Common (opts, unary)
import Calc.Parse.Expr (expr)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, label)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (symbol)

command :: Parsec Void Text Command
command =
    label "Valid command" $
        opts
            [ unary ":a" (Assign A) expr
            , unary ":b" (Assign B) expr
            , unary ":c" (Assign C) expr
            , unary ":f" (Define F) expr
            , unary ":g" (Define G) expr
            , unary ":h" (Define H) expr
            , Quit <$ symbol space ":q"
            , Calculate <$> expr
            ]
