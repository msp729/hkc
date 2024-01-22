module Calc.Command (Command (..), Var (..), Fun (..)) where

import Calc.Expr (Expr)

data Var = A | B | C deriving (Eq)

data Fun = F | G | H deriving (Eq)

data Command
    = Calculate Expr
    | Assign Var Expr
    | Define Fun Expr
    | Quit
