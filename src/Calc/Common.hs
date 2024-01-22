module Calc.Common (Ctx (..), ci) where

import Calc.Expr
import Calc.N

ci :: Ctx
ci = Ctx 0 0 0 0 (Literal 0) (Literal 0) (Literal 0)

data Ctx = Ctx
    { a :: N
    -- ^ var 1
    , b :: N
    -- ^ var 2
    , c :: N
    -- ^ var 3
    , ans :: N
    -- ^ previous answer
    , f :: Expr
    -- ^ custom function 1
    , g :: Expr
    -- ^ custom function 2
    , h :: Expr
    -- ^ custom function 3
    }
