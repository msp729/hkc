module Calc.Common (Ctx (..)) where

import Calc.Expr

data Ctx = Ctx
    { a :: Double -- ^ var 1
    , b :: Double -- ^ var 2
    , c :: Double -- ^ var 3
    , ans :: Double -- ^ previous answer
    , f :: Expr -- ^ custom function 1
    , g :: Expr -- ^ custom function 2
    , h :: Expr -- ^ custom function 3
    }
