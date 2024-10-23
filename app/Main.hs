module Main (main) where

import System.Console.Haskeline (runInputT)
import Calc.Prog (newton, st, loop, ci)

main :: IO ()
main = do
    runInputT st $ loop ci
