module Main (main) where

import Calc.Prog (ci, loop, newton, st)
import System.Console.Haskeline (runInputT)

main :: IO ()
main = do
    runInputT st $ loop ci
