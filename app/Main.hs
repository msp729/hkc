module Main (main) where

import Calc.Command (Command (..), Fun (..), Var (..))
import Calc.Common (Ctx (a, ans, b, c, f, g, h), ci)
import Calc.Parse.Command (command)
import Calc.Parse.Expr (eval)
import System.Console.Haskeline (InputT, Settings (..), defaultSettings, outputStrLn, runInputT, getInputLine, noCompletion)
import System.Console.Haskeline.Completion ()
import Text.Megaparsec (errorBundlePretty, runParser)
import Prelude hiding (getLine)
import Data.Text (pack)
import Calc.N hiding(C) 

ds, st :: Settings IO
ds = defaultSettings
st = ds {complete = noCompletion}

main :: IO ()
main = do
    runInputT st $ loop ci

loop :: Ctx -> InputT IO ()
loop ctx = do
    input <- getInputLine "%> "
    case input of
        Nothing -> outputStrLn "Have a nice day!"
        Just inp -> do
            let ecmd = runParser command "<stdin>" $ pack inp
            case ecmd of
                Left err -> do
                    outputStrLn $ errorBundlePretty err
                    loop ctx
                Right cmd -> case cmd of
                    Calculate e -> maybe (bad ctx) (good ctx) $ eval ctx e
                    Assign v e -> maybe (bad ctx) (assign ctx v) $ eval ctx e
                    Define F e -> loop ctx{f = e}
                    Define G e -> loop ctx{g = e}
                    Define H e -> loop ctx{h = e}
                    Quit -> outputStrLn "Have a nice day!"

bad :: Ctx -> InputT IO ()
bad ctx = do
    outputStrLn "Something went wrong."
    loop ctx

good :: Ctx -> N -> InputT IO ()
good ctx val = do
    outputStrLn $ show val
    loop $ ctx{ans = val}

assign :: Ctx -> Var -> N -> InputT IO ()
assign ctx name value = do
    outputStrLn $ show value
    loop $ case name of
        A -> ctx{a = value}
        B -> ctx{b = value}
        C -> ctx{c = value}
