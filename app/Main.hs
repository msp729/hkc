module Main (main) where

import Calc.Algo (deriv)
import Calc.Command (Command (..), Fun (..), Var (..))
import Calc.Common (Ctx (a, ans, b, c, f, g, h), ci)
import Calc.Expr (Expr (Literal))
import Calc.N
import Calc.Parse.Command (command)
import Calc.Parse.Expr (eval, fval)
import Data.Text (pack)
import System.Console.Haskeline (InputT, Settings (..), defaultSettings, getInputLine, noCompletion, outputStrLn, runInputT)
import System.Console.Haskeline.Completion ()
import Text.Megaparsec (errorBundlePretty, runParser)
import Prelude hiding (getLine)

ds, st :: Settings IO
ds = defaultSettings
st = ds{complete = noCompletion}

main :: IO ()
main = do
    runInputT st $ loop ci

newton :: Int -> Ctx -> N -> N -> Expr -> Expr -> Maybe N
newton n ctx eps v0 f f'
    | n < 100 = do
        fv <- eval ctx $ fval f $ Literal v0
        f'v <- eval ctx $ fval f' $ Literal v0
        let v1 = v0 - fv / f'v
        fv1 <- eval ctx $ fval f $ Literal v1
        if fv1 > fv
            then Nothing
            else
                if fv1 < eps
                    then return v1
                    else newton (n + 1) ctx eps v1 f f'
    | otherwise = Nothing

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
                    Zero eps v0 fn -> maybe (bad ctx) (good ctx) $ do
                        eps' <- eval ctx eps
                        v0' <- eval ctx v0
                        newton 0 ctx eps' v0' fn (deriv ctx fn)
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
