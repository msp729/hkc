{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}

module Calc.Parse.Common (opts, accept, unary, binary, ternary) where

import Control.Applicative (asum)
import Control.Monad (void)
import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec, ParsecT, Stream (Token, Tokens), label, try)
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (symbol)

accept :: (MonadParsec e s m, Token s ~ Char) => Tokens s -> m ()
accept = void . symbol space

opts :: (Foldable f, Functor f, Ord e, Stream s) => f (ParsecT e s m a) -> ParsecT e s m a
opts = asum . fmap try

unary :: (MonadParsec e Text m) => Text -> (a -> b) -> m a -> m b
unary fname fval operand = label (unpack fname) $ fmap fval $ accept fname *> operand

binary :: (MonadParsec e Text m) => Text -> (a -> b -> c) -> m a -> m b -> m c
binary fname fval op1 op2 = do accept fname; fval <$> op1 <*> op2

ternary :: (MonadParsec e Text m) => Text -> (a -> b -> c -> d) -> m a -> m b -> m c -> m d
ternary n f ma mb mc = do accept n; f <$> ma <*> mb <*> mc
