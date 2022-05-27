{-# LANGUAGE FlexibleContexts #-}

module Language.Translucent.Result (Result, sub, block, body, (+++)) where

import Control.Monad.Writer
import Language.Translucent.PythonAst

type Result = Writer [Statement] Expression

sub :: (MonadWriter [Statement] m) => Result -> m Expression
sub m = do
  let (e, s) = runWriter m
  tell s
  return e

block :: Result -> [Statement]
block x = case runWriter x of
  (Constant None, []) -> [Pass]
  (Constant None, s) -> s
  (e, s) -> s ++ [Expr e]

body :: Result -> [Statement]
body x = case runWriter x of
  (Constant None, []) -> [Pass]
  (Constant None, s) -> s
  (e, s) -> s ++ [Return e]

(+++) :: Result -> Result -> Result
a +++ b = do
  let (e, s) =
        let (e1, s1) = runWriter a
            (e2, s2) = runWriter b
         in (e2, s1 ++ [Expr e1] ++ s2)
  tell s
  return e
