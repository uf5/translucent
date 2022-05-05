{-# LANGUAGE FlexibleContexts #-}

module Result (Result, sub, block, funcbody, (+++)) where

import Control.Monad.State
import PythonAST

type Result = State [Statement] Expression

sub :: (MonadState [Statement] m) => Result -> m Expression
sub m = do
  st <- get
  let (e, s) = runState m st
  put s
  return e

block :: Result -> [Statement]
block x = case runState x [] of
  (Constant None, []) -> [Pass]
  (Constant None, s) -> s
  (e, s) -> s ++ [Expr e]

funcbody :: Result -> [Statement]
funcbody x = case runState x [] of
  (Constant None, []) -> [Pass]
  (Constant None, s) -> s
  (e, s) -> s ++ [Return e]

(+++) :: Result -> Result -> Result
a +++ b = do
  let (e, s) =
        ( let (e1, s1) = runState a []
              (e2, s2) = runState b []
           in (e2, s1 ++ [Expr e1] ++ s2)
        )
  put s
  return e