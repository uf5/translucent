{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Result
  ( ResultT,
    block,
    stmts,
    comb,
  )
where

import Control.Monad.Writer
import Data.Functor ((<&>))
import Language.Translucent.PythonAst hiding (body, id)

type ResultT m = WriterT [Statement] m Expression

block :: Monad m => ResultT m -> m [Statement]
block = stmts Expr

stmts :: Monad m => (Expression -> Statement) -> ResultT m -> m [Statement]
stmts c x =
  runWriterT x
    <&> ( \case
            (Constant None, []) -> [Pass]
            (Constant None, s) -> s
            (e, s) -> s ++ [c e]
        )

comb :: Monad m => ResultT m -> ResultT m -> ResultT m
comb a b = WriterT $ do
  (e1, s1) <- runWriterT a
  (e2, s2) <- runWriterT b
  return (e2, s1 ++ [Expr e1] ++ s2)
