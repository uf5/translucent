{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Result
  ( Result,
    ResultT,
    block,
    blockT,
    comb,
  )
where

import Control.Monad.Writer
import Data.Functor ((<&>))
import Language.Translucent.PythonAst hiding (id)

type Result = Writer [Statement] Expression

type ResultT m = WriterT [Statement] m Expression

-- there is no need for a sub functon
-- subT :: (Monad m) => ResultT m -> ResultT m
-- subT = id

block :: Result -> [Statement]
block x = case runWriter x of
  (Constant None, []) -> [Pass]
  (Constant None, s) -> s
  (e, s) -> s ++ [Expr e]

blockT :: Monad m => ResultT m -> m [Statement]
blockT x =
  runWriterT x
    <&> ( \case
            (Constant None, []) -> [Pass]
            (Constant None, s) -> s
            (e, s) -> s ++ [Expr e]
        )

comb :: Monad m => ResultT m -> ResultT m -> ResultT m
comb a b = WriterT $ do
  (e1, s1) <- runWriterT a
  (e2, s2) <- runWriterT b
  return (e2, s1 ++ [Expr e1] ++ s2)
