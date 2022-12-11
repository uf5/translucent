{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Result
  ( ResultT,
    block,
    fnbody,
    comb,
    fromStmt,
  )
where

import Control.Monad.Identity (Identity)
import Control.Monad.Writer
import Data.Functor ((<&>))
import Language.Translucent.Mangler
import Language.Translucent.PythonAst

type ResultT m = WriterT [Statement] m Expression

fromStmt :: Monad m => Statement -> ResultT m
fromStmt s = writer (Constant None, [s])

block x = stmts Expr x

fnbody x = WriterT $ do
  body <- stmts Return x
  name <- genName
  return
    ( Call (Name name Load) [] [],
      [ FunctionDef
          name
          (Arguments [] [] Nothing [] [] Nothing [])
          body
          []
          Nothing
          Nothing
      ]
    )

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
  return (e2, s1 ++ rmUnnecessary e1 ++ s2)
    where rmUnnecessary (Constant None) = []
          rmUnnecessary x = [Expr x]
