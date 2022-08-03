{-# LANGUAGE FlexibleContexts #-}

module Language.Translucent.Context
  ( ContextT,
    ContextState (..),
    runContext,
    withPrefExpr,
    withPrefStmt,
  )
where

import Control.Monad.Reader
import Language.Translucent.PythonAst

type ContextT m = ReaderT ContextState m

data ContextState = ContextState
  { preferStmt :: Bool
  }

runContext x = runReaderT x ContextState {preferStmt = True}

preferStmtFn x s = s {preferStmt = x}

withPrefStmt x = local (preferStmtFn True) x

withPrefExpr x = local (preferStmtFn False) x
