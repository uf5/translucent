{-# LANGUAGE FlexibleContexts #-}

module Language.Translucent.Context
  ( ContextT,
    ContextState (..),
    runContext,
    withPrefExpr,
    withPrefStmt,
    ifPrefer,
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

withPrefStmt :: MonadReader ContextState m => m a -> m a
withPrefStmt = local (preferStmtFn True)

withPrefExpr :: MonadReader ContextState m => m a -> m a
withPrefExpr = local (preferStmtFn False)

ifPrefer :: MonadReader ContextState m => m b -> m b -> m b
ifPrefer a b = do
  ContextState {preferStmt = pref} <- ask
  if pref then a else b
