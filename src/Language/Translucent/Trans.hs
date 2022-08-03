{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Trans (trans, transStmts, transModule) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Language.Translucent.Context
import Language.Translucent.Exceptions
import Language.Translucent.Mangler
import Language.Translucent.PythonAst as P
import Language.Translucent.Result
import Language.Translucent.Types as T

type WrappedResult = ResultT (ManglerT (ContextT (TransExceptT Identity)))

forms :: Map Text ([Lisp] -> WrappedResult)
forms =
  M.fromList
    [ ( "do",
        \lispBody -> do
          ContextState {preferStmt = pref} <- ask
          ( if pref
              then foldl1 comb . map (withPrefStmt . trans)
              else fnbody . foldl1 comb . map (withPrefStmt . trans)
            )
            lispBody
      ),
      ( "set!",
        \[Symbol _ name, value] -> do
          value <- withPrefExpr $ trans value
          writer (Constant P.None, [Assign [P.Name name P.Store] value Nothing])
      ),
      ( "if",
        ( \[x, y, z] -> do
            ContextState {preferStmt = pref} <- ask
            cond <- withPrefExpr x
            if pref
              then do
                yy <- lift $ block y
                zz <- lift $ block z
                writer (Constant P.None, [If cond yy zz])
              else do
                yy <- withPrefExpr y
                zz <- withPrefExpr z
                return (IfExp cond yy zz)
        )
          . map trans
      ),
      ( "=",
        mapM (withPrefExpr . trans)
          >=> \case
            [] -> undefined
            [_] -> undefined
            (h : t) -> return $ Compare h (replicate (length t) Eq) t
      ),
      ( "ctx",
        \_ -> do
          ContextState {preferStmt = pref} <- ask
          return $ Constant $ P.Bool pref
      )
    ]

trans :: Lisp -> WrappedResult
trans (T.None _) = return $ Constant P.None
trans (T.Bool _ x) = return $ Constant $ P.Bool x
trans (T.Int _ x) = return $ Constant $ P.Int x
trans (T.Float _ x) = return $ Constant $ P.Float x
trans (T.String _ x) = return $ Constant $ P.String x
trans (T.Symbol _ x) = mangle x <&> (`P.Name` P.Load)
trans (T.Tuple _ x) =
  mapM (withPrefExpr . trans) x
    >>= \elts -> return (P.Tuple elts P.Load)
trans (T.List _ x) =
  mapM (withPrefExpr . trans) x
    >>= \elts -> return (P.List elts P.Load)
trans (T.SExp _ (h : t)) = case lookupForm h of
  (Just form) -> form t
  Nothing -> do
    fn <- withPrefExpr (trans h)
    args <- mapM (withPrefExpr . trans) t
    return $ Call fn args []
  where
    lookupForm h = case h of
      (T.Symbol _ x) -> M.lookup x forms
      _ -> Nothing
trans x = throwError $ TransError ("unknown expression: " ++ show x)

transStmts :: [Lisp] -> Either TransException [Statement]
transStmts = runIdentity . runExceptT . runContext . runMangler . block . foldl1 comb . map trans

transModule :: [Lisp] -> Either TransException P.Module
transModule x = transStmts x >>= Right . (`Module` [])
