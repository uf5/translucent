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
import Language.Translucent.Lisp as L
import Language.Translucent.Mangler
import Language.Translucent.PythonAst as P
import Language.Translucent.Result

type WrappedResult = ResultT (ManglerT (ContextT (TransExceptT Identity)))

forms :: Map Text ([Lisp] -> WrappedResult)
forms =
  M.fromList
    [ ( "do",
        \lispBody ->
          ifPrefer
            (foldl1 comb $ map (withPrefStmt . trans) lispBody)
            (fnbody $ foldl1 comb $ map (withPrefStmt . trans) lispBody)
      ),
      ( "set!",
        \[Symbol _ name, value] -> do
          value <- withPrefExpr $ trans value
          writer (Constant P.None, [Assign [P.Name name P.Store] value Nothing])
      ),
      ( "if",
        ( \[x, y, z] -> do
            cond <- withPrefExpr x
            ifPrefer
              ( do
                  y' <- lift $ block y
                  z' <- lift $ block z
                  writer (Constant P.None, [If cond y' z'])
              )
              ( do
                  y' <- withPrefExpr y
                  z' <- withPrefExpr z
                  return (IfExp cond y' z')
              )
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
      ( "quote!",
        \[x] -> case x of
          (SExp _ elts) -> do
            elts' <- mapM (withPrefExpr . trans) elts
            return $ P.Tuple elts' P.Load
          _ -> trans x
      )
    ]

trans :: Lisp -> WrappedResult
trans (L.None _) = return $ Constant P.None
trans (L.Bool _ x) = return $ Constant $ P.Bool x
trans (L.Int _ x) = return $ Constant $ P.Int x
trans (L.Float _ x) = return $ Constant $ P.Float x
trans (L.String _ x) = return $ Constant $ P.String x
trans (L.Symbol _ x) = mangle x <&> (`P.Name` P.Load)
trans (L.List _ elts) = do
  elts' <- mapM (withPrefExpr . trans) elts
  return (P.List elts' P.Load)
trans (L.SExp _ (h : t)) = case lookupForm h of
  (Just form) -> form t
  Nothing -> do
    fn <- withPrefExpr (trans h)

    (args, kws) <- lift $ lift $ lift $ parse_args t
    args' <- mapM (withPrefExpr . trans) args
    let (uz_ids, uz_values) = unzip kws
    kws_v' <- mapM (withPrefExpr . trans) uz_values
    let kws' = zipWith P.Keyword uz_ids kws_v'
    return (Call fn args' kws')
  where
    lookupForm h = case h of
      (L.Symbol _ x) -> M.lookup x forms
      _ -> Nothing
    -- split lisp args & keywords into separate lists of args and keywords
    parse_args :: Monad m => [Lisp] -> TransExceptT m ([Lisp], [(Text, Lisp)])
    parse_args ((L.Keyword loc kw) : t) = case t of
      [] -> throwTransError loc "Keyword expression has no value"
      (arg : t) -> do
        t' <- parse_args t
        return $ combine_tuples ([], [(kw, arg)]) t'
    parse_args (arg : t) = do
      t' <- parse_args t
      return $ combine_tuples ([arg], []) t'
    parse_args [] = return ([], [])
    -- combine two tupled lists
    combine_tuples (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)
trans (L.Keyword loc _) = throwTransError loc "Unexpected keyword expression"
trans x = throwTransError (getLoc x) "Unknown expression"

transStmts :: [Lisp] -> Either TransError [Statement]
transStmts = runIdentity . runExceptT . runContext . runMangler . block . foldl1 comb . map trans

transModule :: [Lisp] -> Either TransError P.Module
transModule x = transStmts x >>= Right . (`Module` [])
