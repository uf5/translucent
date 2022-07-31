{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Trans (trans, transStmts, transModule) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Functor
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Translucent.Exceptions
import Language.Translucent.Mangler
import Language.Translucent.PythonAst as P
import Language.Translucent.Result
import Language.Translucent.Types as T

type WrappedResult = ResultT (ManglerT (TransExceptT Identity))

forms :: Map Text ([Lisp] -> WrappedResult)
forms =
  M.fromList
    [ ( "do",
        foldl1 comb . map trans
      ),
      ( "do-fn",
        fnbody . foldl1 comb . map trans
      ),
      ( "set!",
        \[Symbol _ name, value] -> do
          value <- trans value
          writer (Constant P.None, [Assign [P.Name name P.Store] value Nothing])
      ),
      ( "if",
        ( \[x, y, z] -> do
            cond <- x
            yy <- lift $ block y
            zz <- lift $ block z
            writer (Constant P.None, [If cond yy zz])
        )
          . map trans
      ),
      -- TODO
      ( "=",
        mapM trans
          >=> \case
            [] -> undefined
            [_] -> undefined
            (h : t) -> return $ Compare h (replicate (length t) Eq) t
      )
    ]

trans :: Lisp -> WrappedResult
trans (T.None _) = return $ Constant P.None
trans (T.Bool _ x) = return $ Constant $ P.Bool x
trans (T.Int _ x) = return $ Constant $ P.Int x
trans (T.Float _ x) = return $ Constant $ P.Float x
trans (T.String _ x) = return $ Constant $ P.String x
trans (T.Symbol _ x) = mangle x <&> (`P.Name` P.Load)
trans (T.List _ x) =
  mapM trans x
    >>= \elts -> return (P.List elts P.Load)
trans (T.SExp _ (h : t)) = case lookupForm h of
  (Just form) -> form t
  Nothing -> do
    fn <- trans h
    args <- mapM trans t
    return $ Call fn args []
  where
    lookupForm h = case h of
      (T.Symbol _ x) -> M.lookup x forms
      _ -> Nothing
trans x = throwError $ TransError ("unknown expression: " ++ show x)

transStmts :: [Lisp] -> Either TransException [Statement]
transStmts = runIdentity . runExceptT . evalMangler . block . foldl1 comb . map trans

transModule :: [Lisp] -> Either TransException P.Module
transModule x = transStmts x >>= Right . (`Module` [])
