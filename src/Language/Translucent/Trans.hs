{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Trans (trans, transStmts, transModule) where

import Control.Monad.Writer
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text)
import Language.Translucent.Mangler
import Language.Translucent.PythonAst as P hiding (id)
import Language.Translucent.Result
import Language.Translucent.Types as T

type WrappedResult = ResultT (ManglerT Identity)

forms :: HashMap Text ([LispVal] -> WrappedResult)
forms =
  HM.fromList
    [ ( "do",
        foldl1 comb . map trans
      ),
      ( "do-fn",
        fnbody . foldl1 comb . map trans
      ),
      ( "if",
        ( \[x, y, z] -> do
            cond <- x
            yy <- lift $ block y
            zz <- lift $ block z
            tell [If cond yy zz]
            return $ Constant P.None
        )
          . map trans
      ),
      -- TODO
      ( "=",
        ( \[a, b] -> do
            a <- a
            b <- b
            return $ Compare a [Eq] [b]
        )
          . map trans
      )
    ]

trans :: LispVal -> WrappedResult
trans T.None = return $ Constant P.None
trans (T.Bool x) = return $ Constant $ P.Bool x
trans (T.Int x) = return $ Constant $ P.Int x
trans (T.Float x) = return $ Constant $ P.Float x
trans (T.String x) = return $ Constant $ P.String x
trans (T.Symbol x) = mangle x <&> (`P.Name` P.Load)
trans (T.SExp (h : t)) = case lookupForm h of
  (Just form) -> form t
  Nothing -> do
    fn <- trans h
    args <- mapM trans t
    return $ Call fn args []
  where
    lookupForm h = case h of
      (T.Symbol x) -> HM.lookup x forms
      _ -> Nothing
trans x = error $ "unknown expression: " ++ show x

transStmts :: [LispVal] -> [Statement]
transStmts = evalMangler . block . foldl1 comb . map trans

transModule :: [LispVal] -> P.Module
transModule x = P.Module (transStmts x) []
