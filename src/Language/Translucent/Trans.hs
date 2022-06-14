module Language.Translucent.Trans (trans, transStmts, transModule) where

import Control.Monad.Writer
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (pack)
import Language.Translucent.PythonAst as P
import Language.Translucent.Result
import Language.Translucent.Types as T

forms :: HashMap String ([LispVal] -> Result)
forms =
  HashMap.fromList
    [ ( "do",
        foldl1 (+++) . map trans
      ),
      ( "if",
        ( \[x, y, z] -> do
            cond <- sub x
            tell [If cond (block y) (block z)]
            return $ Constant P.None
        )
          . map trans
      ),
      -- TODO
      ( "=",
        ( \[a, b] -> do
            a <- sub a
            b <- sub b
            return $ Compare a [Eq] [b]
        )
          . map trans
      )
    ]

trans :: LispVal -> Result
trans T.None = return $ Constant P.None
trans (T.Bool x) = return $ Constant $ P.Bool x
trans (T.Int x) = return $ Constant $ P.Int x
trans (T.Float x) = return $ Constant $ P.Float x
trans (T.String x) = return $ Constant $ P.String $ pack x
trans (T.Symbol x) = return $ P.Name (pack x) P.Load
trans (T.SExp (h : t)) = case lookupForm of
  (Just form) -> form t
  Nothing -> do
    fn <- sub $ trans h
    args <- mapM (sub . trans) t
    return $ Call fn args []
  where
    lookupForm = case h of
      (T.Symbol x) -> HashMap.lookup x forms
      _ -> Nothing
trans x = error $ "unknown expression: " ++ show x

transStmts :: [LispVal] -> [Statement]
transStmts = block . foldl1 (+++) . map trans

transModule :: [LispVal] -> P.Module
transModule x = P.Module (transStmts x) []
