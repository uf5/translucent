{-# LANGUAGE ExistentialQuantification #-}

module Trans where

import Control.Monad
import PythonAST as P
import Types as T

sexpStmtForms :: [(String, [LispVal] -> Statement)]
sexpStmtForms =
  [ ( "if",
      \[test, body, orelse] -> P.If (trans test) [trans body] [trans orelse]
    )
  ]


class Translate a where
  trans :: LispVal -> a

instance Translate Statement where
  trans (T.SExp v) = do
    let nameSymbol : args = v
    let name = case nameSymbol of
          (T.Symbol v) -> v
          _ -> error "Wrong S-Expression syntax"
    case lookup name sexpStmtForms of
      (Just form) -> form args
      Nothing -> (P.Expression . trans . T.SExp) v
  trans v = (P.Expression . trans) v

instance Translate Expression where
  trans T.None = Constant P.None
  trans (T.Bool v) = (Constant . P.Bool) v
  trans (T.Int v) = (Constant . P.Int) v
  trans (T.Float v) = (Constant . P.Float) v
  trans (T.String v) = (Constant . P.String) v
  trans (T.Symbol v) = P.Name v P.Load
  trans (T.Tuple v) = P.Tuple (map trans v) P.Load
  trans (T.List v) = P.List (map trans v) P.Load
  trans (T.SExp v) = do
    let func : args = map trans v
    P.Call func args []
  trans e = error ("Lisp expression: " ++ show e ++ " is not yet implemented")

transExpr :: LispVal -> P.Expression
transExpr = trans

transStmt :: LispVal -> P.Statement
transStmt = trans
