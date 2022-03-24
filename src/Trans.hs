{-# LANGUAGE ExistentialQuantification #-}

module Trans where

import Control.Monad
import PythonAST as P
import Types as T

newtype Forms a = Forms [(String, a)]

sexpStmtForms :: Forms ([LispVal] -> Statement)
sexpStmtForms =
  Forms
    [ ( "if",
        \[test, body, orelse] -> P.If (trans test) [trans body] [trans orelse]
      )
    ]

sexpExprForms :: Forms ([Expression] -> Expression)
sexpExprForms =
  Forms
    [ ( "if",
        \[test, body, orelse] -> P.IfExp test body orelse
      ),
      ( "+",
        \[a, b] -> P.BinOp a P.Add b
      ),
      ( "-",
        \[a, b] -> P.BinOp a P.Sub b
      ),
      ( "*",
        \[a, b] -> P.BinOp a P.Mult b
      ),
      ( "/",
        \[a, b] -> P.BinOp a P.Div b
      )
    ]

class Translate a where
  trans :: LispVal -> a

instance Translate Statement where
  trans (T.SExp v) = do
    let h : t = v
    case tryGetForm h sexpStmtForms of
      (Just form) -> form t
      Nothing -> (P.Expr . trans . T.SExp) v
  trans v = (P.Expr . trans) v

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
    let h = head v
    let t = map trans (tail v)
    case tryGetForm h sexpExprForms of
      (Just form) -> form t
      Nothing -> P.Call (trans h) t []
  trans e = error ("Lisp expression: " ++ show e ++ " is not yet implemented")

tryGetForm :: LispVal -> Forms a -> Maybe a
tryGetForm (T.Symbol v) (Forms f) =
  case lookup v f of
    Nothing -> Nothing
    v -> v
tryGetForm _ _ = Nothing

transExpr :: LispVal -> P.Expression
transExpr = trans

transStmt :: LispVal -> P.Statement
transStmt = trans

transModule :: [LispVal] -> P.Module
transModule = (`P.Module` []) . map transStmt
