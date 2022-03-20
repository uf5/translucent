module Trans where

import Control.Monad
import PythonAST as P
import Types as T

transExpr :: LispVal -> Expr
transExpr T.None = Constant P.None
transExpr (T.Bool v) = (Constant . P.Bool) v
transExpr (T.Int v) = (Constant . P.Int) v
transExpr (T.Float v) = (Constant . P.Float) v
transExpr (T.String v) = (Constant . P.String) v
transExpr (T.Symbol v) = P.Name v Load
transExpr (T.Tuple v) = P.Tuple (map transExpr v) Load
transExpr (T.List v) = P.List (map transExpr v) Load
transExpr (T.SExp v) = do
  let func:args = map transExpr v
  P.Call func args []

transExpr _ = error "Not yet implemented"

transStmt :: LispVal -> Stmt
transStmt _ = error "TODO"

-- trans :: [LispVal] -> PyModule
-- trans = map transStmt
