module Codegen.Blocks where

import Codegen.Codegen

import Optics
import PythonAst as P

pushStmts :: [Statement] -> Codegen ()
pushStmts x = modify ((block % stmts) %~ (++ x))

pushStmt :: Statement -> Codegen ()
pushStmt x = pushStmts [x]

pushExpr :: Expression -> Codegen ()
pushExpr x = pushStmt (P.Expr x)

setRet :: Expression -> Codegen ()
setRet x = modify ((block % expr) .~ x)

pushRet :: Expression -> Codegen ()
pushRet x = do
  s <- get
  let currentRet = s ^. (block % expr)
  case currentRet of
    (Constant _) -> setRet x
    _ -> do
      pushExpr currentRet
      setRet x

consume :: Codegen () -> Codegen Expression
consume m = do
  let s = execState (runCodegen m) initialState
  let consumedBody = s ^. (block % stmts)
  pushStmts consumedBody
  let consumedExpr = s ^. (block % expr)
  pure consumedExpr
