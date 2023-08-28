module Transpiler.Block where

import Optics
import Python qualified as P
import Transpiler.Monad

pushStmts :: [P.Statement] -> Transpiler ()
pushStmts x = modifying (block % stmts) (++ x)

pushStmt :: P.Statement -> Transpiler ()
pushStmt x = pushStmts [x]

pushExpr :: P.Expression -> Transpiler ()
pushExpr = pushStmt . P.Expr

setRet :: P.Expression -> Transpiler ()
setRet = modifying (block % expr) . const

pushRet :: P.Expression -> Transpiler ()
pushRet x = do
  oldRet <- use (block % expr)
  pushExpr oldRet
  setRet x

consume :: Transpiler a -> Transpiler P.Expression
consume x = x *> extract <&> snd

extract :: Transpiler ([P.Statement], P.Expression)
extract = do
  b <- use block
  pure (b ^. stmts, b ^. expr)
