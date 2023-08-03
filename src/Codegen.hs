module Codegen (module C, cgen) where

import Codegen.Blocks as C
import Codegen.Codegen as C
import Codegen.Mangler as C

import Lisp qualified as L
import PythonAst qualified as P

cgen :: L.Lisp -> Codegen ()
cgen L.None = setRet (P.Constant P.None)
cgen (L.Int v) = setRet (P.Constant (P.Int v))
cgen (L.Float v) = setRet (P.Constant (P.Float v))
cgen (L.String v) = setRet (P.Constant (P.String v))
cgen (L.Symbol v) = do
  n <- pickName v
  setRet (P.Name n P.Load)
cgen (L.Keyword v) = setRet (P.Constant (P.String v))
cgen (L.List v) = do
  elts <- mapM (consume . cgen) v
  setRet (P.List elts P.Load)
cgen _ = error "non implemented yet!"
