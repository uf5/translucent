module Transpiler.Translation where

import Control.Monad.Except
import Lisp qualified as L
import Optics
import Python qualified as P
import Transpiler.Block
import Transpiler.Mangler
import Transpiler.Monad

newtype Form = Form Text

trans :: L.Lisp -> Transpiler ()
trans (L.None, _) = pushRet (P.Constant P.None)
trans (L.Int x, _) = pushRet (P.Constant (P.Int x))
trans (L.Float x, _) = pushRet (P.Constant (P.Float x))
trans (L.String x, _) = pushRet (P.Constant (P.String x))
trans (L.Symbol x, _) = mangle x >>= pushRet . (`P.Name` P.Load)
trans (L.Keyword _, loc) = throwError (TE "Unexpected keyword" loc)
trans (L.SExp xs, loc) = maybe onEmpty dispatchSExp (nonEmpty xs)
  where
    onEmpty = throwError (TE "Empty S-Expression" loc)
    dispatchSExp :: NonEmpty L.Lisp -> Transpiler ()
    dispatchSExp = asFunc
    asFunc :: NonEmpty L.Lisp -> Transpiler ()
    asFunc elts = do
      (f :| args) <- mapM (consume . trans) elts
      pushRet (P.Call f args [])
trans (L.List elts, _) = mapM (consume . trans) elts >>= pushRet . (`P.List` P.Load)
trans (L.Dict elts, loc) = undefined
