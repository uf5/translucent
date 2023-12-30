module Language.Translucent.Translation (translate, transpile) where

import Data.List (singleton)
import Language.Translucent.Lisp qualified as L
import Language.Translucent.Python qualified as P
import Language.Translucent.TransM

translate :: L.Lisp' -> TransM ()
translate (L.Lisp' (L.Int v) _) = pushExpr (P.Constant (P.Int v))
translate (L.Lisp' (L.Float v) _) = pushExpr (P.Constant (P.Float v))
translate (L.Lisp' (L.String v) _) = pushExpr (P.Constant (P.String v))
translate (L.Lisp' (L.Symbol name) _) = do
  name' <- mangle name
  pushExpr P.Name {P._ctx = P.Store, P._id = name'}
translate (L.Lisp' (L.Brackets elts) _) = do
  elts' <- mapM transElem elts
  pushExpr P.List {P._elts = elts', P._ctx = P.Store}
  where
    transElem :: L.Lisp' -> TransM P.Expression
    transElem x = do
      translate x
      gets expr
translate (L.Lisp' _ l) = throwError (TranspilerError' NotYetImplemented l)

transpile :: [L.Lisp'] -> Either TranspilerError' [P.Statement]
transpile = (`evalTranspiler` initialState) . transpile'
  where
    transpile' p =
      mapM_ translate p
        >>= const extract
        >>= \(s, e) -> pure (s ++ singleton (P.Expr e))
