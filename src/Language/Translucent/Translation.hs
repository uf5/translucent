{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Translation (translate, transpile) where

import Data.List (singleton)
import Data.Maybe (fromJust, isJust)
import Language.Translucent.Lisp qualified as L
import Language.Translucent.Parser qualified as Q
import Language.Translucent.Python qualified as P
import Language.Translucent.TransM

type Form a = Q.Parser L.Lisp' a

formErr2te :: [Q.Location] -> Q.Location -> Q.ParseError' L.Lisp' -> TranspilerError'
formErr2te eltsLoc fallback (Q.ParseError' {Q.err = e, Q.loc = l}) = asTE e
  where
    asTE (Q.Unexpected _) = TranspilerError' {err = WrongType, loc = eltsLoc !! Q.location l}
    asTE Q.ExpectedEOF = TranspilerError' {err = TooManyArguments, loc = fallback}
    asTE Q.EOF = TranspilerError' {err = NotEnoughArguments, loc = fallback}
    asTE Q.Empty = undefined

satisfy' :: (i -> Maybe a) -> Q.Parser i a
satisfy' predicate = fromJust . predicate <$> Q.satisfy (isJust . predicate)

fInt :: Form Integer
fInt = satisfy' $ \case
  (L.Lisp' (L.Int v) _) -> pure v
  _ -> Nothing

fFloat :: Form Float
fFloat = satisfy' $ \case
  (L.Lisp' (L.Float v) _) -> pure v
  _ -> Nothing

namedForms :: [(String, Form (TransM ()))]
namedForms = [("test", testFormFn <$> fInt <*> fInt <*> fFloat)]
  where
    testFormFn :: Integer -> Integer -> Float -> TransM ()
    testFormFn i1 i2 f = pushExpr $ P.Constant $ P.Float $ fromInteger i1 + fromInteger i2 + f

translate :: L.Lisp' -> TransM ()
translate (L.Lisp' (L.Int v) _) = pushExpr (P.Constant (P.Int v))
translate (L.Lisp' (L.Float v) _) = pushExpr (P.Constant (P.Float v))
translate (L.Lisp' (L.String v) _) = pushExpr (P.Constant (P.String v))
translate (L.Lisp' (L.Symbol name) _) = do
  name' <- mangle name
  pushExpr P.Name {P._ctx = P.Load, P._id = name'}
translate (L.Lisp' (L.SExp (hd : rest)) l) =
  maybe
    asFnCall
    asForm
    (hdAsSym >>= (`lookup` namedForms))
  where
    hdAsSym = case hd of
      (L.Lisp' (L.Symbol v) _) -> pure v
      _ -> Nothing
    asForm :: Form (TransM ()) -> TransM ()
    asForm form =
      either
        (throwError . formErr2te (map L.loc rest) l)
        id
        (Q.runParser form rest)
    asFnCall = undefined
translate (L.Lisp' (L.Brackets elts) _) = do
  elts' <- mapM transElem elts
  pushExpr P.List {P._elts = elts', P._ctx = P.Load}
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
