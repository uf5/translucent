{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Translation (translate, transpile) where

import Data.List (singleton)
import Language.Translucent.Form
import Language.Translucent.Lisp qualified as L
import Language.Translucent.Python qualified as P
import Language.Translucent.TransM

type FL a = Form L.Lisp' a

fLisp' :: FL L.Lisp'
fLisp' = satisfy pure

fLisp :: FL L.Lisp
fLisp = satisfy (\case (L.Lisp' v _) -> pure v)

fBool :: FL Bool
fBool = satisfy $ \case
  (L.Lisp' (L.Bool v) _) -> pure v
  _ -> Nothing

fInt :: FL Integer
fInt = satisfy $ \case
  (L.Lisp' (L.Int v) _) -> pure v
  _ -> Nothing

fFloat :: FL Float
fFloat = satisfy $ \case
  (L.Lisp' (L.Float v) _) -> pure v
  _ -> Nothing

fString :: FL String
fString = satisfy $ \case
  (L.Lisp' (L.String v) _) -> pure v
  _ -> Nothing

fSymbol :: FL String
fSymbol = satisfy $ \case
  (L.Lisp' (L.Symbol v) _) -> pure v
  _ -> Nothing

fSymbol' :: String -> FL String
fSymbol' v = satisfy $ \case
  (L.Lisp' (L.Symbol x) _) | x == v -> pure v
  _ -> Nothing

fKeyword :: FL String
fKeyword = satisfy $ \case
  (L.Lisp' (L.Keyword v) _) -> pure v
  _ -> Nothing

fSExp :: FL a -> FL a
fSExp x = Form $ \s -> do
  (elts, rest) <- applyArgs' sat s
  res <- applyArgs x elts
  pure (res, rest)
  where
    sat = satisfy $ \case
      (L.Lisp' (L.SExp elts) _) -> pure elts
      _ -> Nothing

fBrackets :: FL a -> FL a
fBrackets x = Form $ \s -> do
  (elts, rest) <- applyArgs' sat s
  res <- applyArgs x elts
  pure (res, rest)
  where
    sat = satisfy $ \case
      (L.Lisp' (L.Brackets elts) _) -> pure elts
      _ -> Nothing

namedForms :: [(String, FL (TransM ()))]
namedForms =
  [ ("do", mapM_ translate <$> many fLisp')
  ]

translate :: L.Lisp' -> TransM ()
translate (L.Lisp' (L.Int v) _) = pushExpr (P.Constant (P.Int v))
translate (L.Lisp' (L.Float v) _) = pushExpr (P.Constant (P.Float v))
translate (L.Lisp' (L.String v) _) = pushExpr (P.Constant (P.String v))
translate (L.Lisp' (L.Symbol name) _) = do
  name' <- mangle name
  pushExpr P.Name {P._ctx = P.Load, P._id = name'}
translate (L.Lisp' (L.SExp (hd : rest)) l) =
  maybe
    asCall
    asForm
    ( do
        hdStr <- unwrapSymbolMaybe hd
        lookup hdStr namedForms
    )
  where
    unwrapSymbolMaybe :: L.Lisp' -> Maybe String
    unwrapSymbolMaybe (L.Lisp' (L.Symbol x) _) = pure x
    unwrapSymbolMaybe _ = Nothing
    asForm :: FL (TransM ()) -> TransM ()
    asForm f =
      either
        ( \e ->
            throwError (TranspilerError' (FormApplicationError e) l)
        )
        id
        (applyArgs f rest)
    asCall :: TransM ()
    asCall = do
      fn <- toEx (translate hd)
      args' <- mapM (toEx . translate) rest
      pushExpr (P.Call {P._func = fn, P._args = args', P._keywords = []})
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
