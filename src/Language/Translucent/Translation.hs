{-# LANGUAGE LambdaCase #-}

module Language.Translucent.Translation (translate, transpile) where

import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (singleton)
import Data.Maybe (catMaybes, fromJust, isJust)
import Language.Translucent.Lisp qualified as L
import Language.Translucent.Parser qualified as Q
import Language.Translucent.Python qualified as P
import Language.Translucent.TransM

type Form a = Q.Parser L.Lisp' a

satisfy' :: (i -> Maybe a) -> Q.Parser i a
satisfy' predicate = fromJust . predicate <$> Q.satisfy (isJust . predicate)

satWrLisp :: (L.Lisp -> Maybe a) -> Q.Parser L.Lisp' a
satWrLisp predicate = satisfy' (predicate . unwrapLisp')
  where
    unwrapLisp' L.Lisp' {L.expr = e} = e

fInt :: Form Integer
fInt = satWrLisp $ \case
  L.Int v -> pure v
  _ -> Nothing

fFloat :: Form Float
fFloat = satWrLisp $ \case
  L.Float v -> pure v
  _ -> Nothing

fSymbol :: Form String
fSymbol = satWrLisp $ \case
  L.Symbol v -> pure v
  _ -> Nothing

fKeyword :: Form String
fKeyword = satWrLisp $ \case
  L.Keyword v -> pure v
  _ -> Nothing

fAny :: Form L.Lisp'
fAny = Q.satisfy (const True)

namedForms :: [(String, Form (TransM ()))]
namedForms = [("do", mapM_ translate <$> Q.many fAny)]

translate :: L.Lisp' -> TransM ()
translate (L.Lisp' (L.Int v) _) = pushExpr (P.Constant (P.Int v))
translate (L.Lisp' (L.Float v) _) = pushExpr (P.Constant (P.Float v))
translate (L.Lisp' (L.String v) _) = pushExpr (P.Constant (P.String v))
translate (L.Lisp' (L.Symbol name) _) = do
  name' <- mangle name
  pushExpr P.Name {P._ctx = P.Load, P._id = name'}
translate (L.Lisp' (L.SExp (hd : rest)) fallback) =
  maybe
    asFnCall
    asForm
    (hdAsSym >>= (`lookup` namedForms))
  where
    hdAsSym = case hd of
      (L.Lisp' (L.Symbol v) _) -> pure v
      _ -> Nothing
    eltsLoc = map L.loc rest
    asForm :: Form (TransM ()) -> TransM ()
    asForm form =
      either
        (throwError . formErr2te)
        id
        (Q.runParser form rest)
    asFnCall = either (throwError . formErr2te) transArgsAndKwargs (Q.runParser fnCallForm rest)
    transArgsAndKwargs (kwargs, args) = do
      fn <- translate hd >> toEx
      args' <- mapM (\x -> translate x >> toEx) args
      kwargs' <-
        mapM
          ( \(k, v) -> do
              v' <- translate v >> toEx
              pure P.Keyword {P.arg = k, P.value = v'}
          )
          kwargs
      pushExpr
        P.Call
          { P._func = fn,
            P._args = args',
            P._keywords = kwargs'
          }
    fnCallForm =
      bimap
        catMaybes
        catMaybes
        . unzip
        <$> Q.many
          ( ((\k v -> (pure (k, v), Nothing)) <$> fKeyword <*> fAny)
              Q.<|> ((\v -> (Nothing, pure v)) <$> fAny)
          )
    formErr2te
      ( Q.ParseError'
          { Q.err = e,
            Q.loc = e_l
          }
        ) = asTE e
        where
          asTE (Q.Unexpected _) =
            TranspilerError'
              { err = WrongType,
                loc = eltsLoc !! Q.location e_l
              }
          asTE Q.ExpectedEOF =
            TranspilerError'
              { err = TooManyArguments,
                loc = fallback
              }
          asTE Q.EOF =
            TranspilerError'
              { err = NotEnoughArguments,
                loc = fallback
              }
          asTE Q.Empty =
            undefined
translate (L.Lisp' (L.Brackets elts) _) = do
  elts' <- mapM (\x -> translate x >> toEx) elts
  pushExpr P.List {P._elts = elts', P._ctx = P.Load}
translate (L.Lisp' _ l) = throwError (TranspilerError' NotYetImplemented l)

transpile :: [L.Lisp'] -> Either TranspilerError' [P.Statement]
transpile = (`evalTranspiler` initialState) . transpile'
  where
    transpile' p =
      mapM_ translate p
        >>= const extract
        >>= \(s, e) -> pure (s ++ singleton (P.Expr e))
