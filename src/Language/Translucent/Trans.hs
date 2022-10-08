{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Trans (trans, transStmts, transModule) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Language.Translucent.Context
import Language.Translucent.Exceptions
import Language.Translucent.Lisp as L
import Language.Translucent.Mangler
import Language.Translucent.Params
import Language.Translucent.PythonAst as P
import Language.Translucent.Result

type WrappedResult = ResultT (ManglerT (ContextT (TransExceptT Identity)))

pLisp :: Param Lisp
pLisp = Param p
  where
    p (h : t) = Right (h, t)
    p [] = Left NotEnoughArguments

pTrans :: Param WrappedResult
pTrans = Param p
  where
    p (h : t) = Right (trans h, t)
    p [] = Left NotEnoughArguments

pSymbol :: Param Text
pSymbol = Param p
  where
    p ((Symbol _ x) : t) = Right (x, t)
    p (h : t) = Left $ WrongType (getLoc h)
    p [] = Left NotEnoughArguments

forms :: Map Text (Param WrappedResult)
forms =
  M.fromList
    [ ( "do",
        ( \body ->
            ifPrefer
              (foldl1 comb body)
              (fnbody $ foldl1 comb body)
        )
          <$> some (withPrefStmt <$> pTrans)
      ),
      ( "if",
        ( \x y z -> do
            cond <- withPrefExpr x
            ifPrefer
              ( do
                  y' <- lift $ block y
                  z' <- lift $ block z
                  writer (Constant P.None, [If cond y' z'])
              )
              ( do
                  y' <- withPrefExpr y
                  z' <- withPrefExpr z
                  return (IfExp cond y' z')
              )
        )
          <$> pTrans
          <*> pTrans
          <*> pTrans
      ),
      -- binop
      ("+", binOpHelper P.Add),
      ("-", binOpHelper P.Sub),
      ("*", binOpHelper P.Mult),
      ("@", binOpHelper P.MatMult),
      ("/", binOpHelper P.Div),
      ("%", binOpHelper P.Mod),
      ("**", binOpHelper P.Pow),
      ("<<", binOpHelper P.LShift),
      (">>", binOpHelper P.RShift),
      ("", binOpHelper P.BitOr),
      ("^", binOpHelper P.BitXor),
      ("&", binOpHelper P.BitAnd),
      ("//", binOpHelper P.FloorDiv),
      -- comparison
      (">", compHelper P.Gt),
      ("<", compHelper P.Lt),
      (">=", compHelper P.GtE),
      ("<=", compHelper P.LtE),
      ("=", compHelper P.Eq),
      ("!=", compHelper P.NotEq),
      ("is", compHelper P.Is),
      ("not-is", compHelper P.IsNot),
      ("in", compHelper P.In),
      ("not-in", compHelper P.NotIn),
      -- set & walrus
      ( "set!",
        ( \name value -> do
            value' <- value
            writer (Constant P.None, [P.Assign [P.Name name P.Store] value' Nothing])
        )
          <$> pSymbol <*> pTrans
      ),
      ( "set-exp!",
        ( \target value -> do
            target' <- target
            value' <- value
            return $ P.NamedExpr target' value'
        ) <$> pTrans <*> pTrans
      )
    ]
  where
    pTransExpr = withPrefExpr <$> pTrans
    binOpHelper op =
      (\args -> sequence args <&> foldl1 (`P.BinOp` op))
        <$> ((:) <$> pTransExpr <*> some pTransExpr)
    compHelper op =
      (\args -> sequence args <&> (\(h : t) -> P.Compare h (replicate (length t) op) t))
        <$> ((:) <$> pTransExpr <*> some pTransExpr)

trans :: Lisp -> WrappedResult
trans (L.None _) = return $ Constant P.None
trans (L.Bool _ x) = return $ Constant $ P.Bool x
trans (L.Int _ x) = return $ Constant $ P.Int x
trans (L.Float _ x) = return $ Constant $ P.Float x
trans (L.String _ x) = return $ Constant $ P.String x
trans (L.Symbol _ x) = mangle x <&> (`P.Name` P.Load)
trans (L.List _ elts) = do
  elts' <- mapM (withPrefExpr . trans) elts
  return (P.List elts' P.Load)
trans (L.SExp loc (h : t)) = case lookupForm h of
  (Just form) ->
    case applyArgs form t of
      (Right (x, [])) -> x
      (Right (_, _)) -> throwTransError loc (show TooManyArguments)
      (Left err) ->
        throwTransError
          ( case err of
              (WrongType loc') -> loc'
              _ -> loc
          )
          (show err)
  Nothing -> do
    fn <- withPrefExpr (trans h)
    (args, kws) <- lift $ lift $ lift $ parse_args t
    args' <- mapM (withPrefExpr . trans) args
    let (uz_ids, uz_values) = unzip kws
    kws_v' <- mapM (withPrefExpr . trans) uz_values
    let kws' = zipWith P.Keyword uz_ids kws_v'
    return (Call fn args' kws')
  where
    lookupForm h = case h of
      (L.Symbol _ x) -> M.lookup x forms
      _ -> Nothing
    -- split lisp args & keywords into separate lists of args and keywords
    parse_args :: Monad m => [Lisp] -> TransExceptT m ([Lisp], [(Text, Lisp)])
    parse_args ((L.Keyword loc kw) : t) = case t of
      [] -> throwTransError loc "Keyword expression has no value"
      (arg : t) -> do
        t' <- parse_args t
        return $ combine_tuples ([], [(kw, arg)]) t'
    parse_args (arg : rest) = do
      rest' <- parse_args rest
      return $ combine_tuples ([arg], []) rest'
    parse_args [] = return ([], [])
    -- combine two tupled lists
    combine_tuples (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)
trans (L.Keyword loc _) = throwTransError loc "Unexpected keyword expression"
trans x = throwTransError (getLoc x) "Unknown expression"

transStmts :: [Lisp] -> Either TransError [Statement]
transStmts = runIdentity . runExceptT . runContext . runMangler . block . foldl1 comb . map trans

transModule :: [Lisp] -> Either TransError P.Module
transModule x = transStmts x >>= Right . (`Module` [])
