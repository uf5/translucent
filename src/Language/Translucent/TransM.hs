{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Translucent.TransM (
  BlockContext (..),
  TranspilerState (..),
  initialState,
  TranspilerError (..),
  TranspilerError' (..),
  te2ge,
  TransM (..),
  pushStmt,
  pushExpr,
  mangle,
  extract,
  evalTranspiler,
  gets,
  module E,
)
where

import Control.Monad.Except
import Control.Monad.Except as E (throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State
import Data.Char qualified as C
import Language.Translucent.Error
import Language.Translucent.Lisp qualified as L
import Language.Translucent.Parser (RLocation (..))
import Language.Translucent.Python qualified as P

data BlockContext
  = Expression
  | Statement

data TranspilerState = TranspilerState
  { -- Block
    stmts :: [P.Statement],
    expr :: P.Expression,
    context :: BlockContext,
    -- Mangler state
    manglerPrefix :: String,
    mangledNames :: [(String, String)]
  }

initialState :: TranspilerState
initialState =
  TranspilerState
    { stmts = [],
      expr = P.Constant P.None,
      context = Statement,
      manglerPrefix = "__tr_gen_n",
      mangledNames = []
    }

data TranspilerError
  = UnexpectedExpression
  | NotYetImplemented
  deriving (Show)

data TranspilerError' = TranspilerError'
  { err :: TranspilerError,
    loc :: L.RLocation
  }

te2ge :: Int -> TranspilerError' -> GeneralError
te2ge
  sourceLength
  ( TranspilerError'
      { err = e,
        loc = (RLocation rl)
      }
    ) = GeneralError (show e) (Location (sourceLength - rl))

instance Show TranspilerError' where
  show (TranspilerError' {err = e}) = show e

newtype TransM a = TransM
  { runTranspiler :: (StateT TranspilerState (Except TranspilerError')) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState TranspilerState,
      MonadError TranspilerError'
    )

pushStmt :: P.Statement -> TransM ()
pushStmt stmt = TransM $ StateT $ \s -> pure ((), s {stmts = stmts s ++ [stmt]})

pushExpr :: P.Expression -> TransM ()
pushExpr e = do
  eOld <- gets expr
  unless (hasNoSideEffects eOld) (pushStmt (P.Expr eOld))
  modify $ \s -> s {expr = e}
  where
    hasNoSideEffects (P.Constant _) = True
    hasNoSideEffects (P.Name _ _) = True
    hasNoSideEffects _ = False

mangle :: String -> TransM String
mangle name
  | isValidPythonName name = pure name
  | otherwise = do
      names <- gets mangledNames
      case lookup name names of
        Just x -> pure x
        Nothing -> do
          prefix <- gets manglerPrefix
          let name' = prefix ++ show (length names)
          modify $ \s -> s {mangledNames = mangledNames s ++ [(name, name')]}
          pure name'
  where
    isValidPythonName (h : t) = isMemOfCat charStart h && all (isMemOfCat charCont) t
    isValidPythonName [] = undefined
    isMemOfCat cat = (`elem` cat) . C.generalCategory
    charStart =
      [ C.UppercaseLetter,
        C.LowercaseLetter,
        C.TitlecaseLetter,
        C.ModifierLetter,
        C.OtherLetter,
        C.LetterNumber
      ]
    charCont =
      charStart
        ++ [ C.NonSpacingMark,
             C.SpacingCombiningMark,
             C.DecimalNumber,
             C.ConnectorPunctuation
           ]

extract :: TransM ([P.Statement], P.Expression)
extract = do
  s <- gets stmts
  e <- gets expr
  pure (s, e)

evalTranspiler :: TransM a -> TranspilerState -> Either TranspilerError' a
evalTranspiler t s =
  runIdentity
    ( runExceptT
        ( evalStateT
            (runTranspiler t)
            s
        )
    )
