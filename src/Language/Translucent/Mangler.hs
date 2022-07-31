{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Mangler
  ( ManglerT,
    ManglerState,
    genName,
    mangle,
    evalMangler,
  )
where

import Control.Monad.State
import Data.Char (GeneralCategory (..), generalCategory)
import Data.Either (isRight)
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec

data ManglerState = ManglerState
  { defined :: Set Text,
    mangled :: Map Text Text,
    counter :: Integer
  }

type ManglerT m = StateT ManglerState m

nameGenRule :: Integer -> Text
nameGenRule x = "__tr_gen_n" <> pack (show x)

genName :: MonadState ManglerState m => m Text
genName =
  state
    ( \s ->
        let (c, generatedName) = findValidName (defined s) (counter s)
         in ( generatedName,
              s
                { defined = S.insert generatedName (defined s),
                  counter = c + 1
                }
            )
    )
  where
    nameAndCounterIter counter = map (\x -> (x, nameGenRule x)) [counter ..]
    findValidName defined counter = fromJust $ L.find (not . (`S.member` defined) . snd) (nameAndCounterIter counter)

mangle :: MonadState ManglerState m => Text -> m Text
mangle x =
  lookupMangled x >>= \case
    (Just y) -> return y
    Nothing -> if isValidPythonId (unpack x) then return x else genName >>= putInMangled
  where
    lookupMangled x = gets (M.lookup x . mangled)
    putInMangled n =
      state
        ( \s ->
            ( n,
              s
                { defined = S.insert n (defined s),
                  mangled = M.insert x n (mangled s)
                }
            )
        )

evalMangler :: Monad m => ManglerT m a -> m a
evalMangler = (`evalStateT` ManglerState S.empty M.empty 0)

pyIdent :: Parsec Void String String
pyIdent = do
  start <- satisfy ((`S.member` id_start) . generalCategory)
  continue <- many (satisfy ((`S.member` id_continue) . generalCategory))
  return (start : continue)
  where
    id_start = S.fromList [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, LetterNumber]
    id_continue = id_start <> S.fromList [NonSpacingMark, SpacingCombiningMark, DecimalNumber, ConnectorPunctuation]

isValidPythonId :: String -> Bool
isValidPythonId "" = error "Got an empty identifier"
isValidPythonId x = not (S.member x reserved_keywords) && isRight (runParser (pyIdent <* eof) "" x)
  where
    reserved_keywords =
      S.fromList
        [ "False",
          "await",
          "else",
          "import",
          "pass",
          "None",
          "break",
          "except",
          "in",
          "raise",
          "True",
          "class",
          "finally",
          "is",
          "return",
          "and",
          "continue",
          "for",
          "lambda",
          "try",
          "as",
          "def",
          "from",
          "nonlocal",
          "while",
          "assert",
          "del",
          "global",
          "not",
          "with",
          "async",
          "elif",
          "if",
          "or",
          "yield"
        ]
