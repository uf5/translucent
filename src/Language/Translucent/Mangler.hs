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
import Data.CharSet as CS
import Data.CharSet.Unicode
import Data.HashMap.Lazy as HM
import Data.HashSet as HS
import Data.List as L (find)
import Data.Maybe (fromJust)
import Data.Text

data ManglerState = ManglerState
  { defined :: HashSet Text,
    mangled :: HashMap Text Text,
    counter :: Integer
  }

type ManglerT m = StateT ManglerState m

nameGenRule :: Integer -> Text
nameGenRule x = "__tr_gen_n" <> pack (show x)

genName :: MonadState ManglerState m => m Text
genName =
  state
    ( \manglerState ->
        let def = defined manglerState
            (new_c, generatedName) =
              fromJust $
                L.find
                  ( not
                      . (`HS.member` def)
                      . snd
                  )
                  (Prelude.map (\x -> (x, nameGenRule x)) [(counter manglerState) ..])
         in ( generatedName,
              manglerState
                { defined = HS.insert generatedName def,
                  counter = new_c + 1
                }
            )
    )

mangle :: MonadState ManglerState m => Text -> m Text
mangle x =
  lookupMangled x >>= \case
    (Just y) -> return y
    Nothing -> if isValidPythonId (unpack x) then return x else genName >>= putInMangled
  where
    lookupMangled x = gets (HM.lookup x . mangled)
    putInMangled n =
      state
        ( \s ->
            ( n,
              s
                { defined = HS.insert n (defined s),
                  mangled = HM.insert x n (mangled s)
                }
            )
        )

isValidPythonId :: String -> Bool
isValidPythonId "" = error "Got an empty identifier"
isValidPythonId x =
  not (HS.member x keywords)
    && let (start : continue) = x
        in CS.member start id_start && Prelude.all (`CS.member` id_continue) continue
  where
    id_start = uppercaseLetter <> lowercaseLetter <> titlecaseLetter <> modifierLetter <> otherLetter <> letterNumber
    id_continue = id_start <> nonSpacingMark <> spacingCombiningMark <> decimalNumber <> connectorPunctuation
    -- Python 3.10 keywords
    keywords =
      HS.fromList
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

evalMangler :: Monad m => ManglerT m a -> m a
evalMangler = (`evalStateT` ManglerState HS.empty HM.empty 0)
