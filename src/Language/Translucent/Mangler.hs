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
import Data.Either (isRight)
import Data.List as L (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text
import Language.Translucent.Parser (pyIdent)
import Text.Megaparsec (MonadParsec (eof), runParser)

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
    ( \manglerState ->
        let def = defined manglerState
            (new_c, generatedName) =
              fromJust $
                L.find
                  ( not
                      . (`S.member` def)
                      . snd
                  )
                  (Prelude.map (\x -> (x, nameGenRule x)) [(counter manglerState) ..])
         in ( generatedName,
              manglerState
                { defined = S.insert generatedName def,
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

isValidPythonId :: String -> Bool
isValidPythonId "" = error "Got an empty identifier"
isValidPythonId x = isRight $ runParser (pyIdent <* eof) "" x

evalMangler :: Monad m => ManglerT m a -> m a
evalMangler = (`evalStateT` ManglerState S.empty M.empty 0)
