{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Translucent.Mangler
  ( ManglerT,
    ManglerState,
    reserve,
    genName,
    evalMangler,
  )
where

import Control.Monad.State
import Data.Bifunctor
import Data.HashSet as HS hiding (map)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text hiding (find, map)

type ManglerState = (HashSet Text, Integer)

type ManglerT m = StateT ManglerState m

nameGenRule :: Integer -> Text
nameGenRule x = "__tr_gen_n" <> pack (show x)

reserve :: MonadState ManglerState m => Text -> m ()
reserve x = modify (first (insert x))

genName :: MonadState ManglerState m => m Text
genName =
  state
    ( \(ns, c) ->
        let (new_c, name) = fromJust $ find (not . (`member` ns) . snd) (map (\x -> (x, nameGenRule x)) [c ..])
         in (name, (insert name ns, new_c + 1))
    )

evalMangler = (`evalState` (HS.empty, 0))
