module Language.Translucent.Expansion (expandModule) where

import Language.Translucent.Types
import Language.Translucent.Util

expand :: [(String, LispVal)] -> LispVal -> LispVal
expand r1 (SExp ((Symbol "where") : (List defs) : body)) =
  let r2 = r1 ++ concatMap (\(SExp [Symbol name, value]) -> [(name, expand r1 value)]) defs
   in wrapInDoIfNeeded (map (expand r2) body)
expand r (Symbol x) = case lookup x r of
  (Just v) -> v
  Nothing -> Symbol x
expand _ (SExp x) = SExp $ map (expand []) x
expand _ (List x) = List $ map (expand []) x
expand _ (Tuple x) = Tuple $ map (expand []) x
expand _ (Set x) = Tuple $ map (expand []) x
expand _ x = x

expandModule :: [LispVal] -> [LispVal]
expandModule = map (expand [])
