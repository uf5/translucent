module Language.Translucent.Expansion (expand) where

import Language.Translucent.Types

expand :: LispVal -> LispVal
expand = expand_ []
  where
    expand_ :: [(String, LispVal)] -> LispVal -> LispVal
    expand_ r1 (SExp ((Symbol "where") : (List defs) : body)) =
      let r2 = r1 ++ concatMap (\(SExp [Symbol name, value]) -> [(name, expand_ r1 value)]) defs
       in lispWrapInDoIfNeeded (map (expand_ r2) body)
    expand_ r (Symbol x) = case lookup x r of
      (Just v) -> v
      Nothing -> Symbol x
    expand_ _ (SExp x) = SExp $ map (expand_ []) x
    expand_ _ (List x) = List $ map (expand_ []) x
    expand_ _ (Tuple x) = Tuple $ map (expand_ []) x
    expand_ _ (Set x) = Tuple $ map (expand_ []) x
    expand_ _ x = x

lispWrapInDoIfNeeded :: [LispVal] -> LispVal
lispWrapInDoIfNeeded [x] = x
lispWrapInDoIfNeeded x = SExp $ Symbol "do" : x
