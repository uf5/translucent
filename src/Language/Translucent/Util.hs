module Language.Translucent.Util (prefix, prefixList, wrapInDoIfNeeded) where
import Language.Translucent.Types

prefix :: String -> LispVal -> LispVal
prefix x y = SExp [Symbol x, y]

prefixList :: String -> [LispVal] -> LispVal
prefixList x y = SExp (Symbol x : y)

wrapInDoIfNeeded :: [LispVal] -> LispVal
wrapInDoIfNeeded [one] = one
wrapInDoIfNeeded many = prefixList "do" many
