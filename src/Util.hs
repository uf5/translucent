module Util (prefix, prefixList) where

import Types

prefix :: String -> LispVal -> LispVal
prefix x y = SExp [Symbol x, y]

prefixList :: String -> [LispVal] -> LispVal
prefixList x y = SExp (Symbol x : y)

