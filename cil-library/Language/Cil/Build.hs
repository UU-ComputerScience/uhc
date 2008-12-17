-- |
-- Combinators for building abstract syntax.
--

module Language.Cil.Build (
    defaultCtor
  , extendsCtor
  ) where

import Language.Cil.Syntax

defaultCtor :: [Parameter] -> MethodDef
defaultCtor = extendsCtor "" "object"

extendsCtor :: Name -> Name -> [Parameter] -> MethodDef
extendsCtor a c ps = Constructor Public ps []
  $ Ldarg 0
  : map Ldarg [1 .. length ps]
  ++
  [ Call Instance Void a c ".ctor" (map (\(Param t _) -> t) ps)
  , Ret
  ]

