module Example where

import Prelude hiding (tail)

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" noExtends [] [] [myMain, myEven, myOdd] []

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , ldc_i4 1000000
  , call StaticCallConv Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myEven :: MethodDef
myEven = Method Static Public Bool "even" [Param Int32 "x"]
  [ ldarg 0
  , brtrue "else"
  , ldc_i4 1
  , ret
  , label "else"
  $ ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call StaticCallConv Bool "" "Haskell.Ehc.Hello" "odd" [Int32]
  , ret
  ]

myOdd :: MethodDef
myOdd = Method Static Public Bool "odd" [Param Int32 "x"]
  [ ldarg 0
  , brtrue "else"
  , ldc_i4 0
  , ret
  , label "else"
  $ ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call StaticCallConv Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , ret
  ]

