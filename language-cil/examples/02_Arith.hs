module Example where

import Prelude hiding (rem)

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" [] [myMain, myAdd, myEven] []

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , ldc_i4 3
  , ldc_i4 2
  , call StaticCallConv Int32 "" "Haskell.Ehc.Hello" "add" [Int32, Int32]
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ldc_i4 3
  , call StaticCallConv Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myAdd :: MethodDef
myAdd = Method Static Public Int32 "add" [Param Int32 "x", Param Int32 "y"]
  [ maxStack 2
  , ldarg 0
  , ldarg 1
  , add
  , ret
  ]

myEven :: MethodDef
myEven = Method Static Public Bool "even" [Param Int32 "x"]
  [ localsInit
      [ Local Int32 "r"
      , Local Bool "b"
      ]
  , ldarg 0
  , ldc_i4 2
  , rem
  , stloc 0
  , ldloc 0
  , ldc_i4 0
  , ceq
  , stloc 1
  , ldloc 1
  , ret
  ]

