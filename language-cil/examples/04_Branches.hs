module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" noExtends [] [] [myMain, hellos, sign] []

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , ldc_i4 4
  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "hellos" [Int32]

  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" []

  , ldc_i4 (-4)
  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 0
  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 4
  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "sign" [Int32]

  , ret
  ]

hellos :: MethodDef
hellos = Method Static Public Void "hellos" [Param Int32 "x"]
  [ maxStack 4
  , localsInit
      [ Local Int32 "i"
      ]

  , ldarg 0
  , stloc 0
  , br "Test"
  , label "Loop"
  $ ldstr "Hello!"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]
  , ldloc 0
  , ldc_i4 1
  , sub
  , stloc 0
  , label "Test"
  $ ldloc 0
  , ldc_i4 0
  , bgt "Loop"
  , ret
  ]

sign :: MethodDef
sign = Method Static Public Void "sign" [Param Int32 "x"]
  [ maxStack 4

  , ldarg 0
  , ldc_i4 0
  , bge "TestPositive"
  , ldstr "input is negative!"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "TestPositive"
  $ ldarg 0
  , brtrue "Positive"
  , ldstr "input is zero!"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "Positive"
  $ ldstr "input is positive!"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]
  , label "End"
  $ ret
  ]

