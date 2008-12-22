module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, hellos, sign]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ nop

  , ldc_i4 4
  , call Static Void "" "Haskell.Ehc.Hello" "hellos" [Int32]

  , call Static Void "mscorlib" "System.Console" "WriteLine" []

  , ldc_i4 (-4)
  , call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 0
  , call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 4
  , call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]

  , ret
  ]

hellos :: MethodDef
hellos = StaticMethod Public Void "hellos" [Param Int32 "x"]
  [ MaxStack 4
  , LocalsInit
      [ Local Int32 "i"
      ]
  ]
  [ ldarg 0
  , stloc 0
  , br "Test"
  , label "Loop"
  $ ldstr "Hello!"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]
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
sign = StaticMethod Public Void "sign" [Param Int32 "x"]
  [ MaxStack 4
  ]
  [ ldarg 0
  , ldc_i4 0
  , bge "TestPositive"
  , ldstr "input is negative!"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "TestPositive"
  $ ldarg 0
  , brtrue "Positive"
  , ldstr "input is zero!"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "Positive"
  $ ldstr "input is positive!"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , label "End"
  $ ret
  ]

