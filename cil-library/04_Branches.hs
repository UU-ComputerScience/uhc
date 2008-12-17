module Example where

import Language.Cil

main = putStrLn $ cil ass ""

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, hellos, sign]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ Nop

  , Ldc_i4 4
  , Call Static Void "" "Haskell.Ehc.Hello" "hellos" [Int32]

  , Call Static Void "mscorlib" "System.Console" "WriteLine" []

  , Ldc_i4 (-4)
  , Call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , Ldc_i4 0
  , Call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , Ldc_i4 4
  , Call Static Void "" "Haskell.Ehc.Hello" "sign" [Int32]

  , Ret
  ]

hellos :: MethodDef
hellos = StaticMethod Public Void "hellos" [Param Int32 "x"]
  [ MaxStack 4
  , LocalsInit
      [ Local Int32 "i"
      ]
  ]
  [ Ldarg 0
  , Stloc 0
  , Br "Test"
  , Label "Loop"
  $ Ldstr "Hello!"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , Ldloc 0
  , Ldc_i4 1
  , Sub
  , Stloc 0
  , Label "Test"
  $ Ldloc 0
  , Ldc_i4 0
  , Bgt "Loop"
  , Ret
  ]

sign :: MethodDef
sign = StaticMethod Public Void "sign" [Param Int32 "x"]
  [ MaxStack 4
  ]
  [ Ldarg 0
  , Ldc_i4 0
  , Bge "TestPositive"
  , Ldstr "input is negative!"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , Br "End"

  , Label "TestPositive"
  $ Ldarg 0
  , Brtrue "Positive"
  , Ldstr "input is zero!"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , Br "End"

  , Label "Positive"
  $ Ldstr "input is positive!"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , Label "End"
  $ Ret
  ]

