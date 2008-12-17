module Example where

import Language.Cil

main = putStrLn $ cil ass ""

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, myEven, myOdd]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ Nop

  , Ldc_i4 1000000
  , Call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , Ret
  ]

myEven :: MethodDef
myEven = StaticMethod Public Bool "even" [Param Int32 "x"]
  []
  [ Nop
  , Ldarg 0
  , Brtrue "else"
  , Ldc_i4 1
  , Ret
  , Label "else"
  $ Ldarg 0
  , Ldc_i4 1
  , Sub
  , Tail
  , Call Static Bool "" "Haskell.Ehc.Hello" "odd" [Int32]
  , Ret
  ]

myOdd :: MethodDef
myOdd = StaticMethod Public Bool "odd" [Param Int32 "x"]
  []
  [ Nop
  , Ldarg 0
  , Brtrue "else"
  , Ldc_i4 0
  , Ret
  , Label "else"
  $ Ldarg 0
  , Ldc_i4 1
  , Sub
  , Tail
  , Call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , Ret
  ]

