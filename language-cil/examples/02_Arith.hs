module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, add, myEven]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ Nop

  , Ldc_i4 3
  , Ldc_i4 2
  , Call Static Int32 "" "Haskell.Ehc.Hello" "add" [Int32, Int32]
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , Ldc_i4 3
  , Call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , Ret
  ]

add :: MethodDef
add = StaticMethod Public Int32 "add" [Param Int32 "x", Param Int32 "y"]
  [ MaxStack 2 ]
  [ Ldarg 0
  , Ldarg 1
  , Add
  , Ret
  ]

myEven :: MethodDef
myEven = StaticMethod Public Bool "even" [Param Int32 "x"]
  [ LocalsInit
      [ Local Int32 "r"
      , Local Bool "b"
      ]
  ]
  [ Ldarg 0
  , Ldc_i4 2
  , Rem
  , Stloc 0
  , Ldloc 0
  , Ldc_i4 0
  , Ceq
  , Stloc 1
  , Ldloc 1
  , Ret
  ]

