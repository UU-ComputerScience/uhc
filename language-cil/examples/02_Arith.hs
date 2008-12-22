module Example where

import Prelude hiding (rem)

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, myAdd, myEven]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ nop

  , ldc_i4 3
  , ldc_i4 2
  , call Static Int32 "" "Haskell.Ehc.Hello" "add" [Int32, Int32]
  , call Static Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ldc_i4 3
  , call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call Static Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myAdd :: MethodDef
myAdd = StaticMethod Public Int32 "add" [Param Int32 "x", Param Int32 "y"]
  [ MaxStack 2 ]
  [ ldarg 0
  , ldarg 1
  , add
  , ret
  ]

myEven :: MethodDef
myEven = StaticMethod Public Bool "even" [Param Int32 "x"]
  [ LocalsInit
      [ Local Int32 "r"
      , Local Bool "b"
      ]
  ]
  [ ldarg 0
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

