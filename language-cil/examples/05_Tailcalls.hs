module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, myEven, myOdd]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ nop

  , ldc_i4 1000000
  , call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call Static Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myEven :: MethodDef
myEven = StaticMethod Public Bool "even" [Param Int32 "x"]
  []
  [ nop
  , ldarg 0
  , brtrue "else"
  , ldc_i4 1
  , ret
  , label "else"
  $ ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call Static Bool "" "Haskell.Ehc.Hello" "odd" [Int32]
  , ret
  ]

myOdd :: MethodDef
myOdd = StaticMethod Public Bool "odd" [Param Int32 "x"]
  []
  [ nop
  , ldarg 0
  , brtrue "else"
  , ldc_i4 0
  , ret
  , label "else"
  $ ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call Static Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , ret
  ]

