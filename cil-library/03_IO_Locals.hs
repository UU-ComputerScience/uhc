module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, ioAge]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ Nop

  , Call Static Void "" "Haskell.Ehc.Hello" "ioAge" []

  , Ret
  ]

ioAge :: MethodDef
ioAge = StaticMethod Public Void "ioAge" []
  [ MaxStack 11
  , LocalsInit
      [ Local Int32 "x"
      , Local (ValueType "mscorlib" "System.DateTime") "d1"
      , Local (ValueType "mscorlib" "System.DateTime") "d2"
      ]
  ]
  [ Ldstr "What year were you born?"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , Call Static String "mscorlib" "System.Console" "ReadLine" []
  , Call Static Int32 "" "int32" "Parse" [String]
  , Stloc 0
  , Call Static (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "get_Now" []
  , Stloc 1
  , Ldloca 1
  , Ldloc 0
  , Neg
  , Call Instance (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "AddYears" [Int32]
  , Stloc 2
  , Ldstr "This year, you turn {0}."
  , Ldloca 2
  , Call Instance Int32 "mscorlib" "System.DateTime" "get_Year" []
  , Box Int32
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String, Object]
  , Ret
  ]

