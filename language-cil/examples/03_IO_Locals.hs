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
  [ nop

  , call Static Void "" "Haskell.Ehc.Hello" "ioAge" []

  , ret
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
  [ ldstr "What year were you born?"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]
  , call Static String "mscorlib" "System.Console" "ReadLine" []
  , call Static Int32 "" "int32" "Parse" [String]
  , stloc 0
  , call Static (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "get_Now" []
  , stloc 1
  , ldloca 1
  , ldloc 0
  , neg
  , call Instance (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "AddYears" [Int32]
  , stloc 2
  , ldstr "This year, you turn {0}."
  , ldloca 2
  , call Instance Int32 "mscorlib" "System.DateTime" "get_Year" []
  , box Int32
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String, Object]
  , ret
  ]

