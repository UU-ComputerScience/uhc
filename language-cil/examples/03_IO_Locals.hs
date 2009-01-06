module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" [] [myMain, ioAge] []

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "ioAge" []

  , ret
  ]

ioAge :: MethodDef
ioAge = Method Static Public Void "ioAge" []
  [ maxStack 11
  , localsInit
      [ Local Int32 "x"
      , Local (ValueType "mscorlib" "System.DateTime") "d1"
      , Local (ValueType "mscorlib" "System.DateTime") "d2"
      ]

  , ldstr "What year were you born?"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]
  , call StaticCallConv String "mscorlib" "System.Console" "ReadLine" []
  , call StaticCallConv Int32 "" "int32" "Parse" [String]
  , stloc 0
  , call StaticCallConv (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "get_Now" []
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
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String, Object]
  , ret
  ]

