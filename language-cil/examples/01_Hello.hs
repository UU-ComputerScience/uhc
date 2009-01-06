module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" [] [myMain, doNothing] []

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , ldstr "Hello, World!"
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]

  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "doNothing" []

  , ret
  ]

doNothing :: MethodDef
doNothing = Method Static Public Void "doNothing" []
  [ ret ]

