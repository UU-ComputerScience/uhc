module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, doNothing]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ Nop

  , Ldstr "Hello, World!"
  , Call Static Void "mscorlib" "System.Console" "WriteLine" [String]

  , Call Static Void "" "Haskell.Ehc.Hello" "doNothing" []

  , Ret
  ]

doNothing :: MethodDef
doNothing = StaticMethod Public Void "doNothing" []
  []
  [ Ret ]

