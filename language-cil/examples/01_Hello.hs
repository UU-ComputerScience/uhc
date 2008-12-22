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
  [ nop

  , ldstr "Hello, World!"
  , call Static Void "mscorlib" "System.Console" "WriteLine" [String]

  , call Static Void "" "Haskell.Ehc.Hello" "doNothing" []

  , ret
  ]

doNothing :: MethodDef
doNothing = StaticMethod Public Void "doNothing" []
  []
  [ ret ]

