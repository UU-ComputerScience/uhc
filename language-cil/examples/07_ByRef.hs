module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" noExtends [] [] [myMain, updateRef] [myClass]

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ localsInit
      [ Local (ReferenceType "" "Haskell.Ehc.Hello/MyClass") "c" ]
  , entryPoint

  , ldc_i4 2
  , newobj "" "Haskell.Ehc.Hello/MyClass" [Int32]
  , stlocN "c"

  , ldloca 0
  , call StaticCallConv Void "" "Haskell.Ehc.Hello" "updateRef" [ByRef $ ReferenceType "" "Haskell.Ehc.Hello/MyClass"]

  , ldlocN "c"
  , ldflda Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , call Instance String "" "int32" "ToString" []
  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [String]

  , ret
  ]

updateRef :: MethodDef
updateRef = Method Static Public Void "updateRef" [Param (ByRef $ ReferenceType "" "Haskell.Ehc.Hello/MyClass") "c"]
  [ ldarg 0
  , ldc_i4 3
  , newobj "" "Haskell.Ehc.Hello/MyClass" [Int32]
  , stind_ref
  , ret
  ]

myClass :: TypeDef
myClass = classDef Private "MyClass" noExtends [] [value] [ctor, toString] []

value :: FieldDef
value = Field Instance2 Public Int32 "Value"

ctor :: MethodDef
ctor = Constructor Public [Param Int32 "value"]
  [ ldarg 0
  , call Instance Void "" "object" ".ctor" []
  , ldarg 0
  , ldarg 1
  , stfld Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , ret
  ]

toString :: MethodDef
toString = Method Instance2 Public String "ToString" []
  [ ldarg 0
  , ldflda Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , call Instance String "" "int32" "ToString" []
  , ret
  ]

