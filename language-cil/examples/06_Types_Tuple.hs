module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef Public "Haskell.Ehc.Hello" noExtends [] [] [myMain, pair] [tuple2]

myMain :: MethodDef
myMain = Method Static Public Void "main" []
  [ entryPoint

  , ldc_i4 3
  , ldc_i4 2
  , newobj "" "Haskell.Ehc.Hello/Tuple" [Int32, Int32]
  , call StaticCallConv Int32 "" "Haskell.Ehc.Hello" "pairadd" [ReferenceType "" "Haskell.Ehc.Hello/Tuple"]

  , call StaticCallConv Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ret
  ]

pair :: MethodDef
pair = Method Static Public Int32 "pairadd" [Param (ReferenceType "" "Haskell.Ehc.Hello/Tuple") "t"]
  [ ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Fst"
  , ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Snd"
  , add
  , ret
  ]

tuple2 :: TypeDef
tuple2 = classDef Private "Tuple" noExtends [] [myFst, mySnd] [tupleCtor] []

myFst :: FieldDef
myFst = Field Static Public Int32 "Fst"

mySnd :: FieldDef
mySnd = Field Static Public Int32 "Snd"

tupleCtor :: MethodDef
tupleCtor = Constructor Public [Param Int32 "fst", Param Int32 "snd"]
  [ ldarg 0
  , call Instance Void "" "object" ".ctor" []
  , ldarg 0
  , ldarg 1
  , stfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Fst"
  , ldarg 0
  , ldarg 2
  , stfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Snd"
  , ret
  ]

