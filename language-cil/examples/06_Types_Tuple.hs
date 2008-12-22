module Example where

import Language.Cil

main = print ass

ass :: Assembly
ass = Assembly "Example" [hello, tuple2]

hello :: TypeDef
hello = Class Public "Haskell.Ehc.Hello" []
              [myMain, pair]

myMain :: MethodDef
myMain = StaticMethod Public Void "main" []
  [ EntryPoint ]
  [ nop

  , ldc_i4 3
  , ldc_i4 2
  , newobj "" "Haskell.Ehc.Tuple" [Int32, Int32]
  , call Static Int32 "" "Haskell.Ehc.Hello" "pairadd" [ReferenceType "" "Haskell.Ehc.Tuple"]

  , call Static Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ret
  ]

pair :: MethodDef
pair = StaticMethod Public Int32 "pairadd" [Param (ReferenceType "" "Haskell.Ehc.Tuple") "t"]
  []
  [ nop
  , ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Tuple" "Fst"
  , ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Tuple" "Snd"
  , add
  , ret
  ]

tuple2 :: TypeDef
tuple2 = Class Public "Haskell.Ehc.Tuple"
               [myFst, mySnd]
               [tupleCtor]

myFst :: FieldDef
myFst = Field Public Int32 "Fst"

mySnd :: FieldDef
mySnd = Field Public Int32 "Snd"

tupleCtor :: MethodDef
tupleCtor = Constructor Public [Param Int32 "fst", Param Int32 "snd"]
  []
  [ nop
  , ldarg 0
  , call Instance Void "" "object" ".ctor" []
  , ldarg 0
  , ldarg 1
  , stfld Int32 "" "Haskell.Ehc.Tuple" "Fst"
  , ldarg 0
  , ldarg 2
  , stfld Int32 "" "Haskell.Ehc.Tuple" "Snd"
  , ret
  ]

