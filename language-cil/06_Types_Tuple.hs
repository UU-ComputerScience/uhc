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
  [ Nop

  , Ldc_i4 3
  , Ldc_i4 2
  , Newobj Instance Void "" "Haskell.Ehc.Tuple" [Int32, Int32]
  , Call Static Int32 "" "Haskell.Ehc.Hello" "pairadd" [ReferenceType "" "Haskell.Ehc.Tuple"]

  , Call Static Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , Ret
  ]

pair :: MethodDef
pair = StaticMethod Public Int32 "pairadd" [Param (ReferenceType "" "Haskell.Ehc.Tuple") "t"]
  []
  [ Nop
  , Ldarg 0
  , Ldfld Int32 "" "Haskell.Ehc.Tuple" "Fst"
  , Ldarg 0
  , Ldfld Int32 "" "Haskell.Ehc.Tuple" "Snd"
  , Add
  , Ret
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
  [ Nop
  , Ldarg 0
  , Call Instance Void "" "object" ".ctor" []
  , Ldarg 0
  , Ldarg 1
  , Stfld Int32 "" "Haskell.Ehc.Tuple" "Fst"
  , Ldarg 0
  , Ldarg 2
  , Stfld Int32 "" "Haskell.Ehc.Tuple" "Snd"
  , Ret
  ]

