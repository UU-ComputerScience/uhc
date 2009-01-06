-- |
-- Combinators for building abstract syntax.
--

module Language.Cil.Build (

  -- * OpCode functions
    add
  , beq
  , bge
  , bgt
  , ble
  , blt
  , box
  , br
  , brfalse
  , brtrue
  , call
  , ceq
  , ldarg
  , ldc_i4
  , ldfld
  , ldloc
  , ldlocName
  , ldloca
  , ldstr
  , neg
  , newobj
  , nop
  , rem
  , ret
  , stfld
  , stloc
  , stlocName
  , sub
  , tailcall
  , tailcall'

  , comment

  -- * Convenient AST functions
  , label
  , defaultCtor
  , extendsCtor
  , simpleAssembly
  ) where

-- If someone uses the `rem' opcode, they can deal with the ambiguous occurence
-- themselves!
import Prelude hiding (rem)

import Language.Cil.Syntax


-- OpCode functions

add :: (Label, OpCode)
add = lbl $ Add

beq :: Label -> (Label, OpCode)
beq = lbl . Beq

bge :: Label -> (Label, OpCode)
bge = lbl . Bge

bgt :: Label -> (Label, OpCode)
bgt = lbl . Bgt

ble :: Label -> (Label, OpCode)
ble = lbl . Ble

blt :: Label -> (Label, OpCode)
blt = lbl . Blt

box :: PrimitiveType -> (Label, OpCode)
box = lbl . Box

br :: Label -> (Label, OpCode)
br = lbl . Br

brfalse :: Label -> (Label, OpCode)
brfalse = lbl . Brfalse

brtrue :: Label -> (Label, OpCode)
brtrue = lbl . Brtrue

call :: Association -> PrimitiveType -> Name -> Name -> Name -> [PrimitiveType]
         -> (Label, OpCode)
call a p l t m ps = lbl $ Call a p l t m ps

ceq :: (Label, OpCode)
ceq = lbl $ Ceq

ldarg :: Offset -> (Label, OpCode)
ldarg 0 = lbl $ Ldarg_0
ldarg 1 = lbl $ Ldarg_1
ldarg 2 = lbl $ Ldarg_2
ldarg 3 = lbl $ Ldarg_3
ldarg x = lbl $ Ldarg x

ldc_i4 :: Int -> (Label, OpCode)
ldc_i4 (-1) = lbl $ Ldc_i4_m1
ldc_i4 0    = lbl $ Ldc_i4_0
ldc_i4 1    = lbl $ Ldc_i4_1
ldc_i4 2    = lbl $ Ldc_i4_2
ldc_i4 3    = lbl $ Ldc_i4_3
ldc_i4 4    = lbl $ Ldc_i4_4
ldc_i4 5    = lbl $ Ldc_i4_5
ldc_i4 6    = lbl $ Ldc_i4_6
ldc_i4 7    = lbl $ Ldc_i4_7
ldc_i4 8    = lbl $ Ldc_i4_8
ldc_i4 x    = lbl $ Ldc_i4 x

ldfld :: PrimitiveType -> Name -> Name -> Name -> (Label, OpCode)
ldfld p a t f = lbl $ Ldfld p a t f

ldloc :: Offset -> (Label, OpCode)
ldloc 0 = lbl $ Ldloc_0
ldloc 1 = lbl $ Ldloc_1
ldloc 2 = lbl $ Ldloc_2
ldloc 3 = lbl $ Ldloc_3
ldloc x = lbl $ Ldloc x

ldlocName :: Name -> (Label, OpCode)
ldlocName = lbl . Ldloc_Name

ldloca :: Offset -> (Label, OpCode)
ldloca = lbl . Ldloca

ldstr :: String -> (Label, OpCode)
ldstr = lbl . Ldstr

neg :: (Label, OpCode)
neg = lbl $ Neg

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: Name -> Name -> [PrimitiveType] -> (Label, OpCode)
newobj a t ps = lbl $ Newobj Void a t ps

nop :: (Label, OpCode)
nop = lbl $ Nop

pop :: (Label, OpCode)
pop = lbl $ Pop

rem :: (Label, OpCode)
rem = lbl $ Rem

ret :: (Label, OpCode)
ret = lbl $ Ret

stfld :: PrimitiveType -> Name -> Name -> Name -> (Label, OpCode)
stfld p a t f = lbl $ Stfld p a t f

stloc :: Offset -> (Label, OpCode)
stloc 0 = lbl $ Stloc_0
stloc 1 = lbl $ Stloc_1
stloc 2 = lbl $ Stloc_2
stloc 3 = lbl $ Stloc_3
stloc x = lbl $ Stloc x

stlocName :: Name -> (Label, OpCode)
stlocName = lbl . Stloc_Name

sub :: (Label, OpCode)
sub = lbl $ Sub

-- | Transforms a Call into a Tailcall.
-- Note that the label associated with the original call will become associated with the new tail call.
-- If this behaviour is not required, use the tailcall' function.
tailcall :: (Label, OpCode) -> (Label, OpCode)
tailcall (l, c@(Call _ _ _ _ _ _)) = (l, Tailcall c)
tailcall (l, _)                    =
  error "Language.Cil.Build: Tail call only supported for 'Call' OpCode."

tailcall' :: (Label, OpCode)
tailcall' = lbl $ Tail


comment :: String -> (Label, OpCode)
comment s = lbl $ Comment s

-- Convenient AST functions

-- | Relabel a labelled OpCode with a new label.
label :: Label -> (Label, OpCode) -> (Label, OpCode)
label l (_, oc) = (l, oc)

defaultCtor :: [Parameter] -> MethodDef
defaultCtor = extendsCtor "" "object"

extendsCtor :: Name -> Name -> [Parameter] -> MethodDef
extendsCtor a c ps = Constructor Public ps []
  $ ldarg 0
  : map ldarg [1 .. length ps]
  ++
  [ call Instance Void a c ".ctor" (map (\(Param t _) -> t) ps)
  , ret
  ]

-- | Create a simple Assembly with one method containing the provided OpCodes.
simpleAssembly :: [(Label, OpCode)] -> Assembly
simpleAssembly ocs = Assembly "DefaultAssemblyName"
  [ Class Public "DefaultClassName" []
    [ StaticMethod Public Void "DefaultMethodName" []
      [ EntryPoint]
      ocs
    ]
  ]

-- Helper functions

lbl :: a -> (Label, a)
lbl oc = ("", oc)

