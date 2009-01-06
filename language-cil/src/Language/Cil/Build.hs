-- |
-- Combinators for building abstract syntax.
--

module Language.Cil.Build (

  -- * Directive functions
    entryPoint
  , localsInit
  , maxStack

  -- * mdecl functions
  , add
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
  , ldloca
  , ldstr
  , neg
  , newobj
  , nop
  , pop
  , rem
  , ret
  , stfld
  , stloc
  , sub
  , tail
  , tailcall

  -- * Convenient AST functions
  , label
  , comment
  , classDef
  , defaultCtor
  , extendsCtor
  , simpleAssembly
  , mscorlibRef
  ) where

-- If someone uses the `rem' or `tail' opcode, they can deal with the ambiguous
-- occurence themselves!
import Prelude hiding (rem, tail)

import Language.Cil.Syntax


-- Directive functions

entryPoint :: MethodDecl
entryPoint = Directive EntryPoint 

localsInit :: [Local] -> MethodDecl
localsInit ls = Directive (LocalsInit ls)

maxStack :: Int -> MethodDecl
maxStack x = Directive (MaxStack x)


-- mdecl functions

add :: MethodDecl
add = mdecl $ Add

beq :: Label -> MethodDecl
beq = mdecl . Beq

bge :: Label -> MethodDecl
bge = mdecl . Bge

bgt :: Label -> MethodDecl
bgt = mdecl . Bgt

ble :: Label -> MethodDecl
ble = mdecl . Ble

blt :: Label -> MethodDecl
blt = mdecl . Blt

box :: PrimitiveType -> MethodDecl
box = mdecl . Box

br :: Label -> MethodDecl
br = mdecl . Br

brfalse :: Label -> MethodDecl
brfalse = mdecl . Brfalse

brtrue :: Label -> MethodDecl
brtrue = mdecl . Brtrue

call :: Association -> PrimitiveType -> DottedName -> DottedName -> DottedName -> [PrimitiveType]
         -> MethodDecl
call a p l t m ps = mdecl $ Call a p l t m ps

ceq :: MethodDecl
ceq = mdecl $ Ceq

ldarg :: Offset -> MethodDecl
ldarg 0 = mdecl $ Ldarg_0
ldarg 1 = mdecl $ Ldarg_1
ldarg 2 = mdecl $ Ldarg_2
ldarg 3 = mdecl $ Ldarg_3
ldarg x = mdecl $ Ldarg x

ldc_i4 :: Int -> MethodDecl
ldc_i4 (-1) = mdecl $ Ldc_i4_m1
ldc_i4 0    = mdecl $ Ldc_i4_0
ldc_i4 1    = mdecl $ Ldc_i4_1
ldc_i4 2    = mdecl $ Ldc_i4_2
ldc_i4 3    = mdecl $ Ldc_i4_3
ldc_i4 4    = mdecl $ Ldc_i4_4
ldc_i4 5    = mdecl $ Ldc_i4_5
ldc_i4 6    = mdecl $ Ldc_i4_6
ldc_i4 7    = mdecl $ Ldc_i4_7
ldc_i4 8    = mdecl $ Ldc_i4_8
ldc_i4 x    = mdecl $ Ldc_i4 x

ldfld :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
ldfld p a t f = mdecl $ Ldfld p a t f

ldloc :: Offset -> MethodDecl
ldloc 0 = mdecl $ Ldloc_0
ldloc 1 = mdecl $ Ldloc_1
ldloc 2 = mdecl $ Ldloc_2
ldloc 3 = mdecl $ Ldloc_3
ldloc x = mdecl $ Ldloc x

ldloca :: Offset -> MethodDecl
ldloca = mdecl . Ldloca

ldstr :: String -> MethodDecl
ldstr = mdecl . Ldstr

neg :: MethodDecl
neg = mdecl $ Neg

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: DottedName -> DottedName -> [PrimitiveType] -> MethodDecl
newobj a t ps = mdecl $ Newobj Void a t ps

nop :: MethodDecl
nop = mdecl $ Nop

pop :: MethodDecl
pop = mdecl $ Pop

rem :: MethodDecl
rem = mdecl $ Rem

ret :: MethodDecl
ret = mdecl $ Ret

stfld :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
stfld p a t f = mdecl $ Stfld p a t f

stloc :: Offset -> MethodDecl
stloc 0 = mdecl $ Stloc_0
stloc 1 = mdecl $ Stloc_1
stloc 2 = mdecl $ Stloc_2
stloc 3 = mdecl $ Stloc_3
stloc x = mdecl $ Stloc x

sub :: MethodDecl
sub = mdecl $ Sub

tail :: MethodDecl
tail = mdecl $ Tail

tailcall :: MethodDecl -> MethodDecl
tailcall (Instr (OpCode oc)) = Instr (OpCode (Tailcall oc))

-- Helper functions

mdecl :: OpCode -> MethodDecl
mdecl i = Instr $ OpCode i

-- Convenient AST functions

-- | Relabel a labelled mdecl with a new label.
label :: Label -> MethodDecl -> MethodDecl
label l (Instr (LabOpCode _ oc)) = Instr $ LabOpCode l oc
label l (Instr (OpCode oc))      = Instr $ LabOpCode l oc
label _ _                        = error $ "Language.Cil.Build.label: "
                                     ++ "Can't label non-Instrs."

comment :: String -> MethodDecl
comment s = Comment s

classDef :: Visibility -> DottedName -> [FieldDef] -> [MethodDef] -> [TypeDef]
              -> TypeDef
classDef v n fs ms ts = Class v n (map FieldDef fs ++ map MethodDef ms
                                     ++ map TypeDef ts)

defaultCtor :: [Parameter] -> MethodDef
defaultCtor = extendsCtor "" "object"

extendsCtor :: DottedName -> DottedName -> [Parameter] -> MethodDef
extendsCtor a c ps = Constructor Public ps
  $ ldarg 0
  : map ldarg [1 .. length ps]
  ++
  [ call Instance Void a c ".ctor" (map (\(Param t _) -> t) ps)
  , ret
  ]

-- | Create a simple Assembly with one method containing the provided MethodDecls.
simpleAssembly :: [MethodDecl] -> Assembly
simpleAssembly ocs = Assembly [mscorlibRef] "DefaultAssemblyName"
  [ Class Public "DefaultClassName"
    [ MethodDef
      $ Method Static Public Void "DefaultMethodName" [] (entryPoint : ocs)
    ]
  ]

mscorlibRef :: AssemblyRef
mscorlibRef = AssemblyRef "mscorlib"

