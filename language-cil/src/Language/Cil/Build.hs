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
  , dup
  , isinst
  , ldarg
  , ldc_i4
  , ldchar
  , ldfld
  , ldflda
  , ldind_ref
  , ldloc
  , ldlocN
  , ldloca
  , ldsfld
  , ldsflda
  , ldstr
  , neg
  , newobj
  , nop
  , pop
  , rem
  , ret
  , stfld
  , stind_ref
  , stloc
  , stlocN
  , stsfld
  , sub
  , tail
  , tailcall
  , unbox

  -- * Convenient AST functions
  , label
  , comment
  , extends
  , noExtends
  , noImplements
  , classDef
  , defaultCtor
  , extendsCtor
  , simpleAssembly
  , mscorlibRef
  ) where

-- If someone uses the `rem' or `tail' opcode, they can deal with the ambiguous
-- occurence themselves!
import Prelude hiding (rem, tail)
import Data.Char (ord)

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

unbox :: PrimitiveType -> MethodDecl
unbox = mdecl . Unbox

br :: Label -> MethodDecl
br = mdecl . Br

brfalse :: Label -> MethodDecl
brfalse = mdecl . Brfalse

brtrue :: Label -> MethodDecl
brtrue = mdecl . Brtrue

call :: Association -> PrimitiveType -> DottedName -> DottedName -> DottedName -> [PrimitiveType]
         -> MethodDecl
call Static _ _ _ _ _ = error $ "Language.Cil.Build.call: "
                      ++ "Invalid association type Static. Try StaticCallConv."
call a p l t m ps = mdecl $ Call a p l t m ps

ceq :: MethodDecl
ceq = mdecl $ Ceq

dup :: MethodDecl
dup = mdecl $ Dup

isinst :: DottedName -> MethodDecl
isinst = mdecl . Isinst

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
ldc_i4 x    = mdecl $ if -127 <= x && x <= 128
                      then Ldc_i4_s x
                      else Ldc_i4 x

ldchar :: Char -> MethodDecl
ldchar c = ldc_i4 (ord c)

ldfld :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
ldfld p a t f = mdecl $ Ldfld p a t f

ldflda :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
ldflda p a t f = mdecl $ Ldflda p a t f

ldind_ref :: MethodDecl
ldind_ref = mdecl $ Ldind_ref

ldloc :: Offset -> MethodDecl
ldloc 0 = mdecl $ Ldloc_0
ldloc 1 = mdecl $ Ldloc_1
ldloc 2 = mdecl $ Ldloc_2
ldloc 3 = mdecl $ Ldloc_3
ldloc x = mdecl $ Ldloc x

ldlocN :: DottedName -> MethodDecl
ldlocN nm = mdecl $ LdlocN nm

ldloca :: Offset -> MethodDecl
ldloca = mdecl . Ldloca

ldlocaN :: DottedName -> MethodDecl
ldlocaN nm = mdecl $ LdlocaN nm

ldsfld :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
ldsfld p a t f = mdecl $ Ldsfld p a t f

ldsflda :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
ldsflda p a t f = mdecl $ Ldsflda p a t f

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

stind_ref :: MethodDecl
stind_ref = mdecl $ Stind_ref

stloc :: Offset -> MethodDecl
stloc 0 = mdecl $ Stloc_0
stloc 1 = mdecl $ Stloc_1
stloc 2 = mdecl $ Stloc_2
stloc 3 = mdecl $ Stloc_3
stloc x = mdecl $ Stloc x

stlocN :: DottedName -> MethodDecl
stlocN nm = mdecl $ StlocN nm

stsfld :: PrimitiveType -> DottedName -> DottedName -> DottedName -> MethodDecl
stsfld p a t f = mdecl $ Stsfld p a t f

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

extends :: DottedName -> Maybe TypeSpec
extends nm = Just (TypeSpec nm)

noExtends :: Maybe TypeSpec
noExtends = Nothing

noImplements :: [TypeSpec]
noImplements = []

classDef :: Visibility -> DottedName -> Maybe TypeSpec -> [TypeSpec]
              -> [FieldDef] -> [MethodDef]-> [TypeDef] -> TypeDef
classDef v n et its fs ms ts = Class v n et its (map FieldDef fs ++ map MethodDef ms
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
  [ Class Public "DefaultClassName" Nothing []
    [ MethodDef
      $ Method Static Public Void "DefaultMethodName" [] (entryPoint : ocs)
    ]
  ]

mscorlibRef :: AssemblyRef
mscorlibRef = AssemblyRef "mscorlib"

