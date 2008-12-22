-- |
-- Pretty-printer for the abstract syntax.
-- Currenty uses ShowS, maybe should use some PP combinator library?
--

module Language.Cil.Pretty (
    Cil (cil)
  ) where

import Data.List (intersperse)
import Language.Cil.Syntax


instance Show Assembly where
  show a = cil a ""

class Cil a where
  -- | Serializes a Cil data structure to a String.
  cil :: a -> ShowS

-- | Serializes a Name, escaping some weird names (such as \'add\').
cilName :: Name -> ShowS
cilName "add" = ("'add'" ++)
cilName "pop" = ("'pop'" ++)
cilName n     = (n ++)

instance Cil Assembly where
  cil (Assembly n ms) =
      (".assembly extern mscorlib {}\n" ++)
    . (".assembly " ++) . cilName n . (" {}\n" ++)
    . foldr (\m s -> cil m . s) id ms

instance Cil TypeDef where
  cil (Class v n fs ms) =
      (".class " ++) . cil v . sp . cilName n . ("\n{\n" ++)
    . foldr (\f s -> cil f . s) id fs
    . nl
    . foldr (\m s -> cil m . s) id ms
    . ("}\n" ++)
  cil (GenericClass v n ps fs ms) =
      (".class " ++) . cil v . sp . cilName n
    . ("`" ++) . shows (length ps) . ("<" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (">\n{\n" ++)
    . foldr (\f s -> cil f . s) id fs
    . nl
    . foldr (\m s -> cil m . s) id ms
    . ("}\n" ++)

instance Cil GenParam where
  cil (GenParam n) = cilName n

instance Cil Visibility where
  cil AssemblyVisible   = ("assembly" ++)
  cil FamilyAndAssembly = ("famandassem" ++)
  cil FamilyOrAssembly  = ("famorassem" ++)
  cil Private           = ("private" ++)
  cil Public            = ("public" ++)

instance Cil FieldDef where
  cil (Field v t n) = 
      ident . (".field " ++) . cil v . sp . cil t . sp . cilName n . nl

instance Cil MethodDef where
  cil (Constructor v ps ds os) =
      ident . (".method " ++) . cil v
    . (" hidebysig instance void .ctor(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . foldr (\o s -> cilLabelledOpCode o . s) id os
    . ident . ("}\n" ++)
  cil (StaticMethod v t n ps ds os) =
      ident . (".method " ++) . cil v
    . (" hidebysig static " ++) . cil t . sp . cilName n . ("(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . foldr (\o s -> cilLabelledOpCode o . s) id os
    . ident . ("}\n" ++)

instance Cil Parameter where
  cil (Param t n) = cil t . sp . cilName n

instance Cil Directive where
  cil (EntryPoint)    = ident . ident . (".entrypoint" ++) . nl
  cil (LocalsInit ls) =
    let bigident = ident . ident . ident . ident
    in
      ident . ident . (".locals init (\n" ++)
    . foldr (.) id (intersperse (",\n" ++) (map (\l -> bigident . cil l) ls))
    . (")\n" ++)
  cil (MaxStack x)    = ident . ident . (".maxstack " ++) . shows x . nl

instance Cil Local where
  cil (Local t n) = cil t . sp . cilName n

cilLabelledOpCode :: (Label, OpCode) -> ShowS
cilLabelledOpCode ("", oc) = ident . ident . cil oc . nl
cilLabelledOpCode (l,  oc) = ident . (l ++) . (":" ++) . nl
                              . ident . ident . cil oc . nl

-- Note: this could be a lot more efficient. For example, there are specialized
-- instructions for loading the constant integers 1 through 8, but for clearity
-- these aren't used.
instance Cil OpCode where
  cil (Add)               = ("add" ++)
  cil (And)               = ("and" ++)
  cil (Beq l)             = ("beq " ++) . (l ++)
  cil (Bge l)             = ("bge " ++) . (l ++)
  cil (Bgt l)             = ("bgt " ++) . (l ++)
  cil (Ble l)             = ("ble " ++) . (l ++)
  cil (Blt l)             = ("blt " ++) . (l ++)
  cil (Box t)             = ("box " ++) . cil t
  cil (Br l)              = ("br " ++) . (l ++)
  cil (Brfalse l)         = ("brfalse " ++) . (l ++)
  cil (Brtrue l)          = ("brtrue " ++) . (l ++)
  cil (Call s t a c m ps) = ("call " ++) . cilsp s . cil t . sp
                             . cilCall a c m ps
  cil (Ceq)               = ("ceq" ++)
  cil (Dup)               = ("dup" ++)
  cil (Ldarg x)           = ("ldarg " ++) . shows x
  cil (Ldarg_0)           = ("ldarg.0 " ++)
  cil (Ldarg_1)           = ("ldarg.1 " ++)
  cil (Ldarg_2)           = ("ldarg.2 " ++)
  cil (Ldarg_3)           = ("ldarg.3 " ++)
  cil (Ldc_i4 x)          = ("ldc.i4 " ++) . shows x
  cil (Ldc_i4_0)          = ("ldc.i4.0 " ++) 
  cil (Ldc_i4_1)          = ("ldc.i4.1 " ++) 
  cil (Ldc_i4_2)          = ("ldc.i4.2 " ++) 
  cil (Ldc_i4_3)          = ("ldc.i4.3 " ++) 
  cil (Ldc_i4_4)          = ("ldc.i4.4 " ++) 
  cil (Ldc_i4_5)          = ("ldc.i4.5 " ++) 
  cil (Ldc_i4_6)          = ("ldc.i4.6 " ++) 
  cil (Ldc_i4_7)          = ("ldc.i4.7 " ++) 
  cil (Ldc_i4_8)          = ("ldc.i4.8 " ++) 
  cil (Ldc_i4_m1)         = ("ldc.i4.m1 " ++) 
  cil (Ldfld t a c f)     = ("ldfld " ++) . cil t . sp . cilFld a c f
  cil (Ldloc x)           = ("ldloc " ++) . shows x
  cil (Ldloc_0)           = ("ldloc.0 " ++)
  cil (Ldloc_1)           = ("ldloc.1 " ++)
  cil (Ldloc_2)           = ("ldloc.2 " ++)
  cil (Ldloc_3)           = ("ldloc.3 " ++)
  cil (Ldloca x)          = ("ldloca " ++) . shows x
  cil (Ldstr s)           = ("ldstr " ++) . shows s
  cil (Neg)               = ("neg" ++)
  cil (Newobj t a c ps)   = ("newobj instance " ++) . cil t . sp
                             . cilNewobj a c ps
  cil (Nop)               = ("nop" ++)
  cil (Pop)               = ("pop" ++)
  cil (Rem)               = ("rem" ++)
  cil (Ret)               = ("ret" ++)
  cil (Stfld t a c f)     = ("stfld " ++) . cil t . sp . cilFld a c f
  cil (Stloc x)           = ("stloc " ++) . shows x
  cil (Stloc_0)           = ("stloc.0 " ++)
  cil (Stloc_1)           = ("stloc.1 " ++)
  cil (Stloc_2)           = ("stloc.2 " ++)
  cil (Stloc_3)           = ("stloc.3 " ++)
  cil (Sub)               = ("sub" ++)
  cil (Tail)              = ("tail." ++)
  cil (Tailcall opcode)       = ("tail. " ++) . cil opcode

cilFld :: Name -> Name -> Name -> ShowS
cilFld a c f = 
    cilAssembly a
  . (if c /= ""
     then cilName c . ("::" ++)
     else id)
  . cilName f

cilNewobj :: Name -> Name -> [PrimitiveType] -> ShowS
cilNewobj a c ps = 
    cilAssembly a
  . (if c /= ""
     then cilName c . ("::" ++)
     else id)
  . (".ctor(" ++)
  . foldr (.) id (intersperse (", " ++) (map cil ps))
  . (")" ++)

cilCall :: Name -> Name -> Name -> [PrimitiveType] -> ShowS
cilCall a c m ps = 
    cilAssembly a
  . (if c /= ""
     then cilName c . ("::" ++)
     else id)
  . cilName m
  . ("(" ++)
  . foldr (.) id (intersperse (", " ++) (map cil ps))
  . (")" ++)

cilAssembly :: Name -> ShowS
cilAssembly a =
    (if a /= ""
     then ("[" ++) . cilName a . ("]" ++)
     else id)

instance Cil Association where
  cil Static   = id
  cil Instance = ("instance" ++)

instance Cil PrimitiveType where
  cil Void                = ("void" ++) 
  cil Bool                = ("bool" ++)
  cil Char                = ("char" ++)
  cil Byte                = ("unsigned int8" ++)
  cil Int32               = ("int32" ++)
  cil Int64               = ("int64" ++)
  cil String              = ("string" ++)
  cil Object              = ("object" ++)
  cil (ValueType a c)     = ("valuetype " ++) . cilAssembly a . cilName c
  cil (ReferenceType a c) = cilAssembly a . cilName c
  cil (GenericType x)     = ("!" ++) . shows x

-- Helper functions, to pretty print
cilsp :: (Cil a) => a -> ShowS
cilsp x = let s = cil x ""
          in if s == "" then id else cil x . sp

ident = ("    " ++)
sp    = (" " ++)
nl    = ('\n' :)

