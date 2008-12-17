-- |
-- Pretty-printer for the abstract syntax.
-- Currenty uses ShowS, maybe should use some PP combinator library?
--

module Language.Cil.Pretty (
    Cil (cil)
  ) where

import Data.List (intersperse)
import Language.Cil.Syntax


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

instance Cil Visibility where
  cil AssemblyVisible   = ("assembly" ++)
  cil FamilyAndAssembly = ("famandassem" ++)
  cil FamilyOrAssembly  = ("famorassem" ++)
  cil Private           = ("private" ++)
  cil Public            = ("public" ++)

instance Cil FieldDef where
  cil (Field v t n) = 
      ident . (".field " ++) .  cil v . sp . cil t . sp . cilName n . nl

instance Cil MethodDef where
  cil (Constructor v ps ds os) =
      ident . (".method " ++) . cil v
    . (" hidebysig instance void .ctor(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . foldr (\o s -> cil o . s) id os
    . ident . ("}\n" ++)
  cil (StaticMethod v t n ps ds os) =
      ident . (".method " ++) . cil v
    . (" hidebysig static " ++) . cil t . sp . cilName n . ("(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . foldr (\o s -> cil o . s) id os
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
  cil (MaxStack x)    = ident . ident . (".maxstack " ++) . (shows x) . nl

instance Cil Local where
  cil (Local t n) = cil t . sp . cilName n

-- Note: this could be a lot more efficient. For example, there are specialized
-- instructions for loading the constant integers 1 through 8, but for clearity
-- these aren't used.
instance Cil OpCode where
  cil (Label l oc)        = ident . (l ++) . (":" ++) . nl . cil oc
  cil (Add)               = ident . ident . ("add" ++) . nl
  cil (And)               = ident . ident . ("and" ++) . nl
  cil (Beq l)             = ident . ident . ("beq " ++) . (l ++) . nl
  cil (Bge l)             = ident . ident . ("bge " ++) . (l ++) . nl
  cil (Bgt l)             = ident . ident . ("bgt " ++) . (l ++) . nl
  cil (Ble l)             = ident . ident . ("ble " ++) . (l ++) . nl
  cil (Blt l)             = ident . ident . ("blt " ++) . (l ++) . nl
  cil (Box t)             = ident . ident . ("box " ++) . cil t . nl
  cil (Br l)              = ident . ident . ("br " ++) . (l ++) . nl
  cil (Brfalse l)         = ident . ident . ("brfalse " ++) . (l ++) . nl
  cil (Brtrue l)          = ident . ident . ("brtrue " ++) . (l ++) . nl
  cil (Break)             = ident . ident . ("break" ++) . nl
  cil (Call s t a c m ps) = ident . ident . ("call " ++) . cil s . sp
                             . cil t . sp . cilCall a c m ps . nl
  cil (Ceq)               = ident . ident . ("ceq" ++) . nl
  cil (Dup)               = ident . ident . ("dup" ++) . nl
  cil (Ldarg x)           = ident . ident . ("ldarg " ++) . shows x . nl
  cil (Ldc_i4 x)          = ident . ident . ("ldc.i4 " ++) . shows x . nl 
  cil (Ldfld t a c f)     = ident . ident . ("ldfld " ++) . cil t . sp
                             . cilFld a c f . nl
  cil (Ldloc x)           = ident . ident . ("ldloc " ++) . shows x . nl
  cil (Ldloca x)          = ident . ident . ("ldloca " ++) . shows x . nl
  cil (Ldstr s)           = ident . ident . ("ldstr " ++) . shows s . nl
  cil (Neg)               = ident . ident . ("neg" ++) . nl
  cil (Newobj s t a c ps) = ident . ident . ("newobj " ++) . cil s . sp
                             . cil t . sp . cilNewobj a c ps . nl
  cil (Nop)               = ident . ident . ("nop" ++) . nl
  cil (Pop)               = ident . ident . ("pop" ++) . nl
  cil (Rem)               = ident . ident . ("rem" ++) . nl
  cil (Ret)               = ident . ident . ("ret" ++) . nl
  cil (Stfld t a c f)     = ident . ident . ("stfld " ++) . cil t . sp
                             . cilFld a c f . nl
  cil (Stloc x)           = ident . ident . ("stloc " ++) . shows x . nl
  cil (Sub)               = ident . ident . ("sub" ++) . nl
  cil (Tail)              = ident . ident . ("tail." ++) . nl

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
  cil Void            = ("void" ++) 
  cil Bool            = ("bool" ++)
  cil Char            = ("char" ++)
  cil Byte            = ("unsigned int8" ++)
  cil Int32           = ("int32" ++)
  cil Int64           = ("int64" ++)
  cil String          = ("string" ++)
  cil Object          = ("object" ++)
  cil (ValueType a c) = ("valuetype " ++) . cilAssembly a . cilName c
  cil (ReferenceType a c) = cilAssembly a . cilName c

-- Helper functions, to pretty print
ident = ("    " ++)
sp    = (" " ++)
nl    = ('\n' :)

