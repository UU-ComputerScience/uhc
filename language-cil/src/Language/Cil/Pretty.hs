-- |
-- Pretty-printer for the abstract syntax.
-- Currenty uses ShowS, maybe should use some PP combinator library?
--

module Language.Cil.Pretty (
    Cil (cil)
  ) where

import Data.List (intersperse, intercalate)
import Language.Cil.Syntax

{- Alternative to deriving Show

instance Show Assembly where
  show a = cil a ""
-}

class Cil a where
  -- | Serializes a Cil data structure to a String.
  cil :: a -> ShowS

-- | Serializes a DottedName, escaping some weird names (such as \'add\' 
-- or '<Thunk>').
cilName :: DottedName -> ShowS
cilName "" = error "Language.Cil.Pretty.cilName: Name cannot be empty"
cilName n  = if n `elem` kw || '<' `elem` n
             then (escape n ++)
             else (n ++)
  where
    kw = ["add", "pop", "value"]
    escape s = intercalate "/" (map (\s -> "'" ++ s ++ "'") (filter (/=[]) (split '/' s)))

split :: Eq a => a -> [a] -> [[a]]
split x xs = f xs []
  where
    f []     zs = [zs]
    f (y:ys) zs = if x == y
                      then zs:f ys []
                      else f ys (zs++[y])

instance Cil Assembly where
  cil (Assembly as n ts) =
      foldr (\a s -> cil a . s) id as
    . (".assembly " ++) . cilName n . (" {}\n" ++)
    . foldr (\t s -> cil t . nl . s) id ts

instance Cil AssemblyRef where
  cil (AssemblyRef n) = (".assembly extern " ++) . (n ++) . (" {}\n" ++)

instance Cil TypeDef where
  cil (Class cas n et its ds) =
      (".class " ++)
    . cilList cas . cilName n
    . maybe id (\e -> sp . ("extends " ++) . cil e) et
    . bool id (sp . ("implements " ++)
                    . foldr (.) id (map cil its)) (null its)
    . ("\n{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . ("}\n" ++)
  cil (GenericClass cas n ps ds) =
      (".class " ++) . cilList cas . cilName n
    . ("`" ++) . shows (length ps) . ("<" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (">\n{\n" ++)
    . foldr (\d s -> cil d . s) id ds
    . ("}\n" ++)

instance Cil GenParam where
  cil (GenParam n) = cilName n

instance Cil ClassAttr where
  cil CaPublic       = ("public" ++)
  cil CaNestedPublic = ("nested public" ++)

instance Cil ClassDecl where
  cil (FieldDef fd)  = cil fd
  cil (MethodDef md) = cil md
  cil (TypeDef td)   = cil td

instance Cil TypeSpec where
  cil (TypeSpec nm) = cilName nm

instance Cil FieldDef where
  cil (Field fas t n) = 
      ident . (".field " ++)
      . cilList fas
      . cil t . sp . cilName n . nl

instance Cil FieldAttr where
  cil (FaStatic)    = ("static" ++)
  cil (FaPublic)    = ("public" ++)
  cil (FaPrivate)   = ("private" ++)
  cil (FaAssembly)  = ("assembly" ++)

instance Cil MethodDef where
  cil (Constructor mas t ps ms) =
      ident . (".method " ++)
    . cilList mas . cil t . sp . (".ctor(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\m s -> cil m . s) id ms
    . ident . ("}\n" ++)
  cil (Method mas t n ps ms) =
      ident . (".method " ++)
    . cilList mas
    . cil t . sp . cilName n . ("(" ++)
    . foldr (.) id (intersperse (", " ++) (map cil ps))
    . (") cil managed\n" ++)
    . ident . ("{\n" ++)
    . foldr (\m s -> cil m . s) id ms
    . ident . ("}\n" ++)

instance Cil MethAttr where
  cil (MaStatic)    = ("static" ++)
  cil (MaPublic)    = ("public" ++)
  cil (MaPrivate)   = ("private" ++)
  cil (MaAssembly)  = ("assembly" ++)
  cil (MaVirtual)   = ("virtual" ++)
  cil (MaHidebysig) = ("hidebysig" ++)

instance Cil Parameter where
  cil (Param t n) = cil t . sp . cilName n

instance Cil MethodDecl where
  cil (Directive d) = cil d
  cil (Instr i)     = cil i
  cil (Comment s)   = ident . ident . ("// " ++) . (s ++) . nl

instance Cil Instr where
  cil (OpCode oc)      = ident . ident . cil oc . nl
  cil (LabOpCode l oc) = ident . (l ++) . (":" ++) . nl
                               . ident . ident . cil oc . nl

instance Cil Directive where
  cil (EntryPoint)    = ident . ident . (".entrypoint" ++) . nl
  cil (LocalsInit ls) =
    let bigident = ident . ident . ident . ident
    in
      ident . ident . (".locals init (" ++)
    . bool id nl (null ls)
    . foldr (.) id (intersperse (",\n" ++) (map (\l -> bigident . cil l) ls))
    . (")\n" ++)
  cil (MaxStack x)    = ident . ident . (".maxstack " ++) . shows x . nl

instance Cil Local where
  cil (Local t n) = cil t . sp . cilName n

instance Cil OpCode where
  cil (Add)                 = ("add" ++)
  cil (And)                 = ("and" ++)
  cil (Beq l)               = ("beq " ++) . (l ++)
  cil (Bge l)               = ("bge " ++) . (l ++)
  cil (Bgt l)               = ("bgt " ++) . (l ++)
  cil (Ble l)               = ("ble " ++) . (l ++)
  cil (Blt l)               = ("blt " ++) . (l ++)
  cil (Box t)               = ("box " ++) . cil t
  cil (Br l)                = ("br " ++) . (l ++)
  cil (Brfalse l)           = ("brfalse " ++) . (l ++)
  cil (Brtrue l)            = ("brtrue " ++) . (l ++)
  cil (Call ccs t a c m ps) = ("call " ++) . cilList ccs . cil t . sp
                                . cilCall a c m ps
  cil (CallVirt t a c m ps) = ("callvirt instance " ++) . cilsp t . sp
                                . cilCall a c m ps
  cil (Ceq)                 = ("ceq" ++)
  cil (Cge)                 = ("cge" ++)
  cil (Cgt)                 = ("cgt" ++)
  cil (Cle)                 = ("cle" ++)
  cil (Clt)                 = ("clt" ++)
  cil (Dup)                 = ("dup" ++)
  cil (Isinst nm)           = ("isinst " ++) . cilName nm
  cil (Ldarg x)             = ("ldarg " ++) . shows x
  cil (Ldarg_0)             = ("ldarg.0 " ++)
  cil (Ldarg_1)             = ("ldarg.1 " ++)
  cil (Ldarg_2)             = ("ldarg.2 " ++)
  cil (Ldarg_3)             = ("ldarg.3 " ++)
  cil (LdargN nm)           = ("ldarg " ++) . cilName nm
  cil (Ldc_i4 x)            = ("ldc.i4 " ++) . shows x
  cil (Ldc_i4_0)            = ("ldc.i4.0 " ++) 
  cil (Ldc_i4_1)            = ("ldc.i4.1 " ++) 
  cil (Ldc_i4_2)            = ("ldc.i4.2 " ++) 
  cil (Ldc_i4_3)            = ("ldc.i4.3 " ++) 
  cil (Ldc_i4_4)            = ("ldc.i4.4 " ++) 
  cil (Ldc_i4_5)            = ("ldc.i4.5 " ++) 
  cil (Ldc_i4_6)            = ("ldc.i4.6 " ++) 
  cil (Ldc_i4_7)            = ("ldc.i4.7 " ++) 
  cil (Ldc_i4_8)            = ("ldc.i4.8 " ++) 
  cil (Ldc_i4_m1)           = ("ldc.i4.m1 " ++) 
  cil (Ldc_i4_s x)          = ("ldc.i4.s " ++)  . shows x
  cil (Ldfld t a c f)       = ("ldfld " ++) . cil t . sp . cilFld a c f
  cil (Ldflda t a c f)      = ("ldflda " ++) . cil t . sp . cilFld a c f
  cil (Ldind_ref)           = ("ldind.ref " ++)
  cil (Ldloc x)             = ("ldloc " ++) . shows x
  cil (Ldloc_0)             = ("ldloc.0 " ++)
  cil (Ldloc_1)             = ("ldloc.1 " ++)
  cil (Ldloc_2)             = ("ldloc.2 " ++)
  cil (Ldloc_3)             = ("ldloc.3 " ++)
  cil (LdlocN nm)           = ("ldloc " ++) . cilName nm
  cil (Ldloca x)            = ("ldloca " ++) . shows x
  cil (LdlocaN nm)          = ("ldloca " ++) . cilName nm
  cil (Ldsfld t a c f)      = ("ldsfld " ++) . cil t . sp . cilFld a c f
  cil (Ldsflda t a c f)     = ("ldsflda " ++) . cil t . sp . cilFld a c f
  cil (Ldstr s)             = ("ldstr " ++) . shows s
  cil (Neg)                 = ("neg" ++)
  cil (Newobj t a c ps)     = ("newobj instance " ++) . cil t . sp
                               . cilNewobj a c ps
  cil (Nop)                 = ("nop" ++)
  cil (Pop)                 = ("pop" ++)
  cil (Rem)                 = ("rem" ++)
  cil (Ret)                 = ("ret" ++)
  cil (Stfld t a c f)       = ("stfld " ++) . cil t . sp . cilFld a c f
  cil (Stind_ref)           = ("stind.ref " ++)
  cil (Stloc x)             = ("stloc " ++) . shows x
  cil (Stloc_0)             = ("stloc.0 " ++)
  cil (Stloc_1)             = ("stloc.1 " ++)
  cil (Stloc_2)             = ("stloc.2 " ++)
  cil (Stloc_3)             = ("stloc.3 " ++)
  cil (StlocN nm)           = ("stloc " ++) . cilName nm
  cil (Stsfld t a c f)      = ("stsfld " ++) . cil t . sp . cilFld a c f
  cil (Sub)                 = ("sub" ++)
  cil (Tail)                = ("tail." ++)
  cil (Tailcall opcode)     = ("tail. " ++) . cil opcode
  cil (Unbox t)             = ("unbox " ++) . cil t

instance Cil CallConv where
  cil (CcInstance) = ("instance" ++)

cilList :: (Cil a) => [a] -> ShowS
cilList = foldr (\x s -> cil x . sp . s) id

cilFld :: DottedName -> DottedName -> DottedName -> ShowS
cilFld a c f = 
    cilAssembly a
  . (if c /= ""
     then cilName c . ("::" ++)
     else id)
  . cilName f

cilNewobj :: DottedName -> DottedName -> [PrimitiveType] -> ShowS
cilNewobj a c ps = 
    cilAssembly a
  . (if c /= ""
     then cilName c . ("::" ++)
     else id)
  . (".ctor(" ++)
  . foldr (.) id (intersperse (", " ++) (map cil ps))
  . (")" ++)

cilCall :: DottedName -> DottedName -> DottedName -> [PrimitiveType] -> ShowS
cilCall a c m ps = 
    cilAssembly a
  . bool id (cilName c . ("::" ++)) (c == "")
  . cilName m
  . ("(" ++)
  . foldr (.) id (intersperse (", " ++) (map cil ps))
  . (")" ++)

cilAssembly :: DottedName -> ShowS
cilAssembly a = bool id (("[" ++) . cilName a . ("]" ++)) (a == "")

instance Cil PrimitiveType where
  cil Void                = ("void" ++) 
  cil Bool                = ("bool" ++)
  cil Char                = ("char" ++)
  cil Byte                = ("uint8" ++)
  cil Int32               = ("int32" ++)
  cil Int64               = ("int64" ++)
  cil String              = ("string" ++)
  cil Object              = ("object" ++)
  cil (ValueType a c)     = ("valuetype " ++) . cilAssembly a . cilName c
  cil (ReferenceType a c) = ("class " ++ ) . cilAssembly a . cilName c
  cil (GenericType x)     = ("!" ++) . shows x
  cil (ByRef pt)          = cil pt . ("&" ++)

-- Helper functions, to pretty print
cilsp :: (Cil a) => a -> ShowS
cilsp x = let s = cil x ""
          in bool id (cil x . sp) (s == "")

ident = ("    " ++)
sp    = (" " ++)
nl    = ('\n' :)

bool :: a -> a -> Bool -> a
bool x y True  = x
bool x y False = y

