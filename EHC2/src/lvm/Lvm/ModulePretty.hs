{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: ModulePretty.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.ModulePretty ( modulePretty, ppId, ppVarId, ppConId, ppString ) where

import Char     ( isAlphaNum, isAlpha, isLower, isUpper )
import Lvm.Common.PPrint
import Lvm.Common.Byte     ( stringFromBytes )
import Lvm.Common.Id       ( Id, stringFromId, idFromString )
import Lvm.Common.IdMap    ( listFromMap )
import Lvm.Common.IdSet    ( IdSet, setFromList, elemSet )
import Lvm.Lvm.Module

----------------------------------------------------------------
--
----------------------------------------------------------------
modulePretty :: (v -> Doc) -> Module v -> Doc
modulePretty ppValue mod
  = ppModule ppValue mod

ppModule ::  (v -> Doc) -> Module v -> Doc
ppModule ppValue (Module moduleName major minor decls)
  =  text "module" <+> ppConId moduleName <+> text "where"
 <$> vcat (map (\decl -> ppDecl ppValue decl <> semi <> line) decls)
 <$> empty

ppDecl :: (v -> Doc) -> Decl v -> Doc
ppDecl ppValue decl
  = nest 2 $
    case decl of
      DeclValue{}     -> ppVarId (declName decl) <+> ppAttrs decl 
                         <$> text "=" <+> ppValue (valueValue decl)
      DeclCon{}       -> text "con" <+> ppConId (declName decl) <+> ppAttrs decl 
                         <$> text "=" <+> text "#(" <> pretty (conTag decl) <> 
                                          text ","  <> pretty (declArity decl) <> text ")"
      DeclCustom{}    -> text "custom" <+> ppDeclKind (declKind decl) <+> ppId (declName decl) <+> ppAttrs decl
      DeclExtern{}    -> text "extern" <+> ppVarId (declName decl) <+> ppAttrs decl
                         <$> text "=" <> ppLinkConv (externLink decl) <> ppCallConv (externCall decl)
                         <+> ppExternName (externLib decl) (externName decl) <+> pretty (declArity decl)
                         <+> ppExternType (externCall decl) (externType decl)
      DeclAbstract{}  -> text "abstract" <+> ppVarId (declName decl) <+> ppNoImpAttrs decl
                         <$> text "=" <+> ppImported (declAccess decl) <+> pretty (declArity decl)
      DeclImport{}    -> text "import" <+> ppDeclKind (importKind (declAccess decl)) 
                         <+> ppId (declName decl) <+> ppNoImpAttrs decl
                         <$> text "=" <+> ppImported (declAccess decl)
      other           -> error "ModulePretty.ppDecl: unknown declaration"

ppLinkConv linkConv
  = case linkConv of
      LinkRuntime -> text " runtime"
      LinkDynamic -> text " dynamic"
      LinkStatic  -> empty

ppCallConv callConv
  = case callConv of
      CallInstr -> text " instruction"
      CallStd   -> text " stdcall"
      CallC     -> empty

ppExternName libName extName
  = case extName of
      Plain name    -> ppQual name
      Decorate name -> text "decorate" <+> ppQual name
      Ordinal i     -> ppQual (show i)
  where
    ppQual name   = (if (null libName) then empty
                                       else ppConId (idFromString libName) <> char '.' )
                    <> ppVarId (idFromString name)

ppExternType callConv tp
  = text "::" <+> case callConv of
                    CallInstr -> text tp
                    other     -> ppString tp



ppNoImpAttrs decl
  = ppAttrsEx True decl

ppAttrs decl
  = ppAttrsEx False decl

ppAttrsEx hideImp decl
  = if (null (declCustoms decl) && not (accessPublic (declAccess decl)))
     then empty
     else text ":" <+> ppAccess (declAccess decl) 
          <+> (if (not hideImp) then ppImportAttr (declAccess decl) else empty) 
          <> ppCustoms (declCustoms decl)

ppAccess acc
  = if (accessPublic acc) 
     then text "public" 
     else text "private"

ppImportAttr  acc
  = case acc of
      Defined public -> empty
      Imported public modid impid impkind major minor
        -> text "import" <+> ppDeclKind impkind <+> ppConId modid <> char '.' <> ppId impid <> space
  
ppImported acc
  = case acc of
      Defined public -> error "ModulePretty.ppImported: internal error: abstract or import value should always be imported!"
      Imported public modid impid impkind major minor
        -> ppConId modid <> char '.' <> ppId impid
  

ppCustoms customs
  = if (null customs) 
     then empty
     else list (map ppCustom customs)

ppCustom custom
  = case custom of
      CustomInt i         -> pretty i
      CustomName id       -> ppId id
      CustomBytes bs      -> dquotes (string (stringFromBytes bs))
      CustomLink id kind  -> text "custom" <+> ppDeclKind kind <+> ppId id
      CustomDecl kind cs  -> text "custom" <+> ppDeclKind kind <+> ppCustoms cs
      CustomNothing       -> text "nothing"
      other               -> error "ModulePretty.ppCustom: unknown custom kind"

ppDeclKind kind
  = case kind of
      DeclKindCustom id   -> ppId id
--      DeclKindName        
--      DeclKindKind
--      DeclKindBytes       
--      DeclKindCode
      DeclKindValue       -> ppId (idFromString "val")
      DeclKindCon         -> ppId (idFromString "con")
      DeclKindImport      -> ppId (idFromString "import")
      DeclKindModule      -> ppId (idFromString "module")
      DeclKindExtern      -> ppId (idFromString "extern")
--      DeclKindExternType      
      other               -> pretty (fromEnum kind)


ppId :: Id -> Doc
ppId id
  = ppEscapeId isAlpha '$' id

ppVarId :: Id -> Doc
ppVarId id
  = ppEscapeId isLower '$' id

ppConId :: Id -> Doc
ppConId id
  = ppEscapeId isUpper '@' id

ppString :: String -> Doc
ppString s
  = dquotes (text (concatMap escape s))

ppEscapeId isValid c id
  = if (not (isReserved id) && firstOk && ordinary)
     then text name
     else char c <> text (concatMap escapeId name) <> char ' '
  where
    name     = stringFromId id
    firstOk  = case name of
                 []     -> False
                 (c:cs) -> isValid c
    ordinary = all idchar name
    idchar c = isAlphaNum c || c == '_' || c == '\''
    
escapeId c
  = case c of
      ' ' -> "\\s"
      _   -> escape c

escape c
  = case c of
      '.'   -> "\\."
      '\a'  -> "\\a"
      '\b'  -> "\\b"
      '\f'  -> "\\f"
      '\n'  -> "\\n"
      '\r'  -> "\\r"
      '\t'  -> "\\t"
      '\v'  -> "\\v"
      '\\'  -> "\\\\"
      '\"'  -> "\\\""
      '\''  -> "\\'"
      _     -> [c]


isReserved :: Id -> Bool
isReserved id
  = elemSet id reserved
  
reserved :: IdSet
reserved
  = setFromList $ map idFromString $
    ["module","where"
    ,"import","abstract","extern"
    ,"custom","val","con"
    ,"match","with"
    ,"let","rec","in"
    ,"static","dynamic","runtime"
    ,"stdcall","ccall","instruction"
    ,"decorate"
    ,"private","public","nothing"
    ,"type","data","forall","exist"
    ,"case","of"
    ,"if","then","else"
    ]
