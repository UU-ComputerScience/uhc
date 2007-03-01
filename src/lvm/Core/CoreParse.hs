{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreParse.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CoreParse
    ( coreParse, coreParseExport, coreParseExpr
    , modulePublic
    , coreParseType, Type(..)
    ) where

import Lvm.Common.PPrint( (<+>), (<>), Doc, text, hsep )
import qualified Lvm.Common.PPrint as PPrint

import Lvm.Common.Standard( foldlStrict )
import Text.ParserCombinators.Parsec hiding (satisfy)
import Lvm.Common.Byte   ( bytesFromString )
import Lvm.Common.Id     ( Id, stringFromId, idFromString, dummyId )
import Lvm.Common.IdSet  ( IdSet, deleteSet, unionSet, emptySet, singleSet, listFromSet )
import Lvm.Common.IdMap
import Lvm.Common.IdSet
import Lvm.Core.Core
import Lvm.Core.CoreLexer
import Lvm.Core.CorePretty

----------------------------------------------------------------
-- Parse a Core source file
----------------------------------------------------------------
coreParse :: FilePath -> IO CoreModule
coreParse = coreParseModule parseModule

coreParseExport :: FilePath -> IO (CoreModule, Bool, (IdSet,IdSet,IdSet,IdSet,IdSet))
coreParseExport = coreParseModule parseModuleExport

coreParseModule :: TokenParser a -> FilePath -> IO a
coreParseModule parser fname =
    do{ input  <- readFile fname
      ; res <- case runParser parser () fname (layout (lexer (1,1) input)) of
          Left err
            -> ioError (userError ("parse error: " ++ show err))
          Right res
            -> return res
      --; (putStrLn.show) res
      ; return res
      }

coreParseAny :: TokenParser a -> String -> String -> a
coreParseAny parser fname input =
    case runParser parser () fname (lexer (1,1) input) of {
        Left  err -> error ("\n" ++ fname ++ ": " ++ show err);
        Right res -> res;
    }

coreParseExpr fname input = coreParseAny pexpr fname input
coreParseType fname input = addForall (coreParseAny ptypeFun fname input)

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------
type TokenParser a  = GenParser Token () a

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
data Type       = TFun    {tp1::Type, tp2::Type}
                | TAp     {tp1::Type, tp2::Type}
                | TForall {tpId::Id, tp::Type}
                | TExist  {tpId::Id, tp::Type}
                | TStrict {tp::Type}
                | TVar    {tpId::Id}
                | TCon    {tpId::Id}
                | TAny
                | TString {tpString::String}
                deriving (Show)

data Kind       = KFun {kind1::Kind, kind2::Kind}
                | KStar
                | KString {kindString::String}

data SuperKind  = Box

arityFromType :: Type -> Int
arityFromType tp
  = case tp of
      TFun    t1 t2   -> arityFromType t2 + 1
      TAp     t1 t2   -> 0                     -- assumes saturated constructors!
      TForall id t    -> arityFromType t
      TExist  id t    -> arityFromType t
      TStrict t       -> arityFromType t
      TVar    id      -> 0
      TCon    id      -> 0
      TAny            -> 0
      TString s       -> error "Core.arityFromType: string type"

arityFromKind :: Kind -> Int
arityFromKind kind
  = case kind of
      KFun    k1 k2   -> arityFromKind k1 + 1
      KStar           -> 0
      KString s       -> error "Core.arityFromKind: string kind"

ppType :: Type -> Doc
ppType tp
  = ppTypeEx 0 tp

ppTypeEx :: Int -> Type -> Doc
ppTypeEx level tp
  = parenthesized $
    case tp of
      TAp (TCon id) t2 | id == idFromString "[]" -> text "[" <> ppType t2 <> text "]" 
      TFun    t1 t2   -> ppHi t1 <+> text "->" <+> ppEq t2
      TAp     t1 t2   -> ppEq t1 <+> ppHi t2
      TForall id t    -> text "forall" <+> ppId id <> text "." <+> ppEq t
      TExist  id t    -> text "exist" <+> ppId id <> text "." <+> ppEq t
      TStrict t       -> ppHi t <> text "!"
      TVar    id      -> ppId id
      TCon    id      -> ppId id
      TAny            -> text "any"
      TString s       -> PPrint.string s
  where
    tplevel           = levelFromType tp
    parenthesized doc | level <= tplevel  = doc
                      | otherwise         = PPrint.parens doc
    ppHi t            | level <= tplevel  = ppTypeEx (tplevel+1) t
                      | otherwise         = ppTypeEx 0 t
    ppEq  t           | level <= tplevel  = ppTypeEx tplevel t
                      | otherwise         = ppTypeEx 0 t


ppKind :: Kind -> Doc
ppKind kind
  = ppKindEx 0 kind

ppKindEx level kind
  = parenthesized $
    case kind of
      KFun k1 k2    -> ppHi k1 <+> text "->" <+> ppEq k2
      KStar         -> text "*"
      KString s     -> PPrint.string s
  where
    (klevel,parenthesized)
      | level <= levelFromKind kind   = (levelFromKind kind,id)
      | otherwise                     = (0,PPrint.parens)

    ppHi k  = ppKindEx (if klevel<=0 then 0 else klevel+1) k
    ppEq k  = ppKindEx klevel k


ppId id
  = PPrint.string (stringFromId id)

levelFromType tp
  = case tp of
      TString s       -> 1 
      TForall id t    -> 2
      TExist  id t    -> 2
      TFun    t1 t2   -> 3
      TAp     t1 t2   -> 4
      TStrict t       -> 5
      TVar    id      -> 6
      TCon    id      -> 6
      TAny            -> 7 

levelFromKind kind
  = case kind of
      KString s     -> 1
      KFun k1 k2    -> 2
      KStar         -> 3

addForall :: Type -> Type
addForall tp
  = foldr TForall tp (listFromSet (varsInType tp))

varsInType :: Type -> IdSet
varsInType tp
  = case tp of
      TForall id t    -> deleteSet id (varsInType t)
      TExist  id t    -> deleteSet id (varsInType t)
      TString s       -> emptySet
      TFun    t1 t2   -> unionSet (varsInType t1) (varsInType t2)
      TAp     t1 t2   -> unionSet (varsInType t1) (varsInType t2)
      TStrict t       -> varsInType t
      TVar    id      -> singleSet id
      TCon    id      -> emptySet
      TAny            -> emptySet
   

----------------------------------------------------------------
-- Program
----------------------------------------------------------------
wrap p
  = do{ x <- p; return [x] }

parseModule :: TokenParser CoreModule
parseModule =
    do{ (mod, _, _) <- parseModuleExport
      ; return mod
      }

parseModuleExport :: TokenParser (CoreModule, Bool, (IdSet,IdSet,IdSet,IdSet,IdSet))
parseModuleExport =
    do{ lexeme LexMODULE
      ; moduleId <- conid <?> "module name"
      ; exports <- pexports
      ; lexeme LexWHERE
      ; lexeme LexLBRACE
      ; declss <- semiList (wrap (ptopDecl <|> pabstract <|> pextern <|> pCustomDecl)
                            <|> pdata <|> pimport <|> ptypeTopDecl)
      ; lexeme LexRBRACE
      ; lexeme LexEOF

      ; return $
            ( case exports of
                Nothing ->
                    let es = (emptySet,emptySet,emptySet,emptySet,emptySet)
                    in
                    ( modulePublic
                        True
                        es
                        (Module moduleId 0 0 (concat declss))
                    , True
                    , es
                    )
                Just es ->
                    ( modulePublic
                        False
                        es
                        (Module moduleId 0 0 (concat declss))
                    , False
                    , es
                    )
            )
      }

modulePublic :: Bool -> (IdSet,IdSet,IdSet,IdSet,IdSet) -> Module v -> Module v
modulePublic implicit (exports,exportCons,exportData,exportDataCon,exportMods) mod
  = mod{ moduleDecls = map setPublic (moduleDecls mod) }
  where
    setPublic decl  | declPublic decl = decl{ declAccess = (declAccess decl){ accessPublic = True } }
                    | otherwise       = decl
    
    isExported decl elemIdSet =
        let
            access = declAccess decl
            name   = declName   decl
        in
        if implicit then
            case decl of
                DeclImport{} ->  False
                _ ->
                    case access of
                        Imported{} -> False
                        _          -> True
        else
            case access of
                Imported{ importModule = id }
                    | elemSet id exportMods               -> True
                    | otherwise                           -> elemIdSet
                Defined{}
                    | elemSet (moduleName mod) exportMods -> True
                    | otherwise                           -> elemIdSet
                other -> elemIdSet
    
    declPublic decl =
        let
            name = declName decl
        in
        case decl of
            DeclValue{}     ->  isExported decl (elemSet name exports)
            DeclAbstract{}  ->  isExported decl (elemSet name exports)
            DeclExtern{}    ->  isExported decl (elemSet name exports)
            DeclCon{}       ->  isExported decl
                                    (  elemSet name exportCons
                                    || elemSet (conTypeName decl) exportDataCon
                                    )
            DeclCustom{}    ->  isExported decl
                                    (   (  declKind decl == customData
                                        || declKind decl == customTypeDecl
                                        )
                                    &&  elemSet name exportData
                                    )
            DeclImport{}    ->  not implicit && case importKind (declAccess decl) of
                                    DeclKindValue  -> isExported decl (elemSet name exports)
                                    DeclKindExtern -> isExported decl (elemSet name exports)
                                    DeclKindCon    -> isExported decl (elemSet name exportCons) 
                                    DeclKindModule -> isExported decl (elemSet name exportMods)
                                    DeclKindCustom id
                                     | id == idFromString "data"
                                       ||
                                       id == idFromString "typedecl" ->
                                         isExported decl (elemSet name exportData)
                                    other          -> False
            other           -> False

    conTypeName (DeclCon{declCustoms=(tp:CustomLink id customData:rest)})  = id
    conTypeName other  = dummyId

----------------------------------------------------------------
-- export list
----------------------------------------------------------------
data Export  = ExportValue Id
             | ExportCon   Id
             | ExportData  Id 
             | ExportDataCon Id
             | ExportModule Id

pexports :: TokenParser (Maybe (IdSet,IdSet,IdSet,IdSet,IdSet))
pexports
  = do{ exports <- commaParens pexport <|> return []
      ; return $
            if null (concat exports) then
                Nothing
            else
                Just (foldlStrict
                    split
                    (emptySet,emptySet,emptySet,emptySet,emptySet)
                    (concat exports)
                )
      }
  where
    split (values,cons,datas,datacons,mods) exp
      = case exp of
          ExportValue   id -> (insertSet id values,cons,datas,datacons,mods)
          ExportCon     id -> (values,insertSet id cons,datas,datacons,mods)
          ExportData    id -> (values,cons,insertSet id datas,datacons,mods)
          ExportDataCon id -> (values,cons,datas,insertSet id datacons,mods)
          ExportModule  id -> (values,cons,datas,datacons,insertSet id mods)

pexport :: TokenParser [Export]
pexport
  = do{ lexeme LexLPAREN
      ; entity <-
            do { id <- opid   ; return (ExportValue id) }
            <|>
            do { id <- conopid; return (ExportCon   id) }
      ; lexeme LexRPAREN
      ; return [entity]
      }
  <|>
    do{ id <- varid
      ; return [ExportValue id]
      }
  <|>
    do{ id <- typeid
      ; do{ lexeme LexLPAREN
          ; cons <- pexportCons id
          ; lexeme LexRPAREN
          ; return (ExportData id:cons)
          }
        <|>
        -- no parenthesis: could be either a
        -- constructor or a type constructor
        return [ExportData id, ExportCon id]
      }      
  <|>
    do{ lexeme LexMODULE
      ; id <- conid
      ; return [ExportModule id]
      }

pexportCons id
  = do{ lexeme LexDOTDOT
      ; return [ExportDataCon id]
      }
  <|>
    do{ ids <- sepBy constructor (lexeme LexCOMMA)
      ; return (map ExportCon ids)
      }


----------------------------------------------------------------
-- abstract declarations
----------------------------------------------------------------
pabstract :: TokenParser CoreDecl
pabstract
  = do{ lexeme LexABSTRACT
      ; pabstractValue <|> pabstractCon
      }

pabstractValue
  = do{ id <- variable
      ; lexeme LexASG
      ; (modid,impid) <- qualifiedVar
      ; (tp,tparity) <- ptypeDecl
      ; arity <- do{ lexeme LexASG; i <- lexInt; return (fromInteger i) } <|> return tparity
      ; return (DeclAbstract id (Imported False modid impid DeclKindValue 0 0) arity [])
      }

pabstractCon
  = do{ id <- conid
      ; lexeme LexASG
      ; (modid,impid) <- qualifiedCon
      ; (tp,arity) <- ptypeDecl
      ; lexeme LexASG
      ; tag <- lexInt
      ; return (DeclCon id (Imported False modid impid DeclKindCon 0 0) arity (fromInteger tag) [])
      }


----------------------------------------------------------------
-- import declarations
----------------------------------------------------------------
pimport :: TokenParser [CoreDecl]
pimport
  = do{ lexeme LexIMPORT
      ; modid <- conid
      ; do{ xss <- commaParens (pImportSpec modid)
          ; return (concat xss)
          }
        <|>
        return [DeclImport modid (Imported False modid dummyId DeclKindModule 0 0) []]
      }

pImportSpec :: Id -> TokenParser [CoreDecl]
pImportSpec modid
  = do{ lexeme LexLPAREN
      ; (kind, id) <-
            do { id <- opid   ; return (DeclKindValue, id) }
            <|>
            do { id <- conopid; return (DeclKindCon  , id) }
      ; lexeme LexRPAREN
      ; impid <- option id (do{ lexeme LexASG; variable })
      ; return [DeclImport id (Imported False modid impid kind 0 0) []]
      }
  <|>
    do{ id <- varid
      ; impid <- option id (do{ lexeme LexASG; variable })
      ; return [DeclImport id (Imported False modid impid DeclKindValue 0 0) []]
      }
  <|>
    do{ lexeme LexCUSTOM
      ; kind <- lexString
      ; id   <- variable <|> constructor
      ; impid <- option id (do { lexeme LexASG; variable <|> constructor })
      ; return [DeclImport id (Imported False modid impid (DeclKindCustom (idFromString kind)) 0 0) []]
      }
  <|>
    do{ id <- typeid
      ; impid <- option id (do{ lexeme LexASG; variable })
      ; do{ lexeme LexLPAREN
          ; cons <- pImportCons modid
          ; lexeme LexRPAREN
          ; return (DeclImport id (Imported False modid impid customData 0 0) [] : cons)
          }
        <|>
        return
            [DeclImport id (Imported False modid impid DeclKindCon 0 0) []]
      }

pImportCons :: Id -> TokenParser [CoreDecl]
pImportCons modid
  = -- do{ lexeme LexDOTDOT
    --   ; return [ExportDataCon id]
    --   }
  -- <|>
    sepBy (pimportCon modid) (lexeme LexCOMMA)

pimportCon :: Id -> TokenParser CoreDecl
pimportCon modid
  = do{ id    <- constructor
      ; impid <- option id (do{ lexeme LexASG; variable })
      ; return (DeclImport id (Imported False modid impid DeclKindCon 0 0) [])
      }

----------------------------------------------------------------
-- value declarations
----------------------------------------------------------------
ptopDecl :: TokenParser CoreDecl
ptopDecl
  = do{ id <- variable
      ; ptopDeclType id <|> ptopDeclDirect id
      }

ptopDeclType id
  = do{ (tp,arity) <- ptypeDecl
      ; lexeme LexSEMI
      ; id'  <- variable
      ; if (id /= id')
         then fail
            (  "identifier for type signature "
            ++ stringFromId id
            ++ " doesn't match the definition"
            ++ stringFromId id'
            )
         else return ()
      ; (access,custom,expr) <- pbindTopRhs
      ; return (DeclValue id access Nothing expr 
                ([customType tp] ++ custom))
      }

ptopDeclDirect id
  = do{ (access,custom,expr) <- pbindTopRhs
      ; return (DeclValue id access Nothing expr custom)
      }
  where
    typeFromExpr expr
      = TString ""

pbindTopRhs
  = do{ args <- many bindid
      ; (access,custom) <- pAttributes
      ; lexeme LexASG
      ; body <- pexpr
      ; let expr = foldr Lam body args
      ; return (access,custom,expr)
      }
  <?> "declaration"



pbind :: TokenParser Bind
pbind
  = do{ id   <- variable
      ; expr <- pbindRhs
      ; return (Bind id expr)
      }

pbindRhs
  = do{ args <- many bindid
      ; lexeme LexASG
      ; body <- pexpr
      ; let expr = foldr Lam body args
      ; return expr
      }
  <?> "declaration"

----------------------------------------------------------------
-- data declarations
----------------------------------------------------------------
customData      = DeclKindCustom (idFromString "data")
customTypeDecl  = DeclKindCustom (idFromString "typedecl")

customType tp   = CustomDecl (DeclKindCustom (idFromString "type")) [CustomBytes (bytesFromString (show (ppType tp)))]
customKind k    = CustomDecl (DeclKindCustom (idFromString "kind")) [CustomBytes (bytesFromString (show (ppKind k)))]

pdata :: TokenParser [CoreDecl]
pdata
  = do{ lexeme LexDATA
      ; id   <- typeid
      ; args <- many typevarid
      ; let kind     = foldr KFun KStar (map (const KStar) args)
            datadecl = DeclCustom id private customData [customKind kind]
      ; do{ lexeme LexASG
          ; let tp  = foldl TAp (TCon id) (map TVar args)
          ; cons <- sepBy1 (pconDecl tp) (lexeme LexBAR)
          ; let con tag (conid,tp) = (DeclCon conid private (arityFromType tp) tag 
                                      [customType tp, 
                                       CustomLink id customData])
          ; return (datadecl:zipWith con [0..] cons)
          }
      <|> {- empty data types -}
        do{ return [datadecl] }
      }

pconDecl :: Type -> TokenParser (Id,Type)
pconDecl tp
  = do{ id   <- constructor
      ; args <- many ptypeAtom
      ; return (id,foldr TFun tp args)
      }

----------------------------------------------------------------
-- type declarations
----------------------------------------------------------------

ptypeTopDecl :: TokenParser [CoreDecl]
ptypeTopDecl
  = do{ lexeme LexTYPE
      ; id   <- typeid
      ; args <- many typevarid
      ; lexeme LexASG
      ; tp   <- ptype
      ; let kind  = foldr KFun KStar (map (const KStar) args)
            tpstr = show $ (ppId id <+> hsep (map ppId args) <+> text "=" <+> ppType tp)
      ; return [DeclCustom id private customTypeDecl 
                     [CustomBytes (bytesFromString tpstr)
                     ,customKind kind]]
      }

----------------------------------------------------------------
-- Custom
----------------------------------------------------------------
pCustomDecl :: TokenParser CoreDecl
pCustomDecl
  = do{ lexeme LexCUSTOM
      ; kind <- pdeclKind
      ; id   <- customid
      ; (access,customs) <- pAttributes
      ; return (DeclCustom id access kind customs)
      }

pAttributes :: TokenParser (Access,[Custom])
pAttributes
  = do{ lexeme LexCOLON
      ; access  <- paccess
      ; customs <- pcustoms
      ; return (access,customs)
      }
  <|> return (private,[])

paccess 
  =   do{ lexeme LexPRIVATE; return private }
  <|> do{ lexeme LexPUBLIC; return public }
  <|> return private

pcustoms
  = do{ lexeme LexLBRACKET
      ; customs <- pcustom `sepBy` (lexeme LexCOMMA)
      ; lexeme LexRBRACKET
      ; return customs
      }

pcustom
  =   do{ i <- lexInt; return (CustomInt (fromInteger i)) }
  <|> do{ s <- lexString; return (CustomBytes (bytesFromString s)) }
  <|> do{ id <- variable <|> constructor; return (CustomName id) }
  <|> do{ lexeme LexNOTHING; return (CustomNothing) }
  <|> do{ lexeme LexCUSTOM 
        ; kind <- pdeclKind
        ; do{ id   <- customid
            ; return (CustomLink id kind)
            }
        <|>
          do{ cs   <- pcustoms
            ; return (CustomDecl kind cs)
            }
        }
  <?> "custom value"


pdeclKind
  =   do{ id <- varid;    return (DeclKindCustom id) }
  <|> do{ i <- lexInt;    return (toEnum (fromInteger i)) }
  <|> do{ s <- lexString; return (DeclKindCustom (idFromString s)) }
  <?> "custom kind"

----------------------------------------------------------------
-- Expressions
----------------------------------------------------------------
pexpr
  = do{ lexeme LexBSLASH
      ; args <- many bindid
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (foldr Lam expr args)
      }
  <|>
    do{ lexeme LexLET
      ; binds <- semiBraces pbind
      ; lexeme LexIN
      ; expr  <- pexpr
      ; return (Let (Rec binds) expr)
      }
  <|>
    do{ lexeme LexCASE
      ; expr <- pexpr
      ; lexeme LexOF
      ; (id,alts) <- palts
      ; case alts of
          [Alt PatDefault rhs] -> return (Let (Strict (Bind id expr)) rhs)
          other                -> return (Let (Strict (Bind id expr)) (Match id alts))
      }
  <|>
    do{ lexeme LexMATCH
      ; id <- variable
      ; lexeme LexWITH
      ; (defid,alts) <- palts
      ; case alts of
          [Alt PatDefault rhs] -> return (Let (NonRec (Bind defid (Var id))) rhs)
          other                -> return (Let (NonRec (Bind defid (Var id))) (Match defid alts))
      }
  <|> 
    do{ lexeme LexLETSTRICT
      ; binds <- semiBraces pbind
      ; lexeme LexIN
      ; expr <- pexpr
      ; return (foldr (Let . Strict) expr binds)
      }
  <|> pexprAp
  <?> "expression"

pexprAp
  = do{ atoms <- many1 patom
      ; return (foldl1 Ap atoms)
      }

patom
  =   do{ id <- varid; return (Var id)  }
  <|> do{ id <- conid; return (Con (ConId id))  }
  <|> do{ lit <- pliteral; return (Lit lit) }
  <|> parenExpr
  <|> listExpr
  <?> "atomic expression"


listExpr
  = do{ lexeme LexLBRACKET
      ; exprs <- sepBy pexpr (lexeme LexCOMMA)
      ; lexeme LexRBRACKET
      ; return (foldr cons nil exprs)
      }
  where
    cons x xs   = Ap (Ap (Con (ConId (idFromString ":"))) x) xs
    nil         = Con (ConId (idFromString "[]"))


parenExpr
  = do{ lexeme LexLPAREN
      ; expr <-   do{ id <- opid
                    ; return (Var id)
                    }
                <|>
                  do{ id <- conopid
                    ; return (Con (ConId id))
                    }
                <|> 
                  do{ lexeme LexAT
                    ; tag   <- ptagExpr 
                    ; lexeme LexCOMMA
                    ; arity <- lexInt <?> "arity"
                    ; return (Con (ConTag tag (fromInteger arity)))
                    }
                <|>
                  do{ exprs <- pexpr `sepBy` (lexeme LexCOMMA)
                    ; case exprs of
                        [expr]  -> return expr
                        other   -> let con = Con (ConTag (Lit (LitInt 0)) (length exprs))
                                       tup = foldl Ap con exprs
                                   in return tup
                    }
      ; lexeme LexRPAREN
      ; return expr
      }

ptagExpr
  =   do{ i <- lexInt; return (Lit (LitInt (fromInteger i))) }
  <|> do{ id <- variable; return (Var id) }
  <?> "tag (integer or variable)"

pliteral
  =   pnumber id id
  <|> do{ s <- lexString; return (LitBytes (bytesFromString s)) }
  <|> do{ c <- lexChar;   return (LitInt (fromEnum c))   }
  <|> do{ lexeme LexDASH
        ; pnumber negate negate
        }
  <?> "literal"

pnumber signint signdouble
  =   do{ i <- lexInt;    return (LitInt (signint (fromInteger i))) }
  <|> do{ d <- lexDouble; return (LitDouble (signdouble d)) }

----------------------------------------------------------------
-- alternatives
----------------------------------------------------------------
palts :: TokenParser (Id,Alts)
palts
  = do{ lexeme LexLBRACE
      ; (id,alts) <- paltSemis
      ; return (id,alts)
      }

paltSemis :: TokenParser (Id,Alts)
paltSemis
  = do{ (id,alt) <- paltDefault
      ; optional (lexeme LexSEMI)
      ; lexeme LexRBRACE
      ; return (id,[alt])
      }
  <|>
    do{ alt <- palt
      ;   do{ lexeme LexSEMI
            ;     do{ (id,alts) <- paltSemis
                    ; return (id,alt:alts)
                    }
              <|> do{ lexeme LexRBRACE
                    ; id <- wildcard
                    ; return (id,[alt])
                    }
            }
      <|> do{ lexeme LexRBRACE
            ; id <- wildcard
            ; return (id,[alt])
            }
      }

palt  
  = do{ pat <- ppat
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (Alt pat expr)
      }

ppat  
  = ppatCon <|> ppatLit <|> ppatParens

ppatParens
  = do{ lexeme LexLPAREN
      ; do{ lexeme LexAT
          ; tag <- lexInt <?> "tag"        
          ; lexeme LexCOMMA
          ; arity <- lexInt <?> "arity"
          ; lexeme LexRPAREN
          ; ids <- many bindid
          ; return (PatCon (ConTag (fromInteger tag) (fromInteger arity)) ids)
          }
        <|>
        do{ id <- conopid
          ; lexeme LexRPAREN
          ; ids <- many bindid
          ; return (PatCon (ConId id) ids)
          }
        <|>
        do{ pat <- ppat <|> ppatTuple 
          ; lexeme LexRPAREN
          ; return pat
          }
      }

ppatCon
  = do{ id   <- conid <|> do{ lexeme LexLBRACKET; lexeme LexRBRACKET; return (idFromString "[]") }      
      ; args <- many bindid
      ; return (PatCon (ConId id) args)
      }

ppatLit
  = do{ lit <- pliteral; return (PatLit lit) }

ppatTuple
  = do{ ids <- bindid `sepBy` (lexeme LexCOMMA)
      ; return (PatCon (ConTag 0 (length ids)) ids)
      }

paltDefault
  = do{ id <- bindid <|> do{ lexeme LexDEFAULT; wildcard }
      ; lexeme LexRARROW
      ; expr <- pexpr
      ; return (id,Alt PatDefault expr)
      }

wildcard :: TokenParser Id
wildcard
  = identifier (return "_")

----------------------------------------------------------------
-- externs
----------------------------------------------------------------
pextern :: TokenParser CoreDecl
pextern
  = do{ lexeme LexEXTERN
      ; linkConv <- plinkConv
      ; callConv <- pcallConv
      ; id  <- varid
      ; mod <- lexString <|> return (stringFromId id)
      ; (modname,name) <- pExternName mod
      ; (TString tp,arity)  <- do{ lexeme LexCOLCOL; ptypeString } -- ptypeDecl
      ; return (DeclExtern id private arity tp linkConv callConv modname name [])
      }
  <|>
    do{ lexeme LexINSTR
      ; id <- varid
      ; s  <- lexString
      ; (tp,arity) <- ptypeDecl
      ; return (DeclExtern id private arity (show (ppType tp)) LinkStatic CallInstr "" (Plain s) [])
      }

------------------

plinkConv
  =   do{ lexeme LexSTATIC; return LinkStatic }
  <|> do{ lexeme LexDYNAMIC; return LinkDynamic }
  <|> do{ lexeme LexRUNTIME; return LinkRuntime }
  <|> return LinkStatic

pcallConv
  =   do{ lexeme LexCCALL; return CallC }
  <|> do{ lexeme LexSTDCALL; return CallStd }
  <|> do{ lexeme LexINSTRCALL; return CallInstr }
  <|> return CallC

pExternName modname
  =   do{ lexeme LexDECORATE
        ; name <- lexString
        ; return (modname,Decorate name)
        }
  <|> do{ lexeme LexORDINAL
        ; ord  <- lexInt
        ; return (modname,Ordinal (fromIntegral ord))
        }
  <|> do{ name <- lexString
        ; return (modname,Plain name)
        }
  <|> return ("",Plain modname)


----------------------------------------------------------------
-- types
----------------------------------------------------------------
ptypeDecl
  = do{ lexeme LexCOLCOL
      ; ptypeNormal <|> ptypeString
      }

ptypeNormal
  = do{ tp <- ptype
      ; return (tp,arityFromType tp)
      }


ptype :: TokenParser Type
ptype
  = ptypeFun

ptypeFun
  = chainr1 ptypeAp pFun
  where
    pFun  = do{ lexeme LexRARROW; return TFun }

ptypeAp
  = do{ atoms <- many1 ptypeAtom
      ; return (foldl1 TAp atoms)
      }

ptypeAtom
  = do{ id <- typeid
      ; ptypeStrict (TCon id) 
      }
  <|>
    do{ id <- typevarid
      ; ptypeStrict (TVar id)
      }
  <|> listType
  <|> parenType
  <?> "atomic type"

ptypeStrict tp
  = do{ lexeme LexEXCL
      ; return (TStrict tp)
      }
  <|> return tp
      

parenType
  = do{ lexeme LexLPAREN
      ; tps <- sepBy ptype (lexeme LexCOMMA)
      ; lexeme LexRPAREN
      ; case tps of
          []    -> do{ id <- identifier (return "()"); return (TCon id) } -- (setSortId SortType id))
          [tp]  -> return tp
          other -> return
                (foldl
                    TAp
                    (TCon (idFromString
                            (  "("
                            ++ replicate (length tps - 1) ','
                            ++ ")"
                            )))
                    tps
                )
      }

listType
  = do{ lexeme LexLBRACKET
      ; do{ tp <- ptype
          ; lexeme LexRBRACKET
          ; id <- identifier (return "[]")
          ; return (TAp (TCon id {- (setSortId SortType id) -}) tp)
          }
      <|>
        do{ lexeme LexRBRACKET
          ; id <- identifier (return "[]")
          ; return (TCon id {-(setSortId SortType id)-})
          }
      }

ptypeString
  = do{ s <- lexString
      ; return (TString s, length s-1)
      }


pKind :: TokenParser Kind
pKind
  = pkindFun

pkindFun
  = chainr1 pkindAtom pFun
  where
    pFun  = do{ lexeme LexRARROW; return KFun }

pkindAtom
  =   pStar
  <|> parens pKind

pStar
  = do{ op <- lexOp
      ; if (op /= "*")
         then fail ("invalid kind: " ++ show op)
         else return KStar
      }

----------------------------------------------------------------
-- helpers
----------------------------------------------------------------
semiBraces p  = braces (semiList p)
commaParens p = parens (sepBy p (lexeme LexCOMMA))

braces p      = between (lexeme LexLBRACE) (lexeme LexRBRACE) p
parens p      = between (lexeme LexLPAREN) (lexeme LexRPAREN) p

-- terminated or seperated
semiList1 p
    = do{ x <- p
        ; do{ lexeme LexSEMI
            ; xs <- semiList p
            ; return (x:xs)
            }
          <|> return [x]
        }

semiList p
    = semiList1 p <|> return []

semiTerm p
    = many (do{ x <- p; lexeme LexSEMI; return x })

----------------------------------------------------------------
-- Lexeme parsers
----------------------------------------------------------------
customid
  =   varid
  <|> conid
  <|> parens (opid <|> conopid)
  <|> do{ s <- lexString; return (idFromString s) }
  <?> "custom identifier"

variable
  = varid <|> parens opid

opid
  = identifier lexOp
  <?> "operator"

varid
  =   identifier lexId
  <?> "variable"

qualifiedVar 
  = do{ (mod,name) <- lexQualifiedId
      ; return (idFromString mod, idFromString name)
      }

bindid :: TokenParser Id
bindid
  = do{ id <- varid
      ; do{ lexeme LexEXCL
          ; return id {- (setSortId SortStrict id) -}
          }
        <|> return id
      }

constructor
  = conid <|> parens conopid

conopid
  =   identifier lexConOp
  <|> do{ lexeme LexCOLON; return (idFromString ":") }
  <?> "constructor operator"

conid
  =   identifier lexCon
  <?> "constructor"

qualifiedCon
  = do{ (mod,name) <- lexQualifiedCon
      ; return (idFromString mod, idFromString name)
      }


typeid
  = do{ id <- identifier lexCon
      ; return id -- (setSortId SortType id)
      }
  <?> "type"

typevarid
  = do{ id <- identifier lexId
      ; return id -- (setSortId SortType id)
      }

identifier p
  = do{ s <- p
      ; return (idFromString s)
      }

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------
lexeme :: Lexeme -> TokenParser Lexeme
lexeme lex
  = satisfy (\lex' -> if (lex == lex') then Just lex else Nothing) <?> show lex


lexChar :: TokenParser Char
lexChar
  = satisfy (\lex -> case lex of { LexChar c -> Just c; other -> Nothing })

lexString :: TokenParser String
lexString
  = satisfy (\lex -> case lex of { LexString s -> Just s; other -> Nothing })

lexDouble :: TokenParser Double
lexDouble
  = satisfy (\lex -> case lex of { LexFloat d -> Just d; other -> Nothing })

lexInt :: TokenParser Integer
lexInt
  = satisfy (\lex -> case lex of { LexInt i -> Just i; other -> Nothing })

lexId :: TokenParser String
lexId
  = satisfy (\lex -> case lex of { LexId s -> Just s; other -> Nothing })

lexQualifiedId
  = satisfy (\lex -> case lex of { LexQualId mod id -> Just (mod,id); other -> Nothing })

lexOp :: TokenParser String
lexOp
  = satisfy (\lex -> case lex of { LexOp s -> Just s; other -> Nothing })

lexCon :: TokenParser String
lexCon
  = satisfy (\lex -> case lex of { LexCon s -> Just s; other -> Nothing })

lexQualifiedCon
  = satisfy (\lex -> case lex of { LexQualCon mod id -> Just (mod,id); other -> Nothing })

lexConOp :: TokenParser String
lexConOp
  = satisfy (\lex -> case lex of { LexConOp s -> Just s; other -> Nothing })

satisfy :: (Lexeme -> Maybe a) -> TokenParser a
satisfy pred
  = tokenPrim showtok nextpos (\(pos,lex) -> pred lex)
  where
    showtok (pos,lex)   = show lex
    nextpos pos _ (((line,col),lex):_)
       = setSourceColumn (setSourceLine pos line) col
    nextpos pos _ []
       = pos
