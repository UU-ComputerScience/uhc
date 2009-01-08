-- |
-- Language.Cil Parser to CIL abstract syntax.
-- Uses the Parser combinators from the Haskell Utrecht Tools.
--

module Language.Cil.Parser (
    parse, pAss
  ) where

import Data.List (intercalate)

import UU.Scanner () -- Instances
import UU.Scanner.Token (Token)
import UU.Scanner.TokenParser
import UU.Parsing hiding (parse)
import qualified UU.Parsing as P (parse)

import Language.Cil.Syntax

type TParser a = Parser Token a

parse :: TParser Assembly -> [Token] -> Assembly
parse p ts = r
  where
    (Pair r _) = evalSteps $ P.parse p ts

pPeriod :: TParser String
pPeriod = pSpec '.'

pId :: TParser Id
pId = pConid <|> pVarid

pDottedName :: TParser DottedName
pDottedName = intercalate "|" <$> pListSep_ng pPeriod pId

pAss :: TParser Assembly
pAss = Assembly
         <$> pList_ng pAssRef
         <*  pPeriod <* pKey "assembly" <*> pDottedName <* pOCurly <* pCCurly
         <*> pList pTypeDef

pAssRef :: TParser AssemblyRef
pAssRef = AssemblyRef
            <$  pPeriod <* pKey "assembly" <* pKey "extern" <*> pDottedName
            -- <*  pCurly (pList pAssemblyRefDecl)
            <*  pOCurly <* pCCurly

pTypeDef :: TParser TypeDef
pTypeDef = (\v n ds -> Class v n Nothing [] ds)
             <$  pPeriod <* pKey "class" <*> pVisibility <*> pDottedName
             <*> pCurly (pList pClassDecl)

pVisibility :: TParser Visibility
pVisibility =  AssemblyVisible <$ pKey "assembly"
           <|> Family <$ pKey "family"
           <|> FamilyAndAssembly <$ pKey "famandassem"
           <|> FamilyOrAssembly <$ pKey "famorassem"
           <|> Public <$ pKey "public"
           <|> Private <$ pKey "private"

pClassDecl :: TParser ClassDecl
pClassDecl =  FieldDef <$> pFieldDef
          <|> MethodDef <$> pMethodDef

pFieldDef :: TParser FieldDef
pFieldDef = Field <$  pPeriod <* pKey "field" <*> pAssociation <*> pVisibility <*> pPrimitiveType
                  <*> pDottedName

pAssociation :: TParser Association
pAssociation =  Static <$  pKey "static"
            <|> pSucceed Instance

pPrimitiveType :: TParser PrimitiveType
pPrimitiveType =  Void <$  pKey "void"
              <|> Bool <$  pKey "bool"
              <|> Char <$  pKey "char"
              <|> Byte <$  pKey "uint8"
              <|> Byte <$  pKey "unsigned" <* pKey "int8"
              <|> Int32 <$ pKey "int32"
              <|> Int64 <$ pKey "int64"
              <|> String <$ pKey "string"
              <|> Object <$ pKey "object"

pMethodDef :: TParser MethodDef
pMethodDef = Method <$  pPeriod <* pKey "method" <*> pAssociation <*> pVisibility
                    <*  pKey "hidebysig" <*> pPrimitiveType <*> pDottedName
                    <*  pOParen <*> pSucceed [] <* pCParen <* pKey "cil" <* pKey "managed"
                    <*> pCurly (pList pMethodDecl)

pMethodDecl :: TParser MethodDecl
pMethodDecl =  pComment
           <|> Directive <$> pDirective
           <|> Instr     <$> pInstr

pComment :: TParser MethodDecl
pComment = Comment <$ pSpec '/' <* pSpec '/' <*> pSucceed "Hello"

pDirective :: TParser Directive
pDirective =  EntryPoint <$ pPeriod <* pKey "entrypoint"

pInstr :: TParser Instr
pInstr = OpCode <$> pOpCode

pOpCode :: TParser OpCode
pOpCode =  Add <$ pKey "add"
       <|> And <$ pKey "and"

