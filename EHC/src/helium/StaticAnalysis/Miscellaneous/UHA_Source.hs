{-| Module      :  UHA_Source
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    The UHA_Source data type is the union of several data types from the abstract
	syntax (UHA),  including expressions and patterns.
-}

module Helium.StaticAnalysis.Miscellaneous.UHA_Source where

import Helium.Utils.OneLiner
import Helium.Syntax.UHA_Range
import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import qualified Helium.Syntax.UHA_OneLine as PP

data UHA_Source =
     UHA_Expr   Expression              
   | UHA_Pat    Pattern
   | UHA_Stat   Statement
   | UHA_Qual   Qualifier
   | UHA_FB     FunctionBinding
   | UHA_RHS    RightHandSide
   | UHA_Decl   Declaration
   | UHA_Decls  Declarations
   | UHA_Def    Name
         
instance Show UHA_Source where
   show = showOneLine 80 . oneLinerSource
	 
rangeOfSource :: UHA_Source -> Range
rangeOfSource source =
   case source of
      UHA_Expr  expr  -> getExprRange expr
      UHA_Pat   pat   -> getPatRange pat
      UHA_Stat  stat  -> getStatementRange stat
      UHA_Qual  qual  -> getQualifierRange qual
      UHA_FB    fb    -> getFBRange fb
      UHA_RHS   rhs   -> getRHSRange rhs
      UHA_Decl  decl  -> getDeclarationRange decl
      UHA_Decls decls -> if null decls then noRange else foldr1 mergeRanges (map getDeclarationRange decls)
      UHA_Def   name  -> getNameRange name


oneLinerSource :: UHA_Source -> OneLineTree
oneLinerSource source = 
   case source of
      UHA_Expr  expr  -> id (PP.sem_Expression expr)
      UHA_Pat   pat   -> id (PP.sem_Pattern pat)
      UHA_Stat  stat  -> id (PP.sem_Statement stat)
      UHA_Qual  qual  -> id (PP.sem_Qualifier qual)
      UHA_FB    fb    -> id (PP.sem_FunctionBinding fb)
      UHA_RHS   rhs   -> id (PP.sem_RightHandSide rhs) ""
      UHA_Decl  decl  -> id (PP.sem_Declaration decl)
      UHA_Decls decls -> PP.encloseSep "{" "; " "}" (id (PP.sem_Declarations decls))
      UHA_Def   name  -> OneLineText (show name)

descriptionOfSource :: UHA_Source -> String
descriptionOfSource source = 
   case source of
      UHA_Expr  _ -> "expression"
      UHA_Pat   _ -> "pattern"
      UHA_Stat  _ -> "statement"
      UHA_Qual  _ -> "qualifier"
      UHA_FB    _ -> "function binding"
      UHA_RHS   _ -> "right-hand side"
      UHA_Decl  _ -> "declaration"
      UHA_Decls _ -> "declarations"
      UHA_Def   _ -> "definition"

nameToUHA_Expr :: Name -> UHA_Source
nameToUHA_Expr name = UHA_Expr (Expression_Variable (getNameRange name) name)

nameToUHA_Pat :: Name -> UHA_Source
nameToUHA_Pat name = UHA_Pat (Pattern_Variable (getNameRange name) name)

nameToUHA_Def :: Name -> UHA_Source
nameToUHA_Def = UHA_Def

convertSources :: (UHA_Source, Maybe UHA_Source) -> [(String, UHA_Source)]
convertSources (source, maybeSource) = 
   (descriptionOfSource source, source) : maybe [] (\s -> [(f s, s)]) maybeSource
  where
    f (UHA_Expr (Expression_Variable _ name))
       | isConstructor  name = "constructor"
       | isOperatorName name = "operator"
    f (UHA_Expr (Expression_Constructor _ name)) 
       | isConstructor name  =  "constructor"
    f (UHA_Pat (Pattern_Variable _ name))
       | isConstructor name  = "constructor"
    f _                      = "term"      
