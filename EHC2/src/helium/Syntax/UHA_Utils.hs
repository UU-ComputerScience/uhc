{-| Module      :  UHA_Utils
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

    Utilities to extract data from the syntax tree
-}

module Helium.Syntax.UHA_Utils where

import Lvm.Common.Id(Id, idFromString, stringFromId)
import Char
import Top.Types(isTupleConstructor)
import Helium.Syntax.UHA_Range(noRange, getNameRange)
import Helium.Syntax.UHA(Name(..), ImportDeclaration(..), Pattern(..))
import Helium.Utils.Utils(internalError)

instance Eq Name where
   n1 == n2 = getNameName n1 == getNameName n2

instance Ord Name where
   n1 <= n2 = getNameName n1 <= getNameName n2

instance Show Name where 
    show = getNameName  

--------------------------------------------------------------
-- NameWithRange 

newtype NameWithRange = NameWithRange { nameWithRangeToName :: Name }

instance Show NameWithRange where
   show (NameWithRange name) = 
      show name ++ " at " ++ show (getNameRange name)
   
instance Eq  NameWithRange where
   NameWithRange name1 == NameWithRange name2 = 
      (name1, getNameRange name1) == (name2, getNameRange name2)
      
instance Ord NameWithRange where
   NameWithRange name1 <= NameWithRange name2 = 
      (name1, getNameRange name1) <= (name2, getNameRange name2)
      
--------------------------------------------------------------

getNameName :: Name -> String -- !!!Name
getNameName (Name_Identifier _ _ name) = name
getNameName (Name_Operator   _ _ name) = name
getNameName (Name_Special    _ _ name) = name

idFromName :: Name -> Id -- !!!Name
idFromName (Name_Special _ _ s) = idFromString s
idFromName (Name_Identifier _ _ s) = idFromString s
idFromName (Name_Operator _ _ s) = idFromString s

nameFromId :: Id -> Name
nameFromId = nameFromString . stringFromId

nameFromString :: String -> Name -- !!!Name
nameFromString str@(first:_) 
    | isAlpha first = Name_Identifier noRange [] str 
    | str == "[]" || isTupleConstructor str || str == "->" 
                    = Name_Special noRange [] str
    | otherwise     = Name_Operator noRange [] str
nameFromString _ = internalError "UHA_Utils" "nameFromString" "empty string"

isOperatorName :: Name -> Bool -- !!!Name
isOperatorName (Name_Operator _ _ _) = True
isOperatorName _ = False

isConstructor :: Name -> Bool -- !!!Name
isConstructor name = 
    case name of
        Name_Operator   _ _ (':':_)   -> True
        Name_Identifier _ _ (first:_) -> isUpper first
        Name_Special    _ _ "()"      -> True
        Name_Special    _ _ "[]"      -> True
        _                             -> False
        
isIdentifierName :: Name -> Bool -- !!!Name
isIdentifierName (Name_Identifier _ _ _) = True
isIdentifierName _ = False

showNameAsOperator :: Name -> String
showNameAsOperator name
   | isIdentifierName name = "`"++show name++"`"
   | otherwise             = show name

showNameAsVariable :: Name -> String
showNameAsVariable name
   | isOperatorName name = "("++show name++")"
   | otherwise           = show name

stringFromImportDeclaration :: ImportDeclaration -> String
stringFromImportDeclaration importDecl =
    case importDecl of
        ImportDeclaration_Import _ _ n _ _ -> getNameName n
        ImportDeclaration_Empty _ -> 
            internalError "UHA_Utils" "stringFromImportDeclaration" "empty import declaration"

-- TODO: daan
intUnaryMinusName, floatUnaryMinusName, enumFromName, enumFromToName, enumFromThenName, enumFromThenToName :: Name
intUnaryMinusName   = nameFromString "$negate"
floatUnaryMinusName = nameFromString "$floatUnaryMinus"
enumFromName        = nameFromString "$enumFrom"
enumFromToName      = nameFromString "$enumFromTo"
enumFromThenName    = nameFromString "$enumFromThen"
enumFromThenToName  = nameFromString "$enumFromThenTo"

patternVars :: Pattern -> [Name]
patternVars p = case p of
    Pattern_Literal _ _                 -> []
    Pattern_Variable _ n                -> [n]
    Pattern_Constructor _ _ ps          -> concatMap patternVars ps
    Pattern_Parenthesized _ p           -> patternVars p
    Pattern_InfixConstructor _ p1 _ p2  -> concatMap patternVars [p1, p2]
    Pattern_List _ ps                   -> concatMap patternVars  ps
    Pattern_Tuple _ ps                  -> concatMap patternVars  ps
    Pattern_Negate _ _                  -> []
    Pattern_As _ n p                    -> n : patternVars p
    Pattern_Wildcard _                  -> []
    Pattern_Irrefutable _ p             -> patternVars p
    Pattern_NegateFloat _ _             -> []
    _ -> internalError "UHA_Utils" "patternVars" "unsupported kind of pattern"
    
