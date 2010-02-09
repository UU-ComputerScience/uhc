{-| Module      :  UHA_Range
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Syntax.UHA_Range where

import Lvm.Common.Id(Id, stringFromId)
import Maybe(isJust)
import List(sort, partition)
import Helium.Syntax.UHA
import Helium.Utils.Utils(internalError)

--------------------------------------------------------------
-- instances for Range and Position

instance Show Range where
    show = showRange
    
instance Eq Range where
    Range_Range start1 stop1 == Range_Range start2 stop2 =
        start1 == start2 && stop1 == stop2

instance Ord Range where
    Range_Range start1 stop1 <= Range_Range start2 stop2 =
        (start1 < start2)
        ||
        (start1 == start2 && stop1 <= stop2)

instance Eq Position where
    Position_Position m1 l1 c1 == Position_Position m2 l2 c2 =
        m1 == m2 && l1 == l2 && c1 == c2
    Position_Unknown           == Position_Unknown        = True
    Position_Unknown           == Position_Position _ _ _ = False
    Position_Position _ _ _    == Position_Unknown        = False

instance Ord Position where
    Position_Position _ l1 c1 <= Position_Position _ l2 c2 =
        (l1 < l2)
        ||
        (l1 == l2 && c1 <= c2)
    Position_Unknown        <= Position_Unknown        = True
    Position_Unknown        <= Position_Position _ _ _ = True
    Position_Position _ _ _ <= Position_Unknown        = False   

--------------------------------------------------------------

getNameRange :: Name -> Range -- !!!Name
getNameRange (Name_Identifier r _ _) = r
getNameRange (Name_Operator   r _ _) = r
getNameRange (Name_Special    r _ _) = r

setNameRange :: Name -> Range -> Name -- !!!Name
setNameRange (Name_Identifier _ s e) r = Name_Identifier r s e 
setNameRange (Name_Operator   _ s e) r = Name_Operator   r s e
setNameRange (Name_Special    _ s e) r = Name_Special    r s e

rangeFromImportDeclaration :: ImportDeclaration -> Range
rangeFromImportDeclaration importDecl =
    case importDecl of
        ImportDeclaration_Import r _ _ _ _ -> r
        ImportDeclaration_Empty r -> r
    
mergeRanges :: Range -> Range -> Range
mergeRanges
    (Range_Range
        (Position_Position startF1 startL1 startC1)
        (Position_Position stopF1  stopL1  stopC1 )
    )
    (Range_Range
        (Position_Position startF2 startL2 startC2)
        (Position_Position stopF2  stopL2  stopC2 )
    )
    | startF1 == stopF1 && startF2 == stopF2 && startF1 == startF2 =
        let
            (startL, startC, stopL, stopC) =
                if startL1 < startL2 || (startL1 == startL2 && startC1 <= startC2) then
                    (startL1, startC1, stopL2, stopC2)
                else
                    (startL2, startC2, stopL1, stopC1)
        in
            Range_Range
                (Position_Position startF1 startL startC)
                (Position_Position startF1 stopL  stopC )
mergeRanges _ _ = Range_Range Position_Unknown Position_Unknown

-- In UHA there is no room for the position of built-in constructs
noRange :: Range
noRange = Range_Range Position_Unknown Position_Unknown

emptyRange :: Range
emptyRange = let p = Position_Position "" 1 1 in Range_Range p p
-----------------------------------------------------
-- Misuse the second position of the range to
-- store where the name was imported from.
makeImportRange :: Id -> Id -> Range
makeImportRange importedInId importedFromId =
    Range_Range
        (Position_Position (stringFromId importedInId  ) 0 0)
        (Position_Position (stringFromId importedFromId) 0 0)

isImportRange :: Range -> Bool
isImportRange = isJust . modulesFromImportRange

isImportName :: Name -> Bool
isImportName = isImportRange.getNameRange

modulesFromImportRange :: Range -> Maybe (String, String)
modulesFromImportRange
    (Range_Range
        (Position_Position importedIn   0 0)
        (Position_Position importedFrom 0 0)
    ) =
        Just (importedIn, importedFrom)
modulesFromImportRange _ = Nothing
-- End of misuse functions
-----------------------------------------------------
getRangeStart :: Range -> Position
getRangeStart (Range_Range start _) = start

getRangeEnd :: Range -> Position
getRangeEnd (Range_Range _ end) = end

getStatementRange :: Statement -> Range
getStatementRange s = 
    case s of
        Statement_Expression r _ -> r
        Statement_Let r _ -> r
        Statement_Generator r _ _ -> r
        Statement_Empty r -> r
        
getPatRange :: Pattern -> Range
getPatRange (Pattern_As r _ _) = r
getPatRange (Pattern_Constructor r _ _) = r
getPatRange (Pattern_InfixConstructor r _ _ _) = r
getPatRange (Pattern_Irrefutable r _) = r
getPatRange (Pattern_List r _) = r
getPatRange (Pattern_Literal r _) = r
getPatRange (Pattern_Negate r _) = r
getPatRange (Pattern_NegateFloat r _) = r
getPatRange (Pattern_Parenthesized r _) = r
getPatRange (Pattern_Record r _ _) = r
getPatRange (Pattern_Successor r _ _) = r
getPatRange (Pattern_Tuple r _) = r
getPatRange (Pattern_Variable r _) = r
getPatRange (Pattern_Wildcard r) = r
getPatRange _ = internalError "UHA_Range" "getPatRange" "unknown pattern"

getExprRange :: Expression -> Range
getExprRange (Expression_Literal            r _    ) = r
getExprRange (Expression_Variable           r _    ) = r
getExprRange (Expression_Constructor        r _    ) = r
getExprRange (Expression_Parenthesized      r _    ) = r
getExprRange (Expression_NormalApplication  r _ _  ) = r
getExprRange (Expression_InfixApplication   r _ _ _) = r
getExprRange (Expression_If                 r _ _ _) = r
getExprRange (Expression_Lambda             r _ _  ) = r
getExprRange (Expression_Case               r _ _  ) = r
getExprRange (Expression_Let                r _ _  ) = r
getExprRange (Expression_Do                 r _    ) = r
getExprRange (Expression_List               r _    ) = r
getExprRange (Expression_Tuple              r _    ) = r
getExprRange (Expression_Comprehension      r _ _  ) = r
getExprRange (Expression_Typed              r _ _  ) = r
getExprRange (Expression_RecordConstruction r _ _  ) = r
getExprRange (Expression_RecordUpdate       r _ _  ) = r
getExprRange (Expression_Enum               r _ _ _) = r
getExprRange (Expression_Negate             r _    ) = r
getExprRange (Expression_NegateFloat        r _    ) = r

getRHSRange :: RightHandSide -> Range
getRHSRange (RightHandSide_Expression r _ _) = r
getRHSRange (RightHandSide_Guarded    r _ _) = r

getMaybeExprRange :: MaybeExpression -> Range
getMaybeExprRange (MaybeExpression_Just e)  = getExprRange e
getMaybeExprRange (MaybeExpression_Nothing) = noRange

getLitRange :: Literal -> Range
getLitRange (Literal_Char   r _) = r
getLitRange (Literal_Float  r _) = r
getLitRange (Literal_Int    r _) = r
getLitRange (Literal_String r _) = r

getQualifierRange :: Qualifier -> Range
getQualifierRange qualifier = case qualifier of
   Qualifier_Guard r _       -> r
   Qualifier_Let r _         -> r
   Qualifier_Generator r _ _ -> r
   Qualifier_Empty r         -> r

getAlternativeRange :: Alternative -> Range
getAlternativeRange alternative = case alternative of
   Alternative_Alternative r _ _ -> r
   Alternative_Empty r           -> r

getLHSRange :: LeftHandSide -> Range
getLHSRange lhs = case lhs of
   LeftHandSide_Function r _ _      -> r
   LeftHandSide_Infix r _ _ _       -> r
   LeftHandSide_Parenthesized r _ _ -> r

getFBRange :: FunctionBinding -> Range
getFBRange fb = case fb of
   FunctionBinding_FunctionBinding r _ _ -> r
        
getDeclarationRange :: Declaration -> Range
getDeclarationRange decl = case decl of
   Declaration_Type r _ _           -> r
   Declaration_Data  r _ _ _ _      -> r
   Declaration_Newtype r _ _ _ _    -> r
   Declaration_Class r _ _ _        -> r
   Declaration_Instance r _ _ _ _   -> r
   Declaration_Default r _          -> r
   Declaration_FunctionBindings r _ -> r
   Declaration_PatternBinding r _ _ -> r
   Declaration_TypeSignature r _ _  -> r
   Declaration_Fixity r _ _ _       -> r
   Declaration_Empty r              -> r

getBodyRange :: Body -> Range
getBodyRange body = case body of
   Body_Body r _ _ -> r

getTypeRange :: Type -> Range
getTypeRange tp = case tp of
   Type_Application r _ _ _ -> r
   Type_Variable r _        -> r
   Type_Constructor r _     -> r
   Type_Qualified r _ _     -> r
   Type_Forall r _ _        -> r
   Type_Exists r _ _        -> r 
   Type_Parenthesized r _   -> r
   
getGuardedExprRange :: GuardedExpression -> Range
getGuardedExprRange gexpr = case gexpr of
   GuardedExpression_GuardedExpression r _ _ -> r

getRHSRangeSpecial :: RightHandSide -> Range
getRHSRangeSpecial rhs = case rhs of 
   RightHandSide_Expression _ expr _ -> getExprRange expr
   RightHandSide_Guarded r _ _       -> r
   
showRanges :: [Range] -> String
showRanges (range:ranges) = show range ++ concatMap ((", " ++) . show) ranges
showRanges [] = ""

-- !!!! In the special case that the range corresponds to the import range,
-- the module name of the second position should be printed
showRange :: Range -> String
showRange range@(Range_Range startPos endPos)
    | isImportRange range =
        moduleFromPosition endPos
    | otherwise =
        showPosition startPos

showFullRange :: Range -> String
showFullRange (Range_Range startPos endPos) =
   showPosition startPos ++ "-" ++ showPosition endPos
	
showPosition :: Position -> String
showPosition (Position_Position _ line column) =
    "(" ++ show line ++ "," ++ show column ++ ")"
showPosition _ =
    "<unknown position>"

sortRanges :: [Range] -> [Range]
sortRanges ranges = let (xs,ys) = partition isImportRange ranges
                    in sort ys ++ xs
                                        
moduleFromPosition :: Position -> String
moduleFromPosition pos =
    case pos of
        Position_Position moduleName _ _ -> 
            moduleName
        _ -> 
            internalError "UHA_Range" "moduleFromPosition" "unknown position"
