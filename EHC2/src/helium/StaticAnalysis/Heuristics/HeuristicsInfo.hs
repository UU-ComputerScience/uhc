{-| Module      :  HeuristicsInfo
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Contains instance declarations. he type graph heuristics can be deployed 
	using the additional information that is stored by the Helium compiler for 
	each type constraint
-}

module Helium.StaticAnalysis.Heuristics.HeuristicsInfo where

import Helium.StaticAnalysis.Miscellaneous.ConstraintInfo
import Helium.StaticAnalysis.Heuristics.RepairHeuristics
import Helium.StaticAnalysis.Heuristics.TieBreakerHeuristics
import Helium.StaticAnalysis.Heuristics.OnlyResultHeuristics
import Helium.StaticAnalysis.Heuristics.UnifierHeuristics
import Helium.StaticAnalysis.Miscellaneous.DoublyLinkedTree
import Helium.Utils.OneLiner
import Helium.StaticAnalysis.Miscellaneous.UHA_Source
import Helium.Syntax.UHA
import Helium.StaticAnalysis.Messages.Messages
import Helium.StaticAnalysis.Messages.HeliumMessages ()
import Helium.StaticAnalysis.Messages.TypeErrors
import Helium.Utils.Utils (internalError)
import Top.Types
import Top.Implementation.TypeGraph.Heuristic
import Data.Maybe
import Data.Char
import qualified Data.Map as M

instance HasTrustFactor ConstraintInfo where
   trustFactor cinfo =
      let ntFactor = case (self . attribute . localInfo) cinfo of
                        UHA_Pat  _ -> 3.0
                        UHA_Decl _ -> 3.0
                        UHA_FB   _ -> 3.0
                        _          -> 1.0
      in product (ntFactor : [ factor | HasTrustFactor factor <- properties cinfo ])

instance HasDirection ConstraintInfo where
   isTopDown cinfo = or [ True | FolkloreConstraint <- properties cinfo ]

instance MaybeImported ConstraintInfo where
   maybeImportedName cinfo = 
      case [ name | IsImported name <- properties cinfo ] of
         []  -> Nothing
         n:_ -> Just (show n)

instance HasTwoTypes ConstraintInfo where
   getTwoTypes = typepair

instance MaybeLiteral ConstraintInfo where
   maybeLiteral cinfo = 
      let literalType x = 
             case x of 
                Literal_Int    _ _ -> "Int"
                Literal_Char   _ _ -> "Char"
                Literal_String _ _ -> "String"
                Literal_Float  _ _ -> "Float"
      in case (self . attribute . localInfo) cinfo of
            UHA_Expr (Expression_Literal _ literal ) -> Just (literalType literal)
            UHA_Pat  (Pattern_Literal    _ literal ) -> Just (literalType literal)
            _                                        -> Nothing

instance IsPattern ConstraintInfo where
   isPattern cinfo = 
      case (self . attribute . localInfo) cinfo of 
         UHA_Pat _ -> True
         _         -> False

instance MaybeApplication ConstraintInfo where
   maybeNumberOfArguments = 
      fmap (length . snd) . maybeApplicationEdge
      
   maybeApplicationEdge cinfo = 
      let list = [ (b, zip (map self infoTrees) (map fromJust tps))
                 | ApplicationEdge b infoTrees <- properties cinfo
                 , let tps = map assignedType infoTrees
                 , all isJust tps
                 ]
      in case list of 
            []      -> Nothing
            tuple:_ -> Just tuple

instance MaybeUnaryMinus ConstraintInfo where
   maybeUnaryMinus cinfo = 
      case (self . attribute . localInfo) cinfo of
         UHA_Expr (Expression_InfixApplication _
              (MaybeExpression_Just _)
              (Expression_Variable _ name)
              (MaybeExpression_Just (Expression_Literal _ literal)))
            | show name == "-" ->
                 case literal of
                    Literal_Int _ s -> Just (Left (read s))
                    _               -> Nothing
            | show name == "-." ->
                 case literal of
                    Literal_Float _ s -> Just (Right (read s))
                    _                 -> Nothing 
         _  -> Nothing
   
instance MaybeNegation ConstraintInfo where
   maybeNegation cinfo = 
      case (self . attribute . localInfo) cinfo of
         UHA_Expr (Expression_Negate      _ _) -> Just True
         UHA_Expr (Expression_NegateFloat _ _) -> Just False
         _                                     -> Nothing

instance IsExprVariable ConstraintInfo where -- misleading name?
   isExprVariable cinfo =
      case (self . attribute . localInfo) cinfo of
         UHA_Expr (Expression_Variable _ _) -> 
            not $ null [ () | InstantiatedTypeScheme _ <- properties cinfo ]
         _ -> False
      
   isEmptyInfixApplication cinfo =
      case (self . attribute . localInfo) cinfo of
         UHA_Expr (Expression_InfixApplication _ MaybeExpression_Nothing _ MaybeExpression_Nothing) -> True
         _  -> False

instance IsFunctionBinding ConstraintInfo where
   isExplicitlyTyped cinfo = 
      or [ True | ExplicitTypedBinding <- properties cinfo ]
      
   maybeFunctionBinding cinfo = 
      case [ t | FuntionBindingEdge t <- properties cinfo ] of
         []  -> Nothing
         t:_ -> Just t 
   
instance IsTupleEdge ConstraintInfo where
   isTupleEdge cinfo = 
     case (self . attribute . localInfo) cinfo of
         UHA_Expr (Expression_Tuple _ _) -> True
         UHA_Pat  (Pattern_Tuple _ _)    -> True
         _                               -> False
      
instance WithHints ConstraintInfo where
  addHint descr str = addProperty (WithHint (descr, MessageString str))
  typeErrorForTerm  = specialApplicationTypeError

instance IsUnifier ConstraintInfo where
   typeErrorForUnifier = specialUnifierTypeError
   isUnifier cinfo = 
      case [ (u, t) | Unifier u t <- properties cinfo ] of
         []  -> Nothing
         t:_ -> Just t

makeUnifier :: Name -> String -> M.Map Name Tp -> InfoTree -> Property
makeUnifier name location environment infoTree = 
   let unifier = maybe (-1) (head . ftv) (M.lookup name environment)
       tuple   = ("variable of "++location, attribute (findVariableInPat name infoTree), "variable")
   in Unifier unifier tuple
 
specialApplicationTypeError :: (Bool,Bool) -> Int -> OneLineTree -> (Tp,Tp) -> Range -> ConstraintInfo -> ConstraintInfo
specialApplicationTypeError (isInfixApplication,isPatternApplication) argumentNumber termOneLiner (t1, t2) range cinfo =
   let typeError = TypeError [range] [oneLiner] table []
       oneLiner  = MessageOneLiner (MessageString ("Type error in " ++ location cinfo))
       table     = [ description1     <:> MessageOneLineTree (oneLinerSource source1)
                   , description2     <:> MessageOneLineTree (oneLinerSource source2)
                   , "type"           >:> MessageType functionType
                   , description3     <:> MessageOneLineTree termOneLiner
                   , "type"           >:> MessageType (toTpScheme t1)
                   , "does not match" >:> MessageType (toTpScheme t2)
                   ]
       (description1, source1, source2) =
          case convertSources (sources cinfo) of
             [(d1,s1), (_, s2)] -> (d1, s1, s2)
             _ -> internalError "ConstraintInfo" "specialApplicationTypeError" "expected two elements in list"
       description2 
          | isPatternApplication   = "constructor"
          | not isInfixApplication = "function"
          | otherwise =  
               case show (MessageOneLineTree (oneLinerSource source2)) of
                  c:_ | isLower c -> "function"
                      | isUpper c -> "constructor"
                  _               -> "operator"
       functionType = toTpScheme (fst (typepair cinfo))
       description3
          | isInfixApplication = if argumentNumber == 0 then "left operand" else "right operand"
          | otherwise          = ordinal False (argumentNumber + 1) ++ " argument"
   in setTypeError typeError (setTypePair (t1, t2) cinfo)
   
specialUnifierTypeError ::  (Tp, Tp) -> (ConstraintInfo, ConstraintInfo) -> ConstraintInfo
specialUnifierTypeError (t1, t2) (info1, info2) =
   let typeError = TypeError [range] [oneLiner] table hints
       range     = rangeOfSource source 
       oneLiner  = MessageOneLiner (MessageString ("Type error in " ++ loc1))
       table     = [ description <:> maybeAddLocation source
                   , descr1      <:> source1
                   , "type"      >:> MessageType (toTpScheme t1)
                   , descr2      <:> source2
                   , "type"      >:> MessageType (toTpScheme t2)
                   ]
       description = descriptionOfSource source
       (loc1, localInfo, descr1) = snd (fromJust (isUnifier info1))
       (_   ,_         , descr2) = snd (fromJust (isUnifier info2))
       source = self localInfo
       (source1, source2) = 
          let f (src, msrc) = maybeAddLocation (maybe src id msrc)
          in (f (sources info1), f (sources info2))
       hints = [] -- [("because", MessageString "these two types cannot be unified")]
   in setTypeError typeError (setTypePair (t1,t2 ) info1)

skip_UHA_FB_RHS :: InfoTree -> InfoTree
skip_UHA_FB_RHS tree = 
   case self (attribute tree) of
      UHA_FB _  -> maybe tree skip_UHA_FB_RHS (parent tree) 
      UHA_RHS _ -> maybe tree skip_UHA_FB_RHS (parent tree)
      _        -> tree
   
findVariableInPat :: Name -> InfoTree -> InfoTree
findVariableInPat name tree = 
   case children tree of
      [] -> tree
      cs -> let p x = case self (attribute x) of
                         UHA_Pat pat -> hasVariable name pat
                         _ -> False
            in case filter p cs of
                  [] -> tree
                  child:_ -> findVariableInPat name child

hasVariable :: Name -> Pattern -> Bool
hasVariable name pattern =
   case pattern of
      Pattern_Variable _ n -> name == n
      Pattern_As _ n pat   -> name == n || hasVariable name pat
      Pattern_Parenthesized _ pat -> hasVariable name pat
      Pattern_InfixConstructor _ pat1 _ pat2 -> hasVariable name pat1 || hasVariable name pat2
      Pattern_Constructor _ _ pats -> any (hasVariable name) pats
      Pattern_List _ pats -> any (hasVariable name) pats
      Pattern_Tuple _ pats -> any (hasVariable name) pats
      _ -> False

maybeAddLocation :: UHA_Source -> MessageBlock
maybeAddLocation src
   | match = 
        MessageCompose 
           [ MessageOneLineTree (oneLinerSource src)
           , MessageString " at "
           , MessageRange (rangeOfSource src)
           ]
   | otherwise =  
        MessageOneLineTree (oneLinerSource src)

 where match =
          case src of
             UHA_Expr (Expression_Variable _ _) -> True
             UHA_Pat  (Pattern_Variable _ _)    -> True
             _ -> False
