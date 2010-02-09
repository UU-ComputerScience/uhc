{-| Module      :  TypeErrors
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Errors that are constructed during type inferece.
-}

module Helium.StaticAnalysis.Messages.TypeErrors where

import Helium.StaticAnalysis.Messages.Messages
import Top.Types
import List       (union, intersperse, partition)
import Helium.Utils.OneLiner   (OneLineTree(..) )
import Helium.Syntax.UHA (Range, Name)
import Helium.Syntax.UHA_Range  (getNameRange)
import Helium.StaticAnalysis.Miscellaneous.UHA_Source
import Data.Maybe

type TypeErrors = [TypeError]
data TypeError  = TypeError
                     [Range]                                -- range(s) of the error
                     [MessageLine]                          -- oneliner messages
                     [(Bool, MessageBlock, MessageBlock)]   -- Hugs-like table
                     [TypeErrorHint]                        -- extra hints
		     
type TypeErrorHint  = (String, MessageBlock)

instance HasMessage TypeError where
   getMessage (TypeError range oneliners table hints) =
      let emptyLine  = MessageOneLiner (MessageString "")
	  maybeTable | null table = [] 
	             | otherwise  = [ MessageTable (table ++ map (uncurry (<:>)) hints) ]
      in oneliners ++ maybeTable ++ [emptyLine]
   getRanges (TypeError ranges oneliner table hints) = ranges

instance Substitutable TypeError where
   sub |-> (TypeError ranges oneliner table hints) =
      let table' = [ (b, sub |-> mb1, sub |-> mb2) | (b, mb1, mb2) <- table ] 
          hints' = [ (s, sub |-> mb) | (s, mb) <- hints ]
      in TypeError ranges (sub |-> oneliner) table' hints'
   ftv (TypeError ranges oneliner table hints) =
      ftv oneliner `union` ftv [ [mb1, mb2] | (_, mb1, mb2) <- table ] `union` ftv (map snd hints)
    
makeNotGeneralEnoughTypeError :: Bool -> Range -> UHA_Source -> TpScheme -> TpScheme -> TypeError
makeNotGeneralEnoughTypeError isAnnotation range source tpscheme1 tpscheme2 =
   let sub      = listToSubstitution (zip (ftv [tpscheme1, tpscheme2]) [ TVar i | i <- [1..] ])
       ts1      = skolemizeFTV (sub |-> tpscheme1)
       ts2      = skolemizeFTV (sub |-> tpscheme2)
       special  = if isAnnotation then "Type annotation" else "Type signature"
       oneliner = MessageOneLiner (MessageString (special ++ " is too general"))
       descr    = if isAnnotation then "expression" else "function"
       table    = [ descr           <:> MessageOneLineTree (oneLinerSource source)
                  , "declared type" >:> MessageType ts2
                  , "inferred type" >:> MessageType ts1
                  ]
       hints    = [ ("hint", MessageString "try removing the type signature") | not (null (ftv tpscheme1)) ] 
   in TypeError [range] [oneliner] table hints
   
makeMissingConstraintTypeError :: Range -> Maybe UHA_Source -> TpScheme -> (Bool, Predicate) -> UHA_Source -> TypeError
makeMissingConstraintTypeError range mSource scheme (original, pred) arisingFrom =
   let special  = if isJust mSource then "signature" else "annotation"
       oneliner = MessageOneLiner (MessageString ("Missing class constraint in type "++special))
       table    = maybe [] (\source -> ["function" <:> MessageOneLineTree (oneLinerSource source)]) mSource ++
                  [ (isJust mSource, MessageString "declared type", MessageType scheme)
                  , "class constraint" <:> MessagePredicate pred
                  , "arising from"     >:> MessageOneLineTree (oneLinerSource arisingFrom)
                  ]
       hints    = [ ("hint", MessageString "add the class constraint to the type signature") | original ]
   in TypeError [range] [oneliner] table hints
   
makeUnresolvedOverloadingError :: UHA_Source -> String -> (TpScheme, TpScheme) -> TypeError
makeUnresolvedOverloadingError source description (functionType, usedAsType) =
   let message = [ MessageOneLiner (MessageString ("Don't know which instance to choose for " ++ description)) ]
       table   = [ "function" <:> MessageOneLineTree (oneLinerSource source)
                 , "type"     >:> MessageType functionType
                 , "used as"  >:> MessageType usedAsType
                 , "hint"     <:> MessageString ( "write an explicit type for this function" ++ 
		                                "\n   e.g. (show :: [Int] -> String)")
                 ]
   in TypeError [rangeOfSource source] message table []
      
makeReductionError :: UHA_Source -> Either (TpScheme, Tp) (String, Maybe Tp) -> ClassEnvironment -> Predicate -> TypeError
makeReductionError source extra classEnvironment predicate@(Predicate className predicateTp) =
   let location = either (const "function") fst extra
       message  = [ MessageOneLiner $ MessageString $ "Type error in overloaded " ++ location ]
       tab1     = case extra of 
                     Left (scheme, tp) -> -- overloaded function
                        [ "function" <:> MessageOneLineTree (oneLinerSource source)
                        , "type"     >:> MessageType scheme
                        , "used as"  >:> MessageType (toTpScheme tp)
                        ]
                     Right (_, mtp) -> -- overloaded language construct
                        [ descriptionOfSource source <:> MessageOneLineTree (oneLinerSource source) ] ++
                        maybe [] (\tp -> ["type" >:> MessageType (toTpScheme tp)]) mtp
       tab2     = [ "problem"  <:> MessageCompose [ MessageType (toTpScheme predicateTp)
                                                  , MessageString (" is not an instance of class "++className)
			 	                                  ]
                  ]
   in TypeError [rangeOfSource source] message (tab1 ++ tab2) [("hint", MessageString hint)]
   
  where  
    hint :: String
    hint = case valids of
              []  -> "there are no valid instances of "++className
              [x] -> "valid instance of "++className++" is "++show x
              xs  -> "valid instances of "++className++" are "++prettyAndList valids
         
    valids :: [String]
    valids = let tps              = [ tp | (Predicate _ tp, _) <- instances className classEnvironment ]
                 (tuples, others) = let p (TCon s) = isTupleConstructor s
                                        p _        = False
                                    in partition (p . fst . leftSpine) tps
                 f tp = listToSubstitution (zip (ftv tp) [ TCon s | s <- variableList ]) |-> tp
             in if length tuples > 4 -- magic number!
                  then map (show . f) others ++ ["tuples"]
                  else map (show . f) tps
                  
makeRestrictedButOverloadedError :: Name -> TpScheme -> TypeError
makeRestrictedButOverloadedError name scheme = 
   let message = MessageOneLiner $ MessageString $ "Illegal overloaded type inferred for " ++ show name
       table   = [ "variable"      <:> MessageString (show name)
                 , "inferred type" >:> MessageType scheme
                 ]
       hint    = "Only functions and simple patterns can have an overloaded type"
   in TypeError [getNameRange name] [message] table [("hint", MessageString hint)]