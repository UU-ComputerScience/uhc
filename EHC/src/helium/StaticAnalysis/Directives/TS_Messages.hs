{-| Module      :  TS_Messages
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    The error messages and warnings that are produced when a .type file is
	compiled.

	(directives based on "Scripting the Type Inference Process", ICFP 2003)
-}

module Helium.StaticAnalysis.Directives.TS_Messages where

import Helium.StaticAnalysis.Messages.Messages
import Helium.StaticAnalysis.Messages.TypeErrors
import Helium.StaticAnalysis.Miscellaneous.ConstraintInfo
import Helium.StaticAnalysis.Messages.HeliumMessages () -- instance Show MessageLines
import Top.Types
import Helium.Utils.Utils (commaList)
import Helium.Syntax.UHA

type TS_Errors = [TS_Error]
data TS_Error 
      = InconsistentConstraint String {- rule name -} ConstraintInfo
      | UndefinedTS String {- rule name -} Name {- variable/constructor -} Entity
      | UnusedMetaVariable String {- rule name -} String {- metavariable -}
      | DuplicatedMetaVariablesPremise String {- rule name -} String {- metavariable -}
      | DuplicatedMetaVariablesConclusion String {- rule name -} String {- metavariable -}      
      | DuplicatedMetaVariableConstraints String {- rule name -} String {- metavariable -}     
      | TypeErrorTS String TypeError      
      | Soundness String {- rule name -} TpScheme {- inferredTpScheme -} TpScheme {- constraintsTpScheme -}
             
type TS_Warnings = [TS_Warning]
data TS_Warning
      = MetaVariableConstraintsNotExplicit String {- rule name -} [String] {- metavariable -}

instance HasMessage TS_Error where
   getMessage  = (:[]) . MessageOneLiner . MessageString . showTS_Error
   getRanges _ = []
      
instance HasMessage TS_Warning where
   getMessage  = (:[]) . MessageOneLiner . MessageString . showTS_Warning
   getRanges _ = []

showTS_Error :: TS_Error -> String
showTS_Error tsError = case tsError of
   (InconsistentConstraint rule hci) ->
      let (t1,t2) = typepair hci
      in "Error in type strategy rule "++show rule++": "++
         "the given constraint set was inconsistent while solving the constraint "++
         show t1++" == "++show t2

   (UndefinedTS rule name entity) ->
      "Undefined "++show entity++" "++show name++" in type strategy rule "++show rule

   (UnusedMetaVariable rule metavariable) ->
      "Unused meta-variable "++show metavariable++" in type strategy rule "++show rule

   (DuplicatedMetaVariablesPremise rule metavariable) ->
      "Duplicated meta-variable "++show metavariable++" in premise of type strategy rule "++show rule   

   (DuplicatedMetaVariablesConclusion rule metavariable) ->
      "Duplicated meta-variable "++show metavariable++" in conclusion of type strategy rule "++show rule   

   (DuplicatedMetaVariableConstraints rule metavariable) ->
      "The constraints for meta-variable "++show metavariable++" of type strategy rule "++show rule++
      " can only be inserted at one place"
      
   (TypeErrorTS rule typeError) ->
      "Type error in type strategy rule "++show rule++" while inferring the type of the conclusion:\n" ++
         show (getMessage typeError)

   (Soundness rule inferred strategy) ->
      unlines [ "The type rule for " ++ show rule ++ " is not correct"
              , "  the type according to the type rule is"
              , "    " ++ show strategy
              , "  whereas the standard type rules infer the type"
              , "    " ++ show inferred
              ]
   
showTS_Warning :: TS_Warning -> String
showTS_Warning tsWarning = case tsWarning of

   (MetaVariableConstraintsNotExplicit rule metavariables) ->
      "The constraint set"++(if length metavariables > 1 then "s" else "")
      ++ " for the meta variable"++(if length metavariables > 1 then "s " else " ")
      ++ commaList metavariables      
      ++ " in the type strategy rule "++show rule
      ++" are not inserted explicitly. By default, they are inserted in the beginning."   
