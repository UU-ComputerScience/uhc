{-| Module      :  StaticErrors
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

    Collection of static error messages.
-}

module Helium.StaticAnalysis.Messages.StaticErrors where

import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range
import Helium.StaticAnalysis.Messages.Messages
import List        (intersperse, sort, partition, isSuffixOf, isPrefixOf)
import Maybe       (fromJust)
import Helium.Utils.Utils       (commaList, internalError, minInt, maxInt)
import Top.Types

-------------------------------------------------------------
-- (Static) Errors

type Errors = [Error]
data Error  = NoFunDef Entity Name {-names in scope-}Names
            | Undefined Entity Name {-names in scope-}Names {-similar name in wrong name-space hint-}[String] {- hints -}
            | Duplicated Entity Names
            | LastStatementNotExpr Range
            | WrongFileName {-file name-}String {-module name-}String Range {- of module name -}
            | TypeVarApplication Name
            | ArityMismatch {-type constructor-}Entity Name {-verwacht aantal parameters-}Int {-aangetroffen aantal parameters-}Int
            | DefArityMismatch Name (Maybe Int) {- verwacht -} Range
            | RecursiveTypeSynonyms Names
            | PatternDefinesNoVars Range
            | IntLiteralTooBig Range String
            | OverloadingDisabled Range
            | OverloadedRestrPat Name
            | WrongOverloadingFlag Bool{- flag? -}
            | AmbiguousContext Name
            | UnknownClass Name
            | NonDerivableClass Name
            | CannotDerive Name Tps
            | TupleTooBig Range

instance HasMessage Error where
   getMessage x = let (oneliner, hints) = showError x
                  in [MessageOneLiner oneliner, MessageHints "Hint" hints]
   getRanges anError = case anError of
   
      NoFunDef _ name _           -> [getNameRange name]
      Undefined _ name _ _        -> [getNameRange name]
      Duplicated _ names          -> sortRanges (map getNameRange names)
      LastStatementNotExpr range  -> [range]
      WrongFileName _ _ range     -> [range]
      TypeVarApplication name     -> [getNameRange name]
      ArityMismatch _ name _ _    -> [getNameRange name]             
      DefArityMismatch _ _ range  -> [range]
      RecursiveTypeSynonyms names -> sortRanges (map getNameRange names)
      PatternDefinesNoVars range  -> [range]
      IntLiteralTooBig range _    -> [range]
      OverloadingDisabled range   -> [range]
      OverloadedRestrPat name     -> [getNameRange name]
      WrongOverloadingFlag flag   -> [emptyRange]
      AmbiguousContext name       -> [getNameRange name]
      UnknownClass name           -> [getNameRange name]
      NonDerivableClass name      -> [getNameRange name]
      CannotDerive name _         -> [getNameRange name]
      TupleTooBig r               -> [r]
      _                           -> internalError "StaticErrors.hs" 
                                                   "instance IsMessage Error" 
                                                   "unknown type of Error"
         
showError :: Error -> (MessageBlock {- oneliner -}, MessageBlocks {- hints -})
showError anError = case anError of 
  
   NoFunDef TypeSignature name inScope ->
      ( MessageString ("Type signature for " ++ show (show name) ++ " without a definition ")
      , [ MessageString ("Did you mean "++prettyOrList (map (show . show) xs)++" ?")
        | let xs = findSimilar name inScope, not (null xs) 
        ] 
      )

   NoFunDef Fixity name inScope ->
      ( MessageString ("Infix declaration for " ++ show (show name) ++ " without a definition ")
      , [ MessageString ("Did you mean "++prettyOrList (map (show . show) xs)++" ?")
        | let xs = findSimilar name inScope, not (null xs) 
        ]
      )

   Undefined entity name inScope hints ->
      ( MessageString ("Undefined " ++ show entity ++ " " ++ show (show name))
      , map MessageString hints
        ++
        [ MessageString ("Did you mean " ++ prettyOrList (map (show . show) xs) ++ " ?")
        | let xs = findSimilar name inScope, not (null xs)
        ]
      )
                         
   Duplicated entity names
      | all isImportRange nameRanges ->
           ( MessageString (
                capitalize (show entity) ++ " " ++
                (show . show . head) names ++
                " imported from multiple modules: " ++ 
                commaList (map (snd.fromJust.modulesFromImportRange) nameRanges)), [])
                
      | any isImportRange nameRanges ->
           let
               (importRanges, localRanges) = partition isImportRange nameRanges
               plural = if length importRanges > 1 then "s" else ""
           in
              ( MessageString ( 
                   capitalize (show entity) ++ " " ++ (show.show.head) names ++
                   " clashes with definition" ++ plural ++
                   " in imported module" ++ plural ++ " " ++ 
                   commaList [ snd (fromJust (modulesFromImportRange importRange)) 
                             | importRange <- importRanges
                             ]), [])

      | otherwise ->
           ( MessageString ("Duplicated " ++ show entity ++ " " ++ (show . show . head) names), [])
                 
    where
        fromRanges = [ if isImportRange range then
                         Range_Range position position
                       else
                         range
                     | range <- nameRanges
                     , let position = getRangeEnd range
                     ]
        nameRanges   = sort (map getNameRange names)

   LastStatementNotExpr range ->
      ( MessageString "Last generator in do {...} must be an expression ", [])
    
   TypeVarApplication name ->
      ( MessageString ("Type variable " ++ show (show name) ++ " cannot be applied to another type"), [])

   ArityMismatch entity name expected actual ->
      ( MessageString ( capitalize (show entity) ++ " " ++show (show name) ++
           " should have " ++ prettyNumberOfParameters expected ++
           ", but has " ++ if actual == 0 then "none" else show actual), [])

   RecursiveTypeSynonyms [string] ->
      ( MessageString ("Recursive type synonym " ++ show (show string))
      , [ MessageString "Use \"data\" to write a recursive data type" ]
      )
      
   RecursiveTypeSynonyms strings ->
      ( MessageString ("Recursive type synonyms " ++
            prettyAndList (map (show . show) (sortNamesByRange strings)))
      , []
      )

   DefArityMismatch name maybeExpected range ->
      ( MessageString ("Arity mismatch in function bindings for " ++ show (show name))
      , [ MessageString (show arity ++ " parameters in most of the clauses")
        | Just arity <- [maybeExpected]
        ]
      )

   PatternDefinesNoVars range ->
      ( MessageString "Left hand side pattern defines no variables", [])

   WrongFileName fileName moduleName range ->
      ( MessageString ("The file name " ++ show fileName ++ " doesn't match the module name " ++ show moduleName), [])
      
   IntLiteralTooBig _ value ->
      ( MessageString ("Integer literal (" ++ value ++ ") too big")
      , [ MessageString $ "Maximum is " ++ show maxInt ]
      )
   
   OverloadedRestrPat name ->
      ( MessageString ("Illegal overloaded type signature for " ++ show (show name))
      , [MessageString "Only functions and simple patterns can have an overloaded type"]
      )
      
   OverloadingDisabled range ->
      ( MessageString ("Cannot handle contexts when overloading is disabled")
      , []
      )

   WrongOverloadingFlag False ->
      ( MessageString ("Using overloaded Prelude while overloading is not enabled")
      , [MessageString "Compile with --overloading, or use the simple Prelude"]
      )

   WrongOverloadingFlag True ->
      ( MessageString ("Using simple Prelude while overloading is enabled")
      , [MessageString "Compile without --overloading, or use the overloaded Prelude"]
      )
      
   AmbiguousContext name ->
      ( MessageString ("Type variable " ++ show (show name) ++ " appears in the context but not in the type")
      , []
      )
      
   UnknownClass name ->
      ( MessageString ("Unknown class " ++ show (show name) ++ " (Helium only supports Eq, Ord, Num, Show, Enum)")
      , []
      )

   NonDerivableClass name ->
      ( MessageString ("Cannot derive class " ++ show (show name))
      , [MessageString "Only Show and Eq instances can be derived"]
      )   

   CannotDerive name tps ->
      ( MessageString ("Cannot derive instance for class " ++ show (show name))
      , let msg = MessageCompose (intersperse (MessageString ", ") (map (MessageType . toTpScheme) tps))
            
        in [ MessageCompose
            [ MessageString "There "
            , MessageString ( if length tps == 1 then "is " else "are ")
            , MessageString ("no " ++ (show name) ++ " instance")
            , MessageString ( if length tps == 1 then " " else "s ")
            , MessageString "for "
            , msg
            ]
           ]
      )      
    
   TupleTooBig r ->
    ( MessageString "Tuples can have up to 10 elements"
    , []
    )

   _ -> internalError "StaticErrors.hs" "showError" "unknown type of Error"

makeUndefined :: Entity -> Names -> Names -> [Error]
makeUndefined entity names inScope = [ Undefined entity name inScope [] | name <- names ]

makeDuplicated :: Entity -> [Names] -> [Error]
makeDuplicated entity nameslist = [ Duplicated entity names | names <- nameslist ]

undefinedConstructorInExpr :: Name -> Names -> Names -> Error
undefinedConstructorInExpr name sims tyconNames =
   let hints = [ "Type constructor "++show (show name)++" cannot be used in an expression"
               | name `elem` tyconNames
               ]
   in Undefined Constructor name sims hints

undefinedConstructorInPat :: Bool -> Name -> Names -> Names -> Error
undefinedConstructorInPat lhsPattern name sims tyconNames =
   let hints = [ "Use identifiers starting with a lower case letter to define a function or a variable" 
               | lhsPattern 
               ] ++
               [ "Type constructor "++show (show name)++" cannot be used in a pattern"
               | name `elem` tyconNames
               ]

   in Undefined Constructor name sims hints

makeNoFunDef :: Entity -> Names -> Names -> [Error]
makeNoFunDef entity names inScope = [ NoFunDef entity name inScope | name <- names ]

-- Log-codes for Errors
errorsLogCode :: Errors -> String
errorsLogCode [] = "[]"
errorsLogCode xs = foldr1 (\x y -> x++","++y) (map errorLogCode xs)

errorLogCode :: Error -> String
errorLogCode anError = case anError of 
          NoFunDef entity _ _     -> "nf" ++ code entity
          Undefined entity _ _ _  -> "un" ++ code entity
          Duplicated entity _     -> "du" ++ code entity
          LastStatementNotExpr _  -> "ls"
          WrongFileName _ _ _     -> "wf"
          TypeVarApplication _    -> "tv"
          ArityMismatch _ _ _ _   -> "am"
          DefArityMismatch _ _ _  -> "da"
          RecursiveTypeSynonyms _ -> "ts"
          PatternDefinesNoVars _  -> "nv"
          IntLiteralTooBig _ _    -> "il"
          OverloadingDisabled _   -> "od"
          OverloadedRestrPat _    -> "or"
          WrongOverloadingFlag _  -> "of"
          AmbiguousContext _      -> "ac"
          UnknownClass _          -> "uc"
          NonDerivableClass _     -> "nd"
          CannotDerive _ _        -> "cd"
          TupleTooBig _           -> "tt"
          _                       -> "??"
   where code entity = maybe "??" id
                     . lookup entity 
                     $ [ (TypeSignature    ,"ts"), (TypeVariable         ,"tv"), (TypeConstructor,"tc")
                       , (Definition       ,"de"), (Constructor          ,"co"), (Variable       ,"va") 
                       , (Import           ,"im"), (ExportVariable       ,"ev"), (ExportModule   ,"em")
                       , (ExportConstructor,"ec"), (ExportTypeConstructor,"et"), (Fixity         ,"fx")
                       ]                    
