module Information where

import Top.Types
import Args
import ImportEnvironment
import CompileUtils
import OperatorTable
import Messages hiding (Constructor)
import UHA_Syntax hiding (Fixity)
import UHA_Utils
import UHA_Range
import qualified Data.Map as M
import Data.List (intersperse)

type Fixity = (Int, Assoc)

data InfoItem
   = Function Name TpScheme (Maybe Fixity)
   | ValueConstructor Name TpScheme (Maybe Fixity)
   | TypeSynonym Name Int (Tps -> Tp) 
   | DataTypeConstructor Name Int [(Name, TpScheme)]
   | TypeClass String Class
   | NotDefined String

showInformation :: Bool -> [Option] -> ImportEnvironment -> IO ()
showInformation reportNotFound options importEnv =
   let items = concat [ makeInfoItem name | Information name <- options ]
   in showMessages items
 
 where
   makeInfoItem :: String -> [InfoItem]
   makeInfoItem string =
      let 
          notFound items = if null items && reportNotFound then [ NotDefined string ] else items
          
          function =
             case lookupWithKey (nameFromString string) (typeEnvironment importEnv) of
                Just (name, scheme) -> 
                   [Function name scheme (M.lookup name (operatorTable importEnv))]
                Nothing -> []
          
          constructor = 
             case lookupWithKey (nameFromString string) (valueConstructors importEnv) of
                Just (name, scheme) -> 
                   [ValueConstructor name scheme (M.lookup name (operatorTable importEnv))]
                Nothing     -> []

          synonyms = 
             case lookupWithKey (nameFromString string) (typeSynonyms importEnv) of
                Just (name, (i, f)) -> 
                   [TypeSynonym name i f]
                Nothing     -> [] 
      
          datatypeconstructor =
             case lookupWithKey (nameFromString string) (typeConstructors importEnv) of
                Just (name, i) | not (M.member name (typeSynonyms importEnv))  
                   -> [DataTypeConstructor name i (findValueConstructors name importEnv)]
                _  -> []
      
          typeclass = 
             case M.lookup string standardClasses of
                Just cl -> [TypeClass string cl]
                Nothing -> []
      in 
         notFound (function ++ constructor ++ synonyms ++ datatypeconstructor ++ typeclass)

itemDescription :: InfoItem -> [String]
itemDescription infoItem =
   case infoItem of
   
      Function name ts _ -> 
         let tp = unqualify (unquantify ts)
             start | isOperatorName name = "operator"
                   | isFunctionType tp   = "function"
                   | otherwise           = "value"
         in [ "-- " ++ start ++ " " ++ show name ++ ", " ++ definedOrImported (getNameRange name) ]

      ValueConstructor name _ _ ->
         [ "-- value constructor " ++ show name ++ ", " ++ definedOrImported (getNameRange name) ]

      TypeSynonym name _ _ -> 
         [ "-- type synonym " ++ show name ++ ", " ++ definedOrImported (getNameRange name) ]

      DataTypeConstructor name _ _ ->
         [ "-- type constructor " ++ show name ++ ", " ++ definedOrImported (getNameRange name) ]
   
      TypeClass s _ ->
         [ " -- type class " ++ s ]
         
      NotDefined _ -> 
         [ ]

definedOrImported :: Range -> String
definedOrImported range
   | isImportRange range = "imported from " ++ show range
   | otherwise           = "defined at " ++ show range

showMaybeFixity :: Name -> Maybe Fixity -> MessageBlocks
showMaybeFixity name =
   let f (prio, assoc) = show assoc ++ " " ++ show prio ++ " " ++ showNameAsOperator name
   in maybe [] ((:[]) . MessageString . f)

instance HasMessage InfoItem where 

   getMessage infoItem = 
      map (MessageOneLiner . MessageString) (itemDescription infoItem)
      ++
      case infoItem of
      
         Function name ts mFixity ->
            map MessageOneLiner
               ( MessageString (showNameAsVariable name ++ " :: " ++ show ts)
               : showMaybeFixity name mFixity
               )

         ValueConstructor name ts mFixity ->
            map MessageOneLiner
               ( MessageString (showNameAsVariable name ++ " :: " ++ show ts)
               : showMaybeFixity name mFixity
               )

         TypeSynonym name i f ->
            let tps  = take i [ TCon [c] | c <- ['a'..] ] 
                text = concat (intersperse " " ("type" : show name : map show tps ++ ["=", show (f tps)]))
            in [ MessageOneLiner (MessageString text) ]

         DataTypeConstructor name i cons ->
            let tps     = take i [ TCon [c] | c <- ['a'..] ]
                text    = concat (intersperse " " ("data" : show name : map show tps))
                related = let f (name, ts) = "   " ++ showNameAsVariable name ++ " :: " ++ show ts
                          in if null cons then [] else "   -- value constructors" : map f cons
            in map MessageOneLiner 
                  ( MessageString text
                  : map MessageString related
                  )

         TypeClass name (supers, instances) ->
            let f s     = s ++ " a"
                text    = "class " ++ showContextSimple (map f supers) ++ f name
                related = let f (p, ps) = "   instance " ++ show (generalizeAll (ps .=>. p))
                          in if null instances then [] else "   -- instances" : map f instances
            in map MessageOneLiner 
                  ( MessageString text
                  : map MessageString related
                  )
         
         NotDefined name ->
            map MessageOneLiner
               [ MessageString (show name ++ " not defined") ]

findValueConstructors :: Name -> ImportEnvironment -> [(Name, TpScheme)]
findValueConstructors name =
   let test = isName . fst . leftSpine . snd . functionSpine . unqualify . unquantify
       isName (TCon s) = s == show name
       isName _        = False
   in M.assocs . M.filter test . valueConstructors

lookupWithKey :: Ord key => key -> M.Map key a -> Maybe (key, a)
lookupWithKey key = M.lookup key . M.mapWithKey (,)