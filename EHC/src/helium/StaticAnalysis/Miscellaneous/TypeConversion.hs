{-| Module      :  TypeConversion
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    The conversion from UHA types to Tp (a simpler representation), and vice versa.
-}

module Helium.StaticAnalysis.Miscellaneous.TypeConversion where

import Helium.Syntax.UHA_Utils (getNameName, nameFromString)
import Helium.Syntax.UHA_Range (noRange)
import Helium.Utils.Utils (internalError)
import List (union)
import Helium.Syntax.UHA
import Top.Types

----------------------------------------------------------------------
-- conversion functions from and to UHA

namesInTypes :: Types -> Names
namesInTypes = foldr union [] . map namesInType

namesInType :: Type -> Names
namesInType uhaType = case uhaType of

      Type_Application _ _ fun args -> namesInTypes (fun : args)                                                 
      Type_Variable _ name          -> [name]      
      Type_Constructor _ _          -> []
      Type_Parenthesized _ t        -> namesInType t                    
      Type_Qualified _ _ t          -> namesInType t
      Type_Forall _ _ _             -> internalError "TypeConversion.hs" "namesInType" "universal types are currently not supported"            
      Type_Exists _ _ _             -> internalError "TypeConversion.hs" "namesInType" "existential types are currently not supported"
      
-- name maps play an important role in converting since they map UHA type variables (string) to TVar's (int)  
makeNameMap :: Names -> [(Name,Tp)]
makeNameMap = flip zip (map TVar [0..])

-- also return the name map
makeTpSchemeFromType' :: Type -> (TpScheme, [(Int, Name)])
makeTpSchemeFromType' uhaType =
   let names   = namesInType uhaType
       nameMap = makeNameMap names
       intMap  = zip [0..] names
       context = predicatesFromContext nameMap uhaType
       tp      = makeTpFromType nameMap uhaType
       scheme  = Quantification (ftv tp, [ (i,getNameName n) | (n,TVar i) <- nameMap], context .=>. tp)
   in (scheme, intMap)

makeTpSchemeFromType :: Type -> TpScheme
makeTpSchemeFromType = fst . makeTpSchemeFromType'

predicatesFromContext :: [(Name,Tp)] -> Type -> Predicates
predicatesFromContext nameMap (Type_Qualified _ is _) =
   concatMap predicateFromContext is
   where
     predicateFromContext (ContextItem_ContextItem _ cn [Type_Variable _ vn]) =
       case lookup vn nameMap of
         Nothing -> []
         Just tp -> [Predicate (getNameName cn) tp]
     predicateFromContext _ = internalError "TypeConversion.hs" "predicateFromContext" "malformed type in context"
predicatesFromContext nameMap _ = []

makeTpFromType :: [(Name,Tp)] -> Type -> Tp    
makeTpFromType nameMap = rec 
  where                    
        rec :: Type -> Tp
        rec uhaType = case uhaType of  
             Type_Application _ _ fun args -> foldl TApp (rec fun) (map rec args)
             Type_Variable _ name          -> maybe (TCon "???") id (lookup name nameMap)                                                      
             Type_Constructor _ name       -> TCon (getNameName name)
             Type_Parenthesized _ t        -> rec t                                                 
             Type_Qualified _ cs t         -> rec t
             Type_Forall _ _ _             -> internalError "TypeConversion.hs" "makeTpFromType" "universal types are currently not supported"            
             Type_Exists _ _ _             -> internalError "TypeConversion.hs" "makeTpFromType" "existential types are currently not supported"

convertFromSimpleTypeAndTypes :: SimpleType -> Types -> (Tp,Tps)
convertFromSimpleTypeAndTypes stp  tps = 
   let SimpleType_SimpleType _ name typevariables = stp
       nameMap    = makeNameMap (foldr union [] (typevariables : map namesInType tps))
       simpletype = foldl TApp (TCon (getNameName name)) (take (length typevariables) (map TVar [0..]))
   in (simpletype,map (makeTpFromType nameMap) tps)
       
makeTypeFromTp :: Tp -> Type
makeTypeFromTp t = 
    let (x,xs) = leftSpine t 
    in if null xs 
        then f x
        else Type_Application noRange True (f x) (map makeTypeFromTp xs)
   where f (TVar i) = Type_Variable noRange    (nameFromString ('v' : show i)) 
         f (TCon s) = Type_Constructor noRange (nameFromString s)
