{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Pretty printing module
module Pretty where

import HML
import Data.List

class Pretty a where
   pp :: a -> String
   
instance Pretty a => Show a where
   show = pp

instance Pretty HsName where
   pp = id

instance Pretty Prefix where
  pp = (\x->"(" ++ x ++ ")") . intercalate "," . map pp
  
instance Pretty TyIndex where
  pp (TyIndex_Group var bound) = "(" ++ pp var ++ " >= " ++ pp bound ++ ")"
  
instance Pretty TyScheme where
  pp TyScheme_Bottom                 = "_|_"
  pp (TyScheme_SystemF typ)          = pp typ
  pp (TyScheme_Qaunt var bound expr) = let x = pp var
                                       in "forall (" ++ x ++ " >= " ++ pp bound ++ "). " ++ pp expr
                                       
instance Pretty TyQuantifiedScheme where
  pp (TyQuantifiedScheme_Quant i b) = "forall (" ++ pp i ++ "). " ++ pp b
  pp TyQuantifiedScheme_Bottom      = "_|_"

instance Pretty TyMono where
  pp (TyMono_Var a)   = pp a
  pp (TyMono_App a b) = pp a ++ " -> " ++ pp b
  pp (TyMono_Con a)   = pp a
  pp (TyMono_AppTop a)= pp a
  
instance Pretty TyPoly where
  pp (TyPoly_Var a)   = pp a
  pp (TyPoly_App a b) = pp a ++ " -> " ++ pp b
  pp (TyPoly_Con a)   = pp a
  pp (TyPoly_AppTop a)= pp a
  
instance TyQu where
  pp TyQu_TyForall = "forall"