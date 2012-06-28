

-- UUAGC 0.9.39.1 (build/ruler2/Ty/Ty.ag)
module Ty.Ty(AGTyItf (..), Ty (..), tyUnk, tyTopNm) where

import Common









tyTopNm :: Ty -> Nm
tyTopNm (Ty_Con n) = n
tyTopNm _          = nmUnk



tyUnk :: Ty
tyUnk = Ty_Con nmUnk
-- AGTyItf -----------------------------------------------------
data AGTyItf  = AGTyItf_AGItf (Ty ) 
-- Ty ----------------------------------------------------------
data Ty  = Ty_App (Ty ) (Ty ) 
         | Ty_Con (Nm) 
         deriving ( Eq,Ord,Show)