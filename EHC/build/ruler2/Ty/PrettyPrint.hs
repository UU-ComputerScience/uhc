

-- UUAGC 0.9.39.1 (build/ruler2/Ty/PrettyPrint.ag)
module Ty.PrettyPrint(ppTy) where

import EH.Util.Pretty
import Common
import Ty.Ty














ppTy :: Ty -> PP_Doc
ppTy e
  = pp_Syn_AGTyItf r2
  where r1 = sem_AGTyItf (AGTyItf_AGItf e)
        r2 = wrap_AGTyItf r1
                (Inh_AGTyItf )

instance PP Ty where
  pp = ppTy

-- AGTyItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child ty             : Ty 
-}
-- cata
sem_AGTyItf :: AGTyItf  ->
               T_AGTyItf 
sem_AGTyItf (AGTyItf_AGItf _ty )  =
    (sem_AGTyItf_AGItf (sem_Ty _ty ) )
-- semantic domain
type T_AGTyItf  = ( PP_Doc)
data Inh_AGTyItf  = Inh_AGTyItf {}
data Syn_AGTyItf  = Syn_AGTyItf {pp_Syn_AGTyItf :: PP_Doc}
wrap_AGTyItf :: T_AGTyItf  ->
                Inh_AGTyItf  ->
                Syn_AGTyItf 
wrap_AGTyItf sem (Inh_AGTyItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_AGTyItf _lhsOpp ))
sem_AGTyItf_AGItf :: T_Ty  ->
                     T_AGTyItf 
sem_AGTyItf_AGItf ty_  =
    (let _lhsOpp :: PP_Doc
         _tyIpp :: PP_Doc
         -- use rule "build/ruler2/Ty/PrettyPrintAG.ag"(line 6, column 29)
         _lhsOpp =
             _tyIpp
         ( _tyIpp) =
             ty_ 
     in  ( _lhsOpp))
-- Ty ----------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative App:
         child lTy            : Ty 
         child rTy            : Ty 
      alternative Con:
         child nm             : {Nm}
-}
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Ty_App _lTy _rTy )  =
    (sem_Ty_App (sem_Ty _lTy ) (sem_Ty _rTy ) )
sem_Ty (Ty_Con _nm )  =
    (sem_Ty_Con _nm )
-- semantic domain
type T_Ty  = ( PP_Doc)
sem_Ty_App :: T_Ty  ->
              T_Ty  ->
              T_Ty 
sem_Ty_App lTy_ rTy_  =
    (let _lhsOpp :: PP_Doc
         _lTyIpp :: PP_Doc
         _rTyIpp :: PP_Doc
         -- "build/ruler2/Ty/PrettyPrintAG.ag"(line 9, column 21)
         _lhsOpp =
             _lTyIpp
             >#< _rTyIpp
         ( _lTyIpp) =
             lTy_ 
         ( _rTyIpp) =
             rTy_ 
     in  ( _lhsOpp))
sem_Ty_Con :: Nm ->
              T_Ty 
sem_Ty_Con nm_  =
    (let _lhsOpp :: PP_Doc
         -- "build/ruler2/Ty/PrettyPrintAG.ag"(line 11, column 21)
         _lhsOpp =
             pp nm_
     in  ( _lhsOpp))