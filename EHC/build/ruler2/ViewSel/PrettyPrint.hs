

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/PrettyPrint.ag)
module ViewSel.PrettyPrint(ppViewSel, ppViewSels) where

import EH.Util.Pretty
import EH.Util.Nm
import ViewSel.ViewSel









ppViewSel :: ViewSel -> PP_Doc
ppViewSel vs
  = pp_Syn_AGViewSelItf r2
  where r1 = sem_AGViewSelItf (AGViewSelItf_AGItf vs)
        r2 = wrap_AGViewSelItf r1 (Inh_AGViewSelItf)

ppViewSels :: ViewSels -> PP_Doc
ppViewSels vs
  = pp_Syn_AGViewSelsItf r2
  where r1 = sem_AGViewSelsItf (AGViewSelsItf_AGItf vs)
        r2 = wrap_AGViewSelsItf r1 (Inh_AGViewSelsItf)

instance PP ViewSel where
  pp = ppViewSel

-- AGRlSelItf --------------------------------------------------
{-
   alternatives:
      alternative AGItf:
         child rlSel          : RlSel 
-}
-- cata
sem_AGRlSelItf :: AGRlSelItf  ->
                  T_AGRlSelItf 
sem_AGRlSelItf (AGRlSelItf_AGItf _rlSel )  =
    (sem_AGRlSelItf_AGItf (sem_RlSel _rlSel ) )
-- semantic domain
type T_AGRlSelItf  = ( )
sem_AGRlSelItf_AGItf :: T_RlSel  ->
                        T_AGRlSelItf 
sem_AGRlSelItf_AGItf rlSel_  =
    (let 
     in  ( ))
-- AGViewSelItf ------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child viewSel        : ViewSel 
-}
-- cata
sem_AGViewSelItf :: AGViewSelItf  ->
                    T_AGViewSelItf 
sem_AGViewSelItf (AGViewSelItf_AGItf _viewSel )  =
    (sem_AGViewSelItf_AGItf (sem_ViewSel _viewSel ) )
-- semantic domain
type T_AGViewSelItf  = ( PP_Doc)
data Inh_AGViewSelItf  = Inh_AGViewSelItf {}
data Syn_AGViewSelItf  = Syn_AGViewSelItf {pp_Syn_AGViewSelItf :: PP_Doc}
wrap_AGViewSelItf :: T_AGViewSelItf  ->
                     Inh_AGViewSelItf  ->
                     Syn_AGViewSelItf 
wrap_AGViewSelItf sem (Inh_AGViewSelItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_AGViewSelItf _lhsOpp ))
sem_AGViewSelItf_AGItf :: T_ViewSel  ->
                          T_AGViewSelItf 
sem_AGViewSelItf_AGItf viewSel_  =
    (let _lhsOpp :: PP_Doc
         _viewSelIpp :: PP_Doc
         -- use rule "build/ruler2/ViewSel/PrettyPrint.ag"(line 34, column 53)
         _lhsOpp =
             _viewSelIpp
         ( _viewSelIpp) =
             viewSel_ 
     in  ( _lhsOpp))
-- AGViewSelsItf -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child viewSels       : ViewSels 
-}
-- cata
sem_AGViewSelsItf :: AGViewSelsItf  ->
                     T_AGViewSelsItf 
sem_AGViewSelsItf (AGViewSelsItf_AGItf _viewSels )  =
    (sem_AGViewSelsItf_AGItf (sem_ViewSels _viewSels ) )
-- semantic domain
type T_AGViewSelsItf  = ( PP_Doc)
data Inh_AGViewSelsItf  = Inh_AGViewSelsItf {}
data Syn_AGViewSelsItf  = Syn_AGViewSelsItf {pp_Syn_AGViewSelsItf :: PP_Doc}
wrap_AGViewSelsItf :: T_AGViewSelsItf  ->
                      Inh_AGViewSelsItf  ->
                      Syn_AGViewSelsItf 
wrap_AGViewSelsItf sem (Inh_AGViewSelsItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_AGViewSelsItf _lhsOpp ))
sem_AGViewSelsItf_AGItf :: T_ViewSels  ->
                           T_AGViewSelsItf 
sem_AGViewSelsItf_AGItf viewSels_  =
    (let _lhsOpp :: PP_Doc
         _viewSelsIpp :: PP_Doc
         -- use rule "build/ruler2/ViewSel/PrettyPrint.ag"(line 34, column 53)
         _lhsOpp =
             _viewSelsIpp
         ( _viewSelsIpp) =
             viewSels_ 
     in  ( _lhsOpp))
-- NmSel -------------------------------------------------------
{-
   alternatives:
      alternative All:
      alternative Nms:
         child nms            : {[Nm]}
-}
-- cata
sem_NmSel :: NmSel  ->
             T_NmSel 
sem_NmSel (NmSel_All )  =
    (sem_NmSel_All )
sem_NmSel (NmSel_Nms _nms )  =
    (sem_NmSel_Nms _nms )
-- semantic domain
type T_NmSel  = ( )
sem_NmSel_All :: T_NmSel 
sem_NmSel_All  =
    (let 
     in  ( ))
sem_NmSel_Nms :: ([Nm]) ->
                 T_NmSel 
sem_NmSel_Nms nms_  =
    (let 
     in  ( ))
-- RlSel -------------------------------------------------------
{-
   alternatives:
      alternative Sel:
         child vwSel          : ViewSels 
         child rsSel          : NmSel 
         child rlSel          : NmSel 
-}
-- cata
sem_RlSel :: RlSel  ->
             T_RlSel 
sem_RlSel (RlSel_Sel _vwSel _rsSel _rlSel )  =
    (sem_RlSel_Sel (sem_ViewSels _vwSel ) (sem_NmSel _rsSel ) (sem_NmSel _rlSel ) )
-- semantic domain
type T_RlSel  = ( )
sem_RlSel_Sel :: T_ViewSels  ->
                 T_NmSel  ->
                 T_NmSel  ->
                 T_RlSel 
sem_RlSel_Sel vwSel_ rsSel_ rlSel_  =
    (let _vwSelIpp :: PP_Doc
         ( _vwSelIpp) =
             vwSel_ 
     in  ( ))
-- ViewSel -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative All:
      alternative Range:
         child vwFr           : ViewSel 
         child vwTo           : ViewSel 
      alternative Renamed:
         child nmNew          : {Nm}
         child nm             : {Nm}
      alternative View:
         child nm             : {Nm}
-}
-- cata
sem_ViewSel :: ViewSel  ->
               T_ViewSel 
sem_ViewSel (ViewSel_All )  =
    (sem_ViewSel_All )
sem_ViewSel (ViewSel_Range _vwFr _vwTo )  =
    (sem_ViewSel_Range (sem_ViewSel _vwFr ) (sem_ViewSel _vwTo ) )
sem_ViewSel (ViewSel_Renamed _nmNew _nm )  =
    (sem_ViewSel_Renamed _nmNew _nm )
sem_ViewSel (ViewSel_View _nm )  =
    (sem_ViewSel_View _nm )
-- semantic domain
type T_ViewSel  = ( PP_Doc)
sem_ViewSel_All :: T_ViewSel 
sem_ViewSel_All  =
    (let _lhsOpp :: PP_Doc
         -- "build/ruler2/ViewSel/PrettyPrint.ag"(line 37, column 21)
         _lhsOpp =
             pp "*"
     in  ( _lhsOpp))
sem_ViewSel_Range :: T_ViewSel  ->
                     T_ViewSel  ->
                     T_ViewSel 
sem_ViewSel_Range vwFr_ vwTo_  =
    (let _lhsOpp :: PP_Doc
         _vwFrIpp :: PP_Doc
         _vwToIpp :: PP_Doc
         -- "build/ruler2/ViewSel/PrettyPrint.ag"(line 39, column 21)
         _lhsOpp =
             _vwFrIpp >#< ".." >#< _vwToIpp
         ( _vwFrIpp) =
             vwFr_ 
         ( _vwToIpp) =
             vwTo_ 
     in  ( _lhsOpp))
sem_ViewSel_Renamed :: Nm ->
                       Nm ->
                       T_ViewSel 
sem_ViewSel_Renamed nmNew_ nm_  =
    (let _lhsOpp :: PP_Doc
         -- use rule "build/ruler2/ViewSel/PrettyPrint.ag"(line 34, column 53)
         _lhsOpp =
             empty
     in  ( _lhsOpp))
sem_ViewSel_View :: Nm ->
                    T_ViewSel 
sem_ViewSel_View nm_  =
    (let _lhsOpp :: PP_Doc
         -- "build/ruler2/ViewSel/PrettyPrint.ag"(line 38, column 21)
         _lhsOpp =
             pp nm_
     in  ( _lhsOpp))
-- ViewSels ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : ViewSel 
         child tl             : ViewSels 
      alternative Nil:
-}
-- cata
sem_ViewSels :: ViewSels  ->
                T_ViewSels 
sem_ViewSels list  =
    (Prelude.foldr sem_ViewSels_Cons sem_ViewSels_Nil (Prelude.map sem_ViewSel list) )
-- semantic domain
type T_ViewSels  = ( PP_Doc)
sem_ViewSels_Cons :: T_ViewSel  ->
                     T_ViewSels  ->
                     T_ViewSels 
sem_ViewSels_Cons hd_ tl_  =
    (let _lhsOpp :: PP_Doc
         _hdIpp :: PP_Doc
         _tlIpp :: PP_Doc
         -- "build/ruler2/ViewSel/PrettyPrint.ag"(line 42, column 21)
         _lhsOpp =
             _hdIpp >|< "," >#< _tlIpp
         ( _hdIpp) =
             hd_ 
         ( _tlIpp) =
             tl_ 
     in  ( _lhsOpp))
sem_ViewSels_Nil :: T_ViewSels 
sem_ViewSels_Nil  =
    (let _lhsOpp :: PP_Doc
         -- use rule "build/ruler2/ViewSel/PrettyPrint.ag"(line 34, column 53)
         _lhsOpp =
             empty
     in  ( _lhsOpp))