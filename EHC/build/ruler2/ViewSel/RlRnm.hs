

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/RlRnm.ag)
module ViewSel.RlRnm(rlSelRnmM, rsSelMapVwNm) where

import qualified Data.Map as Map
import EH.Util.Nm
import ViewSel.ViewSel









wrapRlSel :: RlSel -> Syn_AGRlSelItf
wrapRlSel  vs
  = let r1 = sem_AGRlSelItf (AGRlSelItf_AGItf vs)
    in  wrap_AGRlSelItf r1
            (Inh_AGRlSelItf)

rlSelRnmM :: RlSel -> Map.Map Nm Nm
rlSelRnmM rs
  = vwRnmM_Syn_AGRlSelItf r
  where r = wrapRlSel rs



rsSelMapVwNm :: Maybe RlSel -> Nm -> Nm
rsSelMapVwNm mbRlSel vNm = maybe vNm (maybe vNm id . Map.lookup vNm . rlSelRnmM) mbRlSel

-- AGRlSelItf --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vwRnmM               : Map.Map Nm Nm
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
type T_AGRlSelItf  = ( (Map.Map Nm Nm))
data Inh_AGRlSelItf  = Inh_AGRlSelItf {}
data Syn_AGRlSelItf  = Syn_AGRlSelItf {vwRnmM_Syn_AGRlSelItf :: (Map.Map Nm Nm)}
wrap_AGRlSelItf :: T_AGRlSelItf  ->
                   Inh_AGRlSelItf  ->
                   Syn_AGRlSelItf 
wrap_AGRlSelItf sem (Inh_AGRlSelItf )  =
    (let ( _lhsOvwRnmM) = sem 
     in  (Syn_AGRlSelItf _lhsOvwRnmM ))
sem_AGRlSelItf_AGItf :: T_RlSel  ->
                        T_AGRlSelItf 
sem_AGRlSelItf_AGItf rlSel_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         _rlSelIvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             _rlSelIvwRnmM
         ( _rlSelIvwRnmM) =
             rlSel_ 
     in  ( _lhsOvwRnmM))
-- AGViewSelItf ------------------------------------------------
{-
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
type T_AGViewSelItf  = ( )
sem_AGViewSelItf_AGItf :: T_ViewSel  ->
                          T_AGViewSelItf 
sem_AGViewSelItf_AGItf viewSel_  =
    (let _viewSelIvwRnmM :: (Map.Map Nm Nm)
         ( _viewSelIvwRnmM) =
             viewSel_ 
     in  ( ))
-- AGViewSelsItf -----------------------------------------------
{-
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
type T_AGViewSelsItf  = ( )
sem_AGViewSelsItf_AGItf :: T_ViewSels  ->
                           T_AGViewSelsItf 
sem_AGViewSelsItf_AGItf viewSels_  =
    (let _viewSelsIvwRnmM :: (Map.Map Nm Nm)
         ( _viewSelsIvwRnmM) =
             viewSels_ 
     in  ( ))
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
   visit 0:
      synthesized attribute:
         vwRnmM               : Map.Map Nm Nm
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
type T_RlSel  = ( (Map.Map Nm Nm))
sem_RlSel_Sel :: T_ViewSels  ->
                 T_NmSel  ->
                 T_NmSel  ->
                 T_RlSel 
sem_RlSel_Sel vwSel_ rsSel_ rlSel_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         _vwSelIvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             _vwSelIvwRnmM
         ( _vwSelIvwRnmM) =
             vwSel_ 
     in  ( _lhsOvwRnmM))
-- ViewSel -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vwRnmM               : Map.Map Nm Nm
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
type T_ViewSel  = ( (Map.Map Nm Nm))
sem_ViewSel_All :: T_ViewSel 
sem_ViewSel_All  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             Map.empty
     in  ( _lhsOvwRnmM))
sem_ViewSel_Range :: T_ViewSel  ->
                     T_ViewSel  ->
                     T_ViewSel 
sem_ViewSel_Range vwFr_ vwTo_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         _vwFrIvwRnmM :: (Map.Map Nm Nm)
         _vwToIvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             _vwFrIvwRnmM `Map.union` _vwToIvwRnmM
         ( _vwFrIvwRnmM) =
             vwFr_ 
         ( _vwToIvwRnmM) =
             vwTo_ 
     in  ( _lhsOvwRnmM))
sem_ViewSel_Renamed :: Nm ->
                       Nm ->
                       T_ViewSel 
sem_ViewSel_Renamed nmNew_ nm_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         -- "build/ruler2/ViewSel/RlRnm.ag"(line 38, column 21)
         _lhsOvwRnmM =
             Map.singleton nm_ nmNew_
     in  ( _lhsOvwRnmM))
sem_ViewSel_View :: Nm ->
                    T_ViewSel 
sem_ViewSel_View nm_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             Map.empty
     in  ( _lhsOvwRnmM))
-- ViewSels ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         vwRnmM               : Map.Map Nm Nm
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
type T_ViewSels  = ( (Map.Map Nm Nm))
sem_ViewSels_Cons :: T_ViewSel  ->
                     T_ViewSels  ->
                     T_ViewSels 
sem_ViewSels_Cons hd_ tl_  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         _hdIvwRnmM :: (Map.Map Nm Nm)
         _tlIvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             _hdIvwRnmM `Map.union` _tlIvwRnmM
         ( _hdIvwRnmM) =
             hd_ 
         ( _tlIvwRnmM) =
             tl_ 
     in  ( _lhsOvwRnmM))
sem_ViewSels_Nil :: T_ViewSels 
sem_ViewSels_Nil  =
    (let _lhsOvwRnmM :: (Map.Map Nm Nm)
         -- use rule "build/ruler2/ViewSel/RlRnm.ag"(line 35, column 50)
         _lhsOvwRnmM =
             Map.empty
     in  ( _lhsOvwRnmM))