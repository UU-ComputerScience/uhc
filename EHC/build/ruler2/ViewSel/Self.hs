

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/Self.ag)
module ViewSel.Self(viewSelsSelfT, rlSelSelfT) where

import EH.Util.Nm
import EH.Util.DependencyGraph
import ViewSel.ViewSel









wrapViewSelsT :: T_ViewSels -> Syn_AGViewSelsItf
wrapViewSelsT vs
  = let r1 = sem_AGViewSelsItf_AGItf vs
    in  wrap_AGViewSelsItf r1
            (Inh_AGViewSelsItf)

viewSelsSelfT :: ViewSels -> ViewSels
viewSelsSelfT vs
  = self_Syn_AGViewSelsItf r
  where r = wrapViewSelsT (sem_ViewSels vs)

wrapRlSelT :: T_RlSel -> Syn_AGRlSelItf
wrapRlSelT  vs
  = let r1 = sem_AGRlSelItf_AGItf vs
    in  wrap_AGRlSelItf r1
            (Inh_AGRlSelItf)

rlSelSelfT :: RlSel -> RlSel
rlSelSelfT vs
  = self_Syn_AGRlSelItf r
  where r = wrapRlSelT (sem_RlSel vs)


-- AGRlSelItf --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : RlSel 
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
type T_AGRlSelItf  = ( RlSel )
data Inh_AGRlSelItf  = Inh_AGRlSelItf {}
data Syn_AGRlSelItf  = Syn_AGRlSelItf {self_Syn_AGRlSelItf :: RlSel }
wrap_AGRlSelItf :: T_AGRlSelItf  ->
                   Inh_AGRlSelItf  ->
                   Syn_AGRlSelItf 
wrap_AGRlSelItf sem (Inh_AGRlSelItf )  =
    (let ( _lhsOself) = sem 
     in  (Syn_AGRlSelItf _lhsOself ))
sem_AGRlSelItf_AGItf :: T_RlSel  ->
                        T_AGRlSelItf 
sem_AGRlSelItf_AGItf rlSel_  =
    (let _lhsOself :: RlSel 
         _rlSelIself :: RlSel 
         -- copy rule (up)
         _lhsOself =
             _rlSelIself
         ( _rlSelIself) =
             rlSel_ 
     in  ( _lhsOself))
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
    (let _viewSelIself :: ViewSel 
         ( _viewSelIself) =
             viewSel_ 
     in  ( ))
-- AGViewSelsItf -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : ViewSels 
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
type T_AGViewSelsItf  = ( ViewSels )
data Inh_AGViewSelsItf  = Inh_AGViewSelsItf {}
data Syn_AGViewSelsItf  = Syn_AGViewSelsItf {self_Syn_AGViewSelsItf :: ViewSels }
wrap_AGViewSelsItf :: T_AGViewSelsItf  ->
                      Inh_AGViewSelsItf  ->
                      Syn_AGViewSelsItf 
wrap_AGViewSelsItf sem (Inh_AGViewSelsItf )  =
    (let ( _lhsOself) = sem 
     in  (Syn_AGViewSelsItf _lhsOself ))
sem_AGViewSelsItf_AGItf :: T_ViewSels  ->
                           T_AGViewSelsItf 
sem_AGViewSelsItf_AGItf viewSels_  =
    (let _lhsOself :: ViewSels 
         _viewSelsIself :: ViewSels 
         -- copy rule (up)
         _lhsOself =
             _viewSelsIself
         ( _viewSelsIself) =
             viewSels_ 
     in  ( _lhsOself))
-- NmSel -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative All:
         visit 0:
            local self        : _
      alternative Nms:
         child nms            : {[Nm]}
         visit 0:
            local self        : _
-}
-- cata
sem_NmSel :: NmSel  ->
             T_NmSel 
sem_NmSel (NmSel_All )  =
    (sem_NmSel_All )
sem_NmSel (NmSel_Nms _nms )  =
    (sem_NmSel_Nms _nms )
-- semantic domain
type T_NmSel  = ( NmSel )
sem_NmSel_All :: T_NmSel 
sem_NmSel_All  =
    (let _lhsOself :: NmSel 
         -- self rule
         _self =
             NmSel_All
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_NmSel_Nms :: ([Nm]) ->
                 T_NmSel 
sem_NmSel_Nms nms_  =
    (let _lhsOself :: NmSel 
         -- self rule
         _self =
             NmSel_Nms nms_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- RlSel -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Sel:
         child vwSel          : ViewSels 
         child rsSel          : NmSel 
         child rlSel          : NmSel 
         visit 0:
            local self        : _
-}
-- cata
sem_RlSel :: RlSel  ->
             T_RlSel 
sem_RlSel (RlSel_Sel _vwSel _rsSel _rlSel )  =
    (sem_RlSel_Sel (sem_ViewSels _vwSel ) (sem_NmSel _rsSel ) (sem_NmSel _rlSel ) )
-- semantic domain
type T_RlSel  = ( RlSel )
sem_RlSel_Sel :: T_ViewSels  ->
                 T_NmSel  ->
                 T_NmSel  ->
                 T_RlSel 
sem_RlSel_Sel vwSel_ rsSel_ rlSel_  =
    (let _lhsOself :: RlSel 
         _vwSelIself :: ViewSels 
         _rsSelIself :: NmSel 
         _rlSelIself :: NmSel 
         -- self rule
         _self =
             RlSel_Sel _vwSelIself _rsSelIself _rlSelIself
         -- self rule
         _lhsOself =
             _self
         ( _vwSelIself) =
             vwSel_ 
         ( _rsSelIself) =
             rsSel_ 
         ( _rlSelIself) =
             rlSel_ 
     in  ( _lhsOself))
-- ViewSel -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative All:
         visit 0:
            local self        : _
      alternative Range:
         child vwFr           : ViewSel 
         child vwTo           : ViewSel 
         visit 0:
            local self        : _
      alternative Renamed:
         child nmNew          : {Nm}
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative View:
         child nm             : {Nm}
         visit 0:
            local self        : _
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
type T_ViewSel  = ( ViewSel )
sem_ViewSel_All :: T_ViewSel 
sem_ViewSel_All  =
    (let _lhsOself :: ViewSel 
         -- self rule
         _self =
             ViewSel_All
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ViewSel_Range :: T_ViewSel  ->
                     T_ViewSel  ->
                     T_ViewSel 
sem_ViewSel_Range vwFr_ vwTo_  =
    (let _lhsOself :: ViewSel 
         _vwFrIself :: ViewSel 
         _vwToIself :: ViewSel 
         -- self rule
         _self =
             ViewSel_Range _vwFrIself _vwToIself
         -- self rule
         _lhsOself =
             _self
         ( _vwFrIself) =
             vwFr_ 
         ( _vwToIself) =
             vwTo_ 
     in  ( _lhsOself))
sem_ViewSel_Renamed :: Nm ->
                       Nm ->
                       T_ViewSel 
sem_ViewSel_Renamed nmNew_ nm_  =
    (let _lhsOself :: ViewSel 
         -- self rule
         _self =
             ViewSel_Renamed nmNew_ nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ViewSel_View :: Nm ->
                    T_ViewSel 
sem_ViewSel_View nm_  =
    (let _lhsOself :: ViewSel 
         -- self rule
         _self =
             ViewSel_View nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ViewSels ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : ViewSel 
         child tl             : ViewSels 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_ViewSels :: ViewSels  ->
                T_ViewSels 
sem_ViewSels list  =
    (Prelude.foldr sem_ViewSels_Cons sem_ViewSels_Nil (Prelude.map sem_ViewSel list) )
-- semantic domain
type T_ViewSels  = ( ViewSels )
sem_ViewSels_Cons :: T_ViewSel  ->
                     T_ViewSels  ->
                     T_ViewSels 
sem_ViewSels_Cons hd_ tl_  =
    (let _lhsOself :: ViewSels 
         _hdIself :: ViewSel 
         _tlIself :: ViewSels 
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_ViewSels_Nil :: T_ViewSels 
sem_ViewSels_Nil  =
    (let _lhsOself :: ViewSels 
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))