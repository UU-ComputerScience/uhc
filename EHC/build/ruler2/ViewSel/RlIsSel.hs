

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/RlIsSel.ag)
module ViewSel.RlIsSel(rlSelIsSel, RlSelIsSel) where

import qualified Data.Set as Set
import EH.Util.Nm
import Common
import EH.Util.DependencyGraph
import ViewSel.ViewSel









wrapRlSel :: DpdGr Nm -> RlSel -> Syn_AGRlSelItf
wrapRlSel vwDpdGr vs
  = let r1 = sem_AGRlSelItf (AGRlSelItf_AGItf vs)
    in  wrap_AGRlSelItf r1
            (Inh_AGRlSelItf {vwDpdGr_Inh_AGRlSelItf = vwDpdGr
                            })

rlSelIsSel :: DpdGr Nm -> RlSel -> RlSelIsSel
rlSelIsSel vwDpdGr rs
  = rlSelIsSel_Syn_AGRlSelItf r
  where r = wrapRlSel vwDpdGr rs



type RlSelIsSel = Nm -> Nm -> Nm -> Bool

-- AGRlSelItf --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         rlSelIsSel           : RlSelIsSel
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
type T_AGRlSelItf  = (DpdGr Nm) ->
                     ( RlSelIsSel)
data Inh_AGRlSelItf  = Inh_AGRlSelItf {vwDpdGr_Inh_AGRlSelItf :: (DpdGr Nm)}
data Syn_AGRlSelItf  = Syn_AGRlSelItf {rlSelIsSel_Syn_AGRlSelItf :: RlSelIsSel}
wrap_AGRlSelItf :: T_AGRlSelItf  ->
                   Inh_AGRlSelItf  ->
                   Syn_AGRlSelItf 
wrap_AGRlSelItf sem (Inh_AGRlSelItf _lhsIvwDpdGr )  =
    (let ( _lhsOrlSelIsSel) = sem _lhsIvwDpdGr 
     in  (Syn_AGRlSelItf _lhsOrlSelIsSel ))
sem_AGRlSelItf_AGItf :: T_RlSel  ->
                        T_AGRlSelItf 
sem_AGRlSelItf_AGItf rlSel_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOrlSelIsSel :: RlSelIsSel
              _rlSelOvwDpdGr :: (DpdGr Nm)
              _rlSelIrlSelIsSel :: RlSelIsSel
              -- copy rule (up)
              _lhsOrlSelIsSel =
                  _rlSelIrlSelIsSel
              -- copy rule (down)
              _rlSelOvwDpdGr =
                  _lhsIvwDpdGr
              ( _rlSelIrlSelIsSel) =
                  rlSel_ _rlSelOvwDpdGr 
          in  ( _lhsOrlSelIsSel)))
-- AGViewSelItf ------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         vwSelNmS             : Set.Set Nm
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
type T_AGViewSelItf  = (DpdGr Nm) ->
                       ( (Set.Set Nm))
sem_AGViewSelItf_AGItf :: T_ViewSel  ->
                          T_AGViewSelItf 
sem_AGViewSelItf_AGItf viewSel_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              _viewSelOvwDpdGr :: (DpdGr Nm)
              _viewSelIvwSelNmS :: (Set.Set Nm)
              -- use rule "build/ruler2/ViewSel/NmSAG.ag"(line 1, column 59)
              _lhsOvwSelNmS =
                  _viewSelIvwSelNmS
              -- copy rule (down)
              _viewSelOvwDpdGr =
                  _lhsIvwDpdGr
              ( _viewSelIvwSelNmS) =
                  viewSel_ _viewSelOvwDpdGr 
          in  ( _lhsOvwSelNmS)))
-- AGViewSelsItf -----------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         vwSelNmS             : Set.Set Nm
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
type T_AGViewSelsItf  = (DpdGr Nm) ->
                        ( (Set.Set Nm))
sem_AGViewSelsItf_AGItf :: T_ViewSels  ->
                           T_AGViewSelsItf 
sem_AGViewSelsItf_AGItf viewSels_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              _viewSelsOvwDpdGr :: (DpdGr Nm)
              _viewSelsIvwSelNmS :: (Set.Set Nm)
              -- use rule "build/ruler2/ViewSel/NmSAG.ag"(line 1, column 59)
              _lhsOvwSelNmS =
                  _viewSelsIvwSelNmS
              -- copy rule (down)
              _viewSelsOvwDpdGr =
                  _lhsIvwDpdGr
              ( _viewSelsIvwSelNmS) =
                  viewSels_ _viewSelsOvwDpdGr 
          in  ( _lhsOvwSelNmS)))
-- NmSel -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         isSel                : Nm -> Bool
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
type T_NmSel  = ( (Nm -> Bool))
sem_NmSel_All :: T_NmSel 
sem_NmSel_All  =
    (let _lhsOisSel :: (Nm -> Bool)
         -- "build/ruler2/ViewSel/RlIsSel.ag"(line 50, column 21)
         _lhsOisSel =
             const True
     in  ( _lhsOisSel))
sem_NmSel_Nms :: ([Nm]) ->
                 T_NmSel 
sem_NmSel_Nms nms_  =
    (let _lhsOisSel :: (Nm -> Bool)
         -- "build/ruler2/ViewSel/RlIsSel.ag"(line 51, column 21)
         _lhsOisSel =
             (`elem` nms_)
     in  ( _lhsOisSel))
-- RlSel -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         rlSelIsSel           : RlSelIsSel
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
type T_RlSel  = (DpdGr Nm) ->
                ( RlSelIsSel)
sem_RlSel_Sel :: T_ViewSels  ->
                 T_NmSel  ->
                 T_NmSel  ->
                 T_RlSel 
sem_RlSel_Sel vwSel_ rsSel_ rlSel_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOrlSelIsSel :: RlSelIsSel
              _vwSelOvwDpdGr :: (DpdGr Nm)
              _vwSelIvwSelNmS :: (Set.Set Nm)
              _rsSelIisSel :: (Nm -> Bool)
              _rlSelIisSel :: (Nm -> Bool)
              -- "build/ruler2/ViewSel/RlIsSel.ag"(line 42, column 21)
              _lhsOrlSelIsSel =
                  \nVw nRs nRl
                      ->  (nVw == nmAny || nVw `Set.member` _vwSelIvwSelNmS)
                          && (nRs == nmAny || _rsSelIisSel nRs)
                          && (nRl == nmAny || _rlSelIisSel nRl)
              -- copy rule (down)
              _vwSelOvwDpdGr =
                  _lhsIvwDpdGr
              ( _vwSelIvwSelNmS) =
                  vwSel_ _vwSelOvwDpdGr 
              ( _rsSelIisSel) =
                  rsSel_ 
              ( _rlSelIisSel) =
                  rlSel_ 
          in  ( _lhsOrlSelIsSel)))
-- ViewSel -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         vwSelNmS             : Set.Set Nm
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
type T_ViewSel  = (DpdGr Nm) ->
                  ( (Set.Set Nm))
sem_ViewSel_All :: T_ViewSel 
sem_ViewSel_All  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              -- "build/ruler2/ViewSel/NmSAG.ag"(line 5, column 21)
              _lhsOvwSelNmS =
                  dgVertices _lhsIvwDpdGr
          in  ( _lhsOvwSelNmS)))
sem_ViewSel_Range :: T_ViewSel  ->
                     T_ViewSel  ->
                     T_ViewSel 
sem_ViewSel_Range vwFr_ vwTo_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              _vwFrOvwDpdGr :: (DpdGr Nm)
              _vwToOvwDpdGr :: (DpdGr Nm)
              _vwFrIvwSelNmS :: (Set.Set Nm)
              _vwToIvwSelNmS :: (Set.Set Nm)
              -- "build/ruler2/ViewSel/NmSAG.ag"(line 6, column 21)
              _lhsOvwSelNmS =
                  let toS = Set.fold (\n r -> dgReachableFrom _lhsIvwDpdGr n `Set.union` r) Set.empty _vwToIvwSelNmS
                      frSL = [ dgReachableTo _lhsIvwDpdGr n | n <- Set.toList _vwFrIvwSelNmS ]
                  in  Set.filter (\n -> any (\s -> n `Set.member` s) frSL) toS
              -- copy rule (down)
              _vwFrOvwDpdGr =
                  _lhsIvwDpdGr
              -- copy rule (down)
              _vwToOvwDpdGr =
                  _lhsIvwDpdGr
              ( _vwFrIvwSelNmS) =
                  vwFr_ _vwFrOvwDpdGr 
              ( _vwToIvwSelNmS) =
                  vwTo_ _vwToOvwDpdGr 
          in  ( _lhsOvwSelNmS)))
sem_ViewSel_Renamed :: Nm ->
                       Nm ->
                       T_ViewSel 
sem_ViewSel_Renamed nmNew_ nm_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              -- "build/ruler2/ViewSel/NmSAG.ag"(line 4, column 21)
              _lhsOvwSelNmS =
                  Set.singleton nm_
          in  ( _lhsOvwSelNmS)))
sem_ViewSel_View :: Nm ->
                    T_ViewSel 
sem_ViewSel_View nm_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              -- "build/ruler2/ViewSel/NmSAG.ag"(line 4, column 21)
              _lhsOvwSelNmS =
                  Set.singleton nm_
          in  ( _lhsOvwSelNmS)))
-- ViewSels ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
      synthesized attribute:
         vwSelNmS             : Set.Set Nm
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
type T_ViewSels  = (DpdGr Nm) ->
                   ( (Set.Set Nm))
sem_ViewSels_Cons :: T_ViewSel  ->
                     T_ViewSels  ->
                     T_ViewSels 
sem_ViewSels_Cons hd_ tl_  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              _hdOvwDpdGr :: (DpdGr Nm)
              _tlOvwDpdGr :: (DpdGr Nm)
              _hdIvwSelNmS :: (Set.Set Nm)
              _tlIvwSelNmS :: (Set.Set Nm)
              -- use rule "build/ruler2/ViewSel/NmSAG.ag"(line 1, column 59)
              _lhsOvwSelNmS =
                  _hdIvwSelNmS `Set.union` _tlIvwSelNmS
              -- copy rule (down)
              _hdOvwDpdGr =
                  _lhsIvwDpdGr
              -- copy rule (down)
              _tlOvwDpdGr =
                  _lhsIvwDpdGr
              ( _hdIvwSelNmS) =
                  hd_ _hdOvwDpdGr 
              ( _tlIvwSelNmS) =
                  tl_ _tlOvwDpdGr 
          in  ( _lhsOvwSelNmS)))
sem_ViewSels_Nil :: T_ViewSels 
sem_ViewSels_Nil  =
    (\ _lhsIvwDpdGr ->
         (let _lhsOvwSelNmS :: (Set.Set Nm)
              -- use rule "build/ruler2/ViewSel/NmSAG.ag"(line 1, column 59)
              _lhsOvwSelNmS =
                  Set.empty
          in  ( _lhsOvwSelNmS)))