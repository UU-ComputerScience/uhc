

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/NmS.ag)
module ViewSel.NmS(viewSelsNmS, viewSelNmS) where

import qualified Data.Set as Set
import EH.Util.Nm
import EH.Util.DependencyGraph
import ViewSel.ViewSel









viewSelNmS :: DpdGr Nm -> ViewSel -> Set.Set Nm
viewSelNmS vwDpdGr vs
  = vwSelNmS_Syn_AGViewSelItf r2
  where r1 = sem_AGViewSelItf (AGViewSelItf_AGItf vs)
        r2 = wrap_AGViewSelItf r1
                (Inh_AGViewSelItf {vwDpdGr_Inh_AGViewSelItf = vwDpdGr
                                   })

viewSelsNmS :: DpdGr Nm -> ViewSels -> Set.Set Nm
viewSelsNmS vwDpdGr vs
  = vwSelNmS_Syn_AGViewSelsItf r2
  where r1 = sem_AGViewSelsItf (AGViewSelsItf_AGItf vs)
        r2 = wrap_AGViewSelsItf r1
                (Inh_AGViewSelsItf {vwDpdGr_Inh_AGViewSelsItf = vwDpdGr
                                   })
-- AGRlSelItf --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         vwDpdGr              : DpdGr Nm
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
                     ( )
sem_AGRlSelItf_AGItf :: T_RlSel  ->
                        T_AGRlSelItf 
sem_AGRlSelItf_AGItf rlSel_  =
    (\ _lhsIvwDpdGr ->
         (let _rlSelOvwDpdGr :: (DpdGr Nm)
              -- copy rule (down)
              _rlSelOvwDpdGr =
                  _lhsIvwDpdGr
          in  ( )))
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
data Inh_AGViewSelItf  = Inh_AGViewSelItf {vwDpdGr_Inh_AGViewSelItf :: (DpdGr Nm)}
data Syn_AGViewSelItf  = Syn_AGViewSelItf {vwSelNmS_Syn_AGViewSelItf :: (Set.Set Nm)}
wrap_AGViewSelItf :: T_AGViewSelItf  ->
                     Inh_AGViewSelItf  ->
                     Syn_AGViewSelItf 
wrap_AGViewSelItf sem (Inh_AGViewSelItf _lhsIvwDpdGr )  =
    (let ( _lhsOvwSelNmS) = sem _lhsIvwDpdGr 
     in  (Syn_AGViewSelItf _lhsOvwSelNmS ))
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
data Inh_AGViewSelsItf  = Inh_AGViewSelsItf {vwDpdGr_Inh_AGViewSelsItf :: (DpdGr Nm)}
data Syn_AGViewSelsItf  = Syn_AGViewSelsItf {vwSelNmS_Syn_AGViewSelsItf :: (Set.Set Nm)}
wrap_AGViewSelsItf :: T_AGViewSelsItf  ->
                      Inh_AGViewSelsItf  ->
                      Syn_AGViewSelsItf 
wrap_AGViewSelsItf sem (Inh_AGViewSelsItf _lhsIvwDpdGr )  =
    (let ( _lhsOvwSelNmS) = sem _lhsIvwDpdGr 
     in  (Syn_AGViewSelsItf _lhsOvwSelNmS ))
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
      inherited attribute:
         vwDpdGr              : DpdGr Nm
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
                ( )
sem_RlSel_Sel :: T_ViewSels  ->
                 T_NmSel  ->
                 T_NmSel  ->
                 T_RlSel 
sem_RlSel_Sel vwSel_ rsSel_ rlSel_  =
    (\ _lhsIvwDpdGr ->
         (let _vwSelOvwDpdGr :: (DpdGr Nm)
              _vwSelIvwSelNmS :: (Set.Set Nm)
              -- copy rule (down)
              _vwSelOvwDpdGr =
                  _lhsIvwDpdGr
              ( _vwSelIvwSelNmS) =
                  vwSel_ _vwSelOvwDpdGr 
          in  ( )))
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