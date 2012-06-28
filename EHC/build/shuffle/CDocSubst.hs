

-- UUAGC 0.9.39.1 (src/shuffle/CDocSubst.ag)
module CDocSubst(cdocSubst) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import CDoc
import CDocCommon


wrapAGCDoc_T :: NmChMp -> T_AGCDocItf -> Syn_AGCDocItf
wrapAGCDoc_T nmChMp d
  = wrap_AGCDocItf d
      (Inh_AGCDocItf
         { nmChMp_Inh_AGCDocItf = nmChMp
         })

wrapCDoc :: NmChMp -> CDoc -> Syn_AGCDocItf
wrapCDoc m d = wrapAGCDoc_T m (sem_AGCDocItf (AGCDocItf_AGItf d))

cdocSubst :: NmChMp -> CDoc -> (CDoc,Set.Set Nm,ErrM)
cdocSubst m d
  = (sbRepl_Syn_AGCDocItf r,sbCRefS_Syn_AGCDocItf r,sbErrM_Syn_AGCDocItf r)
  where r = wrapCDoc m d
-- AGCDocItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nmChMp               : NmChMp
      synthesized attributes:
         sbCRefS              : Set.Set Nm
         sbErrM               : ErrM
         sbRepl               : CDoc 
   alternatives:
      alternative AGItf:
         child cdoc           : CDoc 
-}
-- cata
sem_AGCDocItf :: AGCDocItf  ->
                 T_AGCDocItf 
sem_AGCDocItf (AGCDocItf_AGItf _cdoc )  =
    (sem_AGCDocItf_AGItf (sem_CDoc _cdoc ) )
-- semantic domain
type T_AGCDocItf  = NmChMp ->
                    ( (Set.Set Nm),ErrM,CDoc )
data Inh_AGCDocItf  = Inh_AGCDocItf {nmChMp_Inh_AGCDocItf :: NmChMp}
data Syn_AGCDocItf  = Syn_AGCDocItf {sbCRefS_Syn_AGCDocItf :: (Set.Set Nm),sbErrM_Syn_AGCDocItf :: ErrM,sbRepl_Syn_AGCDocItf :: CDoc }
wrap_AGCDocItf :: T_AGCDocItf  ->
                  Inh_AGCDocItf  ->
                  Syn_AGCDocItf 
wrap_AGCDocItf sem (Inh_AGCDocItf _lhsInmChMp )  =
    (let ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl) = sem _lhsInmChMp 
     in  (Syn_AGCDocItf _lhsOsbCRefS _lhsOsbErrM _lhsOsbRepl ))
sem_AGCDocItf_AGItf :: T_CDoc  ->
                       T_AGCDocItf 
sem_AGCDocItf_AGItf cdoc_  =
    (\ _lhsInmChMp ->
         (let _cdocOcpos :: CPos
              _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              _cdocOnmChMp :: NmChMp
              _cdocIsbCRefS :: (Set.Set Nm)
              _cdocIsbErrM :: ErrM
              _cdocIsbRepl :: CDoc 
              -- "src/shuffle/CDocCommonAG.ag"(line 12, column 17)
              _cdocOcpos =
                  CPos emptyFPath 0
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  _cdocIsbCRefS
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  _cdocIsbErrM
              -- copy rule (up)
              _lhsOsbRepl =
                  _cdocIsbRepl
              -- copy rule (down)
              _cdocOnmChMp =
                  _lhsInmChMp
              ( _cdocIsbCRefS,_cdocIsbErrM,_cdocIsbRepl) =
                  cdoc_ _cdocOcpos _cdocOnmChMp 
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
-- CDoc --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cpos                 : CPos
         nmChMp               : NmChMp
      synthesized attributes:
         sbCRefS              : Set.Set Nm
         sbErrM               : ErrM
         sbRepl               : SELF 
   alternatives:
      alternative Emp:
         visit 0:
            local sbRepl      : _
      alternative Hor:
         child ldoc           : CDoc 
         child rdoc           : CDoc 
         visit 0:
            local sbRepl      : _
      alternative Inl:
         child uref           : {URef}
         visit 0:
            local sbRepl      : _
      alternative Pos:
         child cpos           : {CPos}
         child cdoc           : CDoc 
         visit 0:
            local sbRepl      : _
      alternative Ref:
         child cref           : {CRef}
         child mbVariantReqm  : {Maybe VariantReqm}
         child chDest         : {ChDest}
         visit 0:
            local _tup1       : {(CDoc,Set.Set Nm,ErrM)}
            local sbRepl      : _
      alternative Str:
         child str            : {String}
         visit 0:
            local sbRepl      : _
      alternative Ver:
         child ldoc           : CDoc 
         child rdoc           : CDoc 
         visit 0:
            local sbRepl      : _
-}
-- cata
sem_CDoc :: CDoc  ->
            T_CDoc 
sem_CDoc (CDoc_Emp )  =
    (sem_CDoc_Emp )
sem_CDoc (CDoc_Hor _ldoc _rdoc )  =
    (sem_CDoc_Hor (sem_CDoc _ldoc ) (sem_CDoc _rdoc ) )
sem_CDoc (CDoc_Inl _uref )  =
    (sem_CDoc_Inl _uref )
sem_CDoc (CDoc_Pos _cpos _cdoc )  =
    (sem_CDoc_Pos _cpos (sem_CDoc _cdoc ) )
sem_CDoc (CDoc_Ref _cref _mbVariantReqm _chDest )  =
    (sem_CDoc_Ref _cref _mbVariantReqm _chDest )
sem_CDoc (CDoc_Str _str )  =
    (sem_CDoc_Str _str )
sem_CDoc (CDoc_Ver _ldoc _rdoc )  =
    (sem_CDoc_Ver (sem_CDoc _ldoc ) (sem_CDoc _rdoc ) )
-- semantic domain
type T_CDoc  = CPos ->
               NmChMp ->
               ( (Set.Set Nm),ErrM,CDoc )
sem_CDoc_Emp :: T_CDoc 
sem_CDoc_Emp  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  Set.empty
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  Map.empty
              -- self rule
              _sbRepl =
                  CDoc_Emp
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Hor :: T_CDoc  ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Hor ldoc_ rdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              _ldocOcpos :: CPos
              _ldocOnmChMp :: NmChMp
              _rdocOcpos :: CPos
              _rdocOnmChMp :: NmChMp
              _ldocIsbCRefS :: (Set.Set Nm)
              _ldocIsbErrM :: ErrM
              _ldocIsbRepl :: CDoc 
              _rdocIsbCRefS :: (Set.Set Nm)
              _rdocIsbErrM :: ErrM
              _rdocIsbRepl :: CDoc 
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  _ldocIsbCRefS `Set.union` _rdocIsbCRefS
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  _ldocIsbErrM `Map.union` _rdocIsbErrM
              -- self rule
              _sbRepl =
                  CDoc_Hor _ldocIsbRepl _rdocIsbRepl
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
              -- copy rule (down)
              _ldocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _ldocOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _rdocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _rdocOnmChMp =
                  _lhsInmChMp
              ( _ldocIsbCRefS,_ldocIsbErrM,_ldocIsbRepl) =
                  ldoc_ _ldocOcpos _ldocOnmChMp 
              ( _rdocIsbCRefS,_rdocIsbErrM,_rdocIsbRepl) =
                  rdoc_ _rdocOcpos _rdocOnmChMp 
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Inl :: URef ->
                T_CDoc 
sem_CDoc_Inl uref_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  Set.empty
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  Map.empty
              -- self rule
              _sbRepl =
                  CDoc_Inl uref_
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Pos :: CPos ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Pos cpos_ cdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _cdocOcpos :: CPos
              _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              _cdocOnmChMp :: NmChMp
              _cdocIsbCRefS :: (Set.Set Nm)
              _cdocIsbErrM :: ErrM
              _cdocIsbRepl :: CDoc 
              -- "src/shuffle/CDocCommonAG.ag"(line 15, column 17)
              _cdocOcpos =
                  cpos_
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  _cdocIsbCRefS
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  _cdocIsbErrM
              -- self rule
              _sbRepl =
                  CDoc_Pos cpos_ _cdocIsbRepl
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
              -- copy rule (down)
              _cdocOnmChMp =
                  _lhsInmChMp
              ( _cdocIsbCRefS,_cdocIsbErrM,_cdocIsbRepl) =
                  cdoc_ _cdocOcpos _cdocOnmChMp 
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Ref :: CRef ->
                (Maybe VariantReqm) ->
                ChDest ->
                T_CDoc 
sem_CDoc_Ref cref_ mbVariantReqm_ chDest_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let __tup1 :: ((CDoc,Set.Set Nm,ErrM))
              _lhsOsbRepl :: CDoc 
              _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              -- "src/shuffle/CDocSubst.ag"(line 45, column 33)
              __tup1 =
                  case Map.lookup (cref_) _lhsInmChMp of
                    Just i
                      -> case maybe (nciMbCDoc i) (nciMkCDoc i) mbVariantReqm_ of
                           Just d -> (r,Set.insert cref_ s,e)
                                  where (r,s,e) = cdocSubst (Map.delete cref_ _lhsInmChMp) d
                           _      -> (CDoc_Emp,Set.empty,Map.empty)
                    _ -> (_sbRepl,Set.empty,Map.singleton _lhsIcpos (Err_UndefNm _lhsIcpos "chunk reference" [cref_]))
              -- "src/shuffle/CDocSubst.ag"(line 45, column 33)
              (_lhsOsbRepl,_,_) =
                  __tup1
              -- "src/shuffle/CDocSubst.ag"(line 45, column 33)
              (_,_lhsOsbCRefS,_) =
                  __tup1
              -- "src/shuffle/CDocSubst.ag"(line 45, column 33)
              (_,_,_lhsOsbErrM) =
                  __tup1
              -- self rule
              _sbRepl =
                  CDoc_Ref cref_ mbVariantReqm_ chDest_
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Str :: String ->
                T_CDoc 
sem_CDoc_Str str_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  Set.empty
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  Map.empty
              -- self rule
              _sbRepl =
                  CDoc_Str str_
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))
sem_CDoc_Ver :: T_CDoc  ->
                T_CDoc  ->
                T_CDoc 
sem_CDoc_Ver ldoc_ rdoc_  =
    (\ _lhsIcpos
       _lhsInmChMp ->
         (let _lhsOsbCRefS :: (Set.Set Nm)
              _lhsOsbErrM :: ErrM
              _lhsOsbRepl :: CDoc 
              _ldocOcpos :: CPos
              _ldocOnmChMp :: NmChMp
              _rdocOcpos :: CPos
              _rdocOnmChMp :: NmChMp
              _ldocIsbCRefS :: (Set.Set Nm)
              _ldocIsbErrM :: ErrM
              _ldocIsbRepl :: CDoc 
              _rdocIsbCRefS :: (Set.Set Nm)
              _rdocIsbErrM :: ErrM
              _rdocIsbRepl :: CDoc 
              -- use rule "src/shuffle/CDocSubst.ag"(line 41, column 35)
              _lhsOsbCRefS =
                  _ldocIsbCRefS `Set.union` _rdocIsbCRefS
              -- use rule "src/shuffle/CDocSubst.ag"(line 42, column 34)
              _lhsOsbErrM =
                  _ldocIsbErrM `Map.union` _rdocIsbErrM
              -- self rule
              _sbRepl =
                  CDoc_Ver _ldocIsbRepl _rdocIsbRepl
              -- self rule
              _lhsOsbRepl =
                  _sbRepl
              -- copy rule (down)
              _ldocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _ldocOnmChMp =
                  _lhsInmChMp
              -- copy rule (down)
              _rdocOcpos =
                  _lhsIcpos
              -- copy rule (down)
              _rdocOnmChMp =
                  _lhsInmChMp
              ( _ldocIsbCRefS,_ldocIsbErrM,_ldocIsbRepl) =
                  ldoc_ _ldocOcpos _ldocOnmChMp 
              ( _rdocIsbCRefS,_rdocIsbErrM,_rdocIsbRepl) =
                  rdoc_ _rdocOcpos _rdocOnmChMp 
          in  ( _lhsOsbCRefS,_lhsOsbErrM,_lhsOsbRepl)))