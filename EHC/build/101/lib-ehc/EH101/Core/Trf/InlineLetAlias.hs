

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag)
module EH101.Core.Trf.InlineLetAlias(cmodTrfInlineLetAlias) where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Core
import EH101.Ty
import EH101.AbstractCore
import EH.Util.Utils
import qualified Data.Set as Set











cmodTrfInlineLetAlias :: HsNameS -> CModule -> CModule
cmodTrfInlineLetAlias globNmS cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf {noTrfNmS_Inh_CodeAGItf = globNmS})
     in   cTrf_Syn_CodeAGItf t



type NmMp = Map.Map HsName CExpr



trackNames :: Track -> [HsName]
trackNames (TrackSelect n t) = trackNames t
trackNames (TrackVarApply x ts) = x : concatMap trackNames ts
trackNames _ = []

substTrack :: NmMp -> Track -> Track
substTrack m (TrackSelect n t) = TrackSelect n (substTrack m t)
substTrack m (TrackVarApply x ts) = TrackVarApply (maybe x (fromJust . cexprMbVar) (Map.lookup x m)) (map (substTrack m) ts)
substTrack _ t = t
-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local noTrfNmS    : _
            local lev         : _
            local fvS         : _
            local cvi         : _
            local cTrf        : _
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = Bool ->
               CVarIntroMp ->
               CVarIntroMp ->
               Int ->
               NmMp ->
               HsNameS ->
               ( CAlt ,FvS,Int)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfToConNmS ->
         (let _exprOnmMp :: NmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _lhsOlevOf :: Int
              _lhsOfvS :: FvS
              _lhsOcTrf :: CAlt 
              _patOcvarIntroMp :: CVarIntroMp
              _patOintroCVarIntroMp :: CVarIntroMp
              _patOlev :: Int
              _patOnmMp :: NmMp
              _exprOallowTrfToCon :: Bool
              _exprOlev :: Int
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _patIcTrf :: CPat 
              _patIfldNmL :: ([HsName])
              _patIfvS :: FvS
              _patIlevOf :: Int
              _patInmL :: ([HsName])
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 43, column 17)
              _noTrfNmS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 86, column 17)
              _exprOnmMp =
                  foldr Map.delete _lhsInmMp _patInmL
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 10, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 14, column 17)
              _fvS =
                  _exprIfvS `Set.difference` Set.fromList _patInmL
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 15, column 17)
              _cvi =
                  emptyCVarIntro { cviLev  = _lev }
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 36, column 17)
              _exprOcvarIntroMp =
                  Map.fromList (zip _patInmL (repeat _cvi)) `Map.union` _lhsIcvarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 48, column 17)
              _exprOintroCVarIntroMp =
                  Map.fromList (zip _patInmL (repeat _cvi)) `Map.union` _lhsIintroCVarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 70, column 17)
              _lhsOlevOf =
                  fvsLev _lhsIcvarIntroMp cLevModule _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _patOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _patOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (from local)
              _patOlev =
                  _lev
              -- copy rule (down)
              _patOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (from local)
              _exprOlev =
                  _lev
              -- copy rule (from local)
              _exprOnoTrfNmS =
                  _noTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _patIcTrf,_patIfldNmL,_patIfvS,_patIlevOf,_patInmL) =
                  pat_ _patOcvarIntroMp _patOintroCVarIntroMp _patOlev _patOnmMp 
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = Bool ->
                CVarIntroMp ->
                CVarIntroMp ->
                Int ->
                NmMp ->
                HsNameS ->
                ( CAltL ,FvS,Int)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfToConNmS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              _hdOallowTrfToCon :: Bool
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlev :: Int
              _hdOnmMp :: NmMp
              _hdOnoTrfToConNmS :: HsNameS
              _tlOallowTrfToCon :: Bool
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlev :: Int
              _tlOnmMp :: NmMp
              _tlOnoTrfToConNmS :: HsNameS
              _hdIcTrf :: CAlt 
              _hdIfvS :: FvS
              _hdIlevOf :: Int
              _tlIcTrf :: CAltL 
              _tlIfvS :: FvS
              _tlIlevOf :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _hdIlevOf `max` _tlIlevOf
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _hdOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _tlOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _tlOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _hdIcTrf,_hdIfvS,_hdIlevOf) =
                  hd_ _hdOallowTrfToCon _hdOcvarIntroMp _hdOintroCVarIntroMp _hdOlev _hdOnmMp _hdOnoTrfToConNmS 
              ( _tlIcTrf,_tlIfvS,_tlIlevOf) =
                  tl_ _tlOallowTrfToCon _tlOcvarIntroMp _tlOintroCVarIntroMp _tlOlev _tlOnmMp _tlOnoTrfToConNmS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfToConNmS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         lev                  : Int
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         bindL                : [(HsName,CBind)]
         bindNmMp             : NmMp
         bindsIntroCVarIntroMp : CVarIntroMp
         bindsNoTrfNmS        : HsNameS
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levOf                : Int
         nm                   : HsName
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local aspBindNmMp : _
            local cTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = Bool ->
                CVarIntroMp ->
                CVarIntroMp ->
                Bool ->
                Int ->
                NmMp ->
                HsNameS ->
                HsNameS ->
                ( ([(HsName,CBind)]),NmMp,CVarIntroMp,HsNameS,CBind ,CVarIntroMp,FvS,FvSMp,Int,HsName,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _lhsOfvSMp :: FvSMp
              _lhsOnmL :: ([HsName])
              _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CBind 
              _bindAspectsOallowTrfToCon :: Bool
              _bindAspectsOaspBindNmMp :: (ACoreBindAspMp NmMp)
              _bindAspectsOcvarIntroMp :: CVarIntroMp
              _bindAspectsOintroCVarIntroMp :: CVarIntroMp
              _bindAspectsOisGlobal :: Bool
              _bindAspectsOlev :: Int
              _bindAspectsOnmMp :: NmMp
              _bindAspectsOnoTrfNmS :: HsNameS
              _bindAspectsOnoTrfToConNmS :: HsNameS
              _bindAspectsIbindL :: ([(HsName,CBind)])
              _bindAspectsIbindNmMp :: NmMp
              _bindAspectsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindAspectsIbindsNoTrfNmS :: HsNameS
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIcvarIntroExprMp :: CVarIntroMp
              _bindAspectsIfvS :: FvS
              _bindAspectsIfvSMp :: FvSMp
              _bindAspectsIgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _bindAspectsIlevOf :: Int
              _bindAspectsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 146, column 25)
              _aspBindNmMp =
                  _bindAspectsIgathAspBindNmMp
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 4, column 17)
              _bindAspectsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 12, column 17)
              _lhsOnm =
                  nm_
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 11, column 17)
              _lhsOfvSMp =
                  Map.singleton nm_ _bindAspectsIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 19, column 17)
              _lhsOnmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  _bindAspectsIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  _bindAspectsIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _bindAspectsIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  _bindAspectsIbindsNoTrfNmS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _bindAspectsIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindAspectsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _bindAspectsIlevOf
              -- self rule
              _cTrf =
                  CBind_Bind nm_ _bindAspectsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindAspectsOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (from local)
              _bindAspectsOaspBindNmMp =
                  _aspBindNmMp
              -- copy rule (down)
              _bindAspectsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindAspectsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindAspectsOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _bindAspectsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindAspectsOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _bindAspectsOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bindAspectsOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _bindAspectsIbindL,_bindAspectsIbindNmMp,_bindAspectsIbindsIntroCVarIntroMp,_bindAspectsIbindsNoTrfNmS,_bindAspectsIcTrf,_bindAspectsIcvarIntroExprMp,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsIgathAspBindNmMp,_bindAspectsIlevOf,_bindAspectsInmL) =
                  bindAspects_ _bindAspectsOallowTrfToCon _bindAspectsOaspBindNmMp _bindAspectsOcvarIntroMp _bindAspectsOintroCVarIntroMp _bindAspectsOisGlobal _bindAspectsOlev _bindAspectsOnm _bindAspectsOnmMp _bindAspectsOnoTrfNmS _bindAspectsOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOlevOf,_lhsOnm,_lhsOnmL)))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = CVarIntroMp ->
                   CVarIntroMp ->
                   Int ->
                   NmMp ->
                   ( CBindAnn ,FvS,Int,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnn 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBindAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = CVarIntroMp ->
                    CVarIntroMp ->
                    Int ->
                    NmMp ->
                    ( CBindAnnL ,FvS,Int,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlev :: Int
              _hdOnmMp :: NmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlev :: Int
              _tlOnmMp :: NmMp
              _hdIcTrf :: CBindAnn 
              _hdIfvS :: FvS
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CBindAnnL 
              _tlIfvS :: FvS
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _hdIlevOf `max` _tlIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnmMp =
                  _lhsInmMp
              ( _hdIcTrf,_hdIfvS,_hdIlevOf,_hdInmL) =
                  hd_ _hdOcvarIntroMp _hdOintroCVarIntroMp _hdOlev _hdOnmMp 
              ( _tlIcTrf,_tlIfvS,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOintroCVarIntroMp _tlOlev _tlOnmMp 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         lev                  : Int
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         bindL                : [(HsName,CBind)]
         bindNmMp             : NmMp
         bindsIntroCVarIntroMp : CVarIntroMp
         bindsNoTrfNmS        : HsNameS
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = Bool ->
                 CVarIntroMp ->
                 CVarIntroMp ->
                 Bool ->
                 Int ->
                 NmMp ->
                 HsNameS ->
                 HsNameS ->
                 ( ([(HsName,CBind)]),NmMp,CVarIntroMp,HsNameS,CBindL ,CVarIntroMp,FvS,FvSMp,Int,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              _hdOallowTrfToCon :: Bool
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOlev :: Int
              _hdOnmMp :: NmMp
              _hdOnoTrfNmS :: HsNameS
              _hdOnoTrfToConNmS :: HsNameS
              _tlOallowTrfToCon :: Bool
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOlev :: Int
              _tlOnmMp :: NmMp
              _tlOnoTrfNmS :: HsNameS
              _tlOnoTrfToConNmS :: HsNameS
              _hdIbindL :: ([(HsName,CBind)])
              _hdIbindNmMp :: NmMp
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIbindsNoTrfNmS :: HsNameS
              _hdIcTrf :: CBind 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIlevOf :: Int
              _hdInm :: HsName
              _hdInmL :: ([HsName])
              _tlIbindL :: ([(HsName,CBind)])
              _tlIbindNmMp :: NmMp
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIbindsNoTrfNmS :: HsNameS
              _tlIcTrf :: CBindL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  _hdIbindL ++ _tlIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  _hdIbindNmMp `Map.union` _tlIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  _hdIbindsNoTrfNmS `Set.union` _tlIbindsNoTrfNmS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _hdIlevOf `max` _tlIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _hdOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _hdOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _tlOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _tlOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _tlOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _hdIbindL,_hdIbindNmMp,_hdIbindsIntroCVarIntroMp,_hdIbindsNoTrfNmS,_hdIcTrf,_hdIcvarIntroExprMp,_hdIfvS,_hdIfvSMp,_hdIlevOf,_hdInm,_hdInmL) =
                  hd_ _hdOallowTrfToCon _hdOcvarIntroMp _hdOintroCVarIntroMp _hdOisGlobal _hdOlev _hdOnmMp _hdOnoTrfNmS _hdOnoTrfToConNmS 
              ( _tlIbindL,_tlIbindNmMp,_tlIbindsIntroCVarIntroMp,_tlIbindsNoTrfNmS,_tlIcTrf,_tlIcvarIntroExprMp,_tlIfvS,_tlIfvSMp,_tlIlevOf,_tlInmL) =
                  tl_ _tlOallowTrfToCon _tlOcvarIntroMp _tlOintroCVarIntroMp _tlOisGlobal _tlOlev _tlOnmMp _tlOnoTrfNmS _tlOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOlevOf,_lhsOnmL)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOlevOf,_lhsOnmL)))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         aspBindNmMp          : ACoreBindAspMp NmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         bindL                : [(HsName,CBind)]
         bindNmMp             : NmMp
         bindsIntroCVarIntroMp : CVarIntroMp
         bindsNoTrfNmS        : HsNameS
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathAspBindNmMp      : ACoreBindAspMp NmMp
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local _tup1       : {(NmMp,[(HsName,CBind)])}
            local cmetaVal    : _
            local cvi         : _
            local cviExpr     : _
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local bindsNoTrfNmS : _
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local cTrf        : _
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBound :: CBound  ->
              T_CBound 
sem_CBound (CBound_Bind _bindMeta _expr )  =
    (sem_CBound_Bind (sem_CMetas _bindMeta ) (sem_CExpr _expr ) )
sem_CBound (CBound_FFE _callconv _expEnt _expr _ty )  =
    (sem_CBound_FFE _callconv _expEnt (sem_CExpr _expr ) _ty )
sem_CBound (CBound_Meta _aspectKeyS _cmetas )  =
    (sem_CBound_Meta _aspectKeyS (sem_CMetas _cmetas ) )
sem_CBound (CBound_RelevTy _aspectKeyS _relevTy )  =
    (sem_CBound_RelevTy _aspectKeyS _relevTy )
sem_CBound (CBound_Ty _aspectKeyS _ty )  =
    (sem_CBound_Ty _aspectKeyS _ty )
sem_CBound (CBound_Val _aspectKeyS _expr )  =
    (sem_CBound_Val _aspectKeyS (sem_CExpr _expr ) )
-- semantic domain
type T_CBound  = Bool ->
                 (ACoreBindAspMp NmMp) ->
                 CVarIntroMp ->
                 CVarIntroMp ->
                 Bool ->
                 Int ->
                 HsName ->
                 NmMp ->
                 HsNameS ->
                 HsNameS ->
                 ( ([(HsName,CBind)]),NmMp,CVarIntroMp,HsNameS,CBound ,CVarIntroMp,FvS,FvSMp,(ACoreBindAspMp NmMp),Int,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let __tup1 :: ((NmMp,[(HsName,CBind)]))
              _lhsObindNmMp :: NmMp
              _lhsObindL :: ([(HsName,CBind)])
              _exprOallowTrfToCon :: Bool
              _exprOnoTrfToConNmS :: HsNameS
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _bindMetaObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _bindMetaOcvarIntroMp :: CVarIntroMp
              _bindMetaOintroCVarIntroMp :: CVarIntroMp
              _bindMetaOlev :: Int
              _bindMetaOnmMp :: NmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _bindMetaIcTrf :: CMetas 
              _bindMetaIfvS :: FvS
              _bindMetaIlevOf :: Int
              _bindMetaInameS :: HsNameS
              _bindMetaIself :: CMetas 
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 109, column 33)
              __tup1 =
                  case _exprImbRepl of
                    Just r | _lhsInm /= hsnMain
                             && not (_lhsInm `Set.member` _lhsInoTrfNmS)
                      -> (_lhsInm `Map.singleton` r,[(_lhsInm,acoreBind1Asp1 _lhsInm _cTrf)])
                    _ -> (Map.empty,[(_lhsInm,acoreBind1Asp1 _lhsInm _cTrf)])
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 109, column 33)
              (_lhsObindNmMp,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 109, column 33)
              (_,_lhsObindL) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 139, column 11)
              _exprOallowTrfToCon =
                  not (_lhsInm `Set.member` _lhsInoTrfToConNmS)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 140, column 11)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS `Set.union` _bindMetaInameS
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 149, column 25)
              _lhsOgathAspBindNmMp =
                  Map.singleton acbaspkeyDefault _exprIbindNmMp
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 177, column 25)
              _bindMetaObindNmMp =
                  _exprIbindNmMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 10, column 17)
              _cmetaVal =
                  cmetasVal _bindMetaIself
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 10, column 17)
              _cvi =
                  emptyCVarIntro { cviLev  = _lhsIlev   , cviMeta = _cmetaVal }
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 10, column 17)
              _cviExpr =
                  emptyCVarIntro { cviLev  = _exprIlevOf, cviMeta = _cmetaVal }
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 53, column 17)
              _lhsObindsIntroCVarIntroMp =
                  Map.singleton _lhsInm _cvi
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 75, column 17)
              _lhsOcvarIntroExprMp =
                  Map.singleton _lhsInm _cviExpr
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindMetaIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _bindMetaIlevOf `max` _exprIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_Bind _bindMetaIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindMetaOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindMetaOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindMetaOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindMetaOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              ( _bindMetaIcTrf,_bindMetaIfvS,_bindMetaIlevOf,_bindMetaInameS,_bindMetaIself) =
                  bindMeta_ _bindMetaObindNmMp _bindMetaOcvarIntroMp _bindMetaOintroCVarIntroMp _bindMetaOlev _bindMetaOnmMp 
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 169, column 17)
              _bindsNoTrfNmS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  _exprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  _bindsNoTrfNmS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _exprIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_FFE callconv_ expEnt_ _exprIcTrf ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _cmetasObindNmMp :: NmMp
              _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _cmetasOcvarIntroMp :: CVarIntroMp
              _cmetasOintroCVarIntroMp :: CVarIntroMp
              _cmetasOlev :: Int
              _cmetasOnmMp :: NmMp
              _cmetasIcTrf :: CMetas 
              _cmetasIfvS :: FvS
              _cmetasIlevOf :: Int
              _cmetasInameS :: HsNameS
              _cmetasIself :: CMetas 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 178, column 25)
              _cmetasObindNmMp =
                  panicJust "InlineLetAlias.CBound.Meta.aspBindNmMp" $ Map.lookup acbaspkeyDefault _lhsIaspBindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _cmetasIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _cmetasIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_Meta aspectKeyS_ _cmetasIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _cmetasOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _cmetasOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _cmetasOlev =
                  _lhsIlev
              -- copy rule (down)
              _cmetasOnmMp =
                  _lhsInmMp
              ( _cmetasIcTrf,_cmetasIfvS,_cmetasIlevOf,_cmetasInameS,_cmetasIself) =
                  cmetas_ _cmetasObindNmMp _cmetasOcvarIntroMp _cmetasOintroCVarIntroMp _cmetasOlev _cmetasOnmMp 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_RelevTy aspectKeyS_ relevTy_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_Ty aspectKeyS_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  _exprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _exprIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CBound_Val aspectKeyS_ _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         aspBindNmMp          : ACoreBindAspMp NmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         lev                  : Int
         nm                   : HsName
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         bindL                : [(HsName,CBind)]
         bindNmMp             : NmMp
         bindsIntroCVarIntroMp : CVarIntroMp
         bindsNoTrfNmS        : HsNameS
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         gathAspBindNmMp      : ACoreBindAspMp NmMp
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = Bool ->
                  (ACoreBindAspMp NmMp) ->
                  CVarIntroMp ->
                  CVarIntroMp ->
                  Bool ->
                  Int ->
                  HsName ->
                  NmMp ->
                  HsNameS ->
                  HsNameS ->
                  ( ([(HsName,CBind)]),NmMp,CVarIntroMp,HsNameS,CBoundL ,CVarIntroMp,FvS,FvSMp,(ACoreBindAspMp NmMp),Int,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              _hdOallowTrfToCon :: Bool
              _hdOaspBindNmMp :: (ACoreBindAspMp NmMp)
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOlev :: Int
              _hdOnm :: HsName
              _hdOnmMp :: NmMp
              _hdOnoTrfNmS :: HsNameS
              _hdOnoTrfToConNmS :: HsNameS
              _tlOallowTrfToCon :: Bool
              _tlOaspBindNmMp :: (ACoreBindAspMp NmMp)
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOlev :: Int
              _tlOnm :: HsName
              _tlOnmMp :: NmMp
              _tlOnoTrfNmS :: HsNameS
              _tlOnoTrfToConNmS :: HsNameS
              _hdIbindL :: ([(HsName,CBind)])
              _hdIbindNmMp :: NmMp
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIbindsNoTrfNmS :: HsNameS
              _hdIcTrf :: CBound 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIbindL :: ([(HsName,CBind)])
              _tlIbindNmMp :: NmMp
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIbindsNoTrfNmS :: HsNameS
              _tlIcTrf :: CBoundL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  _hdIbindL ++ _tlIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  _hdIbindNmMp `Map.union` _tlIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  _hdIbindsNoTrfNmS `Set.union` _tlIbindsNoTrfNmS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  _hdIgathAspBindNmMp `Map.union` _tlIgathAspBindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _hdIlevOf `max` _tlIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _hdOaspBindNmMp =
                  _lhsIaspBindNmMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
              -- copy rule (down)
              _hdOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _hdOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _hdOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _tlOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _tlOaspBindNmMp =
                  _lhsIaspBindNmMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              -- copy rule (down)
              _tlOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _tlOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _tlOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _hdIbindL,_hdIbindNmMp,_hdIbindsIntroCVarIntroMp,_hdIbindsNoTrfNmS,_hdIcTrf,_hdIcvarIntroExprMp,_hdIfvS,_hdIfvSMp,_hdIgathAspBindNmMp,_hdIlevOf,_hdInmL) =
                  hd_ _hdOallowTrfToCon _hdOaspBindNmMp _hdOcvarIntroMp _hdOintroCVarIntroMp _hdOisGlobal _hdOlev _hdOnm _hdOnmMp _hdOnoTrfNmS _hdOnoTrfToConNmS 
              ( _tlIbindL,_tlIbindNmMp,_tlIbindsIntroCVarIntroMp,_tlIbindsNoTrfNmS,_tlIcTrf,_tlIcvarIntroExprMp,_tlIfvS,_tlIfvSMp,_tlIgathAspBindNmMp,_tlIlevOf,_tlInmL) =
                  tl_ _tlOallowTrfToCon _tlOaspBindNmMp _tlOcvarIntroMp _tlOintroCVarIntroMp _tlOisGlobal _tlOlev _tlOnm _tlOnmMp _tlOnoTrfNmS _tlOnoTrfToConNmS 
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIallowTrfToCon
       _lhsIaspBindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIlev
       _lhsInm
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindL :: ([(HsName,CBind)])
              _lhsObindNmMp :: NmMp
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsObindsNoTrfNmS :: HsNameS
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 105, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 104, column 29)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 102, column 34)
              _lhsObindsNoTrfNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 143, column 74)
              _lhsOgathAspBindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindL,_lhsObindNmMp,_lhsObindsIntroCVarIntroMp,_lhsObindsNoTrfNmS,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgathAspBindNmMp,_lhsOlevOf,_lhsOnmL)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         bindNmMp             : NmMp
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         mbRepl               : Maybe CExpr
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local cTrf        : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local isGlobal    : _
            local fvS         : _
            local levOf       : _
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local levOf       : _
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local lev         : _
            local isGlobal    : _
            local argNm       : _
            local fvS         : _
            local cvi         : _
            local levOf       : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local allowTrf    : _
            local allowStrTrf : _
            local nmMpNew     : _
            local nmMp        : _
            local isGlobal    : _
            local fvS         : _
            local maxBindLev  : _
            local _tup2       : _
            local strLev      : _
            local introCVarIntroMp : _
            local levOf       : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local levOf       : _
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local levOf       : _
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local levOf       : _
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local levOf       : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local nm          : {HsName}
            local nmAsp       : {HsName}
            local cTrf        : _
-}
-- cata
sem_CExpr :: CExpr  ->
             T_CExpr 
sem_CExpr (CExpr_Ann _ann _expr )  =
    (sem_CExpr_Ann (sem_CExprAnn _ann ) (sem_CExpr _expr ) )
sem_CExpr (CExpr_App _func _arg )  =
    (sem_CExpr_App (sem_CExpr _func ) (sem_CBound _arg ) )
sem_CExpr (CExpr_Case _expr _alts _dflt )  =
    (sem_CExpr_Case (sem_CExpr _expr ) (sem_CAltL _alts ) (sem_CExpr _dflt ) )
sem_CExpr (CExpr_CaseAltFail _failReason _errorExpr )  =
    (sem_CExpr_CaseAltFail _failReason (sem_CExpr _errorExpr ) )
sem_CExpr (CExpr_Char _char )  =
    (sem_CExpr_Char _char )
sem_CExpr (CExpr_CoeArg )  =
    (sem_CExpr_CoeArg )
sem_CExpr (CExpr_FFI _callconv _safety _impEnt _ty )  =
    (sem_CExpr_FFI _callconv _safety _impEnt _ty )
sem_CExpr (CExpr_Hole _uid )  =
    (sem_CExpr_Hole _uid )
sem_CExpr (CExpr_HoleLet _bindsUid _body )  =
    (sem_CExpr_HoleLet _bindsUid (sem_CExpr _body ) )
sem_CExpr (CExpr_ImplsApp _func _uid )  =
    (sem_CExpr_ImplsApp (sem_CExpr _func ) _uid )
sem_CExpr (CExpr_ImplsLam _uid _body )  =
    (sem_CExpr_ImplsLam _uid (sem_CExpr _body ) )
sem_CExpr (CExpr_Int _int )  =
    (sem_CExpr_Int _int )
sem_CExpr (CExpr_Integer _integer )  =
    (sem_CExpr_Integer _integer )
sem_CExpr (CExpr_Lam _bind _body )  =
    (sem_CExpr_Lam (sem_CBind _bind ) (sem_CExpr _body ) )
sem_CExpr (CExpr_Let _categ _binds _body )  =
    (sem_CExpr_Let _categ (sem_CBindL _binds ) (sem_CExpr _body ) )
sem_CExpr (CExpr_String _str )  =
    (sem_CExpr_String _str )
sem_CExpr (CExpr_Tup _tag )  =
    (sem_CExpr_Tup _tag )
sem_CExpr (CExpr_TupDel _expr _tag _nm _offset )  =
    (sem_CExpr_TupDel (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) )
sem_CExpr (CExpr_TupIns _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupIns (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_TupUpd _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupUpd (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_Var _ref )  =
    (sem_CExpr_Var _ref )
-- semantic domain
type T_CExpr  = Bool ->
                CVarIntroMp ->
                CVarIntroMp ->
                Int ->
                NmMp ->
                HsNameS ->
                HsNameS ->
                ( NmMp,CExpr ,FvS,Int,(Maybe CExpr))
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOmbRepl :: (Maybe CExpr)
              _annOcvarIntroMp :: CVarIntroMp
              _annOintroCVarIntroMp :: CVarIntroMp
              _annOlev :: Int
              _annOnmMp :: NmMp
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _annIcTrf :: CExprAnn 
              _annIfvS :: FvS
              _annIlevOf :: Int
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _exprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _annIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _annIlevOf `max` _exprIlevOf
              -- self rule
              _cTrf =
                  CExpr_Ann _annIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOmbRepl =
                  _exprImbRepl
              -- copy rule (down)
              _annOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _annOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _annOlev =
                  _lhsIlev
              -- copy rule (down)
              _annOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _annIcTrf,_annIfvS,_annIlevOf) =
                  ann_ _annOcvarIntroMp _annOintroCVarIntroMp _annOlev _annOnmMp 
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _argOaspBindNmMp :: (ACoreBindAspMp NmMp)
              _argOnm :: HsName
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _funcOallowTrfToCon :: Bool
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOlev :: Int
              _funcOnmMp :: NmMp
              _funcOnoTrfNmS :: HsNameS
              _funcOnoTrfToConNmS :: HsNameS
              _argOallowTrfToCon :: Bool
              _argOcvarIntroMp :: CVarIntroMp
              _argOintroCVarIntroMp :: CVarIntroMp
              _argOisGlobal :: Bool
              _argOlev :: Int
              _argOnmMp :: NmMp
              _argOnoTrfNmS :: HsNameS
              _argOnoTrfToConNmS :: HsNameS
              _funcIbindNmMp :: NmMp
              _funcIcTrf :: CExpr 
              _funcIfvS :: FvS
              _funcIlevOf :: Int
              _funcImbRepl :: (Maybe CExpr)
              _argIbindL :: ([(HsName,CBind)])
              _argIbindNmMp :: NmMp
              _argIbindsIntroCVarIntroMp :: CVarIntroMp
              _argIbindsNoTrfNmS :: HsNameS
              _argIcTrf :: CBound 
              _argIcvarIntroExprMp :: CVarIntroMp
              _argIfvS :: FvS
              _argIfvSMp :: FvSMp
              _argIgathAspBindNmMp :: (ACoreBindAspMp NmMp)
              _argIlevOf :: Int
              _argInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 153, column 25)
              _argOaspBindNmMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 7, column 17)
              _argOnm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 8, column 17)
              _fvS =
                  _funcIfvS `Set.union` _argIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 60, column 17)
              _levOf =
                  _funcIlevOf `max` _argIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _funcIbindNmMp `Map.union` _argIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_App _funcIcTrf _argIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _funcOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _funcOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _funcOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _funcOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _funcOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _argOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _argOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _argOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (from local)
              _argOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _argOlev =
                  _lhsIlev
              -- copy rule (down)
              _argOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _argOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _argOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _funcIbindNmMp,_funcIcTrf,_funcIfvS,_funcIlevOf,_funcImbRepl) =
                  func_ _funcOallowTrfToCon _funcOcvarIntroMp _funcOintroCVarIntroMp _funcOlev _funcOnmMp _funcOnoTrfNmS _funcOnoTrfToConNmS 
              ( _argIbindL,_argIbindNmMp,_argIbindsIntroCVarIntroMp,_argIbindsNoTrfNmS,_argIcTrf,_argIcvarIntroExprMp,_argIfvS,_argIfvSMp,_argIgathAspBindNmMp,_argIlevOf,_argInmL) =
                  arg_ _argOallowTrfToCon _argOaspBindNmMp _argOcvarIntroMp _argOintroCVarIntroMp _argOisGlobal _argOlev _argOnm _argOnmMp _argOnoTrfNmS _argOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _altsOallowTrfToCon :: Bool
              _altsOcvarIntroMp :: CVarIntroMp
              _altsOintroCVarIntroMp :: CVarIntroMp
              _altsOlev :: Int
              _altsOnmMp :: NmMp
              _altsOnoTrfToConNmS :: HsNameS
              _dfltOallowTrfToCon :: Bool
              _dfltOcvarIntroMp :: CVarIntroMp
              _dfltOintroCVarIntroMp :: CVarIntroMp
              _dfltOlev :: Int
              _dfltOnmMp :: NmMp
              _dfltOnoTrfNmS :: HsNameS
              _dfltOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              _altsIcTrf :: CAltL 
              _altsIfvS :: FvS
              _altsIlevOf :: Int
              _dfltIbindNmMp :: NmMp
              _dfltIcTrf :: CExpr 
              _dfltIfvS :: FvS
              _dfltIlevOf :: Int
              _dfltImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 61, column 17)
              _levOf =
                  _exprIlevOf `max` _altsIlevOf `max` _dfltIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _exprIbindNmMp `Map.union` _dfltIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _altsOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _altsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _altsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _altsOlev =
                  _lhsIlev
              -- copy rule (down)
              _altsOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _altsOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _dfltOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _dfltOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _dfltOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _dfltOlev =
                  _lhsIlev
              -- copy rule (down)
              _dfltOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _dfltOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _dfltOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
              ( _altsIcTrf,_altsIfvS,_altsIlevOf) =
                  alts_ _altsOallowTrfToCon _altsOcvarIntroMp _altsOintroCVarIntroMp _altsOlev _altsOnmMp _altsOnoTrfToConNmS 
              ( _dfltIbindNmMp,_dfltIcTrf,_dfltIfvS,_dfltIlevOf,_dfltImbRepl) =
                  dflt_ _dfltOallowTrfToCon _dfltOcvarIntroMp _dfltOintroCVarIntroMp _dfltOlev _dfltOnmMp _dfltOnoTrfNmS _dfltOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _errorExprOallowTrfToCon :: Bool
              _errorExprOcvarIntroMp :: CVarIntroMp
              _errorExprOintroCVarIntroMp :: CVarIntroMp
              _errorExprOlev :: Int
              _errorExprOnmMp :: NmMp
              _errorExprOnoTrfNmS :: HsNameS
              _errorExprOnoTrfToConNmS :: HsNameS
              _errorExprIbindNmMp :: NmMp
              _errorExprIcTrf :: CExpr 
              _errorExprIfvS :: FvS
              _errorExprIlevOf :: Int
              _errorExprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _errorExprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _errorExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _errorExprIlevOf
              -- self rule
              _cTrf =
                  CExpr_CaseAltFail failReason_ _errorExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _errorExprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _errorExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _errorExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _errorExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _errorExprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _errorExprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _errorExprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _errorExprIbindNmMp,_errorExprIcTrf,_errorExprIfvS,_errorExprIlevOf,_errorExprImbRepl) =
                  errorExpr_ _errorExprOallowTrfToCon _errorExprOcvarIntroMp _errorExprOintroCVarIntroMp _errorExprOlev _errorExprOnmMp _errorExprOnoTrfNmS _errorExprOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 94, column 17)
              _lhsOmbRepl =
                  if _lhsIallowTrfToCon
                  then Just _cTrf
                  else Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_CoeArg
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_FFI callconv_ safety_ impEnt_ ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_Hole uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _bodyOallowTrfToCon :: Bool
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOlev :: Int
              _bodyOnmMp :: NmMp
              _bodyOnoTrfNmS :: HsNameS
              _bodyOnoTrfToConNmS :: HsNameS
              _bodyIbindNmMp :: NmMp
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIlevOf :: Int
              _bodyImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _bodyIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _bodyIlevOf
              -- self rule
              _cTrf =
                  CExpr_HoleLet bindsUid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bodyOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _bodyOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bodyOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _bodyOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bodyOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _bodyIbindNmMp,_bodyIcTrf,_bodyIfvS,_bodyIlevOf,_bodyImbRepl) =
                  body_ _bodyOallowTrfToCon _bodyOcvarIntroMp _bodyOintroCVarIntroMp _bodyOlev _bodyOnmMp _bodyOnoTrfNmS _bodyOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _funcOallowTrfToCon :: Bool
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOlev :: Int
              _funcOnmMp :: NmMp
              _funcOnoTrfNmS :: HsNameS
              _funcOnoTrfToConNmS :: HsNameS
              _funcIbindNmMp :: NmMp
              _funcIcTrf :: CExpr 
              _funcIfvS :: FvS
              _funcIlevOf :: Int
              _funcImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _funcIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _funcIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _funcIlevOf
              -- self rule
              _cTrf =
                  CExpr_ImplsApp _funcIcTrf uid_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _funcOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _funcOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _funcOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _funcOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _funcOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _funcIbindNmMp,_funcIcTrf,_funcIfvS,_funcIlevOf,_funcImbRepl) =
                  func_ _funcOallowTrfToCon _funcOcvarIntroMp _funcOintroCVarIntroMp _funcOlev _funcOnmMp _funcOnoTrfNmS _funcOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _bodyOallowTrfToCon :: Bool
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOlev :: Int
              _bodyOnmMp :: NmMp
              _bodyOnoTrfNmS :: HsNameS
              _bodyOnoTrfToConNmS :: HsNameS
              _bodyIbindNmMp :: NmMp
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIlevOf :: Int
              _bodyImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _bodyIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _bodyIlevOf
              -- self rule
              _cTrf =
                  CExpr_ImplsLam uid_ _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bodyOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _bodyOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bodyOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _bodyOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bodyOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _bodyIbindNmMp,_bodyIcTrf,_bodyIfvS,_bodyIlevOf,_bodyImbRepl) =
                  body_ _bodyOallowTrfToCon _bodyOcvarIntroMp _bodyOintroCVarIntroMp _bodyOlev _bodyOnmMp _bodyOnoTrfNmS _bodyOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 94, column 17)
              _lhsOmbRepl =
                  if _lhsIallowTrfToCon
                  then Just _cTrf
                  else Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_Integer integer_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _bodyOnmMp :: NmMp
              _lhsOmbRepl :: (Maybe CExpr)
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _bindOallowTrfToCon :: Bool
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOisGlobal :: Bool
              _bindOlev :: Int
              _bindOnmMp :: NmMp
              _bindOnoTrfNmS :: HsNameS
              _bindOnoTrfToConNmS :: HsNameS
              _bodyOallowTrfToCon :: Bool
              _bodyOlev :: Int
              _bodyOnoTrfNmS :: HsNameS
              _bodyOnoTrfToConNmS :: HsNameS
              _bindIbindL :: ([(HsName,CBind)])
              _bindIbindNmMp :: NmMp
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIbindsNoTrfNmS :: HsNameS
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _bodyIbindNmMp :: NmMp
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIlevOf :: Int
              _bodyImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 83, column 25)
              _bodyOnmMp =
                  Map.delete _argNm _lhsInmMp
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 7, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 19, column 25)
              _argNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 5, column 17)
              _fvS =
                  _argNm `Set.delete` _bodyIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 4, column 17)
              _cvi =
                  CVarIntro
                    { cviLev  = _lev
                    , cviMeta = CMetaVal_Val
                    }
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 23, column 17)
              _bodyOcvarIntroMp =
                  Map.insert _argNm _cvi _lhsIcvarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 44, column 17)
              _bodyOintroCVarIntroMp =
                  Map.insert _argNm _cvi _lhsIintroCVarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 65, column 17)
              _levOf =
                  fvsLev _lhsIcvarIntroMp cLevModule _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _bindIbindNmMp `Map.union` _bodyIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Lam _bindIcTrf _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _bindOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _bindOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (from local)
              _bindOisGlobal =
                  _isGlobal
              -- copy rule (from local)
              _bindOlev =
                  _lev
              -- copy rule (down)
              _bindOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _bindOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bindOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _bodyOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (from local)
              _bodyOlev =
                  _lev
              -- copy rule (down)
              _bodyOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bodyOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _bindIbindL,_bindIbindNmMp,_bindIbindsIntroCVarIntroMp,_bindIbindsNoTrfNmS,_bindIcTrf,_bindIcvarIntroExprMp,_bindIfvS,_bindIfvSMp,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOallowTrfToCon _bindOcvarIntroMp _bindOintroCVarIntroMp _bindOisGlobal _bindOlev _bindOnmMp _bindOnoTrfNmS _bindOnoTrfToConNmS 
              ( _bodyIbindNmMp,_bodyIcTrf,_bodyIfvS,_bodyIlevOf,_bodyImbRepl) =
                  body_ _bodyOallowTrfToCon _bodyOcvarIntroMp _bodyOintroCVarIntroMp _bodyOlev _bodyOnmMp _bodyOnoTrfNmS _bodyOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsOcTrf :: CExpr 
              _bindsOcvarIntroMp :: CVarIntroMp
              _bodyOcvarIntroMp :: CVarIntroMp
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _bindsOallowTrfToCon :: Bool
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOisGlobal :: Bool
              _bindsOlev :: Int
              _bindsOnmMp :: NmMp
              _bindsOnoTrfNmS :: HsNameS
              _bindsOnoTrfToConNmS :: HsNameS
              _bodyOallowTrfToCon :: Bool
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOlev :: Int
              _bodyOnmMp :: NmMp
              _bodyOnoTrfNmS :: HsNameS
              _bodyOnoTrfToConNmS :: HsNameS
              _bindsIbindL :: ([(HsName,CBind)])
              _bindsIbindNmMp :: NmMp
              _bindsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindsIbindsNoTrfNmS :: HsNameS
              _bindsIcTrf :: CBindL 
              _bindsIcvarIntroExprMp :: CVarIntroMp
              _bindsIfvS :: FvS
              _bindsIfvSMp :: FvSMp
              _bindsIlevOf :: Int
              _bindsInmL :: ([HsName])
              _bodyIbindNmMp :: NmMp
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIlevOf :: Int
              _bodyImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 58, column 17)
              _allowTrf =
                  categ_ /= CBindCateg_Strict
                  && categ_ /= CBindCateg_FFI
                  && categ_ /= CBindCateg_FFE
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 58, column 17)
              _allowStrTrf =
                  categ_ /= CBindCateg_FFI
                  && categ_ /= CBindCateg_FFE
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 58, column 17)
              _nmMpNew =
                  Map.foldrWithKey
                    (\n r m
                      -> case r of
                           CExpr_Var ref
                             | nm `Map.member` _bindsIbindNmMp
                               -> m
                             | _allowTrf || (_allowStrTrf && cexprIsEvaluated r')
                               -> Map.insert n r' m
                             where nm = acbrefNm ref
                                   r' = maybe r id . Map.lookup nm $ _lhsInmMp
                           _ | _allowTrf
                               -> Map.insert n r m
                           CExpr_Int i
                             | _allowStrTrf
                               -> Map.insert n r m
                           _   -> m
                    )
                    Map.empty
                    _bindsIbindNmMp
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 58, column 17)
              _nmMp =
                  Map.union _nmMpNew $ Map.difference _lhsInmMp _bindsIcvarIntroExprMp
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 159, column 17)
              _lhsOcTrf =
                  if Map.null _nmMpNew
                  then _cTrf
                  else acoreLet categ_
                         [ b | (n,b) <- _bindsIbindL
                             , not (n `Map.member` _nmMpNew)
                               || n `Set.member` _bindsIbindsNoTrfNmS
                         ] _bodyIcTrf
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 15, column 17)
              _isGlobal =
                  _lhsIlev == cLevModule
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 6, column 17)
              _fvS =
                  (_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 24, column 17)
              _maxBindLev =
                  fvsLev _lhsIcvarIntroMp cLevModule _bindsIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              __tup2 =
                  case categ_ of
                      CBindCateg_Strict -> (const _lhsIlev,_lhsIcvarIntroMp)
                      CBindCateg_Rec    -> ( const _maxBindLev
                                           , Map.map (\cvi -> cvi {cviLev = _maxBindLev}) _bindsIcvarIntroExprMp
                                               `Map.union` _lhsIcvarIntroMp
                                           )
                      _                 -> (id,_lhsIcvarIntroMp)
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_strLev,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_,_bindsOcvarIntroMp) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 33, column 17)
              _bodyOcvarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _strLev $ cviLev cvi}) _bindsIcvarIntroExprMp `Map.union` _lhsIcvarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 45, column 17)
              _introCVarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _lhsIlev}) _bindsIbindsIntroCVarIntroMp `Map.union` _lhsIintroCVarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 66, column 17)
              _levOf =
                  fvsLev _lhsIcvarIntroMp cLevModule _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _bindsIbindNmMp `Map.union` _bodyIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- copy rule (down)
              _bindsOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (from local)
              _bindsOintroCVarIntroMp =
                  _introCVarIntroMp
              -- copy rule (from local)
              _bindsOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (from local)
              _bindsOnmMp =
                  _nmMp
              -- copy rule (down)
              _bindsOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bindsOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _bodyOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (from local)
              _bodyOintroCVarIntroMp =
                  _introCVarIntroMp
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (from local)
              _bodyOnmMp =
                  _nmMp
              -- copy rule (down)
              _bodyOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _bodyOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _bindsIbindL,_bindsIbindNmMp,_bindsIbindsIntroCVarIntroMp,_bindsIbindsNoTrfNmS,_bindsIcTrf,_bindsIcvarIntroExprMp,_bindsIfvS,_bindsIfvSMp,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOallowTrfToCon _bindsOcvarIntroMp _bindsOintroCVarIntroMp _bindsOisGlobal _bindsOlev _bindsOnmMp _bindsOnoTrfNmS _bindsOnoTrfToConNmS 
              ( _bodyIbindNmMp,_bodyIcTrf,_bodyIfvS,_bodyIlevOf,_bodyImbRepl) =
                  body_ _bodyOallowTrfToCon _bodyOcvarIntroMp _bodyOintroCVarIntroMp _bodyOlev _bodyOnmMp _bodyOnoTrfNmS _bodyOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 67, column 17)
              _levOf =
                  cLevIntern
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_String str_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExpr_Tup tag_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _offsetOallowTrfToCon :: Bool
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlev :: Int
              _offsetOnmMp :: NmMp
              _offsetOnoTrfNmS :: HsNameS
              _offsetOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              _offsetIbindNmMp :: NmMp
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIlevOf :: Int
              _offsetImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 62, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _exprIbindNmMp `Map.union` _offsetIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _offsetOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _offsetOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _offsetOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
              ( _offsetIbindNmMp,_offsetIcTrf,_offsetIfvS,_offsetIlevOf,_offsetImbRepl) =
                  offset_ _offsetOallowTrfToCon _offsetOcvarIntroMp _offsetOintroCVarIntroMp _offsetOlev _offsetOnmMp _offsetOnoTrfNmS _offsetOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _offsetOallowTrfToCon :: Bool
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlev :: Int
              _offsetOnmMp :: NmMp
              _offsetOnoTrfNmS :: HsNameS
              _offsetOnoTrfToConNmS :: HsNameS
              _fldExprOallowTrfToCon :: Bool
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOlev :: Int
              _fldExprOnmMp :: NmMp
              _fldExprOnoTrfNmS :: HsNameS
              _fldExprOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              _offsetIbindNmMp :: NmMp
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIlevOf :: Int
              _offsetImbRepl :: (Maybe CExpr)
              _fldExprIbindNmMp :: NmMp
              _fldExprIcTrf :: CExpr 
              _fldExprIfvS :: FvS
              _fldExprIlevOf :: Int
              _fldExprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 64, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf `max` _fldExprIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _exprIbindNmMp `Map.union` _offsetIbindNmMp `Map.union` _fldExprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _offsetOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _offsetOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _offsetOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _fldExprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _fldExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _fldExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _fldExprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _fldExprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
              ( _offsetIbindNmMp,_offsetIcTrf,_offsetIfvS,_offsetIlevOf,_offsetImbRepl) =
                  offset_ _offsetOallowTrfToCon _offsetOcvarIntroMp _offsetOintroCVarIntroMp _offsetOlev _offsetOnmMp _offsetOnoTrfNmS _offsetOnoTrfToConNmS 
              ( _fldExprIbindNmMp,_fldExprIcTrf,_fldExprIfvS,_fldExprIlevOf,_fldExprImbRepl) =
                  fldExpr_ _fldExprOallowTrfToCon _fldExprOcvarIntroMp _fldExprOintroCVarIntroMp _fldExprOlev _fldExprOnmMp _fldExprOnoTrfNmS _fldExprOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsObindNmMp :: NmMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _exprOallowTrfToCon :: Bool
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprOnoTrfToConNmS :: HsNameS
              _offsetOallowTrfToCon :: Bool
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlev :: Int
              _offsetOnmMp :: NmMp
              _offsetOnoTrfNmS :: HsNameS
              _offsetOnoTrfToConNmS :: HsNameS
              _fldExprOallowTrfToCon :: Bool
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOlev :: Int
              _fldExprOnmMp :: NmMp
              _fldExprOnoTrfNmS :: HsNameS
              _fldExprOnoTrfToConNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              _offsetIbindNmMp :: NmMp
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIlevOf :: Int
              _offsetImbRepl :: (Maybe CExpr)
              _fldExprIbindNmMp :: NmMp
              _fldExprIcTrf :: CExpr 
              _fldExprIfvS :: FvS
              _fldExprIlevOf :: Int
              _fldExprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 100, column 17)
              _lhsOmbRepl =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 64, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf `max` _fldExprIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  _exprIbindNmMp `Map.union` _offsetIbindNmMp `Map.union` _fldExprIbindNmMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _exprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _offsetOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _offsetOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _offsetOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              -- copy rule (down)
              _fldExprOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _fldExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _fldExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _fldExprOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _fldExprOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
              ( _offsetIbindNmMp,_offsetIcTrf,_offsetIfvS,_offsetIlevOf,_offsetImbRepl) =
                  offset_ _offsetOallowTrfToCon _offsetOcvarIntroMp _offsetOintroCVarIntroMp _offsetOlev _offsetOnmMp _offsetOnoTrfNmS _offsetOnoTrfToConNmS 
              ( _fldExprIbindNmMp,_fldExprIcTrf,_fldExprIfvS,_fldExprIlevOf,_fldExprImbRepl) =
                  fldExpr_ _fldExprOallowTrfToCon _fldExprOcvarIntroMp _fldExprOintroCVarIntroMp _fldExprOlev _fldExprOnmMp _fldExprOnoTrfNmS _fldExprOnoTrfToConNmS 
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOmbRepl :: (Maybe CExpr)
              _lhsOcTrf :: CExpr 
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsObindNmMp :: NmMp
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 92, column 17)
              _lhsOmbRepl =
                  Just _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 166, column 17)
              _lhsOcTrf =
                  maybe _cTrf id $ Map.lookup _nm $ _lhsInmMp
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nm =
                  acbrefNm ref_
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 15, column 17)
              _nmAsp =
                  mkHNm ref_
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 7, column 17)
              _lhsOfvS =
                  Set.singleton _nm
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 59, column 17)
              _lhsOlevOf =
                  fvLev _nm _lhsIcvarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 172, column 41)
              _lhsObindNmMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Var ref_
          in  ( _lhsObindNmMp,_lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOmbRepl)))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local cTrf        : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local cTrf        : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CExprAnn :: CExprAnn  ->
                T_CExprAnn 
sem_CExprAnn (CExprAnn_Coe _coe )  =
    (sem_CExprAnn_Coe _coe )
sem_CExprAnn (CExprAnn_Debug _info )  =
    (sem_CExprAnn_Debug _info )
sem_CExprAnn (CExprAnn_Ty _ty )  =
    (sem_CExprAnn_Ty _ty )
-- semantic domain
type T_CExprAnn  = CVarIntroMp ->
                   CVarIntroMp ->
                   Int ->
                   NmMp ->
                   ( CExprAnn ,FvS,Int)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExprAnn_Coe coe_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExprAnn_Debug info_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CExprAnn_Ty ty_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         self                 : SELF 
   alternatives:
      alternative Apply0:
         visit 0:
            local cTrf        : _
            local self        : _
      alternative Function0:
         visit 0:
            local cTrf        : _
            local self        : _
      alternative Function1:
         visit 0:
            local cTrf        : _
            local self        : _
      alternative Plain:
         visit 0:
            local cTrf        : _
            local self        : _
-}
-- cata
sem_CMetaBind :: CMetaBind  ->
                 T_CMetaBind 
sem_CMetaBind (CMetaBind_Apply0 )  =
    (sem_CMetaBind_Apply0 )
sem_CMetaBind (CMetaBind_Function0 )  =
    (sem_CMetaBind_Function0 )
sem_CMetaBind (CMetaBind_Function1 )  =
    (sem_CMetaBind_Function1 )
sem_CMetaBind (CMetaBind_Plain )  =
    (sem_CMetaBind_Plain )
-- semantic domain
type T_CMetaBind  = CVarIntroMp ->
                    CVarIntroMp ->
                    Int ->
                    NmMp ->
                    ( CMetaBind ,FvS,Int,CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaBind_Apply0
              -- self rule
              _self =
                  CMetaBind_Apply0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaBind_Function0
              -- self rule
              _self =
                  CMetaBind_Function0
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaBind_Function1
              -- self rule
              _self =
                  CMetaBind_Function1
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaBind_Plain
              -- self rule
              _self =
                  CMetaBind_Plain
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOself)))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         bindNmMp             : NmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         nameS                : HsNameS
         self                 : SELF 
   alternatives:
      alternative Dict:
         visit 0:
            local cTrf        : _
            local self        : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
            local self        : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local cTrf        : _
            local self        : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local cTrf        : _
            local self        : _
      alternative Val:
         visit 0:
            local cTrf        : _
            local self        : _
-}
-- cata
sem_CMetaVal :: CMetaVal  ->
                T_CMetaVal 
sem_CMetaVal (CMetaVal_Dict )  =
    (sem_CMetaVal_Dict )
sem_CMetaVal (CMetaVal_DictClass _tracks )  =
    (sem_CMetaVal_DictClass _tracks )
sem_CMetaVal (CMetaVal_DictInstance _tracks )  =
    (sem_CMetaVal_DictInstance _tracks )
sem_CMetaVal (CMetaVal_Track _track )  =
    (sem_CMetaVal_Track _track )
sem_CMetaVal (CMetaVal_Val )  =
    (sem_CMetaVal_Val )
-- semantic domain
type T_CMetaVal  = NmMp ->
                   CVarIntroMp ->
                   CVarIntroMp ->
                   Int ->
                   NmMp ->
                   ( CMetaVal ,FvS,Int,HsNameS,CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnameS :: HsNameS
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 182, column 21)
              _lhsOnameS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaVal_Dict
              -- self rule
              _self =
                  CMetaVal_Dict
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnameS :: HsNameS
              _lhsOcTrf :: CMetaVal 
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOself :: CMetaVal 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 184, column 21)
              _lhsOnameS =
                  Set.fromList (concatMap trackNames tracks_)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 189, column 21)
              _lhsOcTrf =
                  CMetaVal_DictClass    (map (substTrack _lhsIbindNmMp) tracks_)
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaVal_DictClass tracks_
              -- self rule
              _self =
                  CMetaVal_DictClass tracks_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnameS :: HsNameS
              _lhsOcTrf :: CMetaVal 
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOself :: CMetaVal 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 184, column 21)
              _lhsOnameS =
                  Set.fromList (concatMap trackNames tracks_)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 190, column 21)
              _lhsOcTrf =
                  CMetaVal_DictInstance (map (substTrack _lhsIbindNmMp) tracks_)
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _self =
                  CMetaVal_DictInstance tracks_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnameS :: HsNameS
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 186, column 41)
              _lhsOnameS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaVal_Track track_
              -- self rule
              _self =
                  CMetaVal_Track track_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnameS :: HsNameS
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 182, column 21)
              _lhsOnameS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CMetaVal_Val
              -- self rule
              _self =
                  CMetaVal_Val
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         bindNmMp             : NmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         nameS                : HsNameS
         self                 : SELF 
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local cTrf        : _
            local self        : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = NmMp ->
                 CVarIntroMp ->
                 CVarIntroMp ->
                 Int ->
                 NmMp ->
                 ( CMetas ,FvS,Int,HsNameS,CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIbindNmMp
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetas 
              _lhsOself :: CMetas 
              _lhsOnameS :: HsNameS
              _x1OcvarIntroMp :: CVarIntroMp
              _x1OintroCVarIntroMp :: CVarIntroMp
              _x1Olev :: Int
              _x1OnmMp :: NmMp
              _x2ObindNmMp :: NmMp
              _x2OcvarIntroMp :: CVarIntroMp
              _x2OintroCVarIntroMp :: CVarIntroMp
              _x2Olev :: Int
              _x2OnmMp :: NmMp
              _x1IcTrf :: CMetaBind 
              _x1IfvS :: FvS
              _x1IlevOf :: Int
              _x1Iself :: CMetaBind 
              _x2IcTrf :: CMetaVal 
              _x2IfvS :: FvS
              _x2IlevOf :: Int
              _x2InameS :: HsNameS
              _x2Iself :: CMetaVal 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _x1IfvS `Set.union` _x2IfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _x1IlevOf `max` _x2IlevOf
              -- self rule
              _cTrf =
                  (_x1IcTrf,_x2IcTrf)
              -- self rule
              _self =
                  (_x1Iself,_x2Iself)
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (up)
              _lhsOnameS =
                  _x2InameS
              -- copy rule (down)
              _x1OcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _x1OintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _x1Olev =
                  _lhsIlev
              -- copy rule (down)
              _x1OnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _x2ObindNmMp =
                  _lhsIbindNmMp
              -- copy rule (down)
              _x2OcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _x2OintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _x2Olev =
                  _lhsIlev
              -- copy rule (down)
              _x2OnmMp =
                  _lhsInmMp
              ( _x1IcTrf,_x1IfvS,_x1IlevOf,_x1Iself) =
                  x1_ _x1OcvarIntroMp _x1OintroCVarIntroMp _x1Olev _x1OnmMp 
              ( _x2IcTrf,_x2IfvS,_x2IlevOf,_x2InameS,_x2Iself) =
                  x2_ _x2ObindNmMp _x2OcvarIntroMp _x2OintroCVarIntroMp _x2Olev _x2OnmMp 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnameS,_lhsOself)))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = CVarIntroMp ->
                  CVarIntroMp ->
                  Int ->
                  NmMp ->
                  HsNameS ->
                  ( CModule ,FvS,Int)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS ->
         (let _exprOallowTrfToCon :: Bool
              _exprOnoTrfToConNmS :: HsNameS
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CModule 
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOnmMp :: NmMp
              _exprOnoTrfNmS :: HsNameS
              _exprIbindNmMp :: NmMp
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIlevOf :: Int
              _exprImbRepl :: (Maybe CExpr)
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 120, column 11)
              _exprOallowTrfToCon =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 131, column 11)
              _exprOnoTrfToConNmS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _exprIlevOf
              -- self rule
              _cTrf =
                  CModule_Mod moduleNm_ _exprIcTrf ctagsMp_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _exprOnoTrfNmS =
                  _lhsInoTrfNmS
              ( _exprIbindNmMp,_exprIcTrf,_exprIfvS,_exprIlevOf,_exprImbRepl) =
                  expr_ _exprOallowTrfToCon _exprOcvarIntroMp _exprOintroCVarIntroMp _exprOlev _exprOnmMp _exprOnoTrfNmS _exprOnoTrfToConNmS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
         visit 0:
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local cTrf        : _
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local cTrf        : _
      alternative Var:
         child pnm            : {HsName}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPat :: CPat  ->
            T_CPat 
sem_CPat (CPat_BoolExpr _cexpr )  =
    (sem_CPat_BoolExpr _cexpr )
sem_CPat (CPat_Char _char )  =
    (sem_CPat_Char _char )
sem_CPat (CPat_Con _tag _rest _binds )  =
    (sem_CPat_Con _tag (sem_CPatRest _rest ) (sem_CPatFldL _binds ) )
sem_CPat (CPat_Int _int )  =
    (sem_CPat_Int _int )
sem_CPat (CPat_Var _pnm )  =
    (sem_CPat_Var _pnm )
-- semantic domain
type T_CPat  = CVarIntroMp ->
               CVarIntroMp ->
               Int ->
               NmMp ->
               ( CPat ,([HsName]),FvS,Int,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CPat_BoolExpr cexpr_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Char char_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              _restOcvarIntroMp :: CVarIntroMp
              _restOintroCVarIntroMp :: CVarIntroMp
              _restOlev :: Int
              _restOnmMp :: NmMp
              _bindsOcvarIntroMp :: CVarIntroMp
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOlev :: Int
              _bindsOnmMp :: NmMp
              _restIcTrf :: CPatRest 
              _restIfvS :: FvS
              _restIlevOf :: Int
              _restInmL :: ([HsName])
              _bindsIcTrf :: CPatFldL 
              _bindsIfldNmL :: ([HsName])
              _bindsIfvS :: FvS
              _bindsIlevOf :: Int
              _bindsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 29, column 17)
              _lhsOnmL =
                  _restInmL ++ _bindsInmL
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _bindsIfldNmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _restIfvS `Set.union` _bindsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _restIlevOf `max` _bindsIlevOf
              -- self rule
              _cTrf =
                  CPat_Con tag_ _restIcTrf _bindsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _restOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _restOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _restOlev =
                  _lhsIlev
              -- copy rule (down)
              _restOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _bindsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOnmMp =
                  _lhsInmMp
              ( _restIcTrf,_restIfvS,_restIlevOf,_restInmL) =
                  rest_ _restOcvarIntroMp _restOintroCVarIntroMp _restOlev _restOnmMp 
              ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOcvarIntroMp _bindsOintroCVarIntroMp _bindsOlev _bindsOnmMp 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CPat_Int int_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 28, column 17)
              _lhsOnmL =
                  [pnm_]
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CPat_Var pnm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local noTrfNmS    : _
            local fldNm       : _
            local cTrf        : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = CVarIntroMp ->
                  CVarIntroMp ->
                  Int ->
                  NmMp ->
                  ( CPatFld ,([HsName]),FvS,Int,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _offsetOallowTrfToCon :: Bool
              _bindOallowTrfToCon :: Bool
              _offsetOnoTrfToConNmS :: HsNameS
              _bindOnoTrfToConNmS :: HsNameS
              _bindOisGlobal :: Bool
              _lhsOfldNmL :: ([HsName])
              _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatFld 
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlev :: Int
              _offsetOnmMp :: NmMp
              _offsetOnoTrfNmS :: HsNameS
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOlev :: Int
              _bindOnmMp :: NmMp
              _bindOnoTrfNmS :: HsNameS
              _fldAnnsOcvarIntroMp :: CVarIntroMp
              _fldAnnsOintroCVarIntroMp :: CVarIntroMp
              _fldAnnsOlev :: Int
              _fldAnnsOnmMp :: NmMp
              _offsetIbindNmMp :: NmMp
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIlevOf :: Int
              _offsetImbRepl :: (Maybe CExpr)
              _bindIbindL :: ([(HsName,CBind)])
              _bindIbindNmMp :: NmMp
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIbindsNoTrfNmS :: HsNameS
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _fldAnnsIcTrf :: CBindAnnL 
              _fldAnnsIfvS :: FvS
              _fldAnnsIlevOf :: Int
              _fldAnnsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 46, column 17)
              _noTrfNmS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 123, column 11)
              _offsetOallowTrfToCon =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 124, column 19)
              _bindOallowTrfToCon =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 134, column 11)
              _offsetOnoTrfToConNmS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 135, column 19)
              _bindOnoTrfToConNmS =
                  Set.empty
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 19, column 17)
              _bindOisGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 23, column 17)
              _fldNm =
                  _bindInm
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 28, column 17)
              _lhsOfldNmL =
                  [_fldNm]
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 22, column 17)
              _lhsOnmL =
                  [_fldNm]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _offsetIfvS `Set.union` _bindIfvS `Set.union` _fldAnnsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _offsetIlevOf `max` _bindIlevOf `max` _fldAnnsIlevOf
              -- self rule
              _cTrf =
                  CPatFld_Fld lbl_ _offsetIcTrf _bindIcTrf _fldAnnsIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOnmMp =
                  _lhsInmMp
              -- copy rule (from local)
              _offsetOnoTrfNmS =
                  _noTrfNmS
              -- copy rule (down)
              _bindOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindOnmMp =
                  _lhsInmMp
              -- copy rule (from local)
              _bindOnoTrfNmS =
                  _noTrfNmS
              -- copy rule (down)
              _fldAnnsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _fldAnnsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldAnnsOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldAnnsOnmMp =
                  _lhsInmMp
              ( _offsetIbindNmMp,_offsetIcTrf,_offsetIfvS,_offsetIlevOf,_offsetImbRepl) =
                  offset_ _offsetOallowTrfToCon _offsetOcvarIntroMp _offsetOintroCVarIntroMp _offsetOlev _offsetOnmMp _offsetOnoTrfNmS _offsetOnoTrfToConNmS 
              ( _bindIbindL,_bindIbindNmMp,_bindIbindsIntroCVarIntroMp,_bindIbindsNoTrfNmS,_bindIcTrf,_bindIcvarIntroExprMp,_bindIfvS,_bindIfvSMp,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOallowTrfToCon _bindOcvarIntroMp _bindOintroCVarIntroMp _bindOisGlobal _bindOlev _bindOnmMp _bindOnoTrfNmS _bindOnoTrfToConNmS 
              ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsIlevOf,_fldAnnsInmL) =
                  fldAnns_ _fldAnnsOcvarIntroMp _fldAnnsOintroCVarIntroMp _fldAnnsOlev _fldAnnsOnmMp 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
         visit 0:
            local cTrf        : _
      alternative Nil:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = CVarIntroMp ->
                   CVarIntroMp ->
                   Int ->
                   NmMp ->
                   ( CPatFldL ,([HsName]),FvS,Int,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlev :: Int
              _hdOnmMp :: NmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlev :: Int
              _tlOnmMp :: NmMp
              _hdIcTrf :: CPatFld 
              _hdIfldNmL :: ([HsName])
              _hdIfvS :: FvS
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CPatFldL 
              _tlIfldNmL :: ([HsName])
              _tlIfvS :: FvS
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _hdIfldNmL ++ _tlIfldNmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _hdIlevOf `max` _tlIlevOf
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  _hdInmL ++ _tlInmL
              -- self rule
              _cTrf =
                  (:) _hdIcTrf _tlIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnmMp =
                  _lhsInmMp
              ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdIlevOf,_hdInmL) =
                  hd_ _hdOcvarIntroMp _hdOintroCVarIntroMp _hdOlev _hdOnmMp 
              ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOintroCVarIntroMp _tlOlev _tlOnmMp 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  []
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Empty:
         visit 0:
            local cTrf        : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = CVarIntroMp ->
                   CVarIntroMp ->
                   Int ->
                   NmMp ->
                   ( CPatRest ,FvS,Int,([HsName]))
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatRest 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 16, column 31)
              _lhsOnmL =
                  []
              -- self rule
              _cTrf =
                  CPatRest_Empty
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatRest 
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 25, column 17)
              _lhsOnmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  CPatRest_Var nm_
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf,_lhsOnmL)))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         noTrfNmS             : HsNameS
      synthesized attribute:
         cTrf                 : CModule 
   alternatives:
      alternative AGItf:
         child module         : CModule 
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = HsNameS ->
                    ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {noTrfNmS_Inh_CodeAGItf :: !(HsNameS)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsInoTrfNmS )  =
    (let ( _lhsOcTrf) = sem _lhsInoTrfNmS 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsInoTrfNmS ->
         (let _moduleOnmMp :: NmMp
              _moduleOlev :: Int
              _moduleOcvarIntroMp :: CVarIntroMp
              _moduleOintroCVarIntroMp :: CVarIntroMp
              _lhsOcTrf :: CModule 
              _moduleOnoTrfNmS :: HsNameS
              _moduleIcTrf :: CModule 
              _moduleIfvS :: FvS
              _moduleIlevOf :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/InlineLetAlias.ag"(line 55, column 17)
              _moduleOnmMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 4, column 17)
              _moduleOlev =
                  cLevModule
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 20, column 17)
              _moduleOcvarIntroMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 41, column 17)
              _moduleOintroCVarIntroMp =
                  Map.empty
              -- copy rule (up)
              _lhsOcTrf =
                  _moduleIcTrf
              -- copy rule (down)
              _moduleOnoTrfNmS =
                  _lhsInoTrfNmS
              ( _moduleIcTrf,_moduleIfvS,_moduleIlevOf) =
                  module_ _moduleOcvarIntroMp _moduleOintroCVarIntroMp _moduleOlev _moduleOnmMp _moduleOnoTrfNmS 
          in  ( _lhsOcTrf)))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allowTrfToCon        : Bool
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         nmMp                 : NmMp
         noTrfNmS             : HsNameS
         noTrfToConNmS        : HsNameS
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local cTrf        : _
      alternative Nothing:
         visit 0:
            local cTrf        : _
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = Bool ->
                  CVarIntroMp ->
                  CVarIntroMp ->
                  Int ->
                  NmMp ->
                  HsNameS ->
                  HsNameS ->
                  ( MbCExpr ,FvS,Int)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              _justOallowTrfToCon :: Bool
              _justOcvarIntroMp :: CVarIntroMp
              _justOintroCVarIntroMp :: CVarIntroMp
              _justOlev :: Int
              _justOnmMp :: NmMp
              _justOnoTrfNmS :: HsNameS
              _justOnoTrfToConNmS :: HsNameS
              _justIbindNmMp :: NmMp
              _justIcTrf :: CExpr 
              _justIfvS :: FvS
              _justIlevOf :: Int
              _justImbRepl :: (Maybe CExpr)
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _justIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _justIlevOf
              -- self rule
              _cTrf =
                  Just _justIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (down)
              _justOallowTrfToCon =
                  _lhsIallowTrfToCon
              -- copy rule (down)
              _justOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _justOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _justOlev =
                  _lhsIlev
              -- copy rule (down)
              _justOnmMp =
                  _lhsInmMp
              -- copy rule (down)
              _justOnoTrfNmS =
                  _lhsInoTrfNmS
              -- copy rule (down)
              _justOnoTrfToConNmS =
                  _lhsInoTrfToConNmS
              ( _justIbindNmMp,_justIcTrf,_justIfvS,_justIlevOf,_justImbRepl) =
                  just_ _justOallowTrfToCon _justOcvarIntroMp _justOintroCVarIntroMp _justOlev _justOnmMp _justOnoTrfNmS _justOnoTrfToConNmS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIallowTrfToCon
       _lhsIcvarIntroMp
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsInmMp
       _lhsInoTrfNmS
       _lhsInoTrfToConNmS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  cLevModule
              -- self rule
              _cTrf =
                  Nothing
              -- self rule
              _lhsOcTrf =
                  _cTrf
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOlevOf)))