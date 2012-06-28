

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag)
module EH101.Core.Trf.LamGlobalAsArg(cmodTrfLamGlobalAsArg) where

import EH.Util.Utils
import Data.Maybe
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Core
import EH101.Ty
import EH101.Core.Utils
import EH101.AbstractCore
import EH101.Base.Debug
import EH.Util.Pretty
import Debug.Trace















cmodTrfLamGlobalAsArg :: CModule -> CModule
cmodTrfLamGlobalAsArg cmod
  =  let  t = wrap_CodeAGItf  (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                              (Inh_CodeAGItf)
     in   cTrf_Syn_CodeAGItf t



type AspBindLamArgInfo = (LamArgMp,Env)



type LamArgMp = Map.Map HsName CVarIntroL




type Env = Map.Map HsName Track

metaExtendTrack :: LamArgMp -> Env -> Track -> Track
metaExtendTrack mp env (TrackVarApply nm ps) = let r = Map.lookup nm mp
                                                   xs = maybe [] assocLKeys r
                                                   ts = map (\x -> Map.findWithDefault (TrackVarApply x []) x env) xs
                                               in  -- trace (show nm ++ " extended with " ++ show ts) $
                                                         (TrackVarApply nm (ts++ps))
metaExtendTrack _ _ t = t

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local lev         : _
            local whatAbove   : {WhatExpr}
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
type T_CAlt  = CVarReplNmMp ->
               CVarIntroMp ->
               EvalCtx ->
               Int ->
               CVarIntroMp ->
               Bool ->
               Bool ->
               LamArgMp ->
               FvSMp ->
               FvS ->
               Int ->
               FvS ->
               ( CAlt ,FvS,Int,Int)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _exprOmbCtxCount :: (Maybe Int)
              _exprOisDictClass :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _lhsOlevOf :: Int
              _exprOvarS :: FvS
              _lhsOfvS :: FvS
              _lhsOcTrf :: CAlt 
              _lhsOgUniq :: Int
              _patOargMp :: CVarReplNmMp
              _patOcvarIntroMp :: CVarIntroMp
              _patOgUniq :: Int
              _patOintroCVarIntroMp :: CVarIntroMp
              _patOlamArgMp :: LamArgMp
              _patOlamFvSMp :: FvSMp
              _patOlamS :: FvS
              _patOlev :: Int
              _patOvarS :: FvS
              _exprOargMp :: CVarReplNmMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOwhatAbove :: WhatExpr
              _patIcTrf :: CPat 
              _patIfldNmL :: ([HsName])
              _patIfvS :: FvS
              _patIgUniq :: Int
              _patIlevOf :: Int
              _patInmL :: ([HsName])
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 186, column 10)
              _exprOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 187, column 10)
              _exprOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 10, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 47, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 82, column 17)
              _whatAbove =
                  ExprIsOther
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
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 23, column 17)
              _exprOvarS =
                  _lhsIvarS `Set.union` Set.fromList _patInmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _patOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _patOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _patOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _patOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _patOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _patOlamS =
                  _lhsIlamS
              -- copy rule (from local)
              _patOlev =
                  _lev
              -- copy rule (down)
              _patOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _exprOgUniq =
                  _patIgUniq
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (from local)
              _exprOlev =
                  _lev
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _patIcTrf,_patIfldNmL,_patIfvS,_patIgUniq,_patIlevOf,_patInmL) =
                  pat_ _patOargMp _patOcvarIntroMp _patOgUniq _patOintroCVarIntroMp _patOlamArgMp _patOlamFvSMp _patOlamS _patOlev _patOvarS 
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CAltL  = CVarReplNmMp ->
                CVarIntroMp ->
                EvalCtx ->
                Int ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                LamArgMp ->
                FvSMp ->
                FvS ->
                Int ->
                FvS ->
                ( CAltL ,FvS,Int,Int)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              _lhsOgUniq :: Int
              _hdOargMp :: CVarReplNmMp
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: Int
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamArgMp :: LamArgMp
              _hdOlamFvSMp :: FvSMp
              _hdOlamS :: FvS
              _hdOlev :: Int
              _hdOvarS :: FvS
              _tlOargMp :: CVarReplNmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: Int
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamArgMp :: LamArgMp
              _tlOlamFvSMp :: FvSMp
              _tlOlamS :: FvS
              _tlOlev :: Int
              _tlOvarS :: FvS
              _hdIcTrf :: CAlt 
              _hdIfvS :: FvS
              _hdIgUniq :: Int
              _hdIlevOf :: Int
              _tlIcTrf :: CAltL 
              _tlIfvS :: FvS
              _tlIgUniq :: Int
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
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (down)
              _hdOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _hdOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _hdOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _tlOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _tlOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _tlOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOvarS =
                  _lhsIvarS
              ( _hdIcTrf,_hdIfvS,_hdIgUniq,_hdIlevOf) =
                  hd_ _hdOargMp _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisLamBody _hdOisStrict _hdOlamArgMp _hdOlamFvSMp _hdOlamS _hdOlev _hdOvarS 
              ( _tlIcTrf,_tlIfvS,_tlIgUniq,_tlIlevOf) =
                  tl_ _tlOargMp _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisLamBody _tlOisStrict _tlOlamArgMp _tlOlamFvSMp _tlOlamS _tlOlev _tlOvarS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         letBindingsCateg     : CBindCateg
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         bindLamArgMp         : LamArgMp
         bindLamS             : FvS
         bindVarS             : FvS
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         envUp                : Env
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
            local aspBindLamArgMp : _
            local cTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = CVarReplNmMp ->
                CVarIntroMp ->
                EvalCtx ->
                Int ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                Bool ->
                LamArgMp ->
                FvSMp ->
                FvS ->
                CBindCateg ->
                Int ->
                FvS ->
                ( LamArgMp,FvS,FvS,CVarIntroMp,CBind ,CVarIntroMp,Env,FvS,FvSMp,Int,Int,HsName,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIvarS ->
         (let _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _lhsOfvSMp :: FvSMp
              _lhsOnmL :: ([HsName])
              _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CBind 
              _lhsOgUniq :: Int
              _bindAspectsOargMp :: CVarReplNmMp
              _bindAspectsOaspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _bindAspectsOcvarIntroMp :: CVarIntroMp
              _bindAspectsOevalCtx :: EvalCtx
              _bindAspectsOgUniq :: Int
              _bindAspectsOintroCVarIntroMp :: CVarIntroMp
              _bindAspectsOisGlobal :: Bool
              _bindAspectsOisLamBody :: Bool
              _bindAspectsOisStrict :: Bool
              _bindAspectsOlamArgMp :: LamArgMp
              _bindAspectsOlamFvSMp :: FvSMp
              _bindAspectsOlamS :: FvS
              _bindAspectsOletBindingsCateg :: CBindCateg
              _bindAspectsOlev :: Int
              _bindAspectsOvarS :: FvS
              _bindAspectsIbindLamArgMp :: LamArgMp
              _bindAspectsIbindLamS :: FvS
              _bindAspectsIbindVarS :: FvS
              _bindAspectsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIcvarIntroExprMp :: CVarIntroMp
              _bindAspectsIenvUp :: Env
              _bindAspectsIfvS :: FvS
              _bindAspectsIfvSMp :: FvSMp
              _bindAspectsIgUniq :: Int
              _bindAspectsIgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _bindAspectsIlevOf :: Int
              _bindAspectsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 90, column 25)
              _aspBindLamArgMp =
                  _bindAspectsIgathAspBindLamArgMp
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bindAspectsIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  _bindAspectsIbindLamS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  _bindAspectsIbindVarS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _bindAspectsIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _bindAspectsIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  _bindAspectsIenvUp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _bindAspectsIgUniq
              -- copy rule (down)
              _bindAspectsOargMp =
                  _lhsIargMp
              -- copy rule (from local)
              _bindAspectsOaspBindLamArgMp =
                  _aspBindLamArgMp
              -- copy rule (down)
              _bindAspectsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindAspectsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bindAspectsOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bindAspectsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindAspectsOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _bindAspectsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindAspectsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindAspectsOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bindAspectsOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bindAspectsOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bindAspectsOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _bindAspectsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindAspectsOvarS =
                  _lhsIvarS
              ( _bindAspectsIbindLamArgMp,_bindAspectsIbindLamS,_bindAspectsIbindVarS,_bindAspectsIbindsIntroCVarIntroMp,_bindAspectsIcTrf,_bindAspectsIcvarIntroExprMp,_bindAspectsIenvUp,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsIgUniq,_bindAspectsIgathAspBindLamArgMp,_bindAspectsIlevOf,_bindAspectsInmL) =
                  bindAspects_ _bindAspectsOargMp _bindAspectsOaspBindLamArgMp _bindAspectsOcvarIntroMp _bindAspectsOevalCtx _bindAspectsOgUniq _bindAspectsOintroCVarIntroMp _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOlamArgMp _bindAspectsOlamFvSMp _bindAspectsOlamS _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOnm _bindAspectsOvarS 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevOf,_lhsOnm,_lhsOnmL)))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CBindAnn  = CVarReplNmMp ->
                   CVarIntroMp ->
                   Int ->
                   CVarIntroMp ->
                   LamArgMp ->
                   FvSMp ->
                   FvS ->
                   Int ->
                   FvS ->
                   ( CBindAnn ,FvS,Int,Int,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnn 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CBindAnnL  = CVarReplNmMp ->
                    CVarIntroMp ->
                    Int ->
                    CVarIntroMp ->
                    LamArgMp ->
                    FvSMp ->
                    FvS ->
                    Int ->
                    FvS ->
                    ( CBindAnnL ,FvS,Int,Int,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              _lhsOgUniq :: Int
              _hdOargMp :: CVarReplNmMp
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOgUniq :: Int
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlamArgMp :: LamArgMp
              _hdOlamFvSMp :: FvSMp
              _hdOlamS :: FvS
              _hdOlev :: Int
              _hdOvarS :: FvS
              _tlOargMp :: CVarReplNmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOgUniq :: Int
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlamArgMp :: LamArgMp
              _tlOlamFvSMp :: FvSMp
              _tlOlamS :: FvS
              _tlOlev :: Int
              _tlOvarS :: FvS
              _hdIcTrf :: CBindAnn 
              _hdIfvS :: FvS
              _hdIgUniq :: Int
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CBindAnnL 
              _tlIfvS :: FvS
              _tlIgUniq :: Int
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
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (down)
              _hdOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _hdOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _hdOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _tlOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _tlOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _tlOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOvarS =
                  _lhsIvarS
              ( _hdIcTrf,_hdIfvS,_hdIgUniq,_hdIlevOf,_hdInmL) =
                  hd_ _hdOargMp _hdOcvarIntroMp _hdOgUniq _hdOintroCVarIntroMp _hdOlamArgMp _hdOlamFvSMp _hdOlamS _hdOlev _hdOvarS 
              ( _tlIcTrf,_tlIfvS,_tlIgUniq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOargMp _tlOcvarIntroMp _tlOgUniq _tlOintroCVarIntroMp _tlOlamArgMp _tlOlamFvSMp _tlOlamS _tlOlev _tlOvarS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         letBindingsCateg     : CBindCateg
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         bindLamArgMp         : LamArgMp
         bindLamS             : FvS
         bindVarS             : FvS
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         envUp                : Env
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
type T_CBindL  = CVarReplNmMp ->
                 CVarIntroMp ->
                 EvalCtx ->
                 Int ->
                 CVarIntroMp ->
                 Bool ->
                 Bool ->
                 Bool ->
                 LamArgMp ->
                 FvSMp ->
                 FvS ->
                 CBindCateg ->
                 Int ->
                 FvS ->
                 ( LamArgMp,FvS,FvS,CVarIntroMp,CBindL ,CVarIntroMp,Env,FvS,FvSMp,Int,Int,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIvarS ->
         (let _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              _lhsOgUniq :: Int
              _hdOargMp :: CVarReplNmMp
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: Int
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamArgMp :: LamArgMp
              _hdOlamFvSMp :: FvSMp
              _hdOlamS :: FvS
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOvarS :: FvS
              _tlOargMp :: CVarReplNmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: Int
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamArgMp :: LamArgMp
              _tlOlamFvSMp :: FvSMp
              _tlOlamS :: FvS
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOvarS :: FvS
              _hdIbindLamArgMp :: LamArgMp
              _hdIbindLamS :: FvS
              _hdIbindVarS :: FvS
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIcTrf :: CBind 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIenvUp :: Env
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: Int
              _hdIlevOf :: Int
              _hdInm :: HsName
              _hdInmL :: ([HsName])
              _tlIbindLamArgMp :: LamArgMp
              _tlIbindLamS :: FvS
              _tlIbindVarS :: FvS
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIcTrf :: CBindL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIenvUp :: Env
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: Int
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _hdIbindLamArgMp `Map.union` _tlIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  _hdIbindLamS `Set.union` _tlIbindLamS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  _hdIbindVarS `Set.union` _tlIbindVarS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  _hdIenvUp `Map.union` _tlIenvUp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (down)
              _hdOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _hdOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _hdOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _tlOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _tlOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _tlOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOvarS =
                  _lhsIvarS
              ( _hdIbindLamArgMp,_hdIbindLamS,_hdIbindVarS,_hdIbindsIntroCVarIntroMp,_hdIcTrf,_hdIcvarIntroExprMp,_hdIenvUp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIlevOf,_hdInm,_hdInmL) =
                  hd_ _hdOargMp _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOlamArgMp _hdOlamFvSMp _hdOlamS _hdOletBindingsCateg _hdOlev _hdOvarS 
              ( _tlIbindLamArgMp,_tlIbindLamS,_tlIbindVarS,_tlIbindsIntroCVarIntroMp,_tlIcTrf,_tlIcvarIntroExprMp,_tlIenvUp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOargMp _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamArgMp _tlOlamFvSMp _tlOlamS _tlOletBindingsCateg _tlOlev _tlOvarS 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsIvarS ->
         (let _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              _lhsOgUniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         aspBindLamArgMp      : ACoreBindAspMp AspBindLamArgInfo
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isDictClass          : Bool
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         letBindingsCateg     : CBindCateg
         lev                  : Int
         mbCtxCount           : Maybe Int
         nm                   : HsName
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         bindLamArgMp         : LamArgMp
         bindLamS             : FvS
         bindVarS             : FvS
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         envUp                : Env
         fvS                  : FvS
         fvSMp                : FvSMp
         gathAspBindLamArgMp  : ACoreBindAspMp AspBindLamArgInfo
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local _tup1       : _
            local argNewL     : _
            local argMpNew    : _
            local bindLamArgMp : _
            local whatAbove   : {WhatExpr}
            local cmetaVal    : _
            local cvi         : _
            local cviExpr     : _
            local _tup2       : {(FvS,FvS)}
            local cTrf        : _
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
         visit 0:
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
         visit 0:
            local _tup3       : {(LamArgMp,Env)}
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
            local whatAbove   : {WhatExpr}
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
type T_CBound  = CVarReplNmMp ->
                 (ACoreBindAspMp AspBindLamArgInfo) ->
                 CVarIntroMp ->
                 EvalCtx ->
                 Int ->
                 CVarIntroMp ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 LamArgMp ->
                 FvSMp ->
                 FvS ->
                 CBindCateg ->
                 Int ->
                 (Maybe Int) ->
                 HsName ->
                 FvS ->
                 ( LamArgMp,FvS,FvS,CVarIntroMp,CBound ,CVarIntroMp,Env,FvS,FvSMp,Int,(ACoreBindAspMp AspBindLamArgInfo),Int,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let _exprOgUniq :: Int
              _exprOargMp :: CVarReplNmMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOcTrf :: CBound 
              _bindMetaObindLamArgMp :: LamArgMp
              _exprOmbCtxCount :: (Maybe Int)
              _exprOisDictClass :: Bool
              _bindMetaOenvFinal :: Env
              _lhsOenvUp :: Env
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              __tup2 :: ((FvS,FvS))
              _lhsObindVarS :: FvS
              _lhsObindLamS :: FvS
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOgUniq :: Int
              _bindMetaOargMp :: CVarReplNmMp
              _bindMetaOcvarIntroMp :: CVarIntroMp
              _bindMetaOgUniq :: Int
              _bindMetaOintroCVarIntroMp :: CVarIntroMp
              _bindMetaOlamArgMp :: LamArgMp
              _bindMetaOlamFvSMp :: FvSMp
              _bindMetaOlamS :: FvS
              _bindMetaOlev :: Int
              _bindMetaOvarS :: FvS
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _bindMetaIcTrf :: CMetas 
              _bindMetaIfvS :: FvS
              _bindMetaIgUniq :: Int
              _bindMetaIisDictClass :: Bool
              _bindMetaIisDictInstance :: Bool
              _bindMetaIisInstance :: Bool
              _bindMetaIlevOf :: Int
              _bindMetaImbTrack :: (Maybe Track)
              _bindMetaIself :: CMetas 
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 56, column 17)
              _exprOgUniq =
                  _lhsIgUniq + Map.size _argMpNew
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 81, column 17)
              _exprOargMp =
                  _argMpNew `Map.union` _lhsIargMp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 94, column 25)
              _lhsOgathAspBindLamArgMp =
                  Map.singleton acbaspkeyDefault
                            ( _exprIbindLamArgMp
                            , _exprIenvUp
                            )
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 134, column 33)
              __tup1 =
                  if                                    isJust _exprImbLam
                  then  let  argLevL = fvLAsArg _lhsIintroCVarIntroMp $ panicJust "LamGlobalAsArg.CBind.Bind.argLevL" $ Map.lookup _lhsInm $ _lhsIlamFvSMp
                             (argOL,argNL,argONMp) = fvLArgRepl _lhsIgUniq $ argLevL
                        in   (argNL, argONMp, _lhsInm `Map.singleton` argOL)
                  else  ([],Map.empty, Map.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 134, column 33)
              (_argNewL,_,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 134, column 33)
              (_,_argMpNew,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 134, column 33)
              (_,_,_bindLamArgMp) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 142, column 17)
              _lhsOcTrf =
                  (acoreBoundVal1Metas _lhsInm _bindMetaIcTrf $ acoreLam [(n) | (n,cvi) <- _argNewL] _exprIcTrf)
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 146, column 17)
              _bindMetaObindLamArgMp =
                  _exprIbindLamArgMp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 194, column 11)
              _exprOmbCtxCount =
                  if _bindMetaIisDictInstance
                  then Just 0
                  else Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 197, column 11)
              _exprOisDictClass =
                  _bindMetaIisDictClass
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 228, column 11)
              _bindMetaOenvFinal =
                  _exprIenvUp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 234, column 12)
              _lhsOenvUp =
                  maybe (Map.empty)
                        (\t -> Map.singleton _lhsInm t)
                        _bindMetaImbTrack
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
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
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 6, column 33)
              __tup2 =
                  if isJust _exprImbLam
                  then (Set.empty,Set.singleton _lhsInm)
                  else (Set.singleton _lhsInm,Set.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 6, column 33)
              (_lhsObindVarS,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 6, column 33)
              (_,_lhsObindLamS) =
                  __tup2
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _bindMetaOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bindMetaOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindMetaOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bindMetaOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindMetaOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bindMetaOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bindMetaOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bindMetaOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindMetaOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _bindMetaIcTrf,_bindMetaIfvS,_bindMetaIgUniq,_bindMetaIisDictClass,_bindMetaIisDictInstance,_bindMetaIisInstance,_bindMetaIlevOf,_bindMetaImbTrack,_bindMetaIself) =
                  bindMeta_ _bindMetaOargMp _bindMetaObindLamArgMp _bindMetaOcvarIntroMp _bindMetaOenvFinal _bindMetaOgUniq _bindMetaOintroCVarIntroMp _bindMetaOlamArgMp _bindMetaOlamFvSMp _bindMetaOlamS _bindMetaOlev _bindMetaOvarS 
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let _exprOmbCtxCount :: (Maybe Int)
              _exprOisDictClass :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: Int
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 182, column 10)
              _exprOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 183, column 10)
              _exprOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 39, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 76, column 17)
              _whatAbove =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  _exprIenvUp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let __tup3 :: ((LamArgMp,Env))
              _cmetasObindLamArgMp :: LamArgMp
              _cmetasOenvFinal :: Env
              _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: Int
              _cmetasOargMp :: CVarReplNmMp
              _cmetasOcvarIntroMp :: CVarIntroMp
              _cmetasOgUniq :: Int
              _cmetasOintroCVarIntroMp :: CVarIntroMp
              _cmetasOlamArgMp :: LamArgMp
              _cmetasOlamFvSMp :: FvSMp
              _cmetasOlamS :: FvS
              _cmetasOlev :: Int
              _cmetasOvarS :: FvS
              _cmetasIcTrf :: CMetas 
              _cmetasIfvS :: FvS
              _cmetasIgUniq :: Int
              _cmetasIisDictClass :: Bool
              _cmetasIisDictInstance :: Bool
              _cmetasIisInstance :: Bool
              _cmetasIlevOf :: Int
              _cmetasImbTrack :: (Maybe Track)
              _cmetasIself :: CMetas 
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 112, column 49)
              __tup3 =
                  panicJust "LamGlobalAsArg.CBound.Meta.aspBindLamArgMp" $ Map.lookup acbaspkeyDefault _lhsIaspBindLamArgMp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 112, column 49)
              (_cmetasObindLamArgMp,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 112, column 49)
              (_,_cmetasOenvFinal) =
                  __tup3
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _cmetasIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (up)
              _lhsOgUniq =
                  _cmetasIgUniq
              -- copy rule (down)
              _cmetasOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _cmetasOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _cmetasOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _cmetasOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _cmetasOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _cmetasOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _cmetasOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _cmetasOlev =
                  _lhsIlev
              -- copy rule (down)
              _cmetasOvarS =
                  _lhsIvarS
              ( _cmetasIcTrf,_cmetasIfvS,_cmetasIgUniq,_cmetasIisDictClass,_cmetasIisDictInstance,_cmetasIisInstance,_cmetasIlevOf,_cmetasImbTrack,_cmetasIself) =
                  cmetas_ _cmetasOargMp _cmetasObindLamArgMp _cmetasOcvarIntroMp _cmetasOenvFinal _cmetasOgUniq _cmetasOintroCVarIntroMp _cmetasOlamArgMp _cmetasOlamFvSMp _cmetasOlamS _cmetasOlev _cmetasOvarS 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImbCtxCount
       _lhsInm
       _lhsIvarS ->
         (let _exprOmbCtxCount :: (Maybe Int)
              _exprOisDictClass :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: Int
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 198, column 11)
              _exprOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 199, column 11)
              _exprOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  _exprIenvUp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         aspBindLamArgMp      : ACoreBindAspMp AspBindLamArgInfo
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         letBindingsCateg     : CBindCateg
         lev                  : Int
         nm                   : HsName
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         bindLamArgMp         : LamArgMp
         bindLamS             : FvS
         bindVarS             : FvS
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         envUp                : Env
         fvS                  : FvS
         fvSMp                : FvSMp
         gathAspBindLamArgMp  : ACoreBindAspMp AspBindLamArgInfo
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
type T_CBoundL  = CVarReplNmMp ->
                  (ACoreBindAspMp AspBindLamArgInfo) ->
                  CVarIntroMp ->
                  EvalCtx ->
                  Int ->
                  CVarIntroMp ->
                  Bool ->
                  Bool ->
                  Bool ->
                  LamArgMp ->
                  FvSMp ->
                  FvS ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  FvS ->
                  ( LamArgMp,FvS,FvS,CVarIntroMp,CBoundL ,CVarIntroMp,Env,FvS,FvSMp,Int,(ACoreBindAspMp AspBindLamArgInfo),Int,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIvarS ->
         (let _hdOmbCtxCount :: (Maybe Int)
              _hdOisDictClass :: Bool
              _hdOisTopApp :: Bool
              _hdOisTopTup :: Bool
              _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              _lhsOgUniq :: Int
              _hdOargMp :: CVarReplNmMp
              _hdOaspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: Int
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlamArgMp :: LamArgMp
              _hdOlamFvSMp :: FvSMp
              _hdOlamS :: FvS
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOnm :: HsName
              _hdOvarS :: FvS
              _tlOargMp :: CVarReplNmMp
              _tlOaspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: Int
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlamArgMp :: LamArgMp
              _tlOlamFvSMp :: FvSMp
              _tlOlamS :: FvS
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOnm :: HsName
              _tlOvarS :: FvS
              _hdIbindLamArgMp :: LamArgMp
              _hdIbindLamS :: FvS
              _hdIbindVarS :: FvS
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIcTrf :: CBound 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIenvUp :: Env
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: Int
              _hdIgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIbindLamArgMp :: LamArgMp
              _tlIbindLamS :: FvS
              _tlIbindVarS :: FvS
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIcTrf :: CBoundL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIenvUp :: Env
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: Int
              _tlIgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 178, column 10)
              _hdOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 179, column 10)
              _hdOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopTup =
                  True
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _hdIbindLamArgMp `Map.union` _tlIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  _hdIbindLamS `Set.union` _tlIbindLamS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  _hdIbindVarS `Set.union` _tlIbindVarS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  _hdIenvUp `Map.union` _tlIenvUp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
                  _hdIgathAspBindLamArgMp `Map.union` _tlIgathAspBindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (down)
              _hdOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _hdOaspBindLamArgMp =
                  _lhsIaspBindLamArgMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _hdOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _hdOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _hdOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _hdOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _hdOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
              -- copy rule (down)
              _hdOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _tlOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _tlOaspBindLamArgMp =
                  _lhsIaspBindLamArgMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _tlOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOisGlobal =
                  _lhsIisGlobal
              -- copy rule (down)
              _tlOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _tlOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _tlOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _tlOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _tlOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              -- copy rule (down)
              _tlOvarS =
                  _lhsIvarS
              ( _hdIbindLamArgMp,_hdIbindLamS,_hdIbindVarS,_hdIbindsIntroCVarIntroMp,_hdIcTrf,_hdIcvarIntroExprMp,_hdIenvUp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIgathAspBindLamArgMp,_hdIlevOf,_hdInmL) =
                  hd_ _hdOargMp _hdOaspBindLamArgMp _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisDictClass _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOlamArgMp _hdOlamFvSMp _hdOlamS _hdOletBindingsCateg _hdOlev _hdOmbCtxCount _hdOnm _hdOvarS 
              ( _tlIbindLamArgMp,_tlIbindLamS,_tlIbindVarS,_tlIbindsIntroCVarIntroMp,_tlIcTrf,_tlIcvarIntroExprMp,_tlIenvUp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIgathAspBindLamArgMp,_tlIlevOf,_tlInmL) =
                  tl_ _tlOargMp _tlOaspBindLamArgMp _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOlamArgMp _tlOlamFvSMp _tlOlamS _tlOletBindingsCateg _tlOlev _tlOnm _tlOvarS 
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIargMp
       _lhsIaspBindLamArgMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsInm
       _lhsIvarS ->
         (let _lhsObindLamArgMp :: LamArgMp
              _lhsObindLamS :: FvS
              _lhsObindVarS :: FvS
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOenvUp :: Env
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              _lhsOgUniq :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindLamS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 3, column 39)
              _lhsObindVarS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 231, column 32)
              _lhsOenvUp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 87, column 95)
              _lhsOgathAspBindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsObindLamArgMp,_lhsObindLamS,_lhsObindVarS,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOenvUp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOgathAspBindLamArgMp,_lhsOlevOf,_lhsOnmL)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isDictClass          : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         mbCtxCount           : Maybe Int
         varS                 : FvS
         whatAbove            : WhatExpr
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         appFunKind           : AppFunKind
         bindLamArgMp         : LamArgMp
         cTrf                 : SELF 
         envUp                : Env
         fvS                  : FvS
         levOf                : Int
         mbLam                : Maybe [HsName]
         mbVar                : Maybe HsName
         whatBelow            : WhatExpr
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
            local letBindingsCateg : _
            local isTopTup    : _
            local whatBelow   : _
            local isTopApp'   : _
            local whatAbove   : {WhatExpr}
            local fvS         : _
            local levOf       : _
            local cTrf        : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local lev         : _
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local argNm       : _
            local fvS         : _
            local cvi         : _
            local levOf       : _
            local varS        : _
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local lamFvSMp    : _
            local lamArgMp    : _
            local isGlobal    : _
            local letBindingsCateg : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local isTopLet    : _
            local evalCtx     : _
            local fvS         : _
            local maxBindLev  : _
            local _tup4       : _
            local strLev      : _
            local introCVarIntroMp : _
            local levOf       : _
            local varS        : _
            local lamS        : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local nm          : {HsName}
            local nmAsp       : {HsName}
            local mbVar       : {Maybe HsName}
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
type T_CExpr  = CVarReplNmMp ->
                CVarIntroMp ->
                EvalCtx ->
                Int ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                Bool ->
                Bool ->
                Bool ->
                LamArgMp ->
                FvSMp ->
                FvS ->
                Int ->
                (Maybe Int) ->
                FvS ->
                WhatExpr ->
                ( AppFunKind,LamArgMp,CExpr ,Env,FvS,Int,Int,(Maybe [HsName]),(Maybe HsName),WhatExpr)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOappFunKind :: AppFunKind
              _lhsOgUniq :: Int
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _lhsOwhatBelow :: WhatExpr
              _annOargMp :: CVarReplNmMp
              _annOcvarIntroMp :: CVarIntroMp
              _annOgUniq :: Int
              _annOintroCVarIntroMp :: CVarIntroMp
              _annOlamArgMp :: LamArgMp
              _annOlamFvSMp :: FvSMp
              _annOlamS :: FvS
              _annOlev :: Int
              _annOvarS :: FvS
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisDictClass :: Bool
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOmbCtxCount :: (Maybe Int)
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _annIcTrf :: CExprAnn 
              _annIfvS :: FvS
              _annIgUniq :: Int
              _annIlevOf :: Int
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp
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
              _lhsOappFunKind =
                  _exprIappFunKind
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOmbLam =
                  _exprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _exprImbVar
              -- copy rule (up)
              _lhsOwhatBelow =
                  _exprIwhatBelow
              -- copy rule (down)
              _annOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _annOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _annOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _annOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _annOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _annOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _annOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _annOlev =
                  _lhsIlev
              -- copy rule (down)
              _annOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _exprOgUniq =
                  _annIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _exprOisTopApp =
                  _lhsIisTopApp
              -- copy rule (down)
              _exprOisTopTup =
                  _lhsIisTopTup
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _exprOwhatAbove =
                  _lhsIwhatAbove
              ( _annIcTrf,_annIfvS,_annIgUniq,_annIlevOf) =
                  ann_ _annOargMp _annOcvarIntroMp _annOgUniq _annOintroCVarIntroMp _annOlamArgMp _annOlamFvSMp _annOlamS _annOlev _annOvarS 
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _argOaspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _lhsOenvUp :: Env
              _funcOisTopApp :: Bool
              _argOisTopApp :: Bool
              _whatAbove :: WhatExpr
              _argOnm :: HsName
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _funcOargMp :: CVarReplNmMp
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOevalCtx :: EvalCtx
              _funcOgUniq :: Int
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOisDictClass :: Bool
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopTup :: Bool
              _funcOlamArgMp :: LamArgMp
              _funcOlamFvSMp :: FvSMp
              _funcOlamS :: FvS
              _funcOlev :: Int
              _funcOmbCtxCount :: (Maybe Int)
              _funcOvarS :: FvS
              _funcOwhatAbove :: WhatExpr
              _argOargMp :: CVarReplNmMp
              _argOcvarIntroMp :: CVarIntroMp
              _argOevalCtx :: EvalCtx
              _argOgUniq :: Int
              _argOintroCVarIntroMp :: CVarIntroMp
              _argOisDictClass :: Bool
              _argOisGlobal :: Bool
              _argOisLamBody :: Bool
              _argOisStrict :: Bool
              _argOisTopTup :: Bool
              _argOlamArgMp :: LamArgMp
              _argOlamFvSMp :: FvSMp
              _argOlamS :: FvS
              _argOletBindingsCateg :: CBindCateg
              _argOlev :: Int
              _argOmbCtxCount :: (Maybe Int)
              _argOvarS :: FvS
              _funcIappFunKind :: AppFunKind
              _funcIbindLamArgMp :: LamArgMp
              _funcIcTrf :: CExpr 
              _funcIenvUp :: Env
              _funcIfvS :: FvS
              _funcIgUniq :: Int
              _funcIlevOf :: Int
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              _funcIwhatBelow :: WhatExpr
              _argIbindLamArgMp :: LamArgMp
              _argIbindLamS :: FvS
              _argIbindVarS :: FvS
              _argIbindsIntroCVarIntroMp :: CVarIntroMp
              _argIcTrf :: CBound 
              _argIcvarIntroExprMp :: CVarIntroMp
              _argIenvUp :: Env
              _argIfvS :: FvS
              _argIfvSMp :: FvSMp
              _argIgUniq :: Int
              _argIgathAspBindLamArgMp :: (ACoreBindAspMp AspBindLamArgInfo)
              _argIlevOf :: Int
              _argInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 101, column 25)
              _argOaspBindLamArgMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 13, column 17)
              _funcOisTopApp =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 14, column 17)
              _argOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 56, column 17)
              _whatBelow =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _funcIwhatBelow
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 61, column 17)
              _isTopApp' =
                  isNothing $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 67, column 17)
              _whatAbove =
                  maybe (ExprIsApp 1) (\a -> ExprIsApp $ a + 1) $ whatExprMbApp _lhsIwhatAbove
              -- "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 7, column 17)
              _argOnm =
                  hsnUnknown
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 8, column 17)
              _fvS =
                  _funcIfvS `Set.union` _argIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 60, column 17)
              _levOf =
                  _funcIlevOf `max` _argIlevOf
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 14, column 17)
              _lhsOappFunKind =
                  _funcIappFunKind
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _funcIbindLamArgMp `Map.union` _argIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _argIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _funcOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _funcOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _funcOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _funcOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _funcOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _funcOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _argOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _argOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _argOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _argOgUniq =
                  _funcIgUniq
              -- copy rule (down)
              _argOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _argOisDictClass =
                  _lhsIisDictClass
              -- copy rule (from local)
              _argOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _argOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _argOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _argOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _argOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _argOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _argOlamS =
                  _lhsIlamS
              -- copy rule (from local)
              _argOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _argOlev =
                  _lhsIlev
              -- copy rule (down)
              _argOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _argOvarS =
                  _lhsIvarS
              ( _funcIappFunKind,_funcIbindLamArgMp,_funcIcTrf,_funcIenvUp,_funcIfvS,_funcIgUniq,_funcIlevOf,_funcImbLam,_funcImbVar,_funcIwhatBelow) =
                  func_ _funcOargMp _funcOcvarIntroMp _funcOevalCtx _funcOgUniq _funcOintroCVarIntroMp _funcOisDictClass _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamArgMp _funcOlamFvSMp _funcOlamS _funcOlev _funcOmbCtxCount _funcOvarS _funcOwhatAbove 
              ( _argIbindLamArgMp,_argIbindLamS,_argIbindVarS,_argIbindsIntroCVarIntroMp,_argIcTrf,_argIcvarIntroExprMp,_argIenvUp,_argIfvS,_argIfvSMp,_argIgUniq,_argIgathAspBindLamArgMp,_argIlevOf,_argInmL) =
                  arg_ _argOargMp _argOaspBindLamArgMp _argOcvarIntroMp _argOevalCtx _argOgUniq _argOintroCVarIntroMp _argOisDictClass _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOlamArgMp _argOlamFvSMp _argOlamS _argOletBindingsCateg _argOlev _argOmbCtxCount _argOnm _argOvarS 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisDictClass :: Bool
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOmbCtxCount :: (Maybe Int)
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _altsOargMp :: CVarReplNmMp
              _altsOcvarIntroMp :: CVarIntroMp
              _altsOevalCtx :: EvalCtx
              _altsOgUniq :: Int
              _altsOintroCVarIntroMp :: CVarIntroMp
              _altsOisLamBody :: Bool
              _altsOisStrict :: Bool
              _altsOlamArgMp :: LamArgMp
              _altsOlamFvSMp :: FvSMp
              _altsOlamS :: FvS
              _altsOlev :: Int
              _altsOvarS :: FvS
              _dfltOargMp :: CVarReplNmMp
              _dfltOcvarIntroMp :: CVarIntroMp
              _dfltOevalCtx :: EvalCtx
              _dfltOgUniq :: Int
              _dfltOintroCVarIntroMp :: CVarIntroMp
              _dfltOisDictClass :: Bool
              _dfltOisLamBody :: Bool
              _dfltOisStrict :: Bool
              _dfltOisTopApp :: Bool
              _dfltOisTopTup :: Bool
              _dfltOlamArgMp :: LamArgMp
              _dfltOlamFvSMp :: FvSMp
              _dfltOlamS :: FvS
              _dfltOlev :: Int
              _dfltOmbCtxCount :: (Maybe Int)
              _dfltOvarS :: FvS
              _dfltOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              _altsIcTrf :: CAltL 
              _altsIfvS :: FvS
              _altsIgUniq :: Int
              _altsIlevOf :: Int
              _dfltIappFunKind :: AppFunKind
              _dfltIbindLamArgMp :: LamArgMp
              _dfltIcTrf :: CExpr 
              _dfltIenvUp :: Env
              _dfltIfvS :: FvS
              _dfltIgUniq :: Int
              _dfltIlevOf :: Int
              _dfltImbLam :: (Maybe [HsName])
              _dfltImbVar :: (Maybe HsName)
              _dfltIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 61, column 17)
              _levOf =
                  _exprIlevOf `max` _altsIlevOf `max` _dfltIlevOf
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp `Map.union` _dfltIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _dfltIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _exprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _altsOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _altsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _altsOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _altsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _altsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _altsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _altsOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _altsOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _altsOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _altsOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _altsOlev =
                  _lhsIlev
              -- copy rule (down)
              _altsOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _dfltOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _dfltOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _dfltOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _dfltOgUniq =
                  _altsIgUniq
              -- copy rule (down)
              _dfltOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _dfltOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _dfltOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _dfltOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _dfltOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _dfltOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _dfltOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _dfltOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _dfltOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _dfltOlev =
                  _lhsIlev
              -- copy rule (down)
              _dfltOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _dfltOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _dfltOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
              ( _altsIcTrf,_altsIfvS,_altsIgUniq,_altsIlevOf) =
                  alts_ _altsOargMp _altsOcvarIntroMp _altsOevalCtx _altsOgUniq _altsOintroCVarIntroMp _altsOisLamBody _altsOisStrict _altsOlamArgMp _altsOlamFvSMp _altsOlamS _altsOlev _altsOvarS 
              ( _dfltIappFunKind,_dfltIbindLamArgMp,_dfltIcTrf,_dfltIenvUp,_dfltIfvS,_dfltIgUniq,_dfltIlevOf,_dfltImbLam,_dfltImbVar,_dfltIwhatBelow) =
                  dflt_ _dfltOargMp _dfltOcvarIntroMp _dfltOevalCtx _dfltOgUniq _dfltOintroCVarIntroMp _dfltOisDictClass _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlamArgMp _dfltOlamFvSMp _dfltOlamS _dfltOlev _dfltOmbCtxCount _dfltOvarS _dfltOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOappFunKind :: AppFunKind
              _lhsOgUniq :: Int
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOmbVar :: (Maybe HsName)
              _lhsOwhatBelow :: WhatExpr
              _errorExprOargMp :: CVarReplNmMp
              _errorExprOcvarIntroMp :: CVarIntroMp
              _errorExprOevalCtx :: EvalCtx
              _errorExprOgUniq :: Int
              _errorExprOintroCVarIntroMp :: CVarIntroMp
              _errorExprOisDictClass :: Bool
              _errorExprOisLamBody :: Bool
              _errorExprOisStrict :: Bool
              _errorExprOisTopApp :: Bool
              _errorExprOisTopTup :: Bool
              _errorExprOlamArgMp :: LamArgMp
              _errorExprOlamFvSMp :: FvSMp
              _errorExprOlamS :: FvS
              _errorExprOlev :: Int
              _errorExprOmbCtxCount :: (Maybe Int)
              _errorExprOvarS :: FvS
              _errorExprOwhatAbove :: WhatExpr
              _errorExprIappFunKind :: AppFunKind
              _errorExprIbindLamArgMp :: LamArgMp
              _errorExprIcTrf :: CExpr 
              _errorExprIenvUp :: Env
              _errorExprIfvS :: FvS
              _errorExprIgUniq :: Int
              _errorExprIlevOf :: Int
              _errorExprImbLam :: (Maybe [HsName])
              _errorExprImbVar :: (Maybe HsName)
              _errorExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _errorExprIbindLamArgMp
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
              -- copy rule (up)
              _lhsOappFunKind =
                  _errorExprIappFunKind
              -- copy rule (up)
              _lhsOgUniq =
                  _errorExprIgUniq
              -- copy rule (up)
              _lhsOmbLam =
                  _errorExprImbLam
              -- copy rule (up)
              _lhsOmbVar =
                  _errorExprImbVar
              -- copy rule (up)
              _lhsOwhatBelow =
                  _errorExprIwhatBelow
              -- copy rule (down)
              _errorExprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _errorExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _errorExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _errorExprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _errorExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _errorExprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _errorExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _errorExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _errorExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _errorExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _errorExprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _errorExprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _errorExprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _errorExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _errorExprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _errorExprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _errorExprOwhatAbove =
                  _whatAbove
              ( _errorExprIappFunKind,_errorExprIbindLamArgMp,_errorExprIcTrf,_errorExprIenvUp,_errorExprIfvS,_errorExprIgUniq,_errorExprIlevOf,_errorExprImbLam,_errorExprImbVar,_errorExprIwhatBelow) =
                  errorExpr_ _errorExprOargMp _errorExprOcvarIntroMp _errorExprOevalCtx _errorExprOgUniq _errorExprOintroCVarIntroMp _errorExprOisDictClass _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlamArgMp _errorExprOlamFvSMp _errorExprOlamS _errorExprOlev _errorExprOmbCtxCount _errorExprOvarS _errorExprOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 12, column 17)
              _lhsOappFunKind =
                  AppFunKind_FFI
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bodyOargMp :: CVarReplNmMp
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: Int
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisDictClass :: Bool
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamArgMp :: LamArgMp
              _bodyOlamFvSMp :: FvSMp
              _bodyOlamS :: FvS
              _bodyOlev :: Int
              _bodyOmbCtxCount :: (Maybe Int)
              _bodyOvarS :: FvS
              _bodyOwhatAbove :: WhatExpr
              _bodyIappFunKind :: AppFunKind
              _bodyIbindLamArgMp :: LamArgMp
              _bodyIcTrf :: CExpr 
              _bodyIenvUp :: Env
              _bodyIfvS :: FvS
              _bodyIgUniq :: Int
              _bodyIlevOf :: Int
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bodyIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bodyOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bodyOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bodyOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bodyOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bodyOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _bodyOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIappFunKind,_bodyIbindLamArgMp,_bodyIcTrf,_bodyIenvUp,_bodyIfvS,_bodyIgUniq,_bodyIlevOf,_bodyImbLam,_bodyImbVar,_bodyIwhatBelow) =
                  body_ _bodyOargMp _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisDictClass _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamArgMp _bodyOlamFvSMp _bodyOlamS _bodyOlev _bodyOmbCtxCount _bodyOvarS _bodyOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _funcOargMp :: CVarReplNmMp
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOevalCtx :: EvalCtx
              _funcOgUniq :: Int
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOisDictClass :: Bool
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopApp :: Bool
              _funcOisTopTup :: Bool
              _funcOlamArgMp :: LamArgMp
              _funcOlamFvSMp :: FvSMp
              _funcOlamS :: FvS
              _funcOlev :: Int
              _funcOmbCtxCount :: (Maybe Int)
              _funcOvarS :: FvS
              _funcOwhatAbove :: WhatExpr
              _funcIappFunKind :: AppFunKind
              _funcIbindLamArgMp :: LamArgMp
              _funcIcTrf :: CExpr 
              _funcIenvUp :: Env
              _funcIfvS :: FvS
              _funcIgUniq :: Int
              _funcIlevOf :: Int
              _funcImbLam :: (Maybe [HsName])
              _funcImbVar :: (Maybe HsName)
              _funcIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _funcIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _funcIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _funcOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _funcOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _funcOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _funcOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _funcOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _funcOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _funcOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              ( _funcIappFunKind,_funcIbindLamArgMp,_funcIcTrf,_funcIenvUp,_funcIfvS,_funcIgUniq,_funcIlevOf,_funcImbLam,_funcImbVar,_funcIwhatBelow) =
                  func_ _funcOargMp _funcOcvarIntroMp _funcOevalCtx _funcOgUniq _funcOintroCVarIntroMp _funcOisDictClass _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamArgMp _funcOlamFvSMp _funcOlamS _funcOlev _funcOmbCtxCount _funcOvarS _funcOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bodyOargMp :: CVarReplNmMp
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: Int
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisDictClass :: Bool
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamArgMp :: LamArgMp
              _bodyOlamFvSMp :: FvSMp
              _bodyOlamS :: FvS
              _bodyOlev :: Int
              _bodyOmbCtxCount :: (Maybe Int)
              _bodyOvarS :: FvS
              _bodyOwhatAbove :: WhatExpr
              _bodyIappFunKind :: AppFunKind
              _bodyIbindLamArgMp :: LamArgMp
              _bodyIcTrf :: CExpr 
              _bodyIenvUp :: Env
              _bodyIfvS :: FvS
              _bodyIgUniq :: Int
              _bodyIlevOf :: Int
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bodyIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bodyOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bodyOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bodyOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bodyOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bodyOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bodyOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bodyOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _bodyOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIappFunKind,_bodyIbindLamArgMp,_bodyIcTrf,_bodyIenvUp,_bodyIfvS,_bodyIgUniq,_bodyIlevOf,_bodyImbLam,_bodyImbVar,_bodyIwhatBelow) =
                  body_ _bodyOargMp _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisDictClass _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamArgMp _bodyOlamFvSMp _bodyOlamS _bodyOlev _bodyOmbCtxCount _bodyOvarS _bodyOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 55, column 17)
              _whatBelow =
                  ExprIsInt int_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _bodyOmbCtxCount :: (Maybe Int)
              _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bindOargMp :: CVarReplNmMp
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOevalCtx :: EvalCtx
              _bindOgUniq :: Int
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOisGlobal :: Bool
              _bindOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOlamArgMp :: LamArgMp
              _bindOlamFvSMp :: FvSMp
              _bindOlamS :: FvS
              _bindOletBindingsCateg :: CBindCateg
              _bindOlev :: Int
              _bindOvarS :: FvS
              _bodyOargMp :: CVarReplNmMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: Int
              _bodyOisDictClass :: Bool
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamArgMp :: LamArgMp
              _bodyOlamFvSMp :: FvSMp
              _bodyOlamS :: FvS
              _bodyOlev :: Int
              _bodyOvarS :: FvS
              _bodyOwhatAbove :: WhatExpr
              _bindIbindLamArgMp :: LamArgMp
              _bindIbindLamS :: FvS
              _bindIbindVarS :: FvS
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIenvUp :: Env
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: Int
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _bodyIappFunKind :: AppFunKind
              _bodyIbindLamArgMp :: LamArgMp
              _bodyIcTrf :: CExpr 
              _bodyIenvUp :: Env
              _bodyIfvS :: FvS
              _bodyIgUniq :: Int
              _bodyIlevOf :: Int
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 202, column 11)
              _bodyOmbCtxCount =
                  do { n <- _lhsImbCtxCount
                     ; return (n+1)
                     }
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 240, column 12)
              _lhsOenvUp =
                  maybe (if _lhsIisDictClass
                         then Map.insert _argNm TrackSelf _bodyIenvUp
                         else _bodyIenvUp
                        )
                        (\n -> Map.insert _argNm (TrackCtx n) _bodyIenvUp)
                        _lhsImbCtxCount
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 7, column 17)
              _lev =
                  _lhsIlev + 1
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 16, column 17)
              _isGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 5, column 17)
              _letBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 53, column 17)
              _whatBelow =
                  ExprIsLam
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 66, column 17)
              _whatAbove =
                  ExprIsLam
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
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 20, column 17)
              _varS =
                  _argNm `Set.insert` _lhsIvarS
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 4, column 17)
              _lhsOmbLam =
                  Just $ maybe [_argNm] (_argNm:) _bodyImbLam
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bindIbindLamArgMp `Map.union` _bodyIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bindOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bindOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _bindOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _bindOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _bindOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (from local)
              _bindOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bindOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _bindOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bindOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bindOlamS =
                  _lhsIlamS
              -- copy rule (from local)
              _bindOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _bindOlev =
                  _lev
              -- copy rule (from local)
              _bindOvarS =
                  _varS
              -- copy rule (down)
              _bodyOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _bodyOgUniq =
                  _bindIgUniq
              -- copy rule (down)
              _bodyOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _bodyOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bodyOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bodyOlamS =
                  _lhsIlamS
              -- copy rule (from local)
              _bodyOlev =
                  _lev
              -- copy rule (from local)
              _bodyOvarS =
                  _varS
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindIbindLamArgMp,_bindIbindLamS,_bindIbindVarS,_bindIbindsIntroCVarIntroMp,_bindIcTrf,_bindIcvarIntroExprMp,_bindIenvUp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOargMp _bindOcvarIntroMp _bindOevalCtx _bindOgUniq _bindOintroCVarIntroMp _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamArgMp _bindOlamFvSMp _bindOlamS _bindOletBindingsCateg _bindOlev _bindOvarS 
              ( _bodyIappFunKind,_bodyIbindLamArgMp,_bodyIcTrf,_bodyIenvUp,_bodyIfvS,_bodyIgUniq,_bodyIlevOf,_bodyImbLam,_bodyImbVar,_bodyIwhatBelow) =
                  body_ _bodyOargMp _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisDictClass _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamArgMp _bodyOlamFvSMp _bodyOlamS _bodyOlev _bodyOmbCtxCount _bodyOvarS _bodyOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _bodyOmbCtxCount :: (Maybe Int)
              _bodyOisDictClass :: Bool
              _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bindsOisStrict :: Bool
              _bindsOcvarIntroMp :: CVarIntroMp
              _bodyOcvarIntroMp :: CVarIntroMp
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _bindsOlev :: Int
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _bindsOargMp :: CVarReplNmMp
              _bindsOevalCtx :: EvalCtx
              _bindsOgUniq :: Int
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOisGlobal :: Bool
              _bindsOisLamBody :: Bool
              _bindsOlamArgMp :: LamArgMp
              _bindsOlamFvSMp :: FvSMp
              _bindsOlamS :: FvS
              _bindsOletBindingsCateg :: CBindCateg
              _bindsOvarS :: FvS
              _bodyOargMp :: CVarReplNmMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: Int
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamArgMp :: LamArgMp
              _bodyOlamFvSMp :: FvSMp
              _bodyOlamS :: FvS
              _bodyOlev :: Int
              _bodyOvarS :: FvS
              _bodyOwhatAbove :: WhatExpr
              _bindsIbindLamArgMp :: LamArgMp
              _bindsIbindLamS :: FvS
              _bindsIbindVarS :: FvS
              _bindsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindsIcTrf :: CBindL 
              _bindsIcvarIntroExprMp :: CVarIntroMp
              _bindsIenvUp :: Env
              _bindsIfvS :: FvS
              _bindsIfvSMp :: FvSMp
              _bindsIgUniq :: Int
              _bindsIlevOf :: Int
              _bindsInmL :: ([HsName])
              _bodyIappFunKind :: AppFunKind
              _bodyIbindLamArgMp :: LamArgMp
              _bodyIcTrf :: CExpr 
              _bodyIenvUp :: Env
              _bodyIfvS :: FvS
              _bodyIgUniq :: Int
              _bodyIlevOf :: Int
              _bodyImbLam :: (Maybe [HsName])
              _bodyImbVar :: (Maybe HsName)
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 65, column 17)
              _lamFvSMp =
                  (let  start varS = fvsClosure _bindsIbindLamS _lhsIlamS varS _lhsIlamFvSMp _bindsIfvSMp
                   in   case categ_ of
                          CBindCateg_Rec
                            ->  fvsTransClosure lm m
                            where (m,lm)  = start (_lhsIvarS `Set.union` _bindsIbindVarS)
                          _ ->  m
                            where (m,_)   = start _lhsIvarS
                  )
                  `Map.union` _lhsIlamFvSMp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 122, column 17)
              _lamArgMp =
                  _bindsIbindLamArgMp `Map.union` _lhsIlamArgMp
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 205, column 11)
              _bodyOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 206, column 11)
              _bodyOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 239, column 12)
              _lhsOenvUp =
                  Map.union _bindsIenvUp _bodyIenvUp
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 15, column 17)
              _isGlobal =
                  _lhsIlev == cLevModule
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 4, column 17)
              _letBindingsCateg =
                  categ_
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 72, column 17)
              _isTopLet =
                  _lhsIwhatAbove == ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 111, column 17)
              _bindsOisStrict =
                  _isGlobal || categ_ == CBindCateg_Strict
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 124, column 17)
              _evalCtx =
                  if categ_ == CBindCateg_Strict
                  then EvalCtx_Eval
                  else EvalCtx_None
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 6, column 17)
              _fvS =
                  (_bodyIfvS `Set.union` _bindsIfvS) `Set.difference` Set.fromList _bindsInmL
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 24, column 17)
              _maxBindLev =
                  fvsLev _lhsIcvarIntroMp cLevModule _bindsIfvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              __tup4 =
                  case categ_ of
                      CBindCateg_Strict -> (const _lhsIlev,_lhsIcvarIntroMp)
                      CBindCateg_Rec    -> ( const _maxBindLev
                                           , Map.map (\cvi -> cvi {cviLev = _maxBindLev}) _bindsIcvarIntroExprMp
                                               `Map.union` _lhsIcvarIntroMp
                                           )
                      _                 -> (id,_lhsIcvarIntroMp)
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_strLev,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_,_bindsOcvarIntroMp) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 33, column 17)
              _bodyOcvarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _strLev $ cviLev cvi}) _bindsIcvarIntroExprMp `Map.union` _lhsIcvarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 45, column 17)
              _introCVarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _lhsIlev}) _bindsIbindsIntroCVarIntroMp `Map.union` _lhsIintroCVarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 66, column 17)
              _levOf =
                  fvsLev _lhsIcvarIntroMp cLevModule _fvS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 18, column 17)
              _varS =
                  _lhsIvarS `Set.union` _bindsIbindVarS
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 18, column 17)
              _lamS =
                  _lhsIlamS `Set.union` _bindsIbindLamS
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonLevLet.ag"(line 2, column 17)
              _bindsOlev =
                  _lhsIlev + 1
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _bindsIbindLamArgMp `Map.union` _bodyIbindLamArgMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _bindsOargMp =
                  _lhsIargMp
              -- copy rule (from local)
              _bindsOevalCtx =
                  _evalCtx
              -- copy rule (down)
              _bindsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _bindsOintroCVarIntroMp =
                  _introCVarIntroMp
              -- copy rule (from local)
              _bindsOisGlobal =
                  _isGlobal
              -- copy rule (down)
              _bindsOisLamBody =
                  _lhsIisLamBody
              -- copy rule (from local)
              _bindsOlamArgMp =
                  _lamArgMp
              -- copy rule (from local)
              _bindsOlamFvSMp =
                  _lamFvSMp
              -- copy rule (from local)
              _bindsOlamS =
                  _lamS
              -- copy rule (from local)
              _bindsOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _bindsOvarS =
                  _varS
              -- copy rule (down)
              _bodyOargMp =
                  _lhsIargMp
              -- copy rule (from local)
              _bodyOevalCtx =
                  _evalCtx
              -- copy rule (chain)
              _bodyOgUniq =
                  _bindsIgUniq
              -- copy rule (from local)
              _bodyOintroCVarIntroMp =
                  _introCVarIntroMp
              -- copy rule (down)
              _bodyOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _bodyOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _bodyOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _bodyOisTopTup =
                  _isTopTup
              -- copy rule (from local)
              _bodyOlamArgMp =
                  _lamArgMp
              -- copy rule (from local)
              _bodyOlamFvSMp =
                  _lamFvSMp
              -- copy rule (from local)
              _bodyOlamS =
                  _lamS
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (from local)
              _bodyOvarS =
                  _varS
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindsIbindLamArgMp,_bindsIbindLamS,_bindsIbindVarS,_bindsIbindsIntroCVarIntroMp,_bindsIcTrf,_bindsIcvarIntroExprMp,_bindsIenvUp,_bindsIfvS,_bindsIfvSMp,_bindsIgUniq,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOargMp _bindsOcvarIntroMp _bindsOevalCtx _bindsOgUniq _bindsOintroCVarIntroMp _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOlamArgMp _bindsOlamFvSMp _bindsOlamS _bindsOletBindingsCateg _bindsOlev _bindsOvarS 
              ( _bodyIappFunKind,_bodyIbindLamArgMp,_bodyIcTrf,_bodyIenvUp,_bodyIfvS,_bodyIgUniq,_bodyIlevOf,_bodyImbLam,_bodyImbVar,_bodyIwhatBelow) =
                  body_ _bodyOargMp _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisDictClass _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamArgMp _bodyOlamFvSMp _bodyOlamS _bodyOlev _bodyOmbCtxCount _bodyOvarS _bodyOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 67, column 17)
              _levOf =
                  cLevIntern
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 11, column 17)
              _lhsOappFunKind =
                  AppFunKind_Tag tag_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisDictClass :: Bool
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOmbCtxCount :: (Maybe Int)
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _offsetOargMp :: CVarReplNmMp
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: Int
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisDictClass :: Bool
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamArgMp :: LamArgMp
              _offsetOlamFvSMp :: FvSMp
              _offsetOlamS :: FvS
              _offsetOlev :: Int
              _offsetOmbCtxCount :: (Maybe Int)
              _offsetOvarS :: FvS
              _offsetOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIbindLamArgMp :: LamArgMp
              _offsetIcTrf :: CExpr 
              _offsetIenvUp :: Env
              _offsetIfvS :: FvS
              _offsetIgUniq :: Int
              _offsetIlevOf :: Int
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 62, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp `Map.union` _offsetIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _offsetIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _offsetOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _offsetOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _offsetOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
              ( _offsetIappFunKind,_offsetIbindLamArgMp,_offsetIcTrf,_offsetIenvUp,_offsetIfvS,_offsetIgUniq,_offsetIlevOf,_offsetImbLam,_offsetImbVar,_offsetIwhatBelow) =
                  offset_ _offsetOargMp _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisDictClass _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamArgMp _offsetOlamFvSMp _offsetOlamS _offsetOlev _offsetOmbCtxCount _offsetOvarS _offsetOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisDictClass :: Bool
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOmbCtxCount :: (Maybe Int)
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _offsetOargMp :: CVarReplNmMp
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: Int
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisDictClass :: Bool
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamArgMp :: LamArgMp
              _offsetOlamFvSMp :: FvSMp
              _offsetOlamS :: FvS
              _offsetOlev :: Int
              _offsetOmbCtxCount :: (Maybe Int)
              _offsetOvarS :: FvS
              _offsetOwhatAbove :: WhatExpr
              _fldExprOargMp :: CVarReplNmMp
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOevalCtx :: EvalCtx
              _fldExprOgUniq :: Int
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOisDictClass :: Bool
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlamArgMp :: LamArgMp
              _fldExprOlamFvSMp :: FvSMp
              _fldExprOlamS :: FvS
              _fldExprOlev :: Int
              _fldExprOmbCtxCount :: (Maybe Int)
              _fldExprOvarS :: FvS
              _fldExprOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIbindLamArgMp :: LamArgMp
              _offsetIcTrf :: CExpr 
              _offsetIenvUp :: Env
              _offsetIfvS :: FvS
              _offsetIgUniq :: Int
              _offsetIlevOf :: Int
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIwhatBelow :: WhatExpr
              _fldExprIappFunKind :: AppFunKind
              _fldExprIbindLamArgMp :: LamArgMp
              _fldExprIcTrf :: CExpr 
              _fldExprIenvUp :: Env
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: Int
              _fldExprIlevOf :: Int
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 64, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf `max` _fldExprIlevOf
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp `Map.union` _offsetIbindLamArgMp `Map.union` _fldExprIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _offsetOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _offsetOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _offsetOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _fldExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _fldExprOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _fldExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldExprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _fldExprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _fldExprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _fldExprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
              ( _offsetIappFunKind,_offsetIbindLamArgMp,_offsetIcTrf,_offsetIenvUp,_offsetIfvS,_offsetIgUniq,_offsetIlevOf,_offsetImbLam,_offsetImbVar,_offsetIwhatBelow) =
                  offset_ _offsetOargMp _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisDictClass _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamArgMp _offsetOlamFvSMp _offsetOlamS _offsetOlev _offsetOmbCtxCount _offsetOvarS _offsetOwhatAbove 
              ( _fldExprIappFunKind,_fldExprIbindLamArgMp,_fldExprIcTrf,_fldExprIenvUp,_fldExprIfvS,_fldExprIgUniq,_fldExprIlevOf,_fldExprImbLam,_fldExprImbVar,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOargMp _fldExprOcvarIntroMp _fldExprOevalCtx _fldExprOgUniq _fldExprOintroCVarIntroMp _fldExprOisDictClass _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamArgMp _fldExprOlamFvSMp _fldExprOlamS _fldExprOlev _fldExprOmbCtxCount _fldExprOvarS _fldExprOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOenvUp :: Env
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _lhsOmbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: Int
              _lhsOwhatBelow :: WhatExpr
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisDictClass :: Bool
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOmbCtxCount :: (Maybe Int)
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _offsetOargMp :: CVarReplNmMp
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: Int
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisDictClass :: Bool
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamArgMp :: LamArgMp
              _offsetOlamFvSMp :: FvSMp
              _offsetOlamS :: FvS
              _offsetOlev :: Int
              _offsetOmbCtxCount :: (Maybe Int)
              _offsetOvarS :: FvS
              _offsetOwhatAbove :: WhatExpr
              _fldExprOargMp :: CVarReplNmMp
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOevalCtx :: EvalCtx
              _fldExprOgUniq :: Int
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOisDictClass :: Bool
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlamArgMp :: LamArgMp
              _fldExprOlamFvSMp :: FvSMp
              _fldExprOlamS :: FvS
              _fldExprOlev :: Int
              _fldExprOmbCtxCount :: (Maybe Int)
              _fldExprOvarS :: FvS
              _fldExprOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              _offsetIappFunKind :: AppFunKind
              _offsetIbindLamArgMp :: LamArgMp
              _offsetIcTrf :: CExpr 
              _offsetIenvUp :: Env
              _offsetIfvS :: FvS
              _offsetIgUniq :: Int
              _offsetIlevOf :: Int
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIwhatBelow :: WhatExpr
              _fldExprIappFunKind :: AppFunKind
              _fldExprIbindLamArgMp :: LamArgMp
              _fldExprIcTrf :: CExpr 
              _fldExprIenvUp :: Env
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: Int
              _fldExprIlevOf :: Int
              _fldExprImbLam :: (Maybe [HsName])
              _fldExprImbVar :: (Maybe HsName)
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 17, column 17)
              _exprOisTopTup =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 18, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 58, column 17)
              _whatBelow =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 64, column 17)
              _levOf =
                  _exprIlevOf `max` _offsetIlevOf `max` _fldExprIlevOf
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 16, column 17)
              _lhsOappFunKind =
                  AppFunKind_NoApp
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 24, column 33)
              _lhsOmbVar =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  _exprIbindLamArgMp `Map.union` _offsetIbindLamArgMp `Map.union` _fldExprIbindLamArgMp
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
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _offsetOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _offsetOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _offsetOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _offsetOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _offsetOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _offsetOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _offsetOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _offsetOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _offsetOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _offsetOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _fldExprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _fldExprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _fldExprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _fldExprOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _fldExprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldExprOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _fldExprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _fldExprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _fldExprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _fldExprOisTopTup =
                  _isTopTup
              -- copy rule (down)
              _fldExprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _fldExprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _fldExprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _fldExprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
              ( _offsetIappFunKind,_offsetIbindLamArgMp,_offsetIcTrf,_offsetIenvUp,_offsetIfvS,_offsetIgUniq,_offsetIlevOf,_offsetImbLam,_offsetImbVar,_offsetIwhatBelow) =
                  offset_ _offsetOargMp _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisDictClass _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamArgMp _offsetOlamFvSMp _offsetOlamS _offsetOlev _offsetOmbCtxCount _offsetOvarS _offsetOwhatAbove 
              ( _fldExprIappFunKind,_fldExprIbindLamArgMp,_fldExprIcTrf,_fldExprIenvUp,_fldExprIfvS,_fldExprIgUniq,_fldExprIlevOf,_fldExprImbLam,_fldExprImbVar,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOargMp _fldExprOcvarIntroMp _fldExprOevalCtx _fldExprOgUniq _fldExprOintroCVarIntroMp _fldExprOisDictClass _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamArgMp _fldExprOlamFvSMp _fldExprOlamS _fldExprOlev _fldExprOmbCtxCount _fldExprOvarS _fldExprOwhatAbove 
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS
       _lhsIwhatAbove ->
         (let _lhsOcTrf :: CExpr 
              _lhsOenvUp :: Env
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOmbLam :: (Maybe [HsName])
              _lhsOappFunKind :: AppFunKind
              _mbVar :: (Maybe HsName)
              _lhsObindLamArgMp :: LamArgMp
              _lhsOgUniq :: Int
              _lhsOmbVar :: (Maybe HsName)
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 150, column 17)
              _lhsOcTrf =
                  let  r n = fvVarRepl _lhsIargMp n
                       v1 = r _nm
                       mk as = acoreApp v1 $ [(r a) | (a,cvi) <- as]
                       v2 = maybe v1 mk $ Map.lookup _nm $ _lhsIlamArgMp
                  in   v2
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 247, column 12)
              _lhsOenvUp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 54, column 17)
              _whatBelow =
                  ExprIsVar _nm
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
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
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 6, column 33)
              _lhsOmbLam =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 13, column 17)
              _lhsOappFunKind =
                  AppFunKind_Fun ref_
              -- "build/101/lib-ehc/EH101/Core/CommonPred.ag"(line 21, column 17)
              _mbVar =
                  Just _nm
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 108, column 39)
              _lhsObindLamArgMp =
                  Map.empty
              -- self rule
              _cTrf =
                  CExpr_Var ref_
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOmbVar =
                  _mbVar
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOappFunKind,_lhsObindLamArgMp,_lhsOcTrf,_lhsOenvUp,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOmbLam,_lhsOmbVar,_lhsOwhatBelow)))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CExprAnn  = CVarReplNmMp ->
                   CVarIntroMp ->
                   Int ->
                   CVarIntroMp ->
                   LamArgMp ->
                   FvSMp ->
                   FvS ->
                   Int ->
                   FvS ->
                   ( CExprAnn ,FvS,Int,Int)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CMetaBind  = CVarReplNmMp ->
                    CVarIntroMp ->
                    Int ->
                    CVarIntroMp ->
                    LamArgMp ->
                    FvSMp ->
                    FvS ->
                    Int ->
                    FvS ->
                    ( CMetaBind ,FvS,Int,Int,CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOself)))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         bindLamArgMp         : LamArgMp
         cvarIntroMp          : CVarIntroMp
         envFinal             : Env
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         isDictClass          : Bool
         isDictInstance       : Bool
         isInstance           : Bool
         levOf                : Int
         mbTrack              : Maybe Track
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
type T_CMetaVal  = CVarReplNmMp ->
                   LamArgMp ->
                   CVarIntroMp ->
                   Env ->
                   Int ->
                   CVarIntroMp ->
                   LamArgMp ->
                   FvSMp ->
                   FvS ->
                   Int ->
                   FvS ->
                   ( CMetaVal ,FvS,Int,Bool,Bool,Bool,Int,(Maybe Track),CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOisInstance :: Bool
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 128, column 21)
              _lhsOisInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 213, column 20)
              _lhsOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 215, column 20)
              _lhsOisDictInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 225, column 20)
              _lhsOmbTrack =
                  Nothing
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOisInstance :: Bool
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _lhsOcTrf :: CMetaVal 
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 128, column 21)
              _lhsOisInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 212, column 20)
              _lhsOisDictClass =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 215, column 20)
              _lhsOisDictInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 225, column 20)
              _lhsOmbTrack =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 251, column 22)
              _lhsOcTrf =
                  CMetaVal_DictClass    (map (metaExtendTrack _lhsIbindLamArgMp _lhsIenvFinal) tracks_)
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOisInstance :: Bool
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _lhsOcTrf :: CMetaVal 
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 127, column 21)
              _lhsOisInstance =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 213, column 20)
              _lhsOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 214, column 20)
              _lhsOisDictInstance =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 225, column 20)
              _lhsOmbTrack =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 252, column 22)
              _lhsOcTrf =
                  CMetaVal_DictInstance (map (metaExtendTrack _lhsIbindLamArgMp _lhsIenvFinal) tracks_)
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOisInstance :: Bool
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 128, column 21)
              _lhsOisInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 213, column 20)
              _lhsOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 215, column 20)
              _lhsOisDictInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 224, column 20)
              _lhsOmbTrack =
                  Just track_
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOisInstance :: Bool
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: Int
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 128, column 21)
              _lhsOisInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 213, column 20)
              _lhsOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 215, column 20)
              _lhsOisDictInstance =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 225, column 20)
              _lhsOmbTrack =
                  Nothing
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         bindLamArgMp         : LamArgMp
         cvarIntroMp          : CVarIntroMp
         envFinal             : Env
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         isDictClass          : Bool
         isDictInstance       : Bool
         isInstance           : Bool
         levOf                : Int
         mbTrack              : Maybe Track
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
type T_CMetas  = CVarReplNmMp ->
                 LamArgMp ->
                 CVarIntroMp ->
                 Env ->
                 Int ->
                 CVarIntroMp ->
                 LamArgMp ->
                 FvSMp ->
                 FvS ->
                 Int ->
                 FvS ->
                 ( CMetas ,FvS,Int,Bool,Bool,Bool,Int,(Maybe Track),CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIargMp
       _lhsIbindLamArgMp
       _lhsIcvarIntroMp
       _lhsIenvFinal
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetas 
              _lhsOself :: CMetas 
              _lhsOgUniq :: Int
              _lhsOisDictClass :: Bool
              _lhsOisDictInstance :: Bool
              _lhsOisInstance :: Bool
              _lhsOmbTrack :: (Maybe Track)
              _x1OargMp :: CVarReplNmMp
              _x1OcvarIntroMp :: CVarIntroMp
              _x1OgUniq :: Int
              _x1OintroCVarIntroMp :: CVarIntroMp
              _x1OlamArgMp :: LamArgMp
              _x1OlamFvSMp :: FvSMp
              _x1OlamS :: FvS
              _x1Olev :: Int
              _x1OvarS :: FvS
              _x2OargMp :: CVarReplNmMp
              _x2ObindLamArgMp :: LamArgMp
              _x2OcvarIntroMp :: CVarIntroMp
              _x2OenvFinal :: Env
              _x2OgUniq :: Int
              _x2OintroCVarIntroMp :: CVarIntroMp
              _x2OlamArgMp :: LamArgMp
              _x2OlamFvSMp :: FvSMp
              _x2OlamS :: FvS
              _x2Olev :: Int
              _x2OvarS :: FvS
              _x1IcTrf :: CMetaBind 
              _x1IfvS :: FvS
              _x1IgUniq :: Int
              _x1IlevOf :: Int
              _x1Iself :: CMetaBind 
              _x2IcTrf :: CMetaVal 
              _x2IfvS :: FvS
              _x2IgUniq :: Int
              _x2IisDictClass :: Bool
              _x2IisDictInstance :: Bool
              _x2IisInstance :: Bool
              _x2IlevOf :: Int
              _x2ImbTrack :: (Maybe Track)
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
              _lhsOgUniq =
                  _x2IgUniq
              -- copy rule (up)
              _lhsOisDictClass =
                  _x2IisDictClass
              -- copy rule (up)
              _lhsOisDictInstance =
                  _x2IisDictInstance
              -- copy rule (up)
              _lhsOisInstance =
                  _x2IisInstance
              -- copy rule (up)
              _lhsOmbTrack =
                  _x2ImbTrack
              -- copy rule (down)
              _x1OargMp =
                  _lhsIargMp
              -- copy rule (down)
              _x1OcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _x1OgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _x1OintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _x1OlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _x1OlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _x1OlamS =
                  _lhsIlamS
              -- copy rule (down)
              _x1Olev =
                  _lhsIlev
              -- copy rule (down)
              _x1OvarS =
                  _lhsIvarS
              -- copy rule (down)
              _x2OargMp =
                  _lhsIargMp
              -- copy rule (down)
              _x2ObindLamArgMp =
                  _lhsIbindLamArgMp
              -- copy rule (down)
              _x2OcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _x2OenvFinal =
                  _lhsIenvFinal
              -- copy rule (chain)
              _x2OgUniq =
                  _x1IgUniq
              -- copy rule (down)
              _x2OintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _x2OlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _x2OlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _x2OlamS =
                  _lhsIlamS
              -- copy rule (down)
              _x2Olev =
                  _lhsIlev
              -- copy rule (down)
              _x2OvarS =
                  _lhsIvarS
              ( _x1IcTrf,_x1IfvS,_x1IgUniq,_x1IlevOf,_x1Iself) =
                  x1_ _x1OargMp _x1OcvarIntroMp _x1OgUniq _x1OintroCVarIntroMp _x1OlamArgMp _x1OlamFvSMp _x1OlamS _x1Olev _x1OvarS 
              ( _x2IcTrf,_x2IfvS,_x2IgUniq,_x2IisDictClass,_x2IisDictInstance,_x2IisInstance,_x2IlevOf,_x2ImbTrack,_x2Iself) =
                  x2_ _x2OargMp _x2ObindLamArgMp _x2OcvarIntroMp _x2OenvFinal _x2OgUniq _x2OintroCVarIntroMp _x2OlamArgMp _x2OlamFvSMp _x2OlamS _x2Olev _x2OvarS 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOisDictClass,_lhsOisDictInstance,_lhsOisInstance,_lhsOlevOf,_lhsOmbTrack,_lhsOself)))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
            local whatAbove   : {WhatExpr}
            local cTrf        : _
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = CVarReplNmMp ->
                  CVarIntroMp ->
                  Int ->
                  CVarIntroMp ->
                  LamArgMp ->
                  FvSMp ->
                  FvS ->
                  Int ->
                  FvS ->
                  ( CModule ,FvS,Int,Int)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _exprOmbCtxCount :: (Maybe Int)
              _exprOisDictClass :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _exprOisLamBody :: Bool
              _exprOevalCtx :: EvalCtx
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CModule 
              _lhsOgUniq :: Int
              _exprOargMp :: CVarReplNmMp
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOgUniq :: Int
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlamArgMp :: LamArgMp
              _exprOlamFvSMp :: FvSMp
              _exprOlamS :: FvS
              _exprOlev :: Int
              _exprOvarS :: FvS
              _exprOwhatAbove :: WhatExpr
              _exprIappFunKind :: AppFunKind
              _exprIbindLamArgMp :: LamArgMp
              _exprIcTrf :: CExpr 
              _exprIenvUp :: Env
              _exprIfvS :: FvS
              _exprIgUniq :: Int
              _exprIlevOf :: Int
              _exprImbLam :: (Maybe [HsName])
              _exprImbVar :: (Maybe HsName)
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 174, column 10)
              _exprOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 175, column 10)
              _exprOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 29, column 17)
              _exprOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 85, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 96, column 17)
              _exprOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 116, column 17)
              _exprOevalCtx =
                  EvalCtx_Eval
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
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _exprOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _exprOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _exprOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIappFunKind,_exprIbindLamArgMp,_exprIcTrf,_exprIenvUp,_exprIfvS,_exprIgUniq,_exprIlevOf,_exprImbLam,_exprImbVar,_exprIwhatBelow) =
                  expr_ _exprOargMp _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisDictClass _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamArgMp _exprOlamFvSMp _exprOlamS _exprOlev _exprOmbCtxCount _exprOvarS _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CPat  = CVarReplNmMp ->
               CVarIntroMp ->
               Int ->
               CVarIntroMp ->
               LamArgMp ->
               FvSMp ->
               FvS ->
               Int ->
               FvS ->
               ( CPat ,([HsName]),FvS,Int,Int,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: Int
              _restOargMp :: CVarReplNmMp
              _restOcvarIntroMp :: CVarIntroMp
              _restOgUniq :: Int
              _restOintroCVarIntroMp :: CVarIntroMp
              _restOlamArgMp :: LamArgMp
              _restOlamFvSMp :: FvSMp
              _restOlamS :: FvS
              _restOlev :: Int
              _restOvarS :: FvS
              _bindsOargMp :: CVarReplNmMp
              _bindsOcvarIntroMp :: CVarIntroMp
              _bindsOgUniq :: Int
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOlamArgMp :: LamArgMp
              _bindsOlamFvSMp :: FvSMp
              _bindsOlamS :: FvS
              _bindsOlev :: Int
              _bindsOvarS :: FvS
              _restIcTrf :: CPatRest 
              _restIfvS :: FvS
              _restIgUniq :: Int
              _restIlevOf :: Int
              _restInmL :: ([HsName])
              _bindsIcTrf :: CPatFldL 
              _bindsIfldNmL :: ([HsName])
              _bindsIfvS :: FvS
              _bindsIgUniq :: Int
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
              -- copy rule (up)
              _lhsOgUniq =
                  _bindsIgUniq
              -- copy rule (down)
              _restOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _restOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _restOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _restOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _restOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _restOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _restOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _restOlev =
                  _lhsIlev
              -- copy rule (down)
              _restOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _bindsOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bindsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _bindsOgUniq =
                  _restIgUniq
              -- copy rule (down)
              _bindsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindsOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bindsOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bindsOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOvarS =
                  _lhsIvarS
              ( _restIcTrf,_restIfvS,_restIgUniq,_restIlevOf,_restInmL) =
                  rest_ _restOargMp _restOcvarIntroMp _restOgUniq _restOintroCVarIntroMp _restOlamArgMp _restOlamFvSMp _restOlamS _restOlev _restOvarS 
              ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsIgUniq,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOargMp _bindsOcvarIntroMp _bindsOgUniq _bindsOintroCVarIntroMp _bindsOlamArgMp _bindsOlamFvSMp _bindsOlamS _bindsOlev _bindsOvarS 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
            local whatAbove   : {WhatExpr}
            local fldNm       : _
            local cTrf        : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = CVarReplNmMp ->
                  CVarIntroMp ->
                  Int ->
                  CVarIntroMp ->
                  LamArgMp ->
                  FvSMp ->
                  FvS ->
                  Int ->
                  FvS ->
                  ( CPatFld ,([HsName]),FvS,Int,Int,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _offsetOmbCtxCount :: (Maybe Int)
              _offsetOisDictClass :: Bool
              _bindOisGlobal :: Bool
              _bindOletBindingsCateg :: CBindCateg
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _offsetOisStrict :: Bool
              _offsetOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOisLamBody :: Bool
              _offsetOevalCtx :: EvalCtx
              _bindOevalCtx :: EvalCtx
              _lhsOfldNmL :: ([HsName])
              _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatFld 
              _lhsOgUniq :: Int
              _offsetOargMp :: CVarReplNmMp
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOgUniq :: Int
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlamArgMp :: LamArgMp
              _offsetOlamFvSMp :: FvSMp
              _offsetOlamS :: FvS
              _offsetOlev :: Int
              _offsetOvarS :: FvS
              _offsetOwhatAbove :: WhatExpr
              _bindOargMp :: CVarReplNmMp
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOgUniq :: Int
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOlamArgMp :: LamArgMp
              _bindOlamFvSMp :: FvSMp
              _bindOlamS :: FvS
              _bindOlev :: Int
              _bindOvarS :: FvS
              _fldAnnsOargMp :: CVarReplNmMp
              _fldAnnsOcvarIntroMp :: CVarIntroMp
              _fldAnnsOgUniq :: Int
              _fldAnnsOintroCVarIntroMp :: CVarIntroMp
              _fldAnnsOlamArgMp :: LamArgMp
              _fldAnnsOlamFvSMp :: FvSMp
              _fldAnnsOlamS :: FvS
              _fldAnnsOlev :: Int
              _fldAnnsOvarS :: FvS
              _offsetIappFunKind :: AppFunKind
              _offsetIbindLamArgMp :: LamArgMp
              _offsetIcTrf :: CExpr 
              _offsetIenvUp :: Env
              _offsetIfvS :: FvS
              _offsetIgUniq :: Int
              _offsetIlevOf :: Int
              _offsetImbLam :: (Maybe [HsName])
              _offsetImbVar :: (Maybe HsName)
              _offsetIwhatBelow :: WhatExpr
              _bindIbindLamArgMp :: LamArgMp
              _bindIbindLamS :: FvS
              _bindIbindVarS :: FvS
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIenvUp :: Env
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: Int
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _fldAnnsIcTrf :: CBindAnnL 
              _fldAnnsIfvS :: FvS
              _fldAnnsIgUniq :: Int
              _fldAnnsIlevOf :: Int
              _fldAnnsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 190, column 10)
              _offsetOmbCtxCount =
                  Nothing
              -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 191, column 10)
              _offsetOisDictClass =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonLev.ag"(line 19, column 17)
              _bindOisGlobal =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 8, column 17)
              _bindOletBindingsCateg =
                  acoreBindcategPlain
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 43, column 17)
              _offsetOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 79, column 17)
              _whatAbove =
                  ExprIsOther
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisStrict =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 105, column 17)
              _offsetOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisStrict =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 107, column 17)
              _bindOisLamBody =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 119, column 17)
              _offsetOevalCtx =
                  EvalCtx_Eval
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 120, column 33)
              _bindOevalCtx =
                  EvalCtx_None
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
              -- copy rule (up)
              _lhsOgUniq =
                  _fldAnnsIgUniq
              -- copy rule (down)
              _offsetOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _offsetOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _offsetOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (down)
              _bindOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _bindOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _bindOgUniq =
                  _offsetIgUniq
              -- copy rule (down)
              _bindOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _bindOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _bindOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _bindOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _bindOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _fldAnnsOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _fldAnnsOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _fldAnnsOgUniq =
                  _bindIgUniq
              -- copy rule (down)
              _fldAnnsOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _fldAnnsOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _fldAnnsOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _fldAnnsOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _fldAnnsOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldAnnsOvarS =
                  _lhsIvarS
              ( _offsetIappFunKind,_offsetIbindLamArgMp,_offsetIcTrf,_offsetIenvUp,_offsetIfvS,_offsetIgUniq,_offsetIlevOf,_offsetImbLam,_offsetImbVar,_offsetIwhatBelow) =
                  offset_ _offsetOargMp _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisDictClass _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamArgMp _offsetOlamFvSMp _offsetOlamS _offsetOlev _offsetOmbCtxCount _offsetOvarS _offsetOwhatAbove 
              ( _bindIbindLamArgMp,_bindIbindLamS,_bindIbindVarS,_bindIbindsIntroCVarIntroMp,_bindIcTrf,_bindIcvarIntroExprMp,_bindIenvUp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOargMp _bindOcvarIntroMp _bindOevalCtx _bindOgUniq _bindOintroCVarIntroMp _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOlamArgMp _bindOlamFvSMp _bindOlamS _bindOletBindingsCateg _bindOlev _bindOvarS 
              ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsIgUniq,_fldAnnsIlevOf,_fldAnnsInmL) =
                  fldAnns_ _fldAnnsOargMp _fldAnnsOcvarIntroMp _fldAnnsOgUniq _fldAnnsOintroCVarIntroMp _fldAnnsOlamArgMp _fldAnnsOlamFvSMp _fldAnnsOlamS _fldAnnsOlev _fldAnnsOvarS 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CPatFldL  = CVarReplNmMp ->
                   CVarIntroMp ->
                   Int ->
                   CVarIntroMp ->
                   LamArgMp ->
                   FvSMp ->
                   FvS ->
                   Int ->
                   FvS ->
                   ( CPatFldL ,([HsName]),FvS,Int,Int,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOgUniq :: Int
              _hdOargMp :: CVarReplNmMp
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOgUniq :: Int
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlamArgMp :: LamArgMp
              _hdOlamFvSMp :: FvSMp
              _hdOlamS :: FvS
              _hdOlev :: Int
              _hdOvarS :: FvS
              _tlOargMp :: CVarReplNmMp
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOgUniq :: Int
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlamArgMp :: LamArgMp
              _tlOlamFvSMp :: FvSMp
              _tlOlamS :: FvS
              _tlOlev :: Int
              _tlOvarS :: FvS
              _hdIcTrf :: CPatFld 
              _hdIfldNmL :: ([HsName])
              _hdIfvS :: FvS
              _hdIgUniq :: Int
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CPatFldL 
              _tlIfldNmL :: ([HsName])
              _tlIfvS :: FvS
              _tlIgUniq :: Int
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
              -- copy rule (up)
              _lhsOgUniq =
                  _tlIgUniq
              -- copy rule (down)
              _hdOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _hdOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _hdOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOvarS =
                  _lhsIvarS
              -- copy rule (down)
              _tlOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _tlOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _tlOgUniq =
                  _hdIgUniq
              -- copy rule (down)
              _tlOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _tlOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _tlOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _tlOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOvarS =
                  _lhsIvarS
              ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdIgUniq,_hdIlevOf,_hdInmL) =
                  hd_ _hdOargMp _hdOcvarIntroMp _hdOgUniq _hdOintroCVarIntroMp _hdOlamArgMp _hdOlamFvSMp _hdOlamS _hdOlev _hdOvarS 
              ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlIgUniq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOargMp _tlOcvarIntroMp _tlOgUniq _tlOintroCVarIntroMp _tlOlamArgMp _tlOlamFvSMp _tlOlamS _tlOlev _tlOvarS 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
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
type T_CPatRest  = CVarReplNmMp ->
                   CVarIntroMp ->
                   Int ->
                   CVarIntroMp ->
                   LamArgMp ->
                   FvSMp ->
                   FvS ->
                   Int ->
                   FvS ->
                   ( CPatRest ,FvS,Int,Int,([HsName]))
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatRest 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsIvarS ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatRest 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf,_lhsOnmL)))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
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
type T_CodeAGItf  = ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf )  =
    (let ( _lhsOcTrf) = sem 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (let _moduleOgUniq :: Int
         _moduleOlamFvSMp :: FvSMp
         _moduleOargMp :: CVarReplNmMp
         _moduleOlamArgMp :: LamArgMp
         _moduleOlev :: Int
         _moduleOcvarIntroMp :: CVarIntroMp
         _moduleOintroCVarIntroMp :: CVarIntroMp
         _moduleOvarS :: FvS
         _moduleOlamS :: FvS
         _lhsOcTrf :: CModule 
         _moduleIcTrf :: CModule 
         _moduleIfvS :: FvS
         _moduleIgUniq :: Int
         _moduleIlevOf :: Int
         -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 53, column 17)
         _moduleOgUniq =
             0
         -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 62, column 17)
         _moduleOlamFvSMp =
             Map.empty
         -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 78, column 17)
         _moduleOargMp =
             Map.empty
         -- "build/101/lib-ehc/EH101/Core/Trf/LamGlobalAsArg.ag"(line 119, column 17)
         _moduleOlamArgMp =
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
         -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 14, column 17)
         _moduleOvarS =
             Set.empty
         -- "build/101/lib-ehc/EH101/Core/Trf/CommonGlobalAsArg.ag"(line 14, column 17)
         _moduleOlamS =
             Set.empty
         -- copy rule (up)
         _lhsOcTrf =
             _moduleIcTrf
         ( _moduleIcTrf,_moduleIfvS,_moduleIgUniq,_moduleIlevOf) =
             module_ _moduleOargMp _moduleOcvarIntroMp _moduleOgUniq _moduleOintroCVarIntroMp _moduleOlamArgMp _moduleOlamFvSMp _moduleOlamS _moduleOlev _moduleOvarS 
     in  ( _lhsOcTrf))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         argMp                : CVarReplNmMp
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isDictClass          : Bool
         isLamBody            : Bool
         isStrict             : Bool
         lamArgMp             : LamArgMp
         lamFvSMp             : FvSMp
         lamS                 : FvS
         lev                  : Int
         mbCtxCount           : Maybe Int
         varS                 : FvS
      chained attribute:
         gUniq                : Int
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levOf                : Int
   alternatives:
      alternative Just:
         child just           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
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
type T_MbCExpr  = CVarReplNmMp ->
                  CVarIntroMp ->
                  EvalCtx ->
                  Int ->
                  CVarIntroMp ->
                  Bool ->
                  Bool ->
                  Bool ->
                  LamArgMp ->
                  FvSMp ->
                  FvS ->
                  Int ->
                  (Maybe Int) ->
                  FvS ->
                  ( MbCExpr ,FvS,Int,Int)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS ->
         (let _justOisTopApp :: Bool
              _justOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              _lhsOgUniq :: Int
              _justOargMp :: CVarReplNmMp
              _justOcvarIntroMp :: CVarIntroMp
              _justOevalCtx :: EvalCtx
              _justOgUniq :: Int
              _justOintroCVarIntroMp :: CVarIntroMp
              _justOisDictClass :: Bool
              _justOisLamBody :: Bool
              _justOisStrict :: Bool
              _justOlamArgMp :: LamArgMp
              _justOlamFvSMp :: FvSMp
              _justOlamS :: FvS
              _justOlev :: Int
              _justOmbCtxCount :: (Maybe Int)
              _justOvarS :: FvS
              _justOwhatAbove :: WhatExpr
              _justIappFunKind :: AppFunKind
              _justIbindLamArgMp :: LamArgMp
              _justIcTrf :: CExpr 
              _justIenvUp :: Env
              _justIfvS :: FvS
              _justIgUniq :: Int
              _justIlevOf :: Int
              _justImbLam :: (Maybe [HsName])
              _justImbVar :: (Maybe HsName)
              _justIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 25, column 17)
              _justOisTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 88, column 17)
              _whatAbove =
                  ExprIsOther
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
              -- copy rule (up)
              _lhsOgUniq =
                  _justIgUniq
              -- copy rule (down)
              _justOargMp =
                  _lhsIargMp
              -- copy rule (down)
              _justOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _justOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _justOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _justOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _justOisDictClass =
                  _lhsIisDictClass
              -- copy rule (down)
              _justOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _justOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _justOlamArgMp =
                  _lhsIlamArgMp
              -- copy rule (down)
              _justOlamFvSMp =
                  _lhsIlamFvSMp
              -- copy rule (down)
              _justOlamS =
                  _lhsIlamS
              -- copy rule (down)
              _justOlev =
                  _lhsIlev
              -- copy rule (down)
              _justOmbCtxCount =
                  _lhsImbCtxCount
              -- copy rule (down)
              _justOvarS =
                  _lhsIvarS
              -- copy rule (from local)
              _justOwhatAbove =
                  _whatAbove
              ( _justIappFunKind,_justIbindLamArgMp,_justIcTrf,_justIenvUp,_justIfvS,_justIgUniq,_justIlevOf,_justImbLam,_justImbVar,_justIwhatBelow) =
                  just_ _justOargMp _justOcvarIntroMp _justOevalCtx _justOgUniq _justOintroCVarIntroMp _justOisDictClass _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlamArgMp _justOlamFvSMp _justOlamS _justOlev _justOmbCtxCount _justOvarS _justOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIargMp
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisDictClass
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamArgMp
       _lhsIlamFvSMp
       _lhsIlamS
       _lhsIlev
       _lhsImbCtxCount
       _lhsIvarS ->
         (let _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              _lhsOgUniq :: Int
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
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevOf)))