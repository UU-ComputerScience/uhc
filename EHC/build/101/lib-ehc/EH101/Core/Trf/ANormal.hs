

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Trf/ANormal.ag)
module EH101.Core.Trf.ANormal(cmodTrfANormal) where

import EH101.Base.Common
import EH101.Ty
import EH101.Base.Builtin
import Data.Maybe
import qualified Data.Set as Set
import Data.List as List
import qualified Data.Map as Map
import qualified EH.Util.FastSeq as Seq
import EH101.AbstractCore
import EH101.Core
import EH101.Base.Debug













cmodTrfANormal :: HsName -> UID -> CModule -> CModule
cmodTrfANormal modNm uniq cmod
  =  let  t = wrap_CodeAGItf  (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                              (Inh_CodeAGItf
                                 { gUniq_Inh_CodeAGItf = uniq
                                 , modNm_Inh_CodeAGItf = modNm
                                 })
     in   cTrf_Syn_CodeAGItf t



type LevBindSq = Seq.FastSeq (Int,(CBindCateg,CBindL))

levBindSplit :: Int -> LevBindSq -> (LevBindSq,LevBindSq)
levBindSplit lev b
  =  let (b1,b2) = partition (\(l,_) -> l >= lev) $ Seq.toList b
     in  (Seq.fromList b1,Seq.fromList b2)



data AppTop = AppYesLet | AppNoLet deriving (Eq,Ord)
data LamTop = LamYesLet | LamNoLet deriving (Eq,Ord)



mkTrf :: Bool -> Int -> Int -> HsName -> CExpr -> (CExpr,LevBindSq)
mkTrf cond outerLev hereLev n e
  =  if cond
     then (acoreVar n,Seq.singleton (hereLev,(CBindCateg_Plain,[acoreBind1 n e])))
     else (e,Seq.empty)



mkLetTrf' :: LevBindSq -> CExpr -> (Bool,CExpr)
mkLetTrf' bs ce
  =  let  l = Seq.toList bs
     in   (not (List.null l),foldr (\(_,(c,b)) e -> acoreLet c b e) ce l)

mkLetTrf :: LevBindSq -> CExpr -> CExpr
mkLetTrf bs ce = snd (mkLetTrf' bs ce)

-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
         levOf                : Int
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
         visit 0:
            local _tup1       : _
            local hereBindSq  : _
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
type T_CAlt  = CVarIntroMp ->
               EvalCtx ->
               UID ->
               CVarIntroMp ->
               Bool ->
               Bool ->
               Int ->
               HsName ->
               ( CAlt ,FvS,UID,LevBindSq,Int)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOlevBindSq :: LevBindSq
              _exprOappTrfIsOk :: AppTop
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _lhsOcTrf :: CAlt 
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOintroCVarIntroMp :: CVarIntroMp
              _lhsOlevOf :: Int
              _lhsOfvS :: FvS
              _lhsOgUniq :: UID
              _patOcvarIntroMp :: CVarIntroMp
              _patOgUniq :: UID
              _patOintroCVarIntroMp :: CVarIntroMp
              _patOlev :: Int
              _patOmodNm :: HsName
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: UID
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _patIcTrf :: CPat 
              _patIfldNmL :: ([HsName])
              _patIfvS :: FvS
              _patIgUniq :: UID
              _patIlevBindSq :: LevBindSq
              _patIlevOf :: Int
              _patInmL :: ([HsName])
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 100, column 17)
              __tup1 =
                  (_exprIlevBindSq,Seq.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 100, column 17)
              (_hereBindSq,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 100, column 17)
              (_,_lhsOlevBindSq) =
                  __tup1
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 131, column 17)
              _exprOappTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 157, column 17)
              _exprOlamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 183, column 17)
              _exprOletTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 220, column 17)
              _lhsOcTrf =
                  CAlt_Alt _patIcTrf (mkLetTrf _hereBindSq _exprIcTrf)
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- self rule
              _cTrf =
                  CAlt_Alt _patIcTrf _exprIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (down)
              _patOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _patOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _patOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (from local)
              _patOlev =
                  _lev
              -- copy rule (down)
              _patOmodNm =
                  _lhsImodNm
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
              -- copy rule (from local)
              _exprOlev =
                  _lev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _patIcTrf,_patIfldNmL,_patIfvS,_patIgUniq,_patIlevBindSq,_patIlevOf,_patInmL) =
                  pat_ _patOcvarIntroMp _patOgUniq _patOintroCVarIntroMp _patOlev _patOmodNm 
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
type T_CAltL  = CVarIntroMp ->
                EvalCtx ->
                UID ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                Int ->
                HsName ->
                ( CAltL ,FvS,UID,LevBindSq,Int)
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              _lhsOgUniq :: UID
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: UID
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOlev :: Int
              _hdOmodNm :: HsName
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: UID
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOlev :: Int
              _tlOmodNm :: HsName
              _hdIcTrf :: CAlt 
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdIlevBindSq :: LevBindSq
              _hdIlevOf :: Int
              _tlIcTrf :: CAltL 
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlIlevBindSq :: LevBindSq
              _tlIlevOf :: Int
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _hdIlevBindSq Seq.:++: _tlIlevBindSq
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
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmodNm =
                  _lhsImodNm
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
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmodNm =
                  _lhsImodNm
              ( _hdIcTrf,_hdIfvS,_hdIgUniq,_hdIlevBindSq,_hdIlevOf) =
                  hd_ _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisLamBody _hdOisStrict _hdOlev _hdOmodNm 
              ( _tlIcTrf,_tlIfvS,_tlIgUniq,_tlIlevBindSq,_tlIlevOf) =
                  tl_ _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisLamBody _tlOisStrict _tlOlev _tlOmodNm 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CAltL 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindL                : AssocL Int CBind
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levBindSq            : LevBindSq
         levOf                : Int
         nm                   : HsName
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
         visit 0:
            local cTrf        : _
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = CVarIntroMp ->
                EvalCtx ->
                UID ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                Bool ->
                CBindCateg ->
                Int ->
                HsName ->
                ( (AssocL Int CBind),CVarIntroMp,CBind ,CVarIntroMp,FvS,FvSMp,UID,LevBindSq,Int,HsName,([HsName]))
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImodNm ->
         (let _bindAspectsOnm :: HsName
              _lhsOnm :: HsName
              _lhsOfvSMp :: FvSMp
              _lhsOnmL :: ([HsName])
              _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CBind 
              _lhsOgUniq :: UID
              _bindAspectsOcvarIntroMp :: CVarIntroMp
              _bindAspectsOevalCtx :: EvalCtx
              _bindAspectsOgUniq :: UID
              _bindAspectsOintroCVarIntroMp :: CVarIntroMp
              _bindAspectsOisGlobal :: Bool
              _bindAspectsOisLamBody :: Bool
              _bindAspectsOisStrict :: Bool
              _bindAspectsOletBindingsCateg :: CBindCateg
              _bindAspectsOlev :: Int
              _bindAspectsOmodNm :: HsName
              _bindAspectsIbindL :: (AssocL Int CBind)
              _bindAspectsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindAspectsIcTrf :: CBoundL 
              _bindAspectsIcvarIntroExprMp :: CVarIntroMp
              _bindAspectsIfvS :: FvS
              _bindAspectsIfvSMp :: FvSMp
              _bindAspectsIgUniq :: UID
              _bindAspectsIlevBindSq :: LevBindSq
              _bindAspectsIlevOf :: Int
              _bindAspectsInmL :: ([HsName])
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  _bindAspectsIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _bindAspectsIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _bindAspectsIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindAspectsIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _bindAspectsIlevBindSq
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
              _bindAspectsOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _bindAspectsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindAspectsOmodNm =
                  _lhsImodNm
              ( _bindAspectsIbindL,_bindAspectsIbindsIntroCVarIntroMp,_bindAspectsIcTrf,_bindAspectsIcvarIntroExprMp,_bindAspectsIfvS,_bindAspectsIfvSMp,_bindAspectsIgUniq,_bindAspectsIlevBindSq,_bindAspectsIlevOf,_bindAspectsInmL) =
                  bindAspects_ _bindAspectsOcvarIntroMp _bindAspectsOevalCtx _bindAspectsOgUniq _bindAspectsOintroCVarIntroMp _bindAspectsOisGlobal _bindAspectsOisLamBody _bindAspectsOisStrict _bindAspectsOletBindingsCateg _bindAspectsOlev _bindAspectsOmodNm _bindAspectsOnm 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnm,_lhsOnmL)))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                   UID ->
                   CVarIntroMp ->
                   Int ->
                   HsName ->
                   ( CBindAnn ,FvS,UID,LevBindSq,Int,([HsName]))
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnn 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                    UID ->
                    CVarIntroMp ->
                    Int ->
                    HsName ->
                    ( CBindAnnL ,FvS,UID,LevBindSq,Int,([HsName]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              _lhsOgUniq :: UID
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOgUniq :: UID
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlev :: Int
              _hdOmodNm :: HsName
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOgUniq :: UID
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlev :: Int
              _tlOmodNm :: HsName
              _hdIcTrf :: CBindAnn 
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdIlevBindSq :: LevBindSq
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CBindAnnL 
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlIlevBindSq :: LevBindSq
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _hdIlevBindSq Seq.:++: _tlIlevBindSq
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
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmodNm =
                  _lhsImodNm
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
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmodNm =
                  _lhsImodNm
              ( _hdIcTrf,_hdIfvS,_hdIgUniq,_hdIlevBindSq,_hdIlevOf,_hdInmL) =
                  hd_ _hdOcvarIntroMp _hdOgUniq _hdOintroCVarIntroMp _hdOlev _hdOmodNm 
              ( _tlIcTrf,_tlIfvS,_tlIgUniq,_tlIlevBindSq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOgUniq _tlOintroCVarIntroMp _tlOlev _tlOmodNm 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindAnnL 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindL                : AssocL Int CBind
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levBindSq            : LevBindSq
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
type T_CBindL  = CVarIntroMp ->
                 EvalCtx ->
                 UID ->
                 CVarIntroMp ->
                 Bool ->
                 Bool ->
                 Bool ->
                 CBindCateg ->
                 Int ->
                 HsName ->
                 ( (AssocL Int CBind),CVarIntroMp,CBindL ,CVarIntroMp,FvS,FvSMp,UID,LevBindSq,Int,([HsName]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImodNm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              _lhsOgUniq :: UID
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: UID
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOmodNm :: HsName
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: UID
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOmodNm :: HsName
              _hdIbindL :: (AssocL Int CBind)
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIcTrf :: CBind 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: UID
              _hdIlevBindSq :: LevBindSq
              _hdIlevOf :: Int
              _hdInm :: HsName
              _hdInmL :: ([HsName])
              _tlIbindL :: (AssocL Int CBind)
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIcTrf :: CBindL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: UID
              _tlIlevBindSq :: LevBindSq
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  _hdIbindL ++ _tlIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _hdIlevBindSq Seq.:++: _tlIlevBindSq
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
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmodNm =
                  _lhsImodNm
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
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmodNm =
                  _lhsImodNm
              ( _hdIbindL,_hdIbindsIntroCVarIntroMp,_hdIcTrf,_hdIcvarIntroExprMp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIlevBindSq,_hdIlevOf,_hdInm,_hdInmL) =
                  hd_ _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOletBindingsCateg _hdOlev _hdOmodNm 
              ( _tlIbindL,_tlIbindsIntroCVarIntroMp,_tlIcTrf,_tlIcvarIntroExprMp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIlevBindSq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOmodNm 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImodNm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBindL 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CBound ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appTrfIsOk           : AppTop
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamTrfIsOk           : LamTop
         letBindingsCateg     : CBindCateg
         letTrfIsOk           : Bool
         lev                  : Int
         modNm                : HsName
         nm                   : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindL                : AssocL Int CBind
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levBindSq            : LevBindSq
         levOf                : Int
         nmL                  : [HsName]
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
         visit 0:
            local whatAbove   : {WhatExpr}
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
            local whatAbove   : {WhatExpr}
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
type T_CBound  = AppTop ->
                 CVarIntroMp ->
                 EvalCtx ->
                 UID ->
                 CVarIntroMp ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 Bool ->
                 LamTop ->
                 CBindCateg ->
                 Bool ->
                 Int ->
                 HsName ->
                 HsName ->
                 ( (AssocL Int CBind),CVarIntroMp,CBound ,CVarIntroMp,FvS,FvSMp,UID,LevBindSq,Int,([HsName]))
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _exprOappTrfIsOk :: AppTop
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              _bindMetaOcvarIntroMp :: CVarIntroMp
              _bindMetaOgUniq :: UID
              _bindMetaOintroCVarIntroMp :: CVarIntroMp
              _bindMetaOlev :: Int
              _bindMetaOmodNm :: HsName
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: UID
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _bindMetaIcTrf :: CMetas 
              _bindMetaIfvS :: FvS
              _bindMetaIgUniq :: UID
              _bindMetaIlevBindSq :: LevBindSq
              _bindMetaIlevOf :: Int
              _bindMetaIself :: CMetas 
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 94, column 17)
              _lhsObindL =
                  [(_exprIlevOf,acoreBind1Asp1 _lhsInm _cTrf)]
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 116, column 33)
              _exprOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 144, column 33)
              _exprOlamTrfIsOk =
                  LamNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 170, column 33)
              _exprOletTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bindMetaIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _bindMetaIlevBindSq Seq.:++: _exprIlevBindSq
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
              -- copy rule (up)
              _lhsOgUniq =
                  _exprIgUniq
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
              _bindMetaOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindMetaOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOevalCtx =
                  _lhsIevalCtx
              -- copy rule (chain)
              _exprOgUniq =
                  _bindMetaIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _bindMetaIcTrf,_bindMetaIfvS,_bindMetaIgUniq,_bindMetaIlevBindSq,_bindMetaIlevOf,_bindMetaIself) =
                  bindMeta_ _bindMetaOcvarIntroMp _bindMetaOgUniq _bindMetaOintroCVarIntroMp _bindMetaOlev _bindMetaOmodNm 
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _exprOappTrfIsOk :: AppTop
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: UID
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 97, column 17)
              _lhsObindL =
                  [(cLevModule,acoreBind1Asp1 _lhsInm _cTrf)]
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 116, column 33)
              _exprOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 144, column 33)
              _exprOlamTrfIsOk =
                  LamNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 170, column 33)
              _exprOletTrfIsOk =
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _exprIlevBindSq
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
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              _cmetasOcvarIntroMp :: CVarIntroMp
              _cmetasOgUniq :: UID
              _cmetasOintroCVarIntroMp :: CVarIntroMp
              _cmetasOlev :: Int
              _cmetasOmodNm :: HsName
              _cmetasIcTrf :: CMetas 
              _cmetasIfvS :: FvS
              _cmetasIgUniq :: UID
              _cmetasIlevBindSq :: LevBindSq
              _cmetasIlevOf :: Int
              _cmetasIself :: CMetas 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _cmetasIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _cmetasIlevBindSq
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
              _cmetasOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _cmetasOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _cmetasOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _cmetasOlev =
                  _lhsIlev
              -- copy rule (down)
              _cmetasOmodNm =
                  _lhsImodNm
              ( _cmetasIcTrf,_cmetasIfvS,_cmetasIgUniq,_cmetasIlevBindSq,_cmetasIlevOf,_cmetasIself) =
                  cmetas_ _cmetasOcvarIntroMp _cmetasOgUniq _cmetasOintroCVarIntroMp _cmetasOlev _cmetasOmodNm 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletBindingsCateg
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBound 
              _lhsOgUniq :: UID
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: UID
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 75, column 17)
              _whatAbove =
                  ExprIsBind
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 102, column 17)
              _exprOisStrict =
                  _lhsIisStrict || _exprIwhatBelow == ExprIsLam
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _exprIlevBindSq
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
              _exprOappTrfIsOk =
                  _lhsIappTrfIsOk
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
              _exprOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _exprOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isGlobal             : Bool
         isLamBody            : Bool
         isStrict             : Bool
         letBindingsCateg     : CBindCateg
         lev                  : Int
         modNm                : HsName
         nm                   : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         bindL                : AssocL Int CBind
         bindsIntroCVarIntroMp : CVarIntroMp
         cTrf                 : SELF 
         cvarIntroExprMp      : CVarIntroMp
         fvS                  : FvS
         fvSMp                : FvSMp
         levBindSq            : LevBindSq
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
type T_CBoundL  = CVarIntroMp ->
                  EvalCtx ->
                  UID ->
                  CVarIntroMp ->
                  Bool ->
                  Bool ->
                  Bool ->
                  CBindCateg ->
                  Int ->
                  HsName ->
                  HsName ->
                  ( (AssocL Int CBind),CVarIntroMp,CBoundL ,CVarIntroMp,FvS,FvSMp,UID,LevBindSq,Int,([HsName]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _hdOappTrfIsOk :: AppTop
              _hdOlamTrfIsOk :: LamTop
              _hdOletTrfIsOk :: Bool
              _hdOisTopApp :: Bool
              _hdOisTopTup :: Bool
              _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              _lhsOgUniq :: UID
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOevalCtx :: EvalCtx
              _hdOgUniq :: UID
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOisGlobal :: Bool
              _hdOisLamBody :: Bool
              _hdOisStrict :: Bool
              _hdOletBindingsCateg :: CBindCateg
              _hdOlev :: Int
              _hdOmodNm :: HsName
              _hdOnm :: HsName
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOevalCtx :: EvalCtx
              _tlOgUniq :: UID
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOisGlobal :: Bool
              _tlOisLamBody :: Bool
              _tlOisStrict :: Bool
              _tlOletBindingsCateg :: CBindCateg
              _tlOlev :: Int
              _tlOmodNm :: HsName
              _tlOnm :: HsName
              _hdIbindL :: (AssocL Int CBind)
              _hdIbindsIntroCVarIntroMp :: CVarIntroMp
              _hdIcTrf :: CBound 
              _hdIcvarIntroExprMp :: CVarIntroMp
              _hdIfvS :: FvS
              _hdIfvSMp :: FvSMp
              _hdIgUniq :: UID
              _hdIlevBindSq :: LevBindSq
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIbindL :: (AssocL Int CBind)
              _tlIbindsIntroCVarIntroMp :: CVarIntroMp
              _tlIcTrf :: CBoundL 
              _tlIcvarIntroExprMp :: CVarIntroMp
              _tlIfvS :: FvS
              _tlIfvSMp :: FvSMp
              _tlIgUniq :: UID
              _tlIlevBindSq :: LevBindSq
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 134, column 17)
              _hdOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 160, column 17)
              _hdOlamTrfIsOk =
                  LamNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 186, column 17)
              _hdOletTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 33, column 25)
              _hdOisTopTup =
                  True
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  _hdIbindL ++ _tlIbindL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  _hdIbindsIntroCVarIntroMp `Map.union` _tlIbindsIntroCVarIntroMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  _hdIcvarIntroExprMp `Map.union` _tlIcvarIntroExprMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  _hdIfvSMp `Map.union` _tlIfvSMp
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _hdIlevBindSq Seq.:++: _tlIlevBindSq
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
              _hdOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _hdOnm =
                  _lhsInm
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
              _tlOletBindingsCateg =
                  _lhsIletBindingsCateg
              -- copy rule (down)
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _tlOnm =
                  _lhsInm
              ( _hdIbindL,_hdIbindsIntroCVarIntroMp,_hdIcTrf,_hdIcvarIntroExprMp,_hdIfvS,_hdIfvSMp,_hdIgUniq,_hdIlevBindSq,_hdIlevOf,_hdInmL) =
                  hd_ _hdOappTrfIsOk _hdOcvarIntroMp _hdOevalCtx _hdOgUniq _hdOintroCVarIntroMp _hdOisGlobal _hdOisLamBody _hdOisStrict _hdOisTopApp _hdOisTopTup _hdOlamTrfIsOk _hdOletBindingsCateg _hdOletTrfIsOk _hdOlev _hdOmodNm _hdOnm 
              ( _tlIbindL,_tlIbindsIntroCVarIntroMp,_tlIcTrf,_tlIcvarIntroExprMp,_tlIfvS,_tlIfvSMp,_tlIgUniq,_tlIlevBindSq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOevalCtx _tlOgUniq _tlOintroCVarIntroMp _tlOisGlobal _tlOisLamBody _tlOisStrict _tlOletBindingsCateg _tlOlev _tlOmodNm _tlOnm 
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisGlobal
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIletBindingsCateg
       _lhsIlev
       _lhsImodNm
       _lhsInm ->
         (let _lhsObindL :: (AssocL Int CBind)
              _lhsObindsIntroCVarIntroMp :: CVarIntroMp
              _lhsOcvarIntroExprMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOfvSMp :: FvSMp
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CBoundL 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 70, column 26)
              _lhsObindL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 50, column 42)
              _lhsObindsIntroCVarIntroMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 72, column 36)
              _lhsOcvarIntroExprMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 2, column 26)
              _lhsOfvSMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsObindL,_lhsObindsIntroCVarIntroMp,_lhsOcTrf,_lhsOcvarIntroExprMp,_lhsOfvS,_lhsOfvSMp,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appTrfIsOk           : AppTop
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         isTopApp             : Bool
         isTopTup             : Bool
         lamTrfIsOk           : LamTop
         letTrfIsOk           : Bool
         lev                  : Int
         modNm                : HsName
         whatAbove            : WhatExpr
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
         levOf                : Int
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
            local _tup2       : _
            local lUniq       : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup3       : _
            local levBindSq   : _
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
            local _tup4       : _
            local lUniq       : _
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup5       : _
            local levBindSq   : _
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
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative CoeArg:
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
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
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local cTrf        : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local _tup6       : _
            local lUniq       : _
            local lUniq2      : _
            local _tup7       : _
            local hereBindSq  : _
            local remBindSq   : _
            local letTrfIsOk  : _
            local trNm        : _
            local trNm2       : _
            local trfIsOk     : _
            local _tup8       : _
            local levBindSq   : _
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
            local cTrf        : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local _tup9       : _
            local lUniq       : _
            local _tup10      : _
            local hereBindSq  : _
            local remBindSq   : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup11      : _
            local levBindSq   : _
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
            local _tup12      : _
            local strLev      : _
            local introCVarIntroMp : _
            local levOf       : _
            local cTrf        : _
      alternative String:
         child str            : {String}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
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
            local _tup13      : _
            local lUniq       : _
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup14      : _
            local levBindSq   : _
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
            local _tup15      : _
            local lUniq       : _
            local appTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup16      : _
            local levBindSq   : _
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
            local _tup17      : _
            local lUniq       : _
            local appTrfIsOk  : _
            local trNm        : _
            local trfIsOk     : _
            local _tup18      : _
            local levBindSq   : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
            local levOf       : _
            local cTrf        : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local appTrfIsOk  : _
            local lamTrfIsOk  : _
            local letTrfIsOk  : _
            local isTopApp    : {Bool}
            local isTopTup    : _
            local whatBelow   : _
            local whatAbove   : {WhatExpr}
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
type T_CExpr  = AppTop ->
                CVarIntroMp ->
                EvalCtx ->
                UID ->
                CVarIntroMp ->
                Bool ->
                Bool ->
                Bool ->
                Bool ->
                LamTop ->
                Bool ->
                Int ->
                HsName ->
                WhatExpr ->
                ( CExpr ,FvS,UID,LevBindSq,Int,WhatExpr)
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _annOcvarIntroMp :: CVarIntroMp
              _annOgUniq :: UID
              _annOintroCVarIntroMp :: CVarIntroMp
              _annOlev :: Int
              _annOmodNm :: HsName
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOgUniq :: UID
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _annIcTrf :: CExprAnn 
              _annIfvS :: FvS
              _annIgUniq :: UID
              _annIlevBindSq :: LevBindSq
              _annIlevOf :: Int
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _annIfvS `Set.union` _exprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _annIlevBindSq Seq.:++: _exprIlevBindSq
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
              _lhsOgUniq =
                  _exprIgUniq
              -- copy rule (up)
              _lhsOwhatBelow =
                  _exprIwhatBelow
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
              _annOlev =
                  _lhsIlev
              -- copy rule (down)
              _annOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _exprOappTrfIsOk =
                  _lhsIappTrfIsOk
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
              _exprOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _exprOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _exprOwhatAbove =
                  _lhsIwhatAbove
              ( _annIcTrf,_annIfvS,_annIgUniq,_annIlevBindSq,_annIlevOf) =
                  ann_ _annOcvarIntroMp _annOgUniq _annOintroCVarIntroMp _annOlev _annOmodNm 
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _funcOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _funcOappTrfIsOk :: AppTop
              _argOappTrfIsOk :: AppTop
              _lhsOcTrf :: CExpr 
              _funcOisTopApp :: Bool
              _argOisTopApp :: Bool
              _whatAbove :: WhatExpr
              _argOnm :: HsName
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOevalCtx :: EvalCtx
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopTup :: Bool
              _funcOlamTrfIsOk :: LamTop
              _funcOletTrfIsOk :: Bool
              _funcOlev :: Int
              _funcOmodNm :: HsName
              _funcOwhatAbove :: WhatExpr
              _argOcvarIntroMp :: CVarIntroMp
              _argOevalCtx :: EvalCtx
              _argOgUniq :: UID
              _argOintroCVarIntroMp :: CVarIntroMp
              _argOisGlobal :: Bool
              _argOisLamBody :: Bool
              _argOisStrict :: Bool
              _argOisTopTup :: Bool
              _argOlamTrfIsOk :: LamTop
              _argOletBindingsCateg :: CBindCateg
              _argOletTrfIsOk :: Bool
              _argOlev :: Int
              _argOmodNm :: HsName
              _funcIcTrf :: CExpr 
              _funcIfvS :: FvS
              _funcIgUniq :: UID
              _funcIlevBindSq :: LevBindSq
              _funcIlevOf :: Int
              _funcIwhatBelow :: WhatExpr
              _argIbindL :: (AssocL Int CBind)
              _argIbindsIntroCVarIntroMp :: CVarIntroMp
              _argIcTrf :: CBound 
              _argIcvarIntroExprMp :: CVarIntroMp
              _argIfvS :: FvS
              _argIfvSMp :: FvSMp
              _argIgUniq :: UID
              _argIlevBindSq :: LevBindSq
              _argIlevOf :: Int
              _argInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 53, column 17)
              __tup2 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 53, column 17)
              (_funcOgUniq,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 53, column 17)
              (_,_lUniq) =
                  __tup2
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 73, column 17)
              _lhsOlevBindSq =
                  _argIlevBindSq Seq.:++: _funcIlevBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 122, column 17)
              _funcOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 123, column 17)
              _argOappTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 178, column 17)
              _letTrfIsOk =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 225, column 17)
              _trfIsOk =
                  _lhsIappTrfIsOk == AppYesLet && _lhsIisTopApp
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              __tup3 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              (_lhsOcTrf,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              (_,_levBindSq) =
                  __tup3
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_App _funcIcTrf _argIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _argIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (down)
              _funcOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _funcOevalCtx =
                  _lhsIevalCtx
              -- copy rule (down)
              _funcOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _funcOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _funcOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _funcOisTopTup =
                  _isTopTup
              -- copy rule (from local)
              _funcOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _funcOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
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
              -- copy rule (from local)
              _argOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _argOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _argOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _argOlev =
                  _lhsIlev
              -- copy rule (down)
              _argOmodNm =
                  _lhsImodNm
              ( _funcIcTrf,_funcIfvS,_funcIgUniq,_funcIlevBindSq,_funcIlevOf,_funcIwhatBelow) =
                  func_ _funcOappTrfIsOk _funcOcvarIntroMp _funcOevalCtx _funcOgUniq _funcOintroCVarIntroMp _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamTrfIsOk _funcOletTrfIsOk _funcOlev _funcOmodNm _funcOwhatAbove 
              ( _argIbindL,_argIbindsIntroCVarIntroMp,_argIcTrf,_argIcvarIntroExprMp,_argIfvS,_argIfvSMp,_argIgUniq,_argIlevBindSq,_argIlevOf,_argInmL) =
                  arg_ _argOappTrfIsOk _argOcvarIntroMp _argOevalCtx _argOgUniq _argOintroCVarIntroMp _argOisGlobal _argOisLamBody _argOisStrict _argOisTopApp _argOisTopTup _argOlamTrfIsOk _argOletBindingsCateg _argOletTrfIsOk _argOlev _argOmodNm _argOnm 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _exprOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _altsOcvarIntroMp :: CVarIntroMp
              _altsOevalCtx :: EvalCtx
              _altsOgUniq :: UID
              _altsOintroCVarIntroMp :: CVarIntroMp
              _altsOisLamBody :: Bool
              _altsOisStrict :: Bool
              _altsOlev :: Int
              _altsOmodNm :: HsName
              _dfltOappTrfIsOk :: AppTop
              _dfltOcvarIntroMp :: CVarIntroMp
              _dfltOevalCtx :: EvalCtx
              _dfltOgUniq :: UID
              _dfltOintroCVarIntroMp :: CVarIntroMp
              _dfltOisLamBody :: Bool
              _dfltOisStrict :: Bool
              _dfltOisTopApp :: Bool
              _dfltOisTopTup :: Bool
              _dfltOlamTrfIsOk :: LamTop
              _dfltOletTrfIsOk :: Bool
              _dfltOlev :: Int
              _dfltOmodNm :: HsName
              _dfltOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              _altsIcTrf :: CAltL 
              _altsIfvS :: FvS
              _altsIgUniq :: UID
              _altsIlevBindSq :: LevBindSq
              _altsIlevOf :: Int
              _dfltIcTrf :: CExpr 
              _dfltIfvS :: FvS
              _dfltIgUniq :: UID
              _dfltIlevBindSq :: LevBindSq
              _dfltIlevOf :: Int
              _dfltIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              __tup4 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_exprOgUniq,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_,_lUniq) =
                  __tup4
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 74, column 17)
              _lhsOlevBindSq =
                  _exprIlevBindSq Seq.:++: _altsIlevBindSq Seq.:++: _dfltIlevBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 225, column 17)
              _trfIsOk =
                  _lhsIappTrfIsOk == AppYesLet && _lhsIisTopApp
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              __tup5 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              (_lhsOcTrf,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 232, column 17)
              (_,_levBindSq) =
                  __tup5
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _altsIfvS `Set.union` _dfltIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Case _exprIcTrf _altsIcTrf _dfltIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _dfltIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _exprOappTrfIsOk =
                  _appTrfIsOk
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
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _exprOisTopTup =
                  _isTopTup
              -- copy rule (from local)
              _exprOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _exprOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
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
              _altsOlev =
                  _lhsIlev
              -- copy rule (down)
              _altsOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _dfltOappTrfIsOk =
                  _appTrfIsOk
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
              -- copy rule (from local)
              _dfltOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _dfltOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _dfltOlev =
                  _lhsIlev
              -- copy rule (down)
              _dfltOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _dfltOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
              ( _altsIcTrf,_altsIfvS,_altsIgUniq,_altsIlevBindSq,_altsIlevOf) =
                  alts_ _altsOcvarIntroMp _altsOevalCtx _altsOgUniq _altsOintroCVarIntroMp _altsOisLamBody _altsOisStrict _altsOlev _altsOmodNm 
              ( _dfltIcTrf,_dfltIfvS,_dfltIgUniq,_dfltIlevBindSq,_dfltIlevOf,_dfltIwhatBelow) =
                  dflt_ _dfltOappTrfIsOk _dfltOcvarIntroMp _dfltOevalCtx _dfltOgUniq _dfltOintroCVarIntroMp _dfltOisLamBody _dfltOisStrict _dfltOisTopApp _dfltOisTopTup _dfltOlamTrfIsOk _dfltOletTrfIsOk _dfltOlev _dfltOmodNm _dfltOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _errorExprOappTrfIsOk :: AppTop
              _errorExprOcvarIntroMp :: CVarIntroMp
              _errorExprOevalCtx :: EvalCtx
              _errorExprOgUniq :: UID
              _errorExprOintroCVarIntroMp :: CVarIntroMp
              _errorExprOisLamBody :: Bool
              _errorExprOisStrict :: Bool
              _errorExprOisTopApp :: Bool
              _errorExprOisTopTup :: Bool
              _errorExprOlamTrfIsOk :: LamTop
              _errorExprOletTrfIsOk :: Bool
              _errorExprOlev :: Int
              _errorExprOmodNm :: HsName
              _errorExprOwhatAbove :: WhatExpr
              _errorExprIcTrf :: CExpr 
              _errorExprIfvS :: FvS
              _errorExprIgUniq :: UID
              _errorExprIlevBindSq :: LevBindSq
              _errorExprIlevOf :: Int
              _errorExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 15, column 17)
              _isTopApp =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 20, column 17)
              _isTopTup =
                  True
              -- "build/101/lib-ehc/EH101/Core/CommonCtxtPred.ag"(line 69, column 17)
              _whatAbove =
                  ExprIsOther
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _errorExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _errorExprIlevBindSq
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
              _lhsOgUniq =
                  _errorExprIgUniq
              -- copy rule (up)
              _lhsOwhatBelow =
                  _errorExprIwhatBelow
              -- copy rule (down)
              _errorExprOappTrfIsOk =
                  _lhsIappTrfIsOk
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
              _errorExprOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _errorExprOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _errorExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _errorExprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _errorExprOwhatAbove =
                  _whatAbove
              ( _errorExprIcTrf,_errorExprIfvS,_errorExprIgUniq,_errorExprIlevBindSq,_errorExprIlevOf,_errorExprIwhatBelow) =
                  errorExpr_ _errorExprOappTrfIsOk _errorExprOcvarIntroMp _errorExprOevalCtx _errorExprOgUniq _errorExprOintroCVarIntroMp _errorExprOisLamBody _errorExprOisStrict _errorExprOisTopApp _errorExprOisTopTup _errorExprOlamTrfIsOk _errorExprOletTrfIsOk _errorExprOlev _errorExprOmodNm _errorExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _bodyOappTrfIsOk :: AppTop
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: UID
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamTrfIsOk :: LamTop
              _bodyOletTrfIsOk :: Bool
              _bodyOlev :: Int
              _bodyOmodNm :: HsName
              _bodyOwhatAbove :: WhatExpr
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIlevBindSq :: LevBindSq
              _bodyIlevOf :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _bodyIlevBindSq
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
              -- copy rule (from local)
              _bodyOappTrfIsOk =
                  _appTrfIsOk
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
              _bodyOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _bodyOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIcTrf,_bodyIfvS,_bodyIgUniq,_bodyIlevBindSq,_bodyIlevOf,_bodyIwhatBelow) =
                  body_ _bodyOappTrfIsOk _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamTrfIsOk _bodyOletTrfIsOk _bodyOlev _bodyOmodNm _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _funcOappTrfIsOk :: AppTop
              _funcOcvarIntroMp :: CVarIntroMp
              _funcOevalCtx :: EvalCtx
              _funcOgUniq :: UID
              _funcOintroCVarIntroMp :: CVarIntroMp
              _funcOisLamBody :: Bool
              _funcOisStrict :: Bool
              _funcOisTopApp :: Bool
              _funcOisTopTup :: Bool
              _funcOlamTrfIsOk :: LamTop
              _funcOletTrfIsOk :: Bool
              _funcOlev :: Int
              _funcOmodNm :: HsName
              _funcOwhatAbove :: WhatExpr
              _funcIcTrf :: CExpr 
              _funcIfvS :: FvS
              _funcIgUniq :: UID
              _funcIlevBindSq :: LevBindSq
              _funcIlevOf :: Int
              _funcIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _funcIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _funcIlevBindSq
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
              -- copy rule (from local)
              _funcOappTrfIsOk =
                  _appTrfIsOk
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
              -- copy rule (from local)
              _funcOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _funcOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _funcOlev =
                  _lhsIlev
              -- copy rule (down)
              _funcOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _funcOwhatAbove =
                  _whatAbove
              ( _funcIcTrf,_funcIfvS,_funcIgUniq,_funcIlevBindSq,_funcIlevOf,_funcIwhatBelow) =
                  func_ _funcOappTrfIsOk _funcOcvarIntroMp _funcOevalCtx _funcOgUniq _funcOintroCVarIntroMp _funcOisLamBody _funcOisStrict _funcOisTopApp _funcOisTopTup _funcOlamTrfIsOk _funcOletTrfIsOk _funcOlev _funcOmodNm _funcOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _bodyOappTrfIsOk :: AppTop
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: UID
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamTrfIsOk :: LamTop
              _bodyOletTrfIsOk :: Bool
              _bodyOlev :: Int
              _bodyOmodNm :: HsName
              _bodyOwhatAbove :: WhatExpr
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIlevBindSq :: LevBindSq
              _bodyIlevOf :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _bodyIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _bodyIlevBindSq
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
              -- copy rule (from local)
              _bodyOappTrfIsOk =
                  _appTrfIsOk
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
              _bodyOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _bodyOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bodyIcTrf,_bodyIfvS,_bodyIgUniq,_bodyIlevBindSq,_bodyIlevOf,_bodyIwhatBelow) =
                  body_ _bodyOappTrfIsOk _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamTrfIsOk _bodyOletTrfIsOk _bodyOlev _bodyOmodNm _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _bodyOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _bodyOappTrfIsOk :: AppTop
              _bodyOlamTrfIsOk :: LamTop
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bodyOcvarIntroMp :: CVarIntroMp
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOevalCtx :: EvalCtx
              _bindOgUniq :: UID
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOisGlobal :: Bool
              _bindOisLamBody :: Bool
              _bindOisStrict :: Bool
              _bindOletBindingsCateg :: CBindCateg
              _bindOlev :: Int
              _bindOmodNm :: HsName
              _bodyOevalCtx :: EvalCtx
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOletTrfIsOk :: Bool
              _bodyOlev :: Int
              _bodyOmodNm :: HsName
              _bodyOwhatAbove :: WhatExpr
              _bindIbindL :: (AssocL Int CBind)
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: UID
              _bindIlevBindSq :: LevBindSq
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIlevBindSq :: LevBindSq
              _bodyIlevOf :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 54, column 17)
              __tup6 =
                  mkNewLevUID2 _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 54, column 17)
              (_bodyOgUniq,_,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 54, column 17)
              (_,_lUniq,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 54, column 17)
              (_,_,_lUniq2) =
                  __tup6
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 89, column 33)
              __tup7 =
                  (_bodyIlevBindSq,Seq.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 89, column 33)
              (_hereBindSq,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 89, column 33)
              (_,_remBindSq) =
                  __tup7
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 91, column 17)
              _lhsOlevBindSq =
                  _remBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 126, column 17)
              _bodyOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 152, column 17)
              _bodyOlamTrfIsOk =
                  LamNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 193, column 17)
              _trNm2 =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq2
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 226, column 17)
              _trfIsOk =
                  _lhsIlamTrfIsOk == LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 233, column 17)
              __tup8 =
                  let mkTLB = mkTrf _trfIsOk _lhsIlev _levOf _trNm
                            . acoreLam1 _argNm
                            . mkLetTrf _hereBindSq
                  in  if cexprIsLam _bodyIcTrf && not (List.null (Seq.toList _hereBindSq))
                      then mkTLB (CExpr_Let CBindCateg_Plain [acoreBind1Cat CBindCateg_Plain _trNm2 _bodyIcTrf] (acoreVar _trNm2))
                      else mkTLB _bodyIcTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 233, column 17)
              (_lhsOcTrf,_) =
                  __tup8
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 233, column 17)
              (_,_levBindSq) =
                  __tup8
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Lam _bindIcTrf _bodyIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
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
              -- copy rule (from local)
              _bindOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (from local)
              _bindOlev =
                  _lev
              -- copy rule (down)
              _bindOmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _bodyOevalCtx =
                  _lhsIevalCtx
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
              _bodyOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (from local)
              _bodyOlev =
                  _lev
              -- copy rule (down)
              _bodyOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindIbindL,_bindIbindsIntroCVarIntroMp,_bindIcTrf,_bindIcvarIntroExprMp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIlevBindSq,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOcvarIntroMp _bindOevalCtx _bindOgUniq _bindOintroCVarIntroMp _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOmodNm 
              ( _bodyIcTrf,_bodyIfvS,_bodyIgUniq,_bodyIlevBindSq,_bodyIlevOf,_bodyIwhatBelow) =
                  body_ _bodyOappTrfIsOk _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamTrfIsOk _bodyOletTrfIsOk _bodyOlev _bodyOmodNm _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _bindsOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _bodyOappTrfIsOk :: AppTop
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _bindsOisStrict :: Bool
              _bindsOcvarIntroMp :: CVarIntroMp
              _bodyOcvarIntroMp :: CVarIntroMp
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _bindsOevalCtx :: EvalCtx
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOisGlobal :: Bool
              _bindsOisLamBody :: Bool
              _bindsOletBindingsCateg :: CBindCateg
              _bindsOlev :: Int
              _bindsOmodNm :: HsName
              _bodyOevalCtx :: EvalCtx
              _bodyOgUniq :: UID
              _bodyOintroCVarIntroMp :: CVarIntroMp
              _bodyOisLamBody :: Bool
              _bodyOisStrict :: Bool
              _bodyOisTopApp :: Bool
              _bodyOisTopTup :: Bool
              _bodyOlamTrfIsOk :: LamTop
              _bodyOletTrfIsOk :: Bool
              _bodyOlev :: Int
              _bodyOmodNm :: HsName
              _bodyOwhatAbove :: WhatExpr
              _bindsIbindL :: (AssocL Int CBind)
              _bindsIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindsIcTrf :: CBindL 
              _bindsIcvarIntroExprMp :: CVarIntroMp
              _bindsIfvS :: FvS
              _bindsIfvSMp :: FvSMp
              _bindsIgUniq :: UID
              _bindsIlevBindSq :: LevBindSq
              _bindsIlevOf :: Int
              _bindsInmL :: ([HsName])
              _bodyIcTrf :: CExpr 
              _bodyIfvS :: FvS
              _bodyIgUniq :: UID
              _bodyIlevBindSq :: LevBindSq
              _bodyIlevOf :: Int
              _bodyIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 56, column 17)
              __tup9 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 56, column 17)
              (_bindsOgUniq,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 56, column 17)
              (_,_lUniq) =
                  __tup9
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 78, column 17)
              __tup10 =
                  let ((hl,_):_) = _bindsIbindL
                      b1  = _bindsIlevBindSq
                      b2  = Seq.singleton (_strLev hl,(categ_,assocLElts _bindsIbindL))
                      b12 = Seq.singleton (_maxBindLev,(CBindCateg_Rec,concat $ List.map (\(_,(_,b)) -> b) $ Seq.toList $ (b1 Seq.:++: b2)))
                      b3  = _bodyIlevBindSq
                      b   = case categ_ of
                              CBindCateg_Rec -> b12 Seq.:++: b3
                              _              -> b1  Seq.:++: b2 Seq.:++: b3
                  in  (b,Seq.empty)
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 78, column 17)
              (_hereBindSq,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 78, column 17)
              (_,_remBindSq) =
                  __tup10
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 88, column 17)
              _lhsOlevBindSq =
                  _remBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 126, column 17)
              _bodyOappTrfIsOk =
                  AppNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 227, column 17)
              _trfIsOk =
                  _lhsIletTrfIsOk
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 239, column 17)
              __tup11 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm $ mkLetTrf _hereBindSq _bodyIcTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 239, column 17)
              (_lhsOcTrf,_) =
                  __tup11
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 239, column 17)
              (_,_levBindSq) =
                  __tup11
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
              __tup12 =
                  case categ_ of
                      CBindCateg_Strict -> (const _lhsIlev,_lhsIcvarIntroMp)
                      CBindCateg_Rec    -> ( const _maxBindLev
                                           , Map.map (\cvi -> cvi {cviLev = _maxBindLev}) _bindsIcvarIntroExprMp
                                               `Map.union` _lhsIcvarIntroMp
                                           )
                      _                 -> (id,_lhsIcvarIntroMp)
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_strLev,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 25, column 17)
              (_,_bindsOcvarIntroMp) =
                  __tup12
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 33, column 17)
              _bodyOcvarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _strLev $ cviLev cvi}) _bindsIcvarIntroExprMp `Map.union` _lhsIcvarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 45, column 17)
              _introCVarIntroMp =
                  Map.map (\cvi -> cvi {cviLev = _lhsIlev}) _bindsIbindsIntroCVarIntroMp `Map.union` _lhsIintroCVarIntroMp
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 66, column 17)
              _levOf =
                  fvsLev _lhsIcvarIntroMp cLevModule _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _fvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_Let categ_ _bindsIcTrf _bodyIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _bodyIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _bindsOevalCtx =
                  _evalCtx
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
              _bindsOletBindingsCateg =
                  _letBindingsCateg
              -- copy rule (down)
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOmodNm =
                  _lhsImodNm
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
              _bodyOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _bodyOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _bodyOlev =
                  _lhsIlev
              -- copy rule (down)
              _bodyOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _bodyOwhatAbove =
                  _whatAbove
              ( _bindsIbindL,_bindsIbindsIntroCVarIntroMp,_bindsIcTrf,_bindsIcvarIntroExprMp,_bindsIfvS,_bindsIfvSMp,_bindsIgUniq,_bindsIlevBindSq,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOcvarIntroMp _bindsOevalCtx _bindsOgUniq _bindsOintroCVarIntroMp _bindsOisGlobal _bindsOisLamBody _bindsOisStrict _bindsOletBindingsCateg _bindsOlev _bindsOmodNm 
              ( _bodyIcTrf,_bodyIfvS,_bodyIgUniq,_bodyIlevBindSq,_bodyIlevOf,_bodyIwhatBelow) =
                  body_ _bodyOappTrfIsOk _bodyOcvarIntroMp _bodyOevalCtx _bodyOgUniq _bodyOintroCVarIntroMp _bodyOisLamBody _bodyOisStrict _bodyOisTopApp _bodyOisTopTup _bodyOlamTrfIsOk _bodyOletTrfIsOk _bodyOlev _bodyOmodNm _bodyOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _exprOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _offsetOappTrfIsOk :: AppTop
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: UID
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamTrfIsOk :: LamTop
              _offsetOletTrfIsOk :: Bool
              _offsetOlev :: Int
              _offsetOmodNm :: HsName
              _offsetOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIlevBindSq :: LevBindSq
              _offsetIlevOf :: Int
              _offsetIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              __tup13 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_exprOgUniq,_) =
                  __tup13
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_,_lUniq) =
                  __tup13
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 75, column 17)
              _lhsOlevBindSq =
                  _exprIlevBindSq Seq.:++: _offsetIlevBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 125, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 224, column 17)
              _trfIsOk =
                  _lhsIappTrfIsOk == AppYesLet && _lhsIisTopTup
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              __tup14 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_lhsOcTrf,_) =
                  __tup14
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_,_levBindSq) =
                  __tup14
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupDel _exprIcTrf tag_ nm_ _offsetIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _offsetIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _exprOappTrfIsOk =
                  _appTrfIsOk
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
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (from local)
              _exprOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _exprOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (from local)
              _offsetOappTrfIsOk =
                  _appTrfIsOk
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
              -- copy rule (from local)
              _offsetOlamTrfIsOk =
                  _lamTrfIsOk
              -- copy rule (from local)
              _offsetOletTrfIsOk =
                  _letTrfIsOk
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIfvS,_offsetIgUniq,_offsetIlevBindSq,_offsetIlevOf,_offsetIwhatBelow) =
                  offset_ _offsetOappTrfIsOk _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamTrfIsOk _offsetOletTrfIsOk _offsetOlev _offsetOmodNm _offsetOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _exprOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _fldExprOlamTrfIsOk :: LamTop
              _fldExprOletTrfIsOk :: Bool
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _offsetOappTrfIsOk :: AppTop
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: UID
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamTrfIsOk :: LamTop
              _offsetOletTrfIsOk :: Bool
              _offsetOlev :: Int
              _offsetOmodNm :: HsName
              _offsetOwhatAbove :: WhatExpr
              _fldExprOappTrfIsOk :: AppTop
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOevalCtx :: EvalCtx
              _fldExprOgUniq :: UID
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlev :: Int
              _fldExprOmodNm :: HsName
              _fldExprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIlevBindSq :: LevBindSq
              _offsetIlevOf :: Int
              _offsetIwhatBelow :: WhatExpr
              _fldExprIcTrf :: CExpr 
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: UID
              _fldExprIlevBindSq :: LevBindSq
              _fldExprIlevOf :: Int
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              __tup15 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_exprOgUniq,_) =
                  __tup15
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_,_lUniq) =
                  __tup15
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 77, column 17)
              _lhsOlevBindSq =
                  _exprIlevBindSq Seq.:++: _offsetIlevBindSq Seq.:++: _fldExprIlevBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 125, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 151, column 17)
              _fldExprOlamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 177, column 17)
              _fldExprOletTrfIsOk =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 224, column 17)
              _trfIsOk =
                  _lhsIappTrfIsOk == AppYesLet && _lhsIisTopTup
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              __tup16 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_lhsOcTrf,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_,_levBindSq) =
                  __tup16
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupIns _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _exprOappTrfIsOk =
                  _appTrfIsOk
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
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _exprOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (from local)
              _offsetOappTrfIsOk =
                  _appTrfIsOk
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
              _offsetOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _offsetOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (from local)
              _fldExprOappTrfIsOk =
                  _appTrfIsOk
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
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIfvS,_offsetIgUniq,_offsetIlevBindSq,_offsetIlevOf,_offsetIwhatBelow) =
                  offset_ _offsetOappTrfIsOk _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamTrfIsOk _offsetOletTrfIsOk _offsetOlev _offsetOmodNm _offsetOwhatAbove 
              ( _fldExprIcTrf,_fldExprIfvS,_fldExprIgUniq,_fldExprIlevBindSq,_fldExprIlevOf,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOappTrfIsOk _fldExprOcvarIntroMp _fldExprOevalCtx _fldExprOgUniq _fldExprOintroCVarIntroMp _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamTrfIsOk _fldExprOletTrfIsOk _fldExprOlev _fldExprOmodNm _fldExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _exprOgUniq :: UID
              _lhsOlevBindSq :: LevBindSq
              _fldExprOlamTrfIsOk :: LamTop
              _fldExprOletTrfIsOk :: Bool
              _lhsOcTrf :: CExpr 
              _isTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              _exprOappTrfIsOk :: AppTop
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOevalCtx :: EvalCtx
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOisLamBody :: Bool
              _exprOisStrict :: Bool
              _exprOisTopApp :: Bool
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _offsetOappTrfIsOk :: AppTop
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOevalCtx :: EvalCtx
              _offsetOgUniq :: UID
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOisLamBody :: Bool
              _offsetOisStrict :: Bool
              _offsetOisTopApp :: Bool
              _offsetOisTopTup :: Bool
              _offsetOlamTrfIsOk :: LamTop
              _offsetOletTrfIsOk :: Bool
              _offsetOlev :: Int
              _offsetOmodNm :: HsName
              _offsetOwhatAbove :: WhatExpr
              _fldExprOappTrfIsOk :: AppTop
              _fldExprOcvarIntroMp :: CVarIntroMp
              _fldExprOevalCtx :: EvalCtx
              _fldExprOgUniq :: UID
              _fldExprOintroCVarIntroMp :: CVarIntroMp
              _fldExprOisLamBody :: Bool
              _fldExprOisStrict :: Bool
              _fldExprOisTopApp :: Bool
              _fldExprOisTopTup :: Bool
              _fldExprOlev :: Int
              _fldExprOmodNm :: HsName
              _fldExprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIlevBindSq :: LevBindSq
              _offsetIlevOf :: Int
              _offsetIwhatBelow :: WhatExpr
              _fldExprIcTrf :: CExpr 
              _fldExprIfvS :: FvS
              _fldExprIgUniq :: UID
              _fldExprIlevBindSq :: LevBindSq
              _fldExprIlevOf :: Int
              _fldExprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              __tup17 =
                  mkNewUID _lhsIgUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_exprOgUniq,_) =
                  __tup17
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 58, column 17)
              (_,_lUniq) =
                  __tup17
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 77, column 17)
              _lhsOlevBindSq =
                  _exprIlevBindSq Seq.:++: _offsetIlevBindSq Seq.:++: _fldExprIlevBindSq Seq.:++: _levBindSq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 125, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 151, column 17)
              _fldExprOlamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 177, column 17)
              _fldExprOletTrfIsOk =
                  True
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 190, column 17)
              _trNm =
                  hsnQualUniqify _lhsImodNm $
                  uidHNm _lUniq
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 224, column 17)
              _trfIsOk =
                  _lhsIappTrfIsOk == AppYesLet && _lhsIisTopTup
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              __tup18 =
                  mkTrf _trfIsOk _lhsIlev _levOf _trNm _cTrf
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_lhsOcTrf,_) =
                  __tup18
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 231, column 17)
              (_,_levBindSq) =
                  __tup18
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _exprIfvS `Set.union` _offsetIfvS `Set.union` _fldExprIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonLev.ag"(line 56, column 28)
              _lhsOlevOf =
                  _levOf
              -- self rule
              _cTrf =
                  CExpr_TupUpd _exprIcTrf tag_ nm_ _offsetIcTrf _fldExprIcTrf
              -- copy rule (up)
              _lhsOgUniq =
                  _fldExprIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
              -- copy rule (from local)
              _exprOappTrfIsOk =
                  _appTrfIsOk
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
              _exprOisStrict =
                  _lhsIisStrict
              -- copy rule (from local)
              _exprOisTopApp =
                  _isTopApp
              -- copy rule (down)
              _exprOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _exprOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              -- copy rule (from local)
              _offsetOappTrfIsOk =
                  _appTrfIsOk
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
              _offsetOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _offsetOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
              -- copy rule (from local)
              _fldExprOappTrfIsOk =
                  _appTrfIsOk
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
              _fldExprOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldExprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _fldExprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
              ( _offsetIcTrf,_offsetIfvS,_offsetIgUniq,_offsetIlevBindSq,_offsetIlevOf,_offsetIwhatBelow) =
                  offset_ _offsetOappTrfIsOk _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamTrfIsOk _offsetOletTrfIsOk _offsetOlev _offsetOmodNm _offsetOwhatAbove 
              ( _fldExprIcTrf,_fldExprIfvS,_fldExprIgUniq,_fldExprIlevBindSq,_fldExprIlevOf,_fldExprIwhatBelow) =
                  fldExpr_ _fldExprOappTrfIsOk _fldExprOcvarIntroMp _fldExprOevalCtx _fldExprOgUniq _fldExprOintroCVarIntroMp _fldExprOisLamBody _fldExprOisStrict _fldExprOisTopApp _fldExprOisTopTup _fldExprOlamTrfIsOk _fldExprOletTrfIsOk _fldExprOlev _fldExprOmodNm _fldExprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIisTopApp
       _lhsIisTopTup
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm
       _lhsIwhatAbove ->
         (let _isTopApp :: Bool
              _whatAbove :: WhatExpr
              _nm :: HsName
              _nmAsp :: HsName
              _lhsOfvS :: FvS
              _lhsOlevOf :: Int
              _lhsOlevBindSq :: LevBindSq
              _lhsOcTrf :: CExpr 
              _lhsOgUniq :: UID
              _lhsOwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 128, column 17)
              _appTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 154, column 17)
              _lamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 180, column 17)
              _letTrfIsOk =
                  False
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
              -- self rule
              _cTrf =
                  CExpr_Var ref_
              -- self rule
              _lhsOcTrf =
                  _cTrf
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
              -- copy rule (from local)
              _lhsOwhatBelow =
                  _whatBelow
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOwhatBelow)))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                   UID ->
                   CVarIntroMp ->
                   Int ->
                   HsName ->
                   ( CExprAnn ,FvS,UID,LevBindSq,Int)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CExprAnn 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                    UID ->
                    CVarIntroMp ->
                    Int ->
                    HsName ->
                    ( CMetaBind ,FvS,UID,LevBindSq,Int,CMetaBind )
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaBind 
              _lhsOself :: CMetaBind 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
         levOf                : Int
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
type T_CMetaVal  = CVarIntroMp ->
                   UID ->
                   CVarIntroMp ->
                   Int ->
                   HsName ->
                   ( CMetaVal ,FvS,UID,LevBindSq,Int,CMetaVal )
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
              _lhsOcTrf =
                  _cTrf
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOgUniq =
                  _lhsIgUniq
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetaVal 
              _lhsOself :: CMetaVal 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
         levOf                : Int
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
type T_CMetas  = CVarIntroMp ->
                 UID ->
                 CVarIntroMp ->
                 Int ->
                 HsName ->
                 ( CMetas ,FvS,UID,LevBindSq,Int,CMetas )
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CMetas 
              _lhsOself :: CMetas 
              _lhsOgUniq :: UID
              _x1OcvarIntroMp :: CVarIntroMp
              _x1OgUniq :: UID
              _x1OintroCVarIntroMp :: CVarIntroMp
              _x1Olev :: Int
              _x1OmodNm :: HsName
              _x2OcvarIntroMp :: CVarIntroMp
              _x2OgUniq :: UID
              _x2OintroCVarIntroMp :: CVarIntroMp
              _x2Olev :: Int
              _x2OmodNm :: HsName
              _x1IcTrf :: CMetaBind 
              _x1IfvS :: FvS
              _x1IgUniq :: UID
              _x1IlevBindSq :: LevBindSq
              _x1IlevOf :: Int
              _x1Iself :: CMetaBind 
              _x2IcTrf :: CMetaVal 
              _x2IfvS :: FvS
              _x2IgUniq :: UID
              _x2IlevBindSq :: LevBindSq
              _x2IlevOf :: Int
              _x2Iself :: CMetaVal 
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _x1IfvS `Set.union` _x2IfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _x1IlevBindSq Seq.:++: _x2IlevBindSq
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
              _x1Olev =
                  _lhsIlev
              -- copy rule (down)
              _x1OmodNm =
                  _lhsImodNm
              -- copy rule (down)
              _x2OcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (chain)
              _x2OgUniq =
                  _x1IgUniq
              -- copy rule (down)
              _x2OintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _x2Olev =
                  _lhsIlev
              -- copy rule (down)
              _x2OmodNm =
                  _lhsImodNm
              ( _x1IcTrf,_x1IfvS,_x1IgUniq,_x1IlevBindSq,_x1IlevOf,_x1Iself) =
                  x1_ _x1OcvarIntroMp _x1OgUniq _x1OintroCVarIntroMp _x1Olev _x1OmodNm 
              ( _x2IcTrf,_x2IfvS,_x2IgUniq,_x2IlevBindSq,_x2IlevOf,_x2Iself) =
                  x2_ _x2OcvarIntroMp _x2OgUniq _x2OintroCVarIntroMp _x2Olev _x2OmodNm 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOself)))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
type T_CModule  = CVarIntroMp ->
                  UID ->
                  CVarIntroMp ->
                  Int ->
                  HsName ->
                  ( CModule ,FvS,UID,LevBindSq,Int)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _exprOappTrfIsOk :: AppTop
              _exprOlamTrfIsOk :: LamTop
              _exprOletTrfIsOk :: Bool
              _exprOisTopApp :: Bool
              _exprOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _exprOisStrict :: Bool
              _exprOisLamBody :: Bool
              _exprOevalCtx :: EvalCtx
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CModule 
              _lhsOgUniq :: UID
              _exprOcvarIntroMp :: CVarIntroMp
              _exprOgUniq :: UID
              _exprOintroCVarIntroMp :: CVarIntroMp
              _exprOlev :: Int
              _exprOmodNm :: HsName
              _exprOwhatAbove :: WhatExpr
              _exprIcTrf :: CExpr 
              _exprIfvS :: FvS
              _exprIgUniq :: UID
              _exprIlevBindSq :: LevBindSq
              _exprIlevOf :: Int
              _exprIwhatBelow :: WhatExpr
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 111, column 17)
              _exprOappTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 139, column 17)
              _exprOlamTrfIsOk =
                  LamYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 165, column 17)
              _exprOletTrfIsOk =
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _exprIlevBindSq
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
              _exprOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _exprOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _exprOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _exprOlev =
                  _lhsIlev
              -- copy rule (down)
              _exprOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _exprOwhatAbove =
                  _whatAbove
              ( _exprIcTrf,_exprIfvS,_exprIgUniq,_exprIlevBindSq,_exprIlevOf,_exprIwhatBelow) =
                  expr_ _exprOappTrfIsOk _exprOcvarIntroMp _exprOevalCtx _exprOgUniq _exprOintroCVarIntroMp _exprOisLamBody _exprOisStrict _exprOisTopApp _exprOisTopTup _exprOlamTrfIsOk _exprOletTrfIsOk _exprOlev _exprOmodNm _exprOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levBindSq            : LevBindSq
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
               UID ->
               CVarIntroMp ->
               Int ->
               HsName ->
               ( CPat ,([HsName]),FvS,UID,LevBindSq,Int,([HsName]))
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: UID
              _restOcvarIntroMp :: CVarIntroMp
              _restOgUniq :: UID
              _restOintroCVarIntroMp :: CVarIntroMp
              _restOlev :: Int
              _restOmodNm :: HsName
              _bindsOcvarIntroMp :: CVarIntroMp
              _bindsOgUniq :: UID
              _bindsOintroCVarIntroMp :: CVarIntroMp
              _bindsOlev :: Int
              _bindsOmodNm :: HsName
              _restIcTrf :: CPatRest 
              _restIfvS :: FvS
              _restIgUniq :: UID
              _restIlevBindSq :: LevBindSq
              _restIlevOf :: Int
              _restInmL :: ([HsName])
              _bindsIcTrf :: CPatFldL 
              _bindsIfldNmL :: ([HsName])
              _bindsIfvS :: FvS
              _bindsIgUniq :: UID
              _bindsIlevBindSq :: LevBindSq
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _restIlevBindSq Seq.:++: _bindsIlevBindSq
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
              _restOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _restOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _restOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _restOlev =
                  _lhsIlev
              -- copy rule (down)
              _restOmodNm =
                  _lhsImodNm
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
              _bindsOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindsOmodNm =
                  _lhsImodNm
              ( _restIcTrf,_restIfvS,_restIgUniq,_restIlevBindSq,_restIlevOf,_restInmL) =
                  rest_ _restOcvarIntroMp _restOgUniq _restOintroCVarIntroMp _restOlev _restOmodNm 
              ( _bindsIcTrf,_bindsIfldNmL,_bindsIfvS,_bindsIgUniq,_bindsIlevBindSq,_bindsIlevOf,_bindsInmL) =
                  binds_ _bindsOcvarIntroMp _bindsOgUniq _bindsOintroCVarIntroMp _bindsOlev _bindsOmodNm 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPat 
              _lhsOgUniq :: UID
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 28, column 17)
              _lhsOnmL =
                  [pnm_]
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levBindSq            : LevBindSq
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
type T_CPatFld  = CVarIntroMp ->
                  UID ->
                  CVarIntroMp ->
                  Int ->
                  HsName ->
                  ( CPatFld ,([HsName]),FvS,UID,LevBindSq,Int,([HsName]))
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _offsetOappTrfIsOk :: AppTop
              _offsetOlamTrfIsOk :: LamTop
              _offsetOletTrfIsOk :: Bool
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
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatFld 
              _lhsOgUniq :: UID
              _offsetOcvarIntroMp :: CVarIntroMp
              _offsetOgUniq :: UID
              _offsetOintroCVarIntroMp :: CVarIntroMp
              _offsetOlev :: Int
              _offsetOmodNm :: HsName
              _offsetOwhatAbove :: WhatExpr
              _bindOcvarIntroMp :: CVarIntroMp
              _bindOgUniq :: UID
              _bindOintroCVarIntroMp :: CVarIntroMp
              _bindOlev :: Int
              _bindOmodNm :: HsName
              _fldAnnsOcvarIntroMp :: CVarIntroMp
              _fldAnnsOgUniq :: UID
              _fldAnnsOintroCVarIntroMp :: CVarIntroMp
              _fldAnnsOlev :: Int
              _fldAnnsOmodNm :: HsName
              _offsetIcTrf :: CExpr 
              _offsetIfvS :: FvS
              _offsetIgUniq :: UID
              _offsetIlevBindSq :: LevBindSq
              _offsetIlevOf :: Int
              _offsetIwhatBelow :: WhatExpr
              _bindIbindL :: (AssocL Int CBind)
              _bindIbindsIntroCVarIntroMp :: CVarIntroMp
              _bindIcTrf :: CBind 
              _bindIcvarIntroExprMp :: CVarIntroMp
              _bindIfvS :: FvS
              _bindIfvSMp :: FvSMp
              _bindIgUniq :: UID
              _bindIlevBindSq :: LevBindSq
              _bindIlevOf :: Int
              _bindInm :: HsName
              _bindInmL :: ([HsName])
              _fldAnnsIcTrf :: CBindAnnL 
              _fldAnnsIfvS :: FvS
              _fldAnnsIgUniq :: UID
              _fldAnnsIlevBindSq :: LevBindSq
              _fldAnnsIlevOf :: Int
              _fldAnnsInmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 119, column 17)
              _offsetOappTrfIsOk =
                  AppYesLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 147, column 17)
              _offsetOlamTrfIsOk =
                  LamNoLet
              -- "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 173, column 17)
              _offsetOletTrfIsOk =
                  True
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _offsetIlevBindSq Seq.:++: _bindIlevBindSq Seq.:++: _fldAnnsIlevBindSq
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
              _offsetOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _offsetOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _offsetOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _offsetOlev =
                  _lhsIlev
              -- copy rule (down)
              _offsetOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _offsetOwhatAbove =
                  _whatAbove
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
              _bindOlev =
                  _lhsIlev
              -- copy rule (down)
              _bindOmodNm =
                  _lhsImodNm
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
              _fldAnnsOlev =
                  _lhsIlev
              -- copy rule (down)
              _fldAnnsOmodNm =
                  _lhsImodNm
              ( _offsetIcTrf,_offsetIfvS,_offsetIgUniq,_offsetIlevBindSq,_offsetIlevOf,_offsetIwhatBelow) =
                  offset_ _offsetOappTrfIsOk _offsetOcvarIntroMp _offsetOevalCtx _offsetOgUniq _offsetOintroCVarIntroMp _offsetOisLamBody _offsetOisStrict _offsetOisTopApp _offsetOisTopTup _offsetOlamTrfIsOk _offsetOletTrfIsOk _offsetOlev _offsetOmodNm _offsetOwhatAbove 
              ( _bindIbindL,_bindIbindsIntroCVarIntroMp,_bindIcTrf,_bindIcvarIntroExprMp,_bindIfvS,_bindIfvSMp,_bindIgUniq,_bindIlevBindSq,_bindIlevOf,_bindInm,_bindInmL) =
                  bind_ _bindOcvarIntroMp _bindOevalCtx _bindOgUniq _bindOintroCVarIntroMp _bindOisGlobal _bindOisLamBody _bindOisStrict _bindOletBindingsCateg _bindOlev _bindOmodNm 
              ( _fldAnnsIcTrf,_fldAnnsIfvS,_fldAnnsIgUniq,_fldAnnsIlevBindSq,_fldAnnsIlevOf,_fldAnnsInmL) =
                  fldAnns_ _fldAnnsOcvarIntroMp _fldAnnsOgUniq _fldAnnsOintroCVarIntroMp _fldAnnsOlev _fldAnnsOmodNm 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fldNmL               : [HsName]
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                   UID ->
                   CVarIntroMp ->
                   Int ->
                   HsName ->
                   ( CPatFldL ,([HsName]),FvS,UID,LevBindSq,Int,([HsName]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOgUniq :: UID
              _hdOcvarIntroMp :: CVarIntroMp
              _hdOgUniq :: UID
              _hdOintroCVarIntroMp :: CVarIntroMp
              _hdOlev :: Int
              _hdOmodNm :: HsName
              _tlOcvarIntroMp :: CVarIntroMp
              _tlOgUniq :: UID
              _tlOintroCVarIntroMp :: CVarIntroMp
              _tlOlev :: Int
              _tlOmodNm :: HsName
              _hdIcTrf :: CPatFld 
              _hdIfldNmL :: ([HsName])
              _hdIfvS :: FvS
              _hdIgUniq :: UID
              _hdIlevBindSq :: LevBindSq
              _hdIlevOf :: Int
              _hdInmL :: ([HsName])
              _tlIcTrf :: CPatFldL 
              _tlIfldNmL :: ([HsName])
              _tlIfvS :: FvS
              _tlIgUniq :: UID
              _tlIlevBindSq :: LevBindSq
              _tlIlevOf :: Int
              _tlInmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  _hdIfldNmL ++ _tlIfldNmL
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  _hdIfvS `Set.union` _tlIfvS
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _hdIlevBindSq Seq.:++: _tlIlevBindSq
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
              _hdOcvarIntroMp =
                  _lhsIcvarIntroMp
              -- copy rule (down)
              _hdOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _hdOintroCVarIntroMp =
                  _lhsIintroCVarIntroMp
              -- copy rule (down)
              _hdOlev =
                  _lhsIlev
              -- copy rule (down)
              _hdOmodNm =
                  _lhsImodNm
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
              _tlOlev =
                  _lhsIlev
              -- copy rule (down)
              _tlOmodNm =
                  _lhsImodNm
              ( _hdIcTrf,_hdIfldNmL,_hdIfvS,_hdIgUniq,_hdIlevBindSq,_hdIlevOf,_hdInmL) =
                  hd_ _hdOcvarIntroMp _hdOgUniq _hdOintroCVarIntroMp _hdOlev _hdOmodNm 
              ( _tlIcTrf,_tlIfldNmL,_tlIfvS,_tlIgUniq,_tlIlevBindSq,_tlIlevOf,_tlInmL) =
                  tl_ _tlOcvarIntroMp _tlOgUniq _tlOintroCVarIntroMp _tlOlev _tlOmodNm 
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfldNmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatFldL 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/CommonBindNm.ag"(line 25, column 41)
              _lhsOfldNmL =
                  []
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfldNmL,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         cvarIntroMp          : CVarIntroMp
         introCVarIntroMp     : CVarIntroMp
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
                   UID ->
                   CVarIntroMp ->
                   Int ->
                   HsName ->
                   ( CPatRest ,FvS,UID,LevBindSq,Int,([HsName]))
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOnmL :: ([HsName])
              _lhsOcTrf :: CPatRest 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIcvarIntroMp
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOnmL :: ([HsName])
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: CPatRest 
              _lhsOgUniq :: UID
              -- "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 25, column 17)
              _lhsOnmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf,_lhsOnmL)))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         gUniq                : UID
         modNm                : HsName
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
type T_CodeAGItf  = UID ->
                    HsName ->
                    ( CModule )
data Inh_CodeAGItf  = Inh_CodeAGItf {gUniq_Inh_CodeAGItf :: !(UID),modNm_Inh_CodeAGItf :: !(HsName)}
data Syn_CodeAGItf  = Syn_CodeAGItf {cTrf_Syn_CodeAGItf :: !(CModule )}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIgUniq _lhsImodNm )  =
    (let ( _lhsOcTrf) = sem _lhsIgUniq _lhsImodNm 
     in  (Syn_CodeAGItf _lhsOcTrf ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIgUniq
       _lhsImodNm ->
         (let _moduleOlev :: Int
              _moduleOcvarIntroMp :: CVarIntroMp
              _moduleOintroCVarIntroMp :: CVarIntroMp
              _lhsOcTrf :: CModule 
              _moduleOgUniq :: UID
              _moduleOmodNm :: HsName
              _moduleIcTrf :: CModule 
              _moduleIfvS :: FvS
              _moduleIgUniq :: UID
              _moduleIlevBindSq :: LevBindSq
              _moduleIlevOf :: Int
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
              _moduleOgUniq =
                  _lhsIgUniq
              -- copy rule (down)
              _moduleOmodNm =
                  _lhsImodNm
              ( _moduleIcTrf,_moduleIfvS,_moduleIgUniq,_moduleIlevBindSq,_moduleIlevOf) =
                  module_ _moduleOcvarIntroMp _moduleOgUniq _moduleOintroCVarIntroMp _moduleOlev _moduleOmodNm 
          in  ( _lhsOcTrf)))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         appTrfIsOk           : AppTop
         cvarIntroMp          : CVarIntroMp
         evalCtx              : EvalCtx
         introCVarIntroMp     : CVarIntroMp
         isLamBody            : Bool
         isStrict             : Bool
         lamTrfIsOk           : LamTop
         letTrfIsOk           : Bool
         lev                  : Int
         modNm                : HsName
      chained attribute:
         gUniq                : UID
      synthesized attributes:
         cTrf                 : SELF 
         fvS                  : FvS
         levBindSq            : LevBindSq
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
type T_MbCExpr  = AppTop ->
                  CVarIntroMp ->
                  EvalCtx ->
                  UID ->
                  CVarIntroMp ->
                  Bool ->
                  Bool ->
                  LamTop ->
                  Bool ->
                  Int ->
                  HsName ->
                  ( MbCExpr ,FvS,UID,LevBindSq,Int)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm ->
         (let _justOisTopApp :: Bool
              _justOisTopTup :: Bool
              _whatAbove :: WhatExpr
              _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              _lhsOgUniq :: UID
              _justOappTrfIsOk :: AppTop
              _justOcvarIntroMp :: CVarIntroMp
              _justOevalCtx :: EvalCtx
              _justOgUniq :: UID
              _justOintroCVarIntroMp :: CVarIntroMp
              _justOisLamBody :: Bool
              _justOisStrict :: Bool
              _justOlamTrfIsOk :: LamTop
              _justOletTrfIsOk :: Bool
              _justOlev :: Int
              _justOmodNm :: HsName
              _justOwhatAbove :: WhatExpr
              _justIcTrf :: CExpr 
              _justIfvS :: FvS
              _justIgUniq :: UID
              _justIlevBindSq :: LevBindSq
              _justIlevOf :: Int
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
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  _justIlevBindSq
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
              _justOappTrfIsOk =
                  _lhsIappTrfIsOk
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
              _justOisLamBody =
                  _lhsIisLamBody
              -- copy rule (down)
              _justOisStrict =
                  _lhsIisStrict
              -- copy rule (down)
              _justOlamTrfIsOk =
                  _lhsIlamTrfIsOk
              -- copy rule (down)
              _justOletTrfIsOk =
                  _lhsIletTrfIsOk
              -- copy rule (down)
              _justOlev =
                  _lhsIlev
              -- copy rule (down)
              _justOmodNm =
                  _lhsImodNm
              -- copy rule (from local)
              _justOwhatAbove =
                  _whatAbove
              ( _justIcTrf,_justIfvS,_justIgUniq,_justIlevBindSq,_justIlevOf,_justIwhatBelow) =
                  just_ _justOappTrfIsOk _justOcvarIntroMp _justOevalCtx _justOgUniq _justOintroCVarIntroMp _justOisLamBody _justOisStrict _justOisTopApp _justOisTopTup _justOlamTrfIsOk _justOletTrfIsOk _justOlev _justOmodNm _justOwhatAbove 
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIappTrfIsOk
       _lhsIcvarIntroMp
       _lhsIevalCtx
       _lhsIgUniq
       _lhsIintroCVarIntroMp
       _lhsIisLamBody
       _lhsIisStrict
       _lhsIlamTrfIsOk
       _lhsIletTrfIsOk
       _lhsIlev
       _lhsImodNm ->
         (let _lhsOfvS :: FvS
              _lhsOlevBindSq :: LevBindSq
              _lhsOlevOf :: Int
              _lhsOcTrf :: MbCExpr 
              _lhsOgUniq :: UID
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/CommonFv.ag"(line 1, column 26)
              _lhsOfvS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/Core/Trf/ANormal.ag"(line 69, column 32)
              _lhsOlevBindSq =
                  Seq.empty
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
          in  ( _lhsOcTrf,_lhsOfvS,_lhsOgUniq,_lhsOlevBindSq,_lhsOlevOf)))