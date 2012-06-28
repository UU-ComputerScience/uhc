

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag)
module EH101.GrinCode.ToGrinByteCode(grinMod2ByteCodeMod) where

import Data.Maybe
import Data.List
import EH.Util.Pretty
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.Base.Common
import EH101.Opts
import EH101.Base.Builtin
import qualified EH101.Config as Cfg
import EH101.Error
import EH101.BuiltinPrims
import EH101.GrinCode.Pretty
import qualified EH101.GrinByteCode as GB
import EH101.Core
import EH101.GrinCode
import EH101.LamInfo
import EH101.Foreign.Extract
import EH101.Base.Debug
import EH101.Ty(tyRowCanonOrder)
import EH101.HI





















grinMod2ByteCodeMod :: EHCOpts -> LamMp -> [HsName] -> {- [HsName] -> -} HsName2OffsetMpMp -> HsName2OffsetMp -> GrModule -> (GB.Module,[Err])
grinMod2ByteCodeMod opts lamMp allImpNmL {- impNmL -} impNmOffMpMp expNmOffMp gmod
  =  let  t = wrap_GrAGItf  (sem_GrAGItf (GrAGItf_AGItf gmod))
                            (Inh_GrAGItf
                               { expNmOffMp_Inh_GrAGItf     = expNmOffMp
                               , impNmOffMpMp_Inh_GrAGItf   = impNmOffMpMp
                               , allImpNmL_Inh_GrAGItf      = allImpNmL
                               -- , impNmL_Inh_GrAGItf         = offMpKeysSorted impNmOffMpMp -- impNmL
                               , opts_Inh_GrAGItf           = opts
                               , lamMp_Inh_GrAGItf          = lamMp
                               })
     in   (gbMod_Syn_GrAGItf t, errs_Syn_GrAGItf t)



type ConstMp = Map.Map GB.Const Int



constAdd :: GB.Const -> ConstMp -> (Int,ConstMp)
constAdd c constMp
  = case Map.lookup c constMp of
      Just i -> (i,constMp)
      _      -> (constInx,Map.insert c constInx constMp)
             where constInx = Map.size constMp

constCFunAdd :: String -> ConstMp -> (Int,ConstMp)
constCFunAdd nm = constAdd (GB.Const_CFunction nm)

constCCallEncWrapper :: [BasicSize] -> ConstMp -> (Int,ConstMp)
constCCallEncWrapper szs = constAdd (GB.Const_CCallEncWrapper szs)



data AltFetch
  = AltFetch_Many   [HsName]                -- introduced names
  | AltFetch_One    HsName Int              -- name, field offset in node (excluding header)
  | AltFetch_Zero
  deriving Eq



data ReturnCtxt
  = ReturnCtxt_Returns      Int                         -- return from function (nr of arguments)
  | ReturnCtxt_Continues                                -- don't return, leave result on stack
  | ReturnCtxt_CaseReturns  GB.LabelId GB.StackState    -- return from case alt (return destination, return stack depth)
  | ReturnCtxt_CaseFallThrough                          -- return from case with only 1 alt, so don't clean up stack



data NmIntro
  = NmIntro_Single    HsName BasicAnnot         -- name + size in bytes
  | NmIntro_Grp       GrTag [HsName]
  | NmIntro_GrpTag    HsName
  | NmIntro_GrpBasic  HsName BasicAnnot         -- name + size in bytes, corresponding to basic/boxed values
  | NmIntro_None



nmIntroBasicAnnot :: NmIntro -> BasicAnnot
nmIntroBasicAnnot (NmIntro_Single   _ a) = a
nmIntroBasicAnnot (NmIntro_GrpBasic _ a) = a
nmIntroBasicAnnot _                      = BasicAnnot_Dflt



patNmL2DepL :: [HsName] -> AssocL HsName Int
patNmL2DepL nmL = zip (reverse nmL) [0..]

patNmL2DepL2 :: [HsName] -> AssocL HsName Int
patNmL2DepL2 nmL = zip nmL [0, (- GB.nrValWords) ..]

patNmL2VAGam' :: GB.StackDepth -> AssocL HsName Int -> GB.ValAccessGam
patNmL2VAGam' dep nmDepL = Map.fromList [ (n,GB.Val_Local (dep+d) (GB.ValAccessAnnot_Annot basicAnnotWord)) | (n,d) <- nmDepL ]

patNmL2VAGam :: GB.StackDepth -> [HsName] -> GB.ValAccessGam
patNmL2VAGam dep nmL = patNmL2VAGam' dep $ patNmL2DepL nmL



data UnitIntro
  = UnitIntro   GB.GrValIntro





mkRetLabel :: ReturnCtxt -> GB.GBState -> (GB.GBState,GB.LabelId)
mkRetLabel returnCtxt st
  = case returnCtxt of
      ReturnCtxt_CaseReturns _ _
        -> GB.newLabelId st
      _ -> (st,GB.gbstLbl st)

mkRet :: ReturnCtxt -> GB.LabelId -> GB.StackState -> GB.StackState -> (GB.InsSeq,GB.StackState)
mkRet returnCtxt retLocLbl stState incSt
  = case returnCtxt of
      ReturnCtxt_Returns nrArgsOuter
        -> (Seq.fromList [GB.retcall GB.nrValWords nrArgsOuter],dfltState)
      ReturnCtxt_CaseReturns labelId st
        -> ( Seq.fromList
               [ GB.meta' GB.AnnStackDepth stState
               , GB.retcase GB.nrValWords (stkDepth + inc - dep) (GB.linkChainOffset retLocLbl labelId)
               , GB.meta' GB.AnnStackDepth (dep + incret)
               ]
           , GB.ststFromDep incret
           )
        where incret = GB.nrValWords
              dep = GB.ststDepth st
      ReturnCtxt_CaseFallThrough
        -> (Seq.empty,dfltState)
      _ -> (Seq.empty,dfltState)
  where dfltState = GB.ststFromDep inc
        stkDepth  = GB.ststDepth stState
        inc       = GB.ststDepth incSt

-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allImpNmL            : [HsName]
         expNmOffMp           : HsName2OffsetMp
         impNmOffMpMp         : HsName2OffsetMpMp
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         errs                 : [Err]
         gbMod                : GB.Module
   alternatives:
      alternative AGItf:
         child module         : GrModule 
-}
-- cata
sem_GrAGItf :: GrAGItf  ->
               T_GrAGItf 
sem_GrAGItf (GrAGItf_AGItf _module )  =
    (sem_GrAGItf_AGItf (sem_GrModule _module ) )
-- semantic domain
type T_GrAGItf  = ([HsName]) ->
                  HsName2OffsetMp ->
                  HsName2OffsetMpMp ->
                  LamMp ->
                  EHCOpts ->
                  ( ([Err]),(GB.Module))
data Inh_GrAGItf  = Inh_GrAGItf {allImpNmL_Inh_GrAGItf :: !(([HsName])),expNmOffMp_Inh_GrAGItf :: !(HsName2OffsetMp),impNmOffMpMp_Inh_GrAGItf :: !(HsName2OffsetMpMp),lamMp_Inh_GrAGItf :: !(LamMp),opts_Inh_GrAGItf :: !(EHCOpts)}
data Syn_GrAGItf  = Syn_GrAGItf {errs_Syn_GrAGItf :: !(([Err])),gbMod_Syn_GrAGItf :: !((GB.Module))}
wrap_GrAGItf :: T_GrAGItf  ->
                Inh_GrAGItf  ->
                Syn_GrAGItf 
wrap_GrAGItf sem (Inh_GrAGItf _lhsIallImpNmL _lhsIexpNmOffMp _lhsIimpNmOffMpMp _lhsIlamMp _lhsIopts )  =
    (let ( _lhsOerrs,_lhsOgbMod) = sem _lhsIallImpNmL _lhsIexpNmOffMp _lhsIimpNmOffMpMp _lhsIlamMp _lhsIopts 
     in  (Syn_GrAGItf _lhsOerrs _lhsOgbMod ))
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    (\ _lhsIallImpNmL
       _lhsIexpNmOffMp
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts ->
         (let _lhsOerrs :: ([Err])
              _lhsOgbMod :: (GB.Module)
              _moduleOallImpNmL :: ([HsName])
              _moduleOexpNmOffMp :: HsName2OffsetMp
              _moduleOimpNmOffMpMp :: HsName2OffsetMpMp
              _moduleOlamMp :: LamMp
              _moduleOopts :: EHCOpts
              _moduleIerrs :: ([Err])
              _moduleIgbMod :: (GB.Module)
              -- copy rule (up)
              _lhsOerrs =
                  _moduleIerrs
              -- copy rule (up)
              _lhsOgbMod =
                  _moduleIgbMod
              -- copy rule (down)
              _moduleOallImpNmL =
                  _lhsIallImpNmL
              -- copy rule (down)
              _moduleOexpNmOffMp =
                  _lhsIexpNmOffMp
              -- copy rule (down)
              _moduleOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _moduleOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _moduleOopts =
                  _lhsIopts
              ( _moduleIerrs,_moduleIgbMod) =
                  module_ _moduleOallImpNmL _moduleOexpNmOffMp _moduleOimpNmOffMpMp _moduleOlamMp _moduleOopts 
          in  ( _lhsOerrs,_lhsOgbMod)))
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Del:
         child off            : GrVal 
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
-}
-- cata
sem_GrAdapt :: GrAdapt  ->
               T_GrAdapt 
sem_GrAdapt (GrAdapt_Del _off )  =
    (sem_GrAdapt_Del (sem_GrVal _off ) )
sem_GrAdapt (GrAdapt_Ins _off _val )  =
    (sem_GrAdapt_Ins (sem_GrVal _off ) (sem_GrVal _val ) )
sem_GrAdapt (GrAdapt_Upd _off _val )  =
    (sem_GrAdapt_Upd (sem_GrVal _off ) (sem_GrVal _val ) )
-- semantic domain
type T_GrAdapt  = ConstMp ->
                  ( ConstMp,FvInfoMp)
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _offOconstMp :: ConstMp
              _offIconstMp :: ConstMp
              _offIgathFviMp :: FvInfoMp
              _offIgrvalIntro :: (GB.GrValIntro)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _offIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _offIconstMp
              -- copy rule (down)
              _offOconstMp =
                  _lhsIconstMp
              ( _offIconstMp,_offIgathFviMp,_offIgrvalIntro) =
                  off_ _offOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _offOconstMp :: ConstMp
              _valOconstMp :: ConstMp
              _offIconstMp :: ConstMp
              _offIgathFviMp :: FvInfoMp
              _offIgrvalIntro :: (GB.GrValIntro)
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _offIgathFviMp `fviMpUnion` _valIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _valIconstMp
              -- copy rule (down)
              _offOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _valOconstMp =
                  _offIconstMp
              ( _offIconstMp,_offIgathFviMp,_offIgrvalIntro) =
                  off_ _offOconstMp 
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _offOconstMp :: ConstMp
              _valOconstMp :: ConstMp
              _offIconstMp :: ConstMp
              _offIgathFviMp :: FvInfoMp
              _offIgrvalIntro :: (GB.GrValIntro)
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _offIgathFviMp `fviMpUnion` _valIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _valIconstMp
              -- copy rule (down)
              _offOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _valOconstMp =
                  _offIconstMp
              ( _offIconstMp,_offIgathFviMp,_offIgrvalIntro) =
                  off_ _offOconstMp 
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
      alternative Nil:
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = ConstMp ->
                   ( ConstMp,FvInfoMp)
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         impNmOffMpMp         : HsName2OffsetMpMp
         isEnum               : Bool
         lamMp                : LamMp
         nrOfAlts             : Int
         opts                 : EHCOpts
         returnCtxt           : ReturnCtxt
         vaGam                : GB.ValAccessGam
      chained attributes:
         constMp              : ConstMp
         gbState              : GB.GBState
         stState              : GB.StackState
      synthesized attributes:
         altLocRefs           : [GB.LocRef]
         gathFviMp            : FvInfoMp
         gathNrOfAlts         : Int
         i                    : GB.InsSeq
         includeS             : Set.Set String
         isAllEnum            : Bool
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
         visit 0:
            local _tup1       : _
            local altLabel    : _
            local gathNrOfAlts : _
            local altLocRef   : _
            local altLocRefs  : _
            local _tup2       : _
            local altFetch    : _
            local nrOfFlds    : _
            local fetchStState : _
            local exprStState : _
            local bodyStState : _
            local fetchDepth  : _
            local exprStkDepth : _
            local bodyStkDepth : _
            local newVaGam    : _
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = ConstMp ->
                (GB.GBState) ->
                HsName2OffsetMpMp ->
                Bool ->
                LamMp ->
                Int ->
                EHCOpts ->
                ReturnCtxt ->
                (GB.StackState) ->
                (GB.ValAccessGam) ->
                ( ([GB.LocRef]),ConstMp,FvInfoMp,Int,(GB.GBState),(GB.InsSeq),(Set.Set String),Bool,(GB.StackState))
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisEnum
       _lhsIlamMp
       _lhsInrOfAlts
       _lhsIopts
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _exprOgbState :: (GB.GBState)
              _exprOstState :: (GB.StackState)
              _lhsOstState :: (GB.StackState)
              _lhsOisAllEnum :: Bool
              _exprOpatBasicAnnot :: BasicAnnot
              _exprOvaGam :: (GB.ValAccessGam)
              _exprOisSeqArgCtxt :: Bool
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOaltLocRefs :: ([GB.LocRef])
              _lhsOgathNrOfAlts :: Int
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _patOconstMp :: ConstMp
              _exprOconstMp :: ConstMp
              _exprOimpNmOffMpMp :: HsName2OffsetMpMp
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprOreturnCtxt :: ReturnCtxt
              _patIconstMp :: ConstMp
              _patIgathFviMp :: FvInfoMp
              _patIintroNmL :: ([HsName])
              _patItag :: Int
              _exprIconstMp :: ConstMp
              _exprIgathFviMp :: FvInfoMp
              _exprIgbState :: (GB.GBState)
              _exprIi :: (GB.InsSeq)
              _exprIincludeS :: (Set.Set String)
              _exprImbUnitIntro :: (Maybe UnitIntro)
              _exprIprimNrArgForIntl :: Int
              _exprIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 135, column 17)
              __tup1 =
                  GB.newLabelId _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 135, column 17)
              (_exprOgbState,_) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 135, column 17)
              (_,_altLabel) =
                  __tup1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 163, column 17)
              _gathNrOfAlts =
                  1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 168, column 17)
              _altLocRef =
                  if _lhsInrOfAlts < 2 then GB.LocRef_Label _altLabel else GB.LocRef_CaseArm _altLabel _patItag
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 169, column 17)
              _altLocRefs =
                  [_altLocRef]
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 186, column 33)
              __tup2 =
                  let nmOffs = zip _patIintroNmL [0..]
                  in  ( case [ x | x@(n,o) <- nmOffs, n `Map.member` _exprIgathFviMp ] of
                          []      -> AltFetch_Zero
                          [(n,o)] -> AltFetch_One n o
                          _       -> AltFetch_Many _patIintroNmL
                      , length nmOffs
                      )
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 186, column 33)
              (_altFetch,_) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 186, column 33)
              (_,_nrOfFlds) =
                  __tup2
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 216, column 17)
              _fetchStState =
                  GB.ststFromDep _fetchDepth
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 216, column 17)
              _exprStState =
                  _lhsIstState `GB.ststInc` _fetchStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 218, column 17)
              _exprOstState =
                  _exprStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 219, column 17)
              _bodyStState =
                  case _lhsIreturnCtxt of
                    ReturnCtxt_CaseFallThrough
                      -> _exprIstState
                    _ -> _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 223, column 17)
              _lhsOstState =
                  _bodyStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 238, column 17)
              _fetchDepth =
                  case _altFetch of
                    AltFetch_Many ns   -> (length ns - 1) * GB.nrValWords
                    AltFetch_One  _ _  -> GB.nrValWords
                    AltFetch_Zero      -> 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 238, column 17)
              _exprStkDepth =
                  GB.ststDepth _exprStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 243, column 17)
              _bodyStkDepth =
                  GB.ststDepth _bodyStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 278, column 17)
              _lhsOisAllEnum =
                  _nrOfFlds == 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 325, column 17)
              _exprOpatBasicAnnot =
                  BasicAnnot_Dflt
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 357, column 17)
              _newVaGam =
                  case _altFetch of
                    AltFetch_Many ns  -> patNmL2VAGam' _exprStkDepth $ patNmL2DepL2 ns
                    AltFetch_One  n _ -> patNmL2VAGam' _exprStkDepth $ patNmL2DepL2 [n]
                    _                 -> Map.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 361, column 17)
              _exprOvaGam =
                  _newVaGam `Map.union` _lhsIvaGam
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 423, column 17)
              _exprOisSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 699, column 17)
              _lhsOi =
                  let fetch = case _altFetch of
                                AltFetch_Many _  -> Seq.fromList [GB.fetch GB.InsOp_LocB_TOS]
                                AltFetch_One _ o -> Seq.fromList [GB.l2ts (o + GB.nrNodeHdrWords)]
                                AltFetch_Zero    -> Seq.empty
                  in  Seq.fromList [GB.meta' GB.AnnStackDepth _lhsIstState,GB.label _altLocRef]
                      Seq.:++: fetch
                      Seq.:++: Seq.fromList [GB.meta' GB.AnnStackDepth _exprStState]
                      Seq.:++: _exprIi
                      Seq.:++: Seq.fromList [GB.meta' GB.AnnStackDepth _bodyStState]
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 26, column 17)
              _lhsOgathFviMp =
                  _exprIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 165, column 30)
              _lhsOaltLocRefs =
                  _altLocRefs
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 159, column 32)
              _lhsOgathNrOfAlts =
                  _gathNrOfAlts
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _exprIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _exprIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _exprIgbState
              -- copy rule (down)
              _patOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _exprOconstMp =
                  _patIconstMp
              -- copy rule (down)
              _exprOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOreturnCtxt =
                  _lhsIreturnCtxt
              ( _patIconstMp,_patIgathFviMp,_patIintroNmL,_patItag) =
                  pat_ _patOconstMp 
              ( _exprIconstMp,_exprIgathFviMp,_exprIgbState,_exprIi,_exprIincludeS,_exprImbUnitIntro,_exprIprimNrArgForIntl,_exprIstState) =
                  expr_ _exprOconstMp _exprOgbState _exprOimpNmOffMpMp _exprOisSeqArgCtxt _exprOlamMp _exprOopts _exprOpatBasicAnnot _exprOreturnCtxt _exprOstState _exprOvaGam 
          in  ( _lhsOaltLocRefs,_lhsOconstMp,_lhsOgathFviMp,_lhsOgathNrOfAlts,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOisAllEnum,_lhsOstState)))
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         impNmOffMpMp         : HsName2OffsetMpMp
         isEnum               : Bool
         lamMp                : LamMp
         nrOfAlts             : Int
         opts                 : EHCOpts
         returnCtxt           : ReturnCtxt
         vaGam                : GB.ValAccessGam
      chained attributes:
         constMp              : ConstMp
         gbState              : GB.GBState
         stState              : GB.StackState
      synthesized attributes:
         altLocRefs           : [GB.LocRef]
         gathFviMp            : FvInfoMp
         gathNrOfAlts         : Int
         i                    : GB.InsSeq
         includeS             : Set.Set String
         isAllEnum            : Bool
         nrAlts               : Int
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
      alternative Nil:
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = ConstMp ->
                 (GB.GBState) ->
                 HsName2OffsetMpMp ->
                 Bool ->
                 LamMp ->
                 Int ->
                 EHCOpts ->
                 ReturnCtxt ->
                 (GB.StackState) ->
                 (GB.ValAccessGam) ->
                 ( ([GB.LocRef]),ConstMp,FvInfoMp,Int,(GB.GBState),(GB.InsSeq),(Set.Set String),Bool,Int,(GB.StackState))
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisEnum
       _lhsIlamMp
       _lhsInrOfAlts
       _lhsIopts
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOnrAlts :: Int
              _lhsOaltLocRefs :: ([GB.LocRef])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathNrOfAlts :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOisAllEnum :: Bool
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _hdOconstMp :: ConstMp
              _hdOgbState :: (GB.GBState)
              _hdOimpNmOffMpMp :: HsName2OffsetMpMp
              _hdOisEnum :: Bool
              _hdOlamMp :: LamMp
              _hdOnrOfAlts :: Int
              _hdOopts :: EHCOpts
              _hdOreturnCtxt :: ReturnCtxt
              _hdOstState :: (GB.StackState)
              _hdOvaGam :: (GB.ValAccessGam)
              _tlOconstMp :: ConstMp
              _tlOgbState :: (GB.GBState)
              _tlOimpNmOffMpMp :: HsName2OffsetMpMp
              _tlOisEnum :: Bool
              _tlOlamMp :: LamMp
              _tlOnrOfAlts :: Int
              _tlOopts :: EHCOpts
              _tlOreturnCtxt :: ReturnCtxt
              _tlOstState :: (GB.StackState)
              _tlOvaGam :: (GB.ValAccessGam)
              _hdIaltLocRefs :: ([GB.LocRef])
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _hdIgathNrOfAlts :: Int
              _hdIgbState :: (GB.GBState)
              _hdIi :: (GB.InsSeq)
              _hdIincludeS :: (Set.Set String)
              _hdIisAllEnum :: Bool
              _hdIstState :: (GB.StackState)
              _tlIaltLocRefs :: ([GB.LocRef])
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              _tlIgathNrOfAlts :: Int
              _tlIgbState :: (GB.GBState)
              _tlIi :: (GB.InsSeq)
              _tlIincludeS :: (Set.Set String)
              _tlIisAllEnum :: Bool
              _tlInrAlts :: Int
              _tlIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 272, column 17)
              _lhsOnrAlts =
                  1 + _tlInrAlts
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 165, column 30)
              _lhsOaltLocRefs =
                  _hdIaltLocRefs ++ _tlIaltLocRefs
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 159, column 32)
              _lhsOgathNrOfAlts =
                  _hdIgathNrOfAlts + _tlIgathNrOfAlts
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  _hdIi Seq.:++: _tlIi
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _hdIincludeS `Set.union` _tlIincludeS
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 275, column 42)
              _lhsOisAllEnum =
                  _hdIisAllEnum && _tlIisAllEnum
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _tlIgbState
              -- copy rule (up)
              _lhsOstState =
                  _tlIstState
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _hdOgbState =
                  _lhsIgbState
              -- copy rule (down)
              _hdOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _hdOisEnum =
                  _lhsIisEnum
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOnrOfAlts =
                  _lhsInrOfAlts
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOreturnCtxt =
                  _lhsIreturnCtxt
              -- copy rule (down)
              _hdOstState =
                  _lhsIstState
              -- copy rule (down)
              _hdOvaGam =
                  _lhsIvaGam
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              -- copy rule (chain)
              _tlOgbState =
                  _hdIgbState
              -- copy rule (down)
              _tlOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _tlOisEnum =
                  _lhsIisEnum
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOnrOfAlts =
                  _lhsInrOfAlts
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOreturnCtxt =
                  _lhsIreturnCtxt
              -- copy rule (chain)
              _tlOstState =
                  _hdIstState
              -- copy rule (down)
              _tlOvaGam =
                  _lhsIvaGam
              ( _hdIaltLocRefs,_hdIconstMp,_hdIgathFviMp,_hdIgathNrOfAlts,_hdIgbState,_hdIi,_hdIincludeS,_hdIisAllEnum,_hdIstState) =
                  hd_ _hdOconstMp _hdOgbState _hdOimpNmOffMpMp _hdOisEnum _hdOlamMp _hdOnrOfAlts _hdOopts _hdOreturnCtxt _hdOstState _hdOvaGam 
              ( _tlIaltLocRefs,_tlIconstMp,_tlIgathFviMp,_tlIgathNrOfAlts,_tlIgbState,_tlIi,_tlIincludeS,_tlIisAllEnum,_tlInrAlts,_tlIstState) =
                  tl_ _tlOconstMp _tlOgbState _tlOimpNmOffMpMp _tlOisEnum _tlOlamMp _tlOnrOfAlts _tlOopts _tlOreturnCtxt _tlOstState _tlOvaGam 
          in  ( _lhsOaltLocRefs,_lhsOconstMp,_lhsOgathFviMp,_lhsOgathNrOfAlts,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOisAllEnum,_lhsOnrAlts,_lhsOstState)))
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisEnum
       _lhsIlamMp
       _lhsInrOfAlts
       _lhsIopts
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOnrAlts :: Int
              _lhsOaltLocRefs :: ([GB.LocRef])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOgathNrOfAlts :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOisAllEnum :: Bool
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 273, column 17)
              _lhsOnrAlts =
                  0
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 165, column 30)
              _lhsOaltLocRefs =
                  []
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 159, column 32)
              _lhsOgathNrOfAlts =
                  0
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 275, column 42)
              _lhsOisAllEnum =
                  True
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOgbState =
                  _lhsIgbState
              -- copy rule (chain)
              _lhsOstState =
                  _lhsIstState
          in  ( _lhsOaltLocRefs,_lhsOconstMp,_lhsOgathFviMp,_lhsOgathNrOfAlts,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOisAllEnum,_lhsOnrAlts,_lhsOstState)))
-- GrBind ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         impNmOffMpMp         : HsName2OffsetMpMp
         lamMp                : LamMp
         opts                 : EHCOpts
         vaGam                : GB.ValAccessGam
      chained attributes:
         constMp              : ConstMp
         gbState              : GB.GBState
      synthesized attributes:
         entryNms             : Seq.FastSeq GB.EntryInfo
         gathFviMp            : FvInfoMp
         i                    : GB.InsSeq
         includeS             : Set.Set String
   alternatives:
      alternative Arity:
         child nm             : {HsName}
         child arity          : {Int}
      alternative Bind:
         child nm             : {HsName}
         child annot          : {GrBindAnn}
         child argNmL         : {[HsName]}
         child expr           : GrExpr 
         visit 0:
            local stState     : _
            local newVaGam    : _
            local nmEnv       : _
            local entryInx    : _
      alternative Rec:
         child bindL          : GrBindL 
-}
-- cata
sem_GrBind :: GrBind  ->
              T_GrBind 
sem_GrBind (GrBind_Arity _nm _arity )  =
    (sem_GrBind_Arity _nm _arity )
sem_GrBind (GrBind_Bind _nm _annot _argNmL _expr )  =
    (sem_GrBind_Bind _nm _annot _argNmL (sem_GrExpr _expr ) )
sem_GrBind (GrBind_Rec _bindL )  =
    (sem_GrBind_Rec (sem_GrBindL _bindL ) )
-- semantic domain
type T_GrBind  = ConstMp ->
                 (GB.GBState) ->
                 HsName2OffsetMpMp ->
                 LamMp ->
                 EHCOpts ->
                 (GB.ValAccessGam) ->
                 ( ConstMp,(Seq.FastSeq GB.EntryInfo),FvInfoMp,(GB.GBState),(GB.InsSeq),(Set.Set String))
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts
       _lhsIvaGam ->
         (let _lhsOentryNms :: (Seq.FastSeq GB.EntryInfo)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 631, column 29)
              _lhsOentryNms =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOgbState =
                  _lhsIgbState
          in  ( _lhsOconstMp,_lhsOentryNms,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS)))
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts
       _lhsIvaGam ->
         (let _exprOgbState :: (GB.GBState)
              _exprOreturnCtxt :: ReturnCtxt
              _exprOpatBasicAnnot :: BasicAnnot
              _exprOvaGam :: (GB.ValAccessGam)
              _exprOisSeqArgCtxt :: Bool
              _lhsOentryNms :: (Seq.FastSeq GB.EntryInfo)
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _exprOconstMp :: ConstMp
              _exprOimpNmOffMpMp :: HsName2OffsetMpMp
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprOstState :: (GB.StackState)
              _exprIconstMp :: ConstMp
              _exprIgathFviMp :: FvInfoMp
              _exprIgbState :: (GB.GBState)
              _exprIi :: (GB.InsSeq)
              _exprIincludeS :: (Set.Set String)
              _exprImbUnitIntro :: (Maybe UnitIntro)
              _exprIprimNrArgForIntl :: Int
              _exprIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 123, column 17)
              _exprOgbState =
                  _lhsIgbState {GB.gbstMaxStkDepth = 0}
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 198, column 17)
              _stState =
                  GB.emptyStackState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 256, column 17)
              _exprOreturnCtxt =
                  ReturnCtxt_Returns (length argNmL_)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 328, column 17)
              _exprOpatBasicAnnot =
                  BasicAnnot_Dflt
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 353, column 17)
              _newVaGam =
                  Map.fromList [ (n,GB.Val_Local (GB.ststDepth _stState - o) (GB.ValAccessAnnot_Annot basicAnnotWord)) | (n,o) <- zip argNmL_ [GB.nrCallRetWords ..] ]
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 354, column 17)
              _exprOvaGam =
                  _newVaGam `Map.union` _lhsIvaGam
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 371, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 420, column 17)
              _exprOisSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 634, column 17)
              _lhsOentryNms =
                  Seq.singleton (GB.EntryInfo nm_ 0 (null argNmL_) (Just $ GB.StringConst $ show nm_))
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 635, column 17)
              _entryInx =
                  GB.vaEntryInx $ panicJust "GrBind.Bind.entryInx" $ GB.nmEnvLookup nm_ _nmEnv
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 680, column 17)
              _lhsOi =
                  Seq.fromList
                     [ GB.funstart (GB.FunctionInfo (show nm_) nm_ (GB.gbstMaxStkDepth _exprIgbState)
                                                    (maybe [] (\i -> if laminfo1stArgIsStackTrace i then [GB.FunctionInfoFlag_1stArgIsStackTrace] else [])
                                                     $ Map.lookup nm_ _lhsIlamMp)
                                                    )
                     , GB.label (GB.LocRef_CodeEntry _entryInx)
                     , GB.meta' GB.AnnFunStart nm_
                     ]
                  Seq.:++: _exprIi
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 29, column 17)
              _lhsOgathFviMp =
                  _exprIgathFviMp `fviMpDifference` fviMpFromList argNmL_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _exprIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _exprIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _exprIgbState
              -- copy rule (down)
              _exprOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _exprOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (from local)
              _exprOstState =
                  _stState
              ( _exprIconstMp,_exprIgathFviMp,_exprIgbState,_exprIi,_exprIincludeS,_exprImbUnitIntro,_exprIprimNrArgForIntl,_exprIstState) =
                  expr_ _exprOconstMp _exprOgbState _exprOimpNmOffMpMp _exprOisSeqArgCtxt _exprOlamMp _exprOopts _exprOpatBasicAnnot _exprOreturnCtxt _exprOstState _exprOvaGam 
          in  ( _lhsOconstMp,_lhsOentryNms,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS)))
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts
       _lhsIvaGam ->
         (let _lhsOentryNms :: (Seq.FastSeq GB.EntryInfo)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _bindLOconstMp :: ConstMp
              _bindLOgbState :: (GB.GBState)
              _bindLOimpNmOffMpMp :: HsName2OffsetMpMp
              _bindLOlamMp :: LamMp
              _bindLOopts :: EHCOpts
              _bindLOvaGam :: (GB.ValAccessGam)
              _bindLIconstMp :: ConstMp
              _bindLIentryNms :: (Seq.FastSeq GB.EntryInfo)
              _bindLIgathFviMp :: FvInfoMp
              _bindLIgbState :: (GB.GBState)
              _bindLIi :: (GB.InsSeq)
              _bindLIincludeS :: (Set.Set String)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 631, column 29)
              _lhsOentryNms =
                  _bindLIentryNms
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _bindLIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  _bindLIi
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _bindLIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _bindLIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _bindLIgbState
              -- copy rule (down)
              _bindLOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _bindLOgbState =
                  _lhsIgbState
              -- copy rule (down)
              _bindLOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _bindLOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindLOopts =
                  _lhsIopts
              -- copy rule (down)
              _bindLOvaGam =
                  _lhsIvaGam
              ( _bindLIconstMp,_bindLIentryNms,_bindLIgathFviMp,_bindLIgbState,_bindLIi,_bindLIincludeS) =
                  bindL_ _bindLOconstMp _bindLOgbState _bindLOimpNmOffMpMp _bindLOlamMp _bindLOopts _bindLOvaGam 
          in  ( _lhsOconstMp,_lhsOentryNms,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS)))
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         impNmOffMpMp         : HsName2OffsetMpMp
         lamMp                : LamMp
         opts                 : EHCOpts
         vaGam                : GB.ValAccessGam
      chained attributes:
         constMp              : ConstMp
         gbState              : GB.GBState
      synthesized attributes:
         entryNms             : Seq.FastSeq GB.EntryInfo
         gathFviMp            : FvInfoMp
         i                    : GB.InsSeq
         includeS             : Set.Set String
   alternatives:
      alternative Cons:
         child hd             : GrBind 
         child tl             : GrBindL 
      alternative Nil:
-}
-- cata
sem_GrBindL :: GrBindL  ->
               T_GrBindL 
sem_GrBindL list  =
    (Prelude.foldr sem_GrBindL_Cons sem_GrBindL_Nil (Prelude.map sem_GrBind list) )
-- semantic domain
type T_GrBindL  = ConstMp ->
                  (GB.GBState) ->
                  HsName2OffsetMpMp ->
                  LamMp ->
                  EHCOpts ->
                  (GB.ValAccessGam) ->
                  ( ConstMp,(Seq.FastSeq GB.EntryInfo),FvInfoMp,(GB.GBState),(GB.InsSeq),(Set.Set String))
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts
       _lhsIvaGam ->
         (let _lhsOentryNms :: (Seq.FastSeq GB.EntryInfo)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _hdOconstMp :: ConstMp
              _hdOgbState :: (GB.GBState)
              _hdOimpNmOffMpMp :: HsName2OffsetMpMp
              _hdOlamMp :: LamMp
              _hdOopts :: EHCOpts
              _hdOvaGam :: (GB.ValAccessGam)
              _tlOconstMp :: ConstMp
              _tlOgbState :: (GB.GBState)
              _tlOimpNmOffMpMp :: HsName2OffsetMpMp
              _tlOlamMp :: LamMp
              _tlOopts :: EHCOpts
              _tlOvaGam :: (GB.ValAccessGam)
              _hdIconstMp :: ConstMp
              _hdIentryNms :: (Seq.FastSeq GB.EntryInfo)
              _hdIgathFviMp :: FvInfoMp
              _hdIgbState :: (GB.GBState)
              _hdIi :: (GB.InsSeq)
              _hdIincludeS :: (Set.Set String)
              _tlIconstMp :: ConstMp
              _tlIentryNms :: (Seq.FastSeq GB.EntryInfo)
              _tlIgathFviMp :: FvInfoMp
              _tlIgbState :: (GB.GBState)
              _tlIi :: (GB.InsSeq)
              _tlIincludeS :: (Set.Set String)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 631, column 29)
              _lhsOentryNms =
                  _hdIentryNms Seq.:++: _tlIentryNms
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  _hdIi Seq.:++: _tlIi
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _hdIincludeS `Set.union` _tlIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _tlIgbState
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _hdOgbState =
                  _lhsIgbState
              -- copy rule (down)
              _hdOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _hdOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOvaGam =
                  _lhsIvaGam
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              -- copy rule (chain)
              _tlOgbState =
                  _hdIgbState
              -- copy rule (down)
              _tlOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _tlOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOvaGam =
                  _lhsIvaGam
              ( _hdIconstMp,_hdIentryNms,_hdIgathFviMp,_hdIgbState,_hdIi,_hdIincludeS) =
                  hd_ _hdOconstMp _hdOgbState _hdOimpNmOffMpMp _hdOlamMp _hdOopts _hdOvaGam 
              ( _tlIconstMp,_tlIentryNms,_tlIgathFviMp,_tlIgbState,_tlIi,_tlIincludeS) =
                  tl_ _tlOconstMp _tlOgbState _tlOimpNmOffMpMp _tlOlamMp _tlOopts _tlOvaGam 
          in  ( _lhsOconstMp,_lhsOentryNms,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS)))
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts
       _lhsIvaGam ->
         (let _lhsOentryNms :: (Seq.FastSeq GB.EntryInfo)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 631, column 29)
              _lhsOentryNms =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOgbState =
                  _lhsIgbState
          in  ( _lhsOconstMp,_lhsOentryNms,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS)))
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         impNmOffMpMp         : HsName2OffsetMpMp
         isSeqArgCtxt         : Bool
         lamMp                : LamMp
         opts                 : EHCOpts
         patBasicAnnot        : BasicAnnot
         returnCtxt           : ReturnCtxt
         vaGam                : GB.ValAccessGam
      chained attributes:
         constMp              : ConstMp
         gbState              : GB.GBState
         stState              : GB.StackState
      synthesized attributes:
         gathFviMp            : FvInfoMp
         i                    : GB.InsSeq
         includeS             : Set.Set String
         mbUnitIntro          : Maybe UnitIntro
         primNrArgForIntl     : Int
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local mbDebugStr  : _
            local _tup3       : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup4       : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local _tup5       : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
         visit 0:
            local mbDebugStrNm : _
            local _tup6       : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup7       : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local _tup8       : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
         visit 0:
            local _tup9       : _
            local aftLblGBState : _
            local aftCaseLabel : _
            local atNrAltsLabel : _
            local at1stOffLabel : _
            local aftCaseLocRef : _
            local nrOfAlts    : _
            local valStState  : _
            local ldStState   : _
            local enumStkInc  : _
            local valDepth    : _
            local returnCtxt  : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup10      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local scrutineeStInc : _
            local aftLdGBState : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
         visit 0:
            local nmEnv       : _
            local isSeqArgCtxt : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative Eval:
         child nm             : {HsName}
         visit 0:
            local mbDebugStr  : _
            local _tup11      : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup12      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local _tup13      : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local _tup14      : _
            local constInx    : _
            local constMp2    : _
            local mbDebugStr  : _
            local _tup15      : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local mbGbPrim    : _
            local optimForIntlPrim : _
            local _tup16      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local constMp3    : _
            local _tup17      : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
            local foreignEntInfo : _
            local impEntNm    : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
         visit 0:
            local aftLdGBState : _
            local _tup18      : _
            local aftLblGBState : _
            local retLocLabel : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative FetchNode:
         child nm             : {HsName}
         visit 0:
            local aftLdGBState : _
            local _tup19      : _
            local aftLblGBState : _
            local retLocLabel : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
         visit 0:
            local _tup20      : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup21      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
         visit 0:
            local bodyStState : _
            local ldStState   : _
            local bodyStkDepth : _
            local nmEnv       : _
            local _tup22      : _
            local seqIns      : _
            local seqStInc    : _
            local newVaGam    : _
            local aftLdGBState : _
            local retIns      : _
            local retFixOffs  : _
            local gathBodyFviMp : {FvInfoMp}
            local gathFviMp   : _
      alternative Store:
         child val            : GrVal 
         visit 0:
            local _tup23      : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup24      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local aftLdGBState : _
            local _tup25      : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
      alternative Throw:
         child nm             : {HsName}
         visit 0:
            local aftLdGBState : _
            local _tup26      : _
            local aftLblGBState : _
            local retLocLabel : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
         visit 0:
            local _tup27      : _
            local aftLblGBState : _
            local retLocLabel : _
            local valStState  : _
            local ldStState   : _
            local valDepth    : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local _tup28      : _
            local valIns      : _
            local valStInc    : _
            local retIsDone   : _
            local mbUnitIntro : _
            local aftLdGBState : _
            local _tup29      : _
            local retIns      : _
            local retStInc    : _
            local gathFviMp   : _
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local aftLdGBState : _
            local _tup30      : _
            local aftLblGBState : _
            local retLocLabel : _
            local nmEnv       : _
            local isSeqArgCtxt : _
            local retIns      : _
            local retFixOffs  : _
            local gathFviMp   : _
-}
-- cata
sem_GrExpr :: GrExpr  ->
              T_GrExpr 
sem_GrExpr (GrExpr_App _nm _argL )  =
    (sem_GrExpr_App _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Call _nm _argL )  =
    (sem_GrExpr_Call _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Case _val _altL )  =
    (sem_GrExpr_Case (sem_GrVal _val ) (sem_GrAltL _altL ) )
sem_GrExpr (GrExpr_Catch _body _arg _handler )  =
    (sem_GrExpr_Catch (sem_GrExpr _body ) _arg (sem_GrExpr _handler ) )
sem_GrExpr (GrExpr_Eval _nm )  =
    (sem_GrExpr_Eval _nm )
sem_GrExpr (GrExpr_FFI _callconv _impEnt _ffiAnnot _argL )  =
    (sem_GrExpr_FFI _callconv _impEnt _ffiAnnot (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_FetchField _nm _offset _mbTag )  =
    (sem_GrExpr_FetchField _nm _offset _mbTag )
sem_GrExpr (GrExpr_FetchNode _nm )  =
    (sem_GrExpr_FetchNode _nm )
sem_GrExpr (GrExpr_FetchUpdate _src _dst )  =
    (sem_GrExpr_FetchUpdate _src _dst )
sem_GrExpr (GrExpr_Seq _expr _pat _body )  =
    (sem_GrExpr_Seq (sem_GrExpr _expr ) (sem_GrPatLam _pat ) (sem_GrExpr _body ) )
sem_GrExpr (GrExpr_Store _val )  =
    (sem_GrExpr_Store (sem_GrVal _val ) )
sem_GrExpr (GrExpr_Throw _nm )  =
    (sem_GrExpr_Throw _nm )
sem_GrExpr (GrExpr_Unit _val _type )  =
    (sem_GrExpr_Unit (sem_GrVal _val ) (sem_GrType _type ) )
sem_GrExpr (GrExpr_UpdateUnit _nm _val )  =
    (sem_GrExpr_UpdateUnit _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrExpr  = ConstMp ->
                 (GB.GBState) ->
                 HsName2OffsetMpMp ->
                 Bool ->
                 LamMp ->
                 EHCOpts ->
                 BasicAnnot ->
                 ReturnCtxt ->
                 (GB.StackState) ->
                 (GB.ValAccessGam) ->
                 ( ConstMp,FvInfoMp,(GB.GBState),(GB.InsSeq),(Set.Set String),(Maybe UnitIntro),Int,(GB.StackState))
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _argLOconstMp :: ConstMp
              _argLIconstMp :: ConstMp
              _argLIgathFviMp :: FvInfoMp
              _argLIgrvalIntroL :: ([GB.GrValIntro])
              _argLIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 114, column 17)
              _mbDebugStr =
                  if ehcOptGenDebug _lhsIopts then Just (show nm_) else Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup3 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup3
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 514, column 33)
              __tup4 =
                  case GB.gviLdFold _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _aftLblGBState _argLIgrvalIntroL of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                      -> case _lhsIreturnCtxt of
                           ReturnCtxt_Returns nrArgsOuter
                             -> ( ains1 Seq.:++: ains2, GB.ststFromDep 0, True, gbState2 )
                             where ains2 = Seq.fromList [GB.tailapply GB.InsOp_LocB_TOS nArgMine nrArgsOuter]
                                   nArgMine = nargs + 2 * GB.nrValWords
                           _ -> ( ains1 Seq.:++: ains2, GB.ststFromDep GB.nrValWords, False, gbState2 )
                             where ains2 = Seq.fromList [GB.apply _mbDebugStr GB.InsOp_LocB_TOS]
                      where (fins,_,gbState2) = GB.nmLd GB.defaultLoadCtxt _nmEnv (_ldStState `GB.ststInc` inc `GB.ststIncDep` GB.nrValWords) gbState nm_
                            ains1 = ins Seq.:++: Seq.fromList [GB.ldc nargs] Seq.:++: fins
                            nargs = length _argLIgrvalIntroL
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 514, column 33)
              (_valIns,_,_,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 514, column 33)
              (_,_valStInc,_,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 514, column 33)
              (_,_,_retIsDone,_) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 514, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup4
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup5 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup5
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 14, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _argLIconstMp
              -- copy rule (down)
              _argLOconstMp =
                  _lhsIconstMp
              ( _argLIconstMp,_argLIgathFviMp,_argLIgrvalIntroL,_argLIlength) =
                  argL_ _argLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _argLOconstMp :: ConstMp
              _argLIconstMp :: ConstMp
              _argLIgathFviMp :: FvInfoMp
              _argLIgrvalIntroL :: ([GB.GrValIntro])
              _argLIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 115, column 17)
              _mbDebugStrNm =
                  Just (if ehcOptGenDebug _lhsIopts then Just (show nm_) else Nothing, nm_)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup6 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup6
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup6
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 463, column 33)
              __tup7 =
                  case GB.gviLdFold _lhsIopts (GB.defaultLoadCtxt {GB.lcxDoLdTOS = True}) _nmEnv _ldStState _aftLblGBState _argLIgrvalIntroL of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                      -> case _lhsIreturnCtxt of
                           ReturnCtxt_Returns nrArgsOuter
                             -> ( ins Seq.:++: fins Seq.:++: Seq.fromList [GB.tailcall GB.InsOp_LocB_TOS nArgMine nrArgsOuter                 ]
                                , GB.ststFromDep 0, True, gbState2
                                )
                             where (fins,fdep,gbState2) = GB.nmLd GB.defaultLoadCtxt _nmEnv (_ldStState `GB.ststInc` inc) gbState nm_
                                   nArgMine    = length _argLIgrvalIntroL
                           _ -> (ins', inc', False, gbState2)
                             where (ins',inc',gbState2) = GB.gvCall _mbDebugStrNm _nmEnv ins inc _ldStState gbState nm_
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 463, column 33)
              (_valIns,_,_,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 463, column 33)
              (_,_valStInc,_,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 463, column 33)
              (_,_,_retIsDone,_) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 463, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup7
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup8 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup8
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup8
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 14, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton' FvUse_Call nm_, _argLIgathFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _argLIconstMp
              -- copy rule (down)
              _argLOconstMp =
                  _lhsIconstMp
              ( _argLIconstMp,_argLIgathFviMp,_argLIgrvalIntroL,_argLIlength) =
                  argL_ _argLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _altLOgbState :: (GB.GBState)
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _altLOstState :: (GB.StackState)
              _altLOisEnum :: Bool
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _valOconstMp :: ConstMp
              _altLOconstMp :: ConstMp
              _altLOimpNmOffMpMp :: HsName2OffsetMpMp
              _altLOlamMp :: LamMp
              _altLOnrOfAlts :: Int
              _altLOopts :: EHCOpts
              _altLOreturnCtxt :: ReturnCtxt
              _altLOvaGam :: (GB.ValAccessGam)
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              _altLIaltLocRefs :: ([GB.LocRef])
              _altLIconstMp :: ConstMp
              _altLIgathFviMp :: FvInfoMp
              _altLIgathNrOfAlts :: Int
              _altLIgbState :: (GB.GBState)
              _altLIi :: (GB.InsSeq)
              _altLIincludeS :: (Set.Set String)
              _altLIisAllEnum :: Bool
              _altLInrAlts :: Int
              _altLIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 126, column 17)
              _altLOgbState =
                  _aftLblGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 138, column 17)
              __tup9 =
                  let (s1,l1) = GB.newLabelId _lhsIgbState
                      (s2,l2) = GB.newLabelId s1
                      (s3,l3) = GB.newLabelId s2
                  in  (s3,l1,l2,l3)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 138, column 17)
              (_aftLblGBState,_,_,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 138, column 17)
              (_,_aftCaseLabel,_,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 138, column 17)
              (_,_,_atNrAltsLabel,_) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 138, column 17)
              (_,_,_,_at1stOffLabel) =
                  __tup9
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 172, column 17)
              _aftCaseLocRef =
                  GB.LocRef_EndSwitch _aftCaseLabel
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 175, column 17)
              _nrOfAlts =
                  _altLIgathNrOfAlts
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 204, column 17)
              _lhsOstState =
                  _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 209, column 17)
              _altLOstState =
                  _lhsIstState `GB.ststInc` _scrutineeStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 210, column 17)
              _valStState =
                  case _returnCtxt of
                    ReturnCtxt_CaseReturns _ st
                      -> st `GB.ststIncDep` GB.nrValWords
                    _ -> _altLIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 234, column 17)
              _enumStkInc =
                  if _altLIisAllEnum then - GB.nrValWords else 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 234, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 260, column 17)
              _returnCtxt =
                  case _lhsIreturnCtxt of
                    ReturnCtxt_Continues
                      | _altLInrAlts == 1 -> ReturnCtxt_CaseFallThrough
                      | otherwise         -> ReturnCtxt_CaseReturns _aftCaseLabel _lhsIstState
                    ReturnCtxt_CaseFallThrough
                      | _altLInrAlts == 1 -> _lhsIreturnCtxt
                      | otherwise         -> ReturnCtxt_CaseReturns _aftCaseLabel _lhsIstState
                    _                     -> _lhsIreturnCtxt
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 281, column 17)
              _altLOisEnum =
                  _altLIisAllEnum
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              __tup10 =
                  case GB.gviLd _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _altLIgbState _valIgrvalIntro of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                      | _altLInrAlts == 1
                        -> ( ins Seq.:++: _altLIi, GB.ststFromDep GB.nrValWords, True, inc, gbState )
                      | otherwise
                        -> ( ins Seq.:++: insc Seq.:++: ins', GB.ststFromDep GB.nrValWords, True, inc `GB.ststIncDep` _enumStkInc, gbState2 )
                        where ins' = Seq.fromList [GB.casecall (GB.linkChainOffsets _atNrAltsLabel _nrOfAlts),GB.label (GB.LocRef_Label _at1stOffLabel)]
                                      Seq.:++: Seq.fromList                     [ GB.labelref l | l <- _altLIaltLocRefs ]
                                      Seq.:++: _altLIi
                                      Seq.:++: Seq.fromList [GB.label _aftCaseLocRef]
                              insc = if _altLIisAllEnum then Seq.empty else Seq.fromList [GB.ldnt]
                              (inslblrefs,gbState2) = GB.labelref2 (map GB.lrefId _altLIaltLocRefs) gbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              (_valIns,_,_,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              (_,_valStInc,_,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              (_,_,_retIsDone,_,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              (_,_,_,_scrutineeStInc,_) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 532, column 33)
              (_,_,_,_,_aftLdGBState) =
                  __tup10
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 23, column 17)
              _gathFviMp =
                  fviMpUnions [_valIgathFviMp, _altLIgathFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _altLIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _altLIconstMp
              -- copy rule (down)
              _valOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _altLOconstMp =
                  _valIconstMp
              -- copy rule (down)
              _altLOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _altLOlamMp =
                  _lhsIlamMp
              -- copy rule (from local)
              _altLOnrOfAlts =
                  _nrOfAlts
              -- copy rule (down)
              _altLOopts =
                  _lhsIopts
              -- copy rule (from local)
              _altLOreturnCtxt =
                  _returnCtxt
              -- copy rule (down)
              _altLOvaGam =
                  _lhsIvaGam
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
              ( _altLIaltLocRefs,_altLIconstMp,_altLIgathFviMp,_altLIgathNrOfAlts,_altLIgbState,_altLIi,_altLIincludeS,_altLIisAllEnum,_altLInrAlts,_altLIstState) =
                  altL_ _altLOconstMp _altLOgbState _altLOimpNmOffMpMp _altLOisEnum _altLOlamMp _altLOnrOfAlts _altLOopts _altLOreturnCtxt _altLOstState _altLOvaGam 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _bodyOconstMp :: ConstMp
              _bodyOgbState :: (GB.GBState)
              _bodyOimpNmOffMpMp :: HsName2OffsetMpMp
              _bodyOisSeqArgCtxt :: Bool
              _bodyOlamMp :: LamMp
              _bodyOopts :: EHCOpts
              _bodyOpatBasicAnnot :: BasicAnnot
              _bodyOreturnCtxt :: ReturnCtxt
              _bodyOstState :: (GB.StackState)
              _bodyOvaGam :: (GB.ValAccessGam)
              _handlerOconstMp :: ConstMp
              _handlerOgbState :: (GB.GBState)
              _handlerOimpNmOffMpMp :: HsName2OffsetMpMp
              _handlerOisSeqArgCtxt :: Bool
              _handlerOlamMp :: LamMp
              _handlerOopts :: EHCOpts
              _handlerOpatBasicAnnot :: BasicAnnot
              _handlerOreturnCtxt :: ReturnCtxt
              _handlerOstState :: (GB.StackState)
              _handlerOvaGam :: (GB.ValAccessGam)
              _bodyIconstMp :: ConstMp
              _bodyIgathFviMp :: FvInfoMp
              _bodyIgbState :: (GB.GBState)
              _bodyIi :: (GB.InsSeq)
              _bodyIincludeS :: (Set.Set String)
              _bodyImbUnitIntro :: (Maybe UnitIntro)
              _bodyIprimNrArgForIntl :: Int
              _bodyIstState :: (GB.StackState)
              _handlerIconstMp :: ConstMp
              _handlerIgathFviMp :: FvInfoMp
              _handlerIgbState :: (GB.GBState)
              _handlerIi :: (GB.InsSeq)
              _handlerIincludeS :: (Set.Set String)
              _handlerImbUnitIntro :: (Maybe UnitIntro)
              _handlerIprimNrArgForIntl :: Int
              _handlerIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 19, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton arg_, _bodyIgathFviMp, _handlerIgathFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  _bodyIi Seq.:++: _handlerIi
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _bodyIincludeS `Set.union` _handlerIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _handlerIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _handlerIgbState
              -- copy rule (up)
              _lhsOstState =
                  _handlerIstState
              -- copy rule (down)
              _bodyOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _bodyOgbState =
                  _lhsIgbState
              -- copy rule (down)
              _bodyOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (from local)
              _bodyOisSeqArgCtxt =
                  _isSeqArgCtxt
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOpatBasicAnnot =
                  _lhsIpatBasicAnnot
              -- copy rule (down)
              _bodyOreturnCtxt =
                  _lhsIreturnCtxt
              -- copy rule (down)
              _bodyOstState =
                  _lhsIstState
              -- copy rule (down)
              _bodyOvaGam =
                  _lhsIvaGam
              -- copy rule (chain)
              _handlerOconstMp =
                  _bodyIconstMp
              -- copy rule (chain)
              _handlerOgbState =
                  _bodyIgbState
              -- copy rule (down)
              _handlerOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (from local)
              _handlerOisSeqArgCtxt =
                  _isSeqArgCtxt
              -- copy rule (down)
              _handlerOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _handlerOopts =
                  _lhsIopts
              -- copy rule (down)
              _handlerOpatBasicAnnot =
                  _lhsIpatBasicAnnot
              -- copy rule (down)
              _handlerOreturnCtxt =
                  _lhsIreturnCtxt
              -- copy rule (chain)
              _handlerOstState =
                  _bodyIstState
              -- copy rule (down)
              _handlerOvaGam =
                  _lhsIvaGam
              ( _bodyIconstMp,_bodyIgathFviMp,_bodyIgbState,_bodyIi,_bodyIincludeS,_bodyImbUnitIntro,_bodyIprimNrArgForIntl,_bodyIstState) =
                  body_ _bodyOconstMp _bodyOgbState _bodyOimpNmOffMpMp _bodyOisSeqArgCtxt _bodyOlamMp _bodyOopts _bodyOpatBasicAnnot _bodyOreturnCtxt _bodyOstState _bodyOvaGam 
              ( _handlerIconstMp,_handlerIgathFviMp,_handlerIgbState,_handlerIi,_handlerIincludeS,_handlerImbUnitIntro,_handlerIprimNrArgForIntl,_handlerIstState) =
                  handler_ _handlerOconstMp _handlerOgbState _handlerOimpNmOffMpMp _handlerOisSeqArgCtxt _handlerOlamMp _handlerOopts _handlerOpatBasicAnnot _handlerOreturnCtxt _handlerOstState _handlerOvaGam 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 114, column 17)
              _mbDebugStr =
                  if ehcOptGenDebug _lhsIopts then Just (show nm_) else Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup11 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup11
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup11
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 506, column 33)
              __tup12 =
                  let (ins,inc,gbState) = GB.nmLd (GB.defaultLoadCtxt {GB.lcxOmitTOSLd = True }) _nmEnv _ldStState _aftLblGBState nm_
                  in  case _lhsIreturnCtxt of
                        ReturnCtxt_Returns nrArgsOuter
                          -> (ins Seq.:++: eins, GB.ststFromDep 0, True, gbState)
                          where eins       = Seq.fromList [GB.taileval _mbDebugStr GB.InsOp_LocB_TOS nrArgsOuter                 ]
                        _ -> (ins Seq.:++: eins, inc, False, gbState)
                          where eins = Seq.fromList [GB.eval _mbDebugStr GB.InsOp_LocB_TOS]
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 506, column 33)
              (_valIns,_,_,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 506, column 33)
              (_,_valStInc,_,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 506, column 33)
              (_,_,_retIsDone,_) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 506, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup12
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup13 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup13
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup13
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOincludeS :: (Set.Set String)
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _argLOconstMp :: ConstMp
              _argLIconstMp :: ConstMp
              _argLIgathFviMp :: FvInfoMp
              _argLIgrvalIntroL :: ([GB.GrValIntro])
              _argLIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 109, column 17)
              __tup14 =
                  constCFunAdd _impEntNm _lhsIconstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 109, column 17)
              (_constInx,_) =
                  __tup14
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 109, column 17)
              (_,_constMp2) =
                  __tup14
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 110, column 17)
              _lhsOconstMp =
                  _constMp3
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 113, column 17)
              _mbDebugStr =
                  if ehcOptGenDebug _lhsIopts then Just (_impEntNm) else Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup15 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup15
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup15
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 441, column 17)
              _mbGbPrim =
                  lookupPrim BackendGrinByteCode _impEntNm
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 441, column 17)
              _optimForIntlPrim =
                  maybe False (\p -> gbprimNrArgs p == _argLIlength) _mbGbPrim
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 443, column 17)
              _lhsOprimNrArgForIntl =
                  if _optimForIntlPrim then _argLIlength else 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              __tup16 =
                  if _optimForIntlPrim
                  then case gbprimMk (fromJust _mbGbPrim) _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _aftLblGBState _argLIgrvalIntroL of
                         (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                           -> (ins, inc, False, gbState, _constMp2)
                         _ -> panic ("ToGrinByteCode.mbGbPrim:" ++ show _impEntNm)
                  else case GB.gviLdFold _lhsIopts (GB.defaultLoadCtxt {GB.lcxDoLdTOS = True}) _nmEnv _ldStState _aftLblGBState _argLIgrvalIntroL of
                         (GB.GrValIntroAlt_OnTOS ins inc _ basicsz, gbState)
                           -> ( ins Seq.:++: fins Seq.:++:
                                Seq.fromList ([GB.meta' GB.AnnIdUse (_impEntNm >#< "szargs=" >|< GB.ststDepth inc >#< "basicsz=" >|< basicszStr)] ++ call)
                              , GB.ststFromDepPerm (basicSizeInWords patbasicsz) (grinBasicAnnotGCPermit _lhsIpatBasicAnnot), False
                              , gbState3 `GB.gbstIncByStackState` callStState
                              , constMp3
                              )
                           where (gbState2,constLbl  ) = GB.newLabelId gbState
                                 (gbState3,callencLbl) = GB.newLabelId gbState2
                                 fins = Seq.fromList [GB.ldg GB.InsOp_LocB_TOS $ GB.linkChainConst constLbl _constInx]
                                 finc = GB.ststFromDepPerm 1 GCPermit_Not
                                 patbasicsz = grinBasicAnnotSize _lhsIpatBasicAnnot
                                 basicszs = patbasicsz : basicsz
                                 (callencInx,constMp3) = constCCallEncWrapper basicszs _constMp2
                                 basicszStr = concatMap basicGrinSizeCharEncoding basicszs
                                 callStState = _ldStState `GB.ststInc` inc `GB.ststInc` finc
                                 call  = if forextractOptIsPtr _foreignEntInfo then [] else cl
                                       where cl = [ GB.meta' GB.AnnStackDepth callStState
                                                  , GB.callc _mbDebugStr (callencLbl,callencInx,basicszs) callStState (GB.ststDepth inc)
                                                  ]
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              (_valIns,_,_,_,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              (_,_valStInc,_,_,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              (_,_,_retIsDone,_,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              (_,_,_,_aftLdGBState,_) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 475, column 33)
              (_,_,_,_,_constMp3) =
                  __tup16
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup17 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup17
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup17
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 652, column 17)
              _lhsOincludeS =
                  Set.fromList $ forextractIncludes _foreignEntInfo
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 15, column 17)
              _gathFviMp =
                  _argLIgathFviMp
              -- "build/101/lib-ehc/EH101/GrinCode/CommonForGen.ag"(line 2, column 17)
              _foreignEntInfo =
                  foreignEntExtract impEnt_
              -- "build/101/lib-ehc/EH101/GrinCode/CommonForGen.ag"(line 2, column 17)
              _impEntNm =
                  forextractEnt _foreignEntInfo
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- copy rule (down)
              _argLOconstMp =
                  _lhsIconstMp
              ( _argLIconstMp,_argLIgathFviMp,_argLIgrvalIntroL,_argLIlength) =
                  argL_ _argLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 129, column 17)
              _aftLdGBState =
                  _aftLblGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 130, column 17)
              _lhsOgbState =
                  _aftLdGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup18 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup18
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup18
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOstState =
                  _lhsIstState
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 129, column 17)
              _aftLdGBState =
                  _aftLblGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 130, column 17)
              _lhsOgbState =
                  _aftLdGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup19 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup19
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup19
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOstState =
                  _lhsIstState
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup20 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup20
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup20
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 204, column 17)
              _lhsOstState =
                  _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 502, column 33)
              __tup21 =
                  case GB.gviLdFold _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _aftLblGBState $ map GB.GrValIntro_Nm $ [src_,dst_] of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                      -> (ins Seq.:++: Seq.fromList [GB.fetchupdate], GB.ststFromDep 0, False, gbState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 502, column 33)
              (_valIns,_,_,_) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 502, column 33)
              (_,_valStInc,_,_) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 502, column 33)
              (_,_,_retIsDone,_) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 502, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup21
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 18, column 17)
              _gathFviMp =
                  fviMpFromList [src_,dst_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _bodyOgbState :: (GB.GBState)
              _bodyOstState :: (GB.StackState)
              _exprOreturnCtxt :: ReturnCtxt
              _exprOpatBasicAnnot :: BasicAnnot
              _bodyOvaGam :: (GB.ValAccessGam)
              _exprOisSeqArgCtxt :: Bool
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _patOprimArgForIntl :: Bool
              _lhsOprimNrArgForIntl :: Int
              _patOprimResForIntl :: Bool
              _lhsOi :: (GB.InsSeq)
              _gathBodyFviMp :: FvInfoMp
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _exprOconstMp :: ConstMp
              _exprOgbState :: (GB.GBState)
              _exprOimpNmOffMpMp :: HsName2OffsetMpMp
              _exprOlamMp :: LamMp
              _exprOopts :: EHCOpts
              _exprOstState :: (GB.StackState)
              _exprOvaGam :: (GB.ValAccessGam)
              _patOconstMp :: ConstMp
              _bodyOconstMp :: ConstMp
              _bodyOimpNmOffMpMp :: HsName2OffsetMpMp
              _bodyOisSeqArgCtxt :: Bool
              _bodyOlamMp :: LamMp
              _bodyOopts :: EHCOpts
              _bodyOpatBasicAnnot :: BasicAnnot
              _bodyOreturnCtxt :: ReturnCtxt
              _exprIconstMp :: ConstMp
              _exprIgathFviMp :: FvInfoMp
              _exprIgbState :: (GB.GBState)
              _exprIi :: (GB.InsSeq)
              _exprIincludeS :: (Set.Set String)
              _exprImbUnitIntro :: (Maybe UnitIntro)
              _exprIprimNrArgForIntl :: Int
              _exprIstState :: (GB.StackState)
              _patIconstMp :: ConstMp
              _patIgathFviMp :: FvInfoMp
              _patIi :: (GB.InsSeq)
              _patIintroNmL :: ([HsName])
              _patImbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _patInmIntro :: NmIntro
              _bodyIconstMp :: ConstMp
              _bodyIgathFviMp :: FvInfoMp
              _bodyIgbState :: (GB.GBState)
              _bodyIi :: (GB.InsSeq)
              _bodyIincludeS :: (Set.Set String)
              _bodyImbUnitIntro :: (Maybe UnitIntro)
              _bodyIprimNrArgForIntl :: Int
              _bodyIstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 127, column 17)
              _bodyOgbState =
                  GB.gbstIncByStackState _aftLdGBState _seqStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 207, column 17)
              _bodyStState =
                  _exprIstState `GB.ststInc` _seqStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 208, column 17)
              _bodyOstState =
                  _bodyStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 228, column 17)
              _ldStState =
                  _exprIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 233, column 17)
              _bodyStkDepth =
                  GB.ststDepth _bodyStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 259, column 17)
              _exprOreturnCtxt =
                  ReturnCtxt_Continues
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 322, column 17)
              _exprOpatBasicAnnot =
                  nmIntroBasicAnnot _patInmIntro
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 350, column 17)
              _bodyOvaGam =
                  _newVaGam `Map.union` _lhsIvaGam
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 426, column 17)
              _exprOisSeqArgCtxt =
                  True
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 444, column 17)
              _patOprimArgForIntl =
                  _bodyIprimNrArgForIntl > 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 445, column 17)
              _lhsOprimNrArgForIntl =
                  if _exprIprimNrArgForIntl > 0 then _exprIprimNrArgForIntl else _bodyIprimNrArgForIntl - 1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 451, column 17)
              _patOprimResForIntl =
                  _exprIprimNrArgForIntl > 0
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 545, column 33)
              __tup22 =
                  let patupd (Just _) annot inc = GB.ststFromDepPerm (GB.ststDepth inc) (grinBasicAnnotGCPermit annot)
                      patupd _        _     inc = inc
                      nm ins incSt gbState nmIntro
                        = case nmIntro of
                            (NmIntro_Single nm annot)
                              -> ( ins, incSt `GB.ststInc` GB.ststPatchDepPerm (grinBasicAnnotSizeInWords annot) (grinBasicAnnotGCPermit annot)
                                 , Map.fromList [(nm,GB.Val_Local (GB.ststDepth _ldStState + inc) (GB.ValAccessAnnot_Annot annot))], gbState
                                 )
                            (NmIntro_GrpTag nm)
                              -> (ins Seq.:++: patins,incSt `GB.ststInc` patinc,Map.fromList [(nm,GB.Val_Local (GB.ststDepth _ldStState + inc) (GB.ValAccessAnnot_Annot BasicAnnot_Dflt))],gbState)
                              where (patins,patinc) = maybe (Seq.empty,GB.emptyStackState) (\(ins,_,inc) -> (ins,inc)) _patImbi
                            (NmIntro_GrpBasic nm annot)
                              -> (ins Seq.:++: patins,incSt       `GB.ststInc` patinc,Map.fromList [(nm,val)],gbState)
                              where (patins,val,patinc)
                                      = case (annot,_patImbi) of
                                          (BasicAnnot_Size _ _ BasicAnnotTagging_FromPtr _, Just (i,bSz,patinc))
                                            -> (i        ,GB.Val_Local (GB.ststDepth _ldStState + inc)        (GB.ValAccessAnnot_Basic bSz    gcPermit), patinc              )
                                          (BasicAnnot_Size _ _ BasicAnnotTagging_FromPtr _, _                  )
                                            -> (Seq.empty,GB.Val_Local (GB.ststDepth _ldStState + inc)        (GB.ValAccessAnnot_Annot BasicAnnot_Dflt), GB.emptyStackState  )
                                          (_                           , Just (i,bSz,patinc))
                                            -> (i        ,GB.Val_NodeFldLocal (GB.ststDepth _ldStState + inc) (GB.ValAccessAnnot_Basic bSz    gcPermit), GB.emptyStackState  )
                                          (_                           , _                  )
                                            -> (Seq.empty,GB.Val_NodeFldLocal (GB.ststDepth _ldStState + inc) (GB.ValAccessAnnot_Annot annot          ), GB.emptyStackState  )
                                      where gcPermit = grinBasicAnnotGCPermit annot
                            (NmIntro_Grp _ nmL) | not (null nmL)
                              -> (ins Seq.:++: fins, GB.ststFromDep (length nmL * GB.nrValWords),g,gbState)
                              where fins = Seq.fromList [GB.fetch GB.InsOp_LocB_TOS]
                                    g = patNmL2VAGam (GB.ststDepth _ldStState + inc) nmL
                            _ -> (ins,incSt,Map.empty,gbState)
                        where inc = GB.ststDepth incSt
                  in  case (_exprImbUnitIntro,_patInmIntro) of
                        (Just (UnitIntro (GB.GrValIntro_Int i)),NmIntro_Single nmp annot)
                          | bsz <= Cfg.sizeofGrWord
                            -> (Seq.empty,GB.ststFromDep 0,Map.singleton nmp (GB.Val_Int i),_exprIgbState)
                            where bsz = grinBasicAnnotSizeInBytes annot
                        (Just (UnitIntro gvi),nmi)
                            -> nm ins inc gbState nmi
                            where (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                                    = GB.gviLd _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _exprIgbState gvi
                        _   -> nm Seq.empty (GB.ststFromDep 0) _exprIgbState _patInmIntro
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 545, column 33)
              (_seqIns,_,_,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 545, column 33)
              (_,_seqStInc,_,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 545, column 33)
              (_,_,_newVaGam,_) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 545, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup22
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 693, column 17)
              _lhsOi =
                  _exprIi Seq.:++: Seq.fromList [GB.meta' GB.AnnStackDepth _ldStState]
                          Seq.:++: _seqIns Seq.:++: _bodyIi
                          Seq.:++: Seq.fromList [GB.meta' GB.AnnStackDepth _bodyStState]
                          Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 12, column 17)
              _gathBodyFviMp =
                  _bodyIgathFviMp `fviMpDifference` fviMpFromList _patIintroNmL
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 12, column 17)
              _gathFviMp =
                  fviMpUnions [_exprIgathFviMp, _gathBodyFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  _exprIincludeS `Set.union` _bodyIincludeS
              -- copy rule (up)
              _lhsOconstMp =
                  _bodyIconstMp
              -- copy rule (up)
              _lhsOgbState =
                  _bodyIgbState
              -- copy rule (up)
              _lhsOstState =
                  _bodyIstState
              -- copy rule (down)
              _exprOconstMp =
                  _lhsIconstMp
              -- copy rule (down)
              _exprOgbState =
                  _lhsIgbState
              -- copy rule (down)
              _exprOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _exprOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOstState =
                  _lhsIstState
              -- copy rule (down)
              _exprOvaGam =
                  _lhsIvaGam
              -- copy rule (chain)
              _patOconstMp =
                  _exprIconstMp
              -- copy rule (chain)
              _bodyOconstMp =
                  _patIconstMp
              -- copy rule (down)
              _bodyOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _bodyOisSeqArgCtxt =
                  _lhsIisSeqArgCtxt
              -- copy rule (down)
              _bodyOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bodyOopts =
                  _lhsIopts
              -- copy rule (down)
              _bodyOpatBasicAnnot =
                  _lhsIpatBasicAnnot
              -- copy rule (down)
              _bodyOreturnCtxt =
                  _lhsIreturnCtxt
              ( _exprIconstMp,_exprIgathFviMp,_exprIgbState,_exprIi,_exprIincludeS,_exprImbUnitIntro,_exprIprimNrArgForIntl,_exprIstState) =
                  expr_ _exprOconstMp _exprOgbState _exprOimpNmOffMpMp _exprOisSeqArgCtxt _exprOlamMp _exprOopts _exprOpatBasicAnnot _exprOreturnCtxt _exprOstState _exprOvaGam 
              ( _patIconstMp,_patIgathFviMp,_patIi,_patIintroNmL,_patImbi,_patInmIntro) =
                  pat_ _patOconstMp _patOprimArgForIntl _patOprimResForIntl 
              ( _bodyIconstMp,_bodyIgathFviMp,_bodyIgbState,_bodyIi,_bodyIincludeS,_bodyImbUnitIntro,_bodyIprimNrArgForIntl,_bodyIstState) =
                  body_ _bodyOconstMp _bodyOgbState _bodyOimpNmOffMpMp _bodyOisSeqArgCtxt _bodyOlamMp _bodyOopts _bodyOpatBasicAnnot _bodyOreturnCtxt _bodyOstState _bodyOvaGam 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _valOconstMp :: ConstMp
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup23 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup23
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup23
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 459, column 33)
              __tup24 =
                  case GB.gviLd _lhsIopts GB.defaultLoadCtxt _nmEnv _ldStState _aftLblGBState _valIgrvalIntro of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState)
                      -> (ins, inc, False, gbState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 459, column 33)
              (_valIns,_,_,_) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 459, column 33)
              (_,_valStInc,_,_) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 459, column 33)
              (_,_,_retIsDone,_) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 459, column 33)
              (_,_,_,_aftLdGBState) =
                  __tup24
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup25 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup25
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup25
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 20, column 17)
              _gathFviMp =
                  _valIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _valIconstMp
              -- copy rule (down)
              _valOconstMp =
                  _lhsIconstMp
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOstState :: (GB.StackState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 129, column 17)
              _aftLdGBState =
                  _aftLblGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 130, column 17)
              _lhsOgbState =
                  _aftLdGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup26 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup26
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup26
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 17, column 17)
              _gathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _lhsOstState =
                  _lhsIstState
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOstState :: (GB.StackState)
              _lhsOprimNrArgForIntl :: Int
              _lhsOi :: (GB.InsSeq)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _valOconstMp :: ConstMp
              _typeOconstMp :: ConstMp
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              _typeIconstMp :: ConstMp
              _typeIgathFviMp :: FvInfoMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 132, column 17)
              _lhsOgbState =
                  GB.gbstIncByStackState _aftLdGBState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup27 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup27
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup27
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 202, column 17)
              _valStState =
                  _lhsIstState `GB.ststInc` _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 206, column 17)
              _lhsOstState =
                  _lhsIstState `GB.ststInc`_retStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 227, column 17)
              _ldStState =
                  _lhsIstState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 232, column 17)
              _valDepth =
                  GB.ststDepth _valStState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              __tup28 =
                  case GB.gviLd _lhsIopts (GB.defaultLoadCtxt {GB.lcxOmitTOSLd = True }) _nmEnv _ldStState _aftLblGBState _valIgrvalIntro of
                    (GB.GrValIntroAlt_OnTOS ins inc _ _, gbState) | not _lhsIisSeqArgCtxt
                      -> (ins,inc,False,Nothing, gbState)
                    _ -> (Seq.empty,GB.ststFromDep 0,True,Just (UnitIntro _valIgrvalIntro), _aftLblGBState)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              (_valIns,_,_,_,_) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              (_,_valStInc,_,_,_) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              (_,_,_retIsDone,_,_) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              (_,_,_,_mbUnitIntro,_) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 454, column 33)
              (_,_,_,_,_aftLdGBState) =
                  __tup28
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              __tup29 =
                  if _retIsDone
                  then (Seq.empty,_valStInc)
                  else mkRet _lhsIreturnCtxt _retLocLabel _lhsIstState _valStInc
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_retIns,_) =
                  __tup29
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 624, column 17)
              (_,_retStInc) =
                  __tup29
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 692, column 17)
              _lhsOi =
                  _valIns Seq.:++: _retIns
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 21, column 17)
              _gathFviMp =
                  _valIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _typeIconstMp
              -- copy rule (from local)
              _lhsOmbUnitIntro =
                  _mbUnitIntro
              -- copy rule (down)
              _valOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _typeOconstMp =
                  _valIconstMp
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
              ( _typeIconstMp,_typeIgathFviMp) =
                  type_ _typeOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (\ _lhsIconstMp
       _lhsIgbState
       _lhsIimpNmOffMpMp
       _lhsIisSeqArgCtxt
       _lhsIlamMp
       _lhsIopts
       _lhsIpatBasicAnnot
       _lhsIreturnCtxt
       _lhsIstState
       _lhsIvaGam ->
         (let _lhsOgbState :: (GB.GBState)
              _lhsOmbUnitIntro :: (Maybe UnitIntro)
              _lhsOprimNrArgForIntl :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOincludeS :: (Set.Set String)
              _lhsOconstMp :: ConstMp
              _lhsOstState :: (GB.StackState)
              _valOconstMp :: ConstMp
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 129, column 17)
              _aftLdGBState =
                  _aftLblGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 130, column 17)
              _lhsOgbState =
                  _aftLdGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              __tup30 =
                  mkRetLabel _lhsIreturnCtxt _lhsIgbState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_aftLblGBState,_) =
                  __tup30
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 144, column 17)
              (_,_retLocLabel) =
                  __tup30
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 364, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _lhsIvaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 427, column 17)
              _isSeqArgCtxt =
                  False
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 432, column 17)
              _lhsOmbUnitIntro =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 446, column 17)
              _lhsOprimNrArgForIntl =
                  -1
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retIns =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 628, column 17)
              _retFixOffs =
                  Seq.empty
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 22, column 17)
              _gathFviMp =
                  fviMpUnions [fviMpSingleton nm_, _valIgathFviMp]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _gathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 649, column 39)
              _lhsOincludeS =
                  Set.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _valIconstMp
              -- copy rule (chain)
              _lhsOstState =
                  _lhsIstState
              -- copy rule (down)
              _valOconstMp =
                  _lhsIconstMp
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgbState,_lhsOi,_lhsOincludeS,_lhsOmbUnitIntro,_lhsOprimNrArgForIntl,_lhsOstState)))
-- GrFFIAnnot --------------------------------------------------
{-
   alternatives:
      alternative IsResEval:
         child isEvaluated    : {Bool}
-}
-- cata
sem_GrFFIAnnot :: GrFFIAnnot  ->
                  T_GrFFIAnnot 
sem_GrFFIAnnot (GrFFIAnnot_IsResEval _isEvaluated )  =
    (sem_GrFFIAnnot_IsResEval _isEvaluated )
-- semantic domain
type T_GrFFIAnnot  = ( )
sem_GrFFIAnnot_IsResEval :: Bool ->
                            T_GrFFIAnnot 
sem_GrFFIAnnot_IsResEval isEvaluated_  =
    (let 
     in  ( ))
-- GrGlobal ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Global:
         child nm             : {HsName}
         child val            : GrVal 
-}
-- cata
sem_GrGlobal :: GrGlobal  ->
                T_GrGlobal 
sem_GrGlobal (GrGlobal_Global _nm _val )  =
    (sem_GrGlobal_Global _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrGlobal  = ConstMp ->
                   ( ConstMp,FvInfoMp)
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _valOconstMp :: ConstMp
              _valIconstMp :: ConstMp
              _valIgathFviMp :: FvInfoMp
              _valIgrvalIntro :: (GB.GrValIntro)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _valIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _valIconstMp
              -- copy rule (down)
              _valOconstMp =
                  _lhsIconstMp
              ( _valIconstMp,_valIgathFviMp,_valIgrvalIntro) =
                  val_ _valOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrGlobal 
         child tl             : GrGlobalL 
      alternative Nil:
-}
-- cata
sem_GrGlobalL :: GrGlobalL  ->
                 T_GrGlobalL 
sem_GrGlobalL list  =
    (Prelude.foldr sem_GrGlobalL_Cons sem_GrGlobalL_Nil (Prelude.map sem_GrGlobal list) )
-- semantic domain
type T_GrGlobalL  = ConstMp ->
                    ( ConstMp,FvInfoMp)
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrModule ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allImpNmL            : [HsName]
         expNmOffMp           : HsName2OffsetMp
         impNmOffMpMp         : HsName2OffsetMpMp
         lamMp                : LamMp
         opts                 : EHCOpts
      synthesized attributes:
         errs                 : [Err]
         gbMod                : GB.Module
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
         visit 0:
            local moduleNmAlpha : _
            local gbState     : _
            local vaGam       : _
            local nmEnv       : _
            local errs        : _
            local _tup31      : _
            local entryNmMp   : _
            local entryL      : _
            local isCafL      : _
            local mainEntry   : _
            local i           : _
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = ([HsName]) ->
                   HsName2OffsetMp ->
                   HsName2OffsetMpMp ->
                   LamMp ->
                   EHCOpts ->
                   ( ([Err]),(GB.Module))
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (\ _lhsIallImpNmL
       _lhsIexpNmOffMp
       _lhsIimpNmOffMpMp
       _lhsIlamMp
       _lhsIopts ->
         (let _globalLOconstMp :: ConstMp
              _lhsOgbMod :: (GB.Module)
              _lhsOerrs :: ([Err])
              _bindLOconstMp :: ConstMp
              _bindLOgbState :: (GB.GBState)
              _bindLOimpNmOffMpMp :: HsName2OffsetMpMp
              _bindLOlamMp :: LamMp
              _bindLOopts :: EHCOpts
              _bindLOvaGam :: (GB.ValAccessGam)
              _globalLIconstMp :: ConstMp
              _globalLIgathFviMp :: FvInfoMp
              _bindLIconstMp :: ConstMp
              _bindLIentryNms :: (Seq.FastSeq GB.EntryInfo)
              _bindLIgathFviMp :: FvInfoMp
              _bindLIgbState :: (GB.GBState)
              _bindLIi :: (GB.InsSeq)
              _bindLIincludeS :: (Set.Set String)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 105, column 17)
              _moduleNmAlpha =
                  hsnShowAlphanumeric moduleNm_
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 106, column 33)
              _globalLOconstMp =
                  Map.empty
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 120, column 17)
              _gbState =
                  GB.emptyGBState
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 347, column 17)
              _vaGam =
                  Map.fromList $ map (\(n,o) -> (n,GB.Val_GlobEntry $ GB.eiEntryNr o)) $ Map.toList _entryNmMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 378, column 17)
              _nmEnv =
                  GB.NmEnv
                    { GB.neVAGam = _vaGam
                    , GB.neImpNmMp = _lhsIimpNmOffMpMp
                    , GB.neLamMp   = _lhsIlamMp
                    }
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 387, column 17)
              _errs =
                  let undefNms = (Map.keysSet _bindLIgathFviMp `Set.difference` Map.keysSet _vaGam)
                                   `Set.difference` offMpMpKeysSet _lhsIimpNmOffMpMp
                  in  if Set.null undefNms
                      then []
                      else [rngLift emptyRange mkErr_NamesNotIntrod "(Internal) GRIN ByteCode location" $ Set.toList undefNms]
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 638, column 33)
              __tup31 =
                  let entryL = zipWith (\i e -> e {GB.eiEntryNr = i}) [0..] $ Seq.toList _bindLIentryNms
                      mpMod = Map.fromList [ (GB.eiNm e, e) | e <- entryL ]
                      mpReExp = Map.fromList $ zip [ n | n <- Map.keys _lhsIexpNmOffMp, not (n `Map.member` mpMod) ] [Map.size mpMod ..]
                  in  ( mpMod, entryL
                      , map GB.eiIsCAF entryL
                      )
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 638, column 33)
              (_entryNmMp,_,_) =
                  __tup31
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 638, column 33)
              (_,_entryL,_) =
                  __tup31
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 638, column 33)
              (_,_,_isCafL) =
                  __tup31
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 647, column 17)
              _mainEntry =
                  maybe 0 GB.vaEntryInx $ GB.nmEnvLookup hsnMain _nmEnv
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 657, column 17)
              _lhsOgbMod =
                  GB.Module_Mod _moduleNmAlpha
                    [ (n,hsnShowAlphanumeric n) | n <- _lhsIallImpNmL ]
                    (offMpKeysSorted _lhsIimpNmOffMpMp)
                    [ m | (n,_) <- sortOn snd $ Map.toList _lhsIexpNmOffMp
                        , let m = panicJust ("ToGrinByteCode.GrModule.Mod: " ++ show n) (Map.lookup n _entryNmMp)
                    ]
                    _entryL
                    (Seq.toList _i)
                    [ c | (c,_) <- sortOn snd $ Map.toList _bindLIconstMp ]
                    [ i | (i,True) <- zip [0..] _isCafL ]
                    _mainEntry
                    (Set.toList _bindLIincludeS)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 673, column 17)
              _i =
                  Seq.fromList
                     [ GB.halt
                     ]
                  Seq.:++: _bindLIi
              -- copy rule (from local)
              _lhsOerrs =
                  _errs
              -- copy rule (chain)
              _bindLOconstMp =
                  _globalLIconstMp
              -- copy rule (from local)
              _bindLOgbState =
                  _gbState
              -- copy rule (down)
              _bindLOimpNmOffMpMp =
                  _lhsIimpNmOffMpMp
              -- copy rule (down)
              _bindLOlamMp =
                  _lhsIlamMp
              -- copy rule (down)
              _bindLOopts =
                  _lhsIopts
              -- copy rule (from local)
              _bindLOvaGam =
                  _vaGam
              ( _globalLIconstMp,_globalLIgathFviMp) =
                  globalL_ _globalLOconstMp 
              ( _bindLIconstMp,_bindLIentryNms,_bindLIgathFviMp,_bindLIgbState,_bindLIi,_bindLIincludeS) =
                  bindL_ _bindLOconstMp _bindLOgbState _bindLOimpNmOffMpMp _bindLOlamMp _bindLOopts _bindLOvaGam 
          in  ( _lhsOerrs,_lhsOgbMod)))
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
         tag                  : Int
   alternatives:
      alternative LitInt:
         child int            : {Int}
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
      alternative Otherwise:
      alternative Tag:
         child tag            : GrTag 
-}
-- cata
sem_GrPatAlt :: GrPatAlt  ->
                T_GrPatAlt 
sem_GrPatAlt (GrPatAlt_LitInt _int )  =
    (sem_GrPatAlt_LitInt _int )
sem_GrPatAlt (GrPatAlt_Node _tag _fldL )  =
    (sem_GrPatAlt_Node (sem_GrTag _tag ) _fldL )
sem_GrPatAlt (GrPatAlt_NodeSplit _tag _nm _fldL )  =
    (sem_GrPatAlt_NodeSplit (sem_GrTag _tag ) _nm (sem_GrSplitL _fldL ) )
sem_GrPatAlt (GrPatAlt_Otherwise )  =
    (sem_GrPatAlt_Otherwise )
sem_GrPatAlt (GrPatAlt_Tag _tag )  =
    (sem_GrPatAlt_Tag (sem_GrTag _tag ) )
-- semantic domain
type T_GrPatAlt  = ConstMp ->
                   ( ConstMp,FvInfoMp,([HsName]),Int)
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 150, column 25)
              _lhsOtag =
                  int_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL,_lhsOtag)))
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (\ _lhsIconstMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _lhsOtag :: Int
              _tagOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 4, column 17)
              _lhsOintroNmL =
                  fldL_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tagIconstMp
              -- copy rule (up)
              _lhsOtag =
                  _tagItag
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL,_lhsOtag)))
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _tagOconstMp :: ConstMp
              _fldLOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              _fldLIconstMp :: ConstMp
              _fldLIgathFviMp :: FvInfoMp
              _fldLIintroNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 153, column 33)
              _lhsOtag =
                  panic "ToGrinByteCode.GrPatAlt.*-LitInt(etc).tag"
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 5, column 17)
              _lhsOintroNmL =
                  nm_ : _fldLIintroNmL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp `fviMpUnion` _fldLIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _fldLIconstMp
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _fldLOconstMp =
                  _tagIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
              ( _fldLIconstMp,_fldLIgathFviMp,_fldLIintroNmL) =
                  fldL_ _fldLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL,_lhsOtag)))
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 151, column 17)
              _lhsOtag =
                  -1
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL,_lhsOtag)))
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              _lhsOtag :: Int
              _tagOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (up)
              _lhsOconstMp =
                  _tagIconstMp
              -- copy rule (up)
              _lhsOtag =
                  _tagItag
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL,_lhsOtag)))
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         primArgForIntl       : Bool
         primResForIntl       : Bool
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         i                    : GB.InsSeq
         introNmL             : [HsName]
         mbi                  : Maybe (GB.InsSeq,BasicSize,GB.StackState)
         nmIntro              : NmIntro
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative Empty:
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative EnumNode:
         child nm             : {HsName}
      alternative OpaqueAnnot:
         child nm             : {HsName}
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrVarL 
-}
-- cata
sem_GrPatLam :: GrPatLam  ->
                T_GrPatLam 
sem_GrPatLam (GrPatLam_BasicAnnot _annot _nm )  =
    (sem_GrPatLam_BasicAnnot _annot _nm )
sem_GrPatLam (GrPatLam_BasicNode _annot _nm )  =
    (sem_GrPatLam_BasicNode _annot _nm )
sem_GrPatLam (GrPatLam_Empty )  =
    (sem_GrPatLam_Empty )
sem_GrPatLam (GrPatLam_EnumAnnot _tycon _nm )  =
    (sem_GrPatLam_EnumAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_EnumNode _nm )  =
    (sem_GrPatLam_EnumNode _nm )
sem_GrPatLam (GrPatLam_OpaqueAnnot _nm )  =
    (sem_GrPatLam_OpaqueAnnot _nm )
sem_GrPatLam (GrPatLam_OpaqueNode _nm )  =
    (sem_GrPatLam_OpaqueNode _nm )
sem_GrPatLam (GrPatLam_PtrAnnot _tycon _nm )  =
    (sem_GrPatLam_PtrAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_PtrNode _nm )  =
    (sem_GrPatLam_PtrNode _nm )
sem_GrPatLam (GrPatLam_Var _nm )  =
    (sem_GrPatLam_Var _nm )
sem_GrPatLam (GrPatLam_VarNode _fldL )  =
    (sem_GrPatLam_VarNode (sem_GrVarL _fldL ) )
-- semantic domain
type T_GrPatLam  = ConstMp ->
                   Bool ->
                   Bool ->
                   ( ConstMp,FvInfoMp,(GB.InsSeq),([HsName]),(Maybe (GB.InsSeq,BasicSize,GB.StackState)),NmIntro)
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 307, column 17)
              _lhsOnmIntro =
                  let ann = case annot_ of
                              BasicAnnot_Size _ _ BasicAnnotTagging_ToPtr _
                                | _lhsIprimResForIntl -> BasicAnnot_Dflt
                              _                       -> annot_
                  in  NmIntro_Single nm_ ann
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 305, column 17)
              _lhsOnmIntro =
                  NmIntro_GrpBasic nm_ annot_
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 712, column 17)
              _lhsOmbi =
                  let basicSize = grinBasicAnnotSize annot_
                  in  case annot_ of
                        BasicAnnot_Size bsz _ BasicAnnotTagging_FromPtr sgn
                          | not _lhsIprimArgForIntl
                            -> Just (Seq.fromList [GB.untag2 sgn                            ],basicSize,GB.ststPatchTOSGCNot)
                        _   -> Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 317, column 17)
              _lhsOnmIntro =
                  NmIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 312, column 17)
              _lhsOnmIntro =
                  let ann = if _lhsIprimResForIntl
                            then BasicAnnot_Dflt
                            else BasicAnnot_Size basicSizeWord BasicTy_Word BasicAnnotTagging_ToPtr False
                  in  NmIntro_Single nm_ ann
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 306, column 17)
              _lhsOnmIntro =
                  NmIntro_GrpTag nm_
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 718, column 17)
              _lhsOmbi =
                  if _lhsIprimArgForIntl
                  then Nothing
                  else Just ( Seq.fromList [GB.untag2 False]
                            , grinBasicAnnotSize BasicAnnot_Dflt
                            , GB.ststPatchTOSGCNot
                            )
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 317, column 17)
              _lhsOnmIntro =
                  NmIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 317, column 17)
              _lhsOnmIntro =
                  NmIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 317, column 17)
              _lhsOnmIntro =
                  NmIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 317, column 17)
              _lhsOnmIntro =
                  NmIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 11, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 303, column 17)
              _lhsOnmIntro =
                  NmIntro_Single nm_ BasicAnnot_Dflt
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 8, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (\ _lhsIconstMp
       _lhsIprimArgForIntl
       _lhsIprimResForIntl ->
         (let _lhsOnmIntro :: NmIntro
              _lhsOmbi :: (Maybe (GB.InsSeq,BasicSize,GB.StackState))
              _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOi :: (GB.InsSeq)
              _lhsOconstMp :: ConstMp
              _fldLOconstMp :: ConstMp
              _fldLIconstMp :: ConstMp
              _fldLIgathFviMp :: FvInfoMp
              _fldLIintroNmL :: ([HsName])
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 304, column 17)
              _lhsOnmIntro =
                  NmIntro_Grp (panic "GrPatLam.VarNode.grpHead") (tail _fldLIintroNmL)
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 725, column 17)
              _lhsOmbi =
                  Nothing
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 9, column 17)
              _lhsOintroNmL =
                  tail _fldLIintroNmL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _fldLIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 670, column 41)
              _lhsOi =
                  Seq.empty
              -- copy rule (up)
              _lhsOconstMp =
                  _fldLIconstMp
              -- copy rule (down)
              _fldLOconstMp =
                  _lhsIconstMp
              ( _fldLIconstMp,_fldLIgathFviMp,_fldLIintroNmL) =
                  fldL_ _fldLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOi,_lhsOintroNmL,_lhsOmbi,_lhsOnmIntro)))
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = ConstMp ->
                  ( ConstMp,FvInfoMp,([HsName]))
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (\ _lhsIconstMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _offOconstMp :: ConstMp
              _offIconstMp :: ConstMp
              _offIgathFviMp :: FvInfoMp
              _offIgrvalIntro :: (GB.GrValIntro)
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 19, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _offIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _offIconstMp
              -- copy rule (down)
              _offOconstMp =
                  _lhsIconstMp
              ( _offIconstMp,_offIgathFviMp,_offIgrvalIntro) =
                  off_ _offOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
      alternative Nil:
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = ConstMp ->
                   ( ConstMp,FvInfoMp,([HsName]))
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _hdIintroNmL :: ([HsName])
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              _tlIintroNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  _hdIintroNmL ++ _tlIintroNmL
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp,_hdIintroNmL) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp,_tlIintroNmL) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
-- GrTag -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         self                 : SELF 
         tag                  : Int
   alternatives:
      alternative App:
         child nm             : {HsName}
         visit 0:
            local self        : _
      alternative Con:
         child grtgAnn        : {GrTagAnn}
         child int            : {Int}
         child nm             : {HsName}
         visit 0:
            local self        : _
      alternative Fun:
         child nm             : {HsName}
         visit 0:
            local self        : _
      alternative Hole:
         visit 0:
            local self        : _
      alternative PApp:
         child needs          : {Int}
         child nm             : {HsName}
         visit 0:
            local self        : _
      alternative Rec:
         visit 0:
            local self        : _
      alternative Unboxed:
         visit 0:
            local self        : _
-}
-- cata
sem_GrTag :: GrTag  ->
             T_GrTag 
sem_GrTag (GrTag_App _nm )  =
    (sem_GrTag_App _nm )
sem_GrTag (GrTag_Con _grtgAnn _int _nm )  =
    (sem_GrTag_Con _grtgAnn _int _nm )
sem_GrTag (GrTag_Fun _nm )  =
    (sem_GrTag_Fun _nm )
sem_GrTag (GrTag_Hole )  =
    (sem_GrTag_Hole )
sem_GrTag (GrTag_PApp _needs _nm )  =
    (sem_GrTag_PApp _needs _nm )
sem_GrTag (GrTag_Rec )  =
    (sem_GrTag_Rec )
sem_GrTag (GrTag_Unboxed )  =
    (sem_GrTag_Unboxed )
-- semantic domain
type T_GrTag  = ConstMp ->
                ( ConstMp,FvInfoMp,GrTag ,Int)
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _self =
                  GrTag_App nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 156, column 25)
              _lhsOtag =
                  int_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _self =
                  GrTag_Con grtgAnn_ int_ nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 4, column 17)
              _lhsOgathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- self rule
              _self =
                  GrTag_Fun nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _self =
                  GrTag_Hole
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 4, column 17)
              _lhsOgathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- self rule
              _self =
                  GrTag_PApp needs_ nm_
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _self =
                  GrTag_Rec
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (\ _lhsIconstMp ->
         (let _lhsOtag :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOself :: GrTag 
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 157, column 25)
              _lhsOtag =
                  panic "ToGrinByteCode.GrTag.*-Con.tag"
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- self rule
              _self =
                  GrTag_Unboxed
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOself,_lhsOtag)))
-- GrTagL ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrTag 
         child tl             : GrTagL 
      alternative Nil:
-}
-- cata
sem_GrTagL :: GrTagL  ->
              T_GrTagL 
sem_GrTagL list  =
    (Prelude.foldr sem_GrTagL_Cons sem_GrTagL_Nil (Prelude.map sem_GrTag list) )
-- semantic domain
type T_GrTagL  = ConstMp ->
                 ( ConstMp,FvInfoMp)
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _hdIself :: GrTag 
              _hdItag :: Int
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp,_hdIself,_hdItag) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrType ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Arrow:
         child args           : GrTypeBaseL 
         child res            : GrTypeBase 
      alternative None:
-}
-- cata
sem_GrType :: GrType  ->
              T_GrType 
sem_GrType (GrType_Arrow _args _res )  =
    (sem_GrType_Arrow (sem_GrTypeBaseL _args ) (sem_GrTypeBase _res ) )
sem_GrType (GrType_None )  =
    (sem_GrType_None )
-- semantic domain
type T_GrType  = ConstMp ->
                 ( ConstMp,FvInfoMp)
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _argsOconstMp :: ConstMp
              _resOconstMp :: ConstMp
              _argsIconstMp :: ConstMp
              _argsIgathFviMp :: FvInfoMp
              _resIconstMp :: ConstMp
              _resIgathFviMp :: FvInfoMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _argsIgathFviMp `fviMpUnion` _resIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _resIconstMp
              -- copy rule (down)
              _argsOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _resOconstMp =
                  _argsIconstMp
              ( _argsIconstMp,_argsIgathFviMp) =
                  args_ _argsOconstMp 
              ( _resIconstMp,_resIgathFviMp) =
                  res_ _resOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Node:
      alternative Pointer:
-}
-- cata
sem_GrTypeBase :: GrTypeBase  ->
                  T_GrTypeBase 
sem_GrTypeBase (GrTypeBase_Node )  =
    (sem_GrTypeBase_Node )
sem_GrTypeBase (GrTypeBase_Pointer )  =
    (sem_GrTypeBase_Pointer )
-- semantic domain
type T_GrTypeBase  = ConstMp ->
                     ( ConstMp,FvInfoMp)
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attribute:
         gathFviMp            : FvInfoMp
   alternatives:
      alternative Cons:
         child hd             : GrTypeBase 
         child tl             : GrTypeBaseL 
      alternative Nil:
-}
-- cata
sem_GrTypeBaseL :: GrTypeBaseL  ->
                   T_GrTypeBaseL 
sem_GrTypeBaseL list  =
    (Prelude.foldr sem_GrTypeBaseL_Cons sem_GrTypeBaseL_Nil (Prelude.map sem_GrTypeBase list) )
-- semantic domain
type T_GrTypeBaseL  = ConstMp ->
                      ( ConstMp,FvInfoMp)
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp)))
-- GrVal -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         grvalIntro           : GB.GrValIntro
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
      alternative Empty:
      alternative EnumNode:
         child nm             : {HsName}
      alternative LitInt:
         child int            : {Int}
      alternative LitStr:
         child str            : {String}
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Tag:
         child tag            : GrTag 
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrValL 
-}
-- cata
sem_GrVal :: GrVal  ->
             T_GrVal 
sem_GrVal (GrVal_BasicNode _tag _nm )  =
    (sem_GrVal_BasicNode (sem_GrTag _tag ) _nm )
sem_GrVal (GrVal_Empty )  =
    (sem_GrVal_Empty )
sem_GrVal (GrVal_EnumNode _nm )  =
    (sem_GrVal_EnumNode _nm )
sem_GrVal (GrVal_LitInt _int )  =
    (sem_GrVal_LitInt _int )
sem_GrVal (GrVal_LitStr _str )  =
    (sem_GrVal_LitStr _str )
sem_GrVal (GrVal_Node _tag _fldL )  =
    (sem_GrVal_Node (sem_GrTag _tag ) (sem_GrValL _fldL ) )
sem_GrVal (GrVal_NodeAdapt _nm _fldL )  =
    (sem_GrVal_NodeAdapt _nm (sem_GrAdaptL _fldL ) )
sem_GrVal (GrVal_OpaqueNode _nm )  =
    (sem_GrVal_OpaqueNode _nm )
sem_GrVal (GrVal_PtrNode _nm )  =
    (sem_GrVal_PtrNode _nm )
sem_GrVal (GrVal_Tag _tag )  =
    (sem_GrVal_Tag (sem_GrTag _tag ) )
sem_GrVal (GrVal_Var _nm )  =
    (sem_GrVal_Var _nm )
sem_GrVal (GrVal_VarNode _fldL )  =
    (sem_GrVal_VarNode (sem_GrValL _fldL ) )
-- semantic domain
type T_GrVal  = ConstMp ->
                ( ConstMp,FvInfoMp,(GB.GrValIntro))
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _tagOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 401, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Basic _tagIself nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tagIconstMp
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 402, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Enum  nm_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 398, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Int   (toInteger int_)
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 399, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Str   str_
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _tagOconstMp :: ConstMp
              _fldLOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              _fldLIconstMp :: ConstMp
              _fldLIgathFviMp :: FvInfoMp
              _fldLIgrvalIntroL :: ([GB.GrValIntro])
              _fldLIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 400, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Grp   _tagIself _fldLIgrvalIntroL
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp `fviMpUnion` _fldLIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _fldLIconstMp
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _fldLOconstMp =
                  _tagIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
              ( _fldLIconstMp,_fldLIgathFviMp,_fldLIgrvalIntroL,_fldLIlength) =
                  fldL_ _fldLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _fldLOconstMp :: ConstMp
              _fldLIconstMp :: ConstMp
              _fldLIgathFviMp :: FvInfoMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 8, column 17)
              _lhsOgathFviMp =
                  fviMpUnions [fviMpSingleton nm_, _fldLIgathFviMp]
              -- copy rule (up)
              _lhsOconstMp =
                  _fldLIconstMp
              -- copy rule (down)
              _fldLOconstMp =
                  _lhsIconstMp
              ( _fldLIconstMp,_fldLIgathFviMp) =
                  fldL_ _fldLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _tagOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tagIconstMp
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 397, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_Nm    nm_
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 7, column 17)
              _lhsOgathFviMp =
                  fviMpSingleton' FvUse_Val nm_
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntro :: (GB.GrValIntro)
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _fldLOconstMp :: ConstMp
              _fldLIconstMp :: ConstMp
              _fldLIgathFviMp :: FvInfoMp
              _fldLIgrvalIntroL :: ([GB.GrValIntro])
              _fldLIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 404, column 17)
              _lhsOgrvalIntro =
                  GB.GrValIntro_None
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _fldLIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _fldLIconstMp
              -- copy rule (down)
              _fldLOconstMp =
                  _lhsIconstMp
              ( _fldLIconstMp,_fldLIgathFviMp,_fldLIgrvalIntroL,_fldLIlength) =
                  fldL_ _fldLOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntro)))
-- GrValL ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         grvalIntroL          : [GB.GrValIntro]
         length               : Int
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
      alternative Nil:
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = ConstMp ->
                 ( ConstMp,FvInfoMp,([GB.GrValIntro]),Int)
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntroL :: ([GB.GrValIntro])
              _lhsOlength :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _hdIgrvalIntro :: (GB.GrValIntro)
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              _tlIgrvalIntroL :: ([GB.GrValIntro])
              _tlIlength :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 409, column 17)
              _lhsOgrvalIntroL =
                  _hdIgrvalIntro : _tlIgrvalIntroL
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 410, column 17)
              _lhsOlength =
                  1              + _tlIlength
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp,_hdIgrvalIntro) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp,_tlIgrvalIntroL,_tlIlength) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntroL,_lhsOlength)))
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgrvalIntroL :: ([GB.GrValIntro])
              _lhsOlength :: Int
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 407, column 17)
              _lhsOgrvalIntroL =
                  []
              -- "build/101/lib-ehc/EH101/GrinCode/ToGrinByteCode.ag"(line 408, column 17)
              _lhsOlength =
                  0
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOgrvalIntroL,_lhsOlength)))
-- GrVar -------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Ignore:
      alternative KnownTag:
         child tag            : GrTag 
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_GrVar :: GrVar  ->
             T_GrVar 
sem_GrVar (GrVar_Ignore )  =
    (sem_GrVar_Ignore )
sem_GrVar (GrVar_KnownTag _tag )  =
    (sem_GrVar_KnownTag (sem_GrTag _tag ) )
sem_GrVar (GrVar_Var _nm )  =
    (sem_GrVar_Var _nm )
-- semantic domain
type T_GrVar  = ConstMp ->
                ( ConstMp,FvInfoMp,([HsName]))
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (\ _lhsIconstMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 16, column 17)
              _lhsOintroNmL =
                  [ ]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (\ _lhsIconstMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              _tagOconstMp :: ConstMp
              _tagIconstMp :: ConstMp
              _tagIgathFviMp :: FvInfoMp
              _tagIself :: GrTag 
              _tagItag :: Int
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 15, column 17)
              _lhsOintroNmL =
                  [ error "introNmL known tag" ]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _tagIgathFviMp
              -- copy rule (up)
              _lhsOconstMp =
                  _tagIconstMp
              -- copy rule (down)
              _tagOconstMp =
                  _lhsIconstMp
              ( _tagIconstMp,_tagIgathFviMp,_tagIself,_tagItag) =
                  tag_ _tagOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (\ _lhsIconstMp ->
         (let _lhsOintroNmL :: ([HsName])
              _lhsOgathFviMp :: FvInfoMp
              _lhsOconstMp :: ConstMp
              -- "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 14, column 17)
              _lhsOintroNmL =
                  [nm_]
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      chained attribute:
         constMp              : ConstMp
      synthesized attributes:
         gathFviMp            : FvInfoMp
         introNmL             : [HsName]
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
      alternative Nil:
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = ConstMp ->
                 ( ConstMp,FvInfoMp,([HsName]))
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              _hdOconstMp :: ConstMp
              _tlOconstMp :: ConstMp
              _hdIconstMp :: ConstMp
              _hdIgathFviMp :: FvInfoMp
              _hdIintroNmL :: ([HsName])
              _tlIconstMp :: ConstMp
              _tlIgathFviMp :: FvInfoMp
              _tlIintroNmL :: ([HsName])
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  _hdIgathFviMp `fviMpUnion` _tlIgathFviMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  _hdIintroNmL ++ _tlIintroNmL
              -- copy rule (up)
              _lhsOconstMp =
                  _tlIconstMp
              -- copy rule (down)
              _hdOconstMp =
                  _lhsIconstMp
              -- copy rule (chain)
              _tlOconstMp =
                  _hdIconstMp
              ( _hdIconstMp,_hdIgathFviMp,_hdIintroNmL) =
                  hd_ _hdOconstMp 
              ( _tlIconstMp,_tlIgathFviMp,_tlIintroNmL) =
                  tl_ _tlOconstMp 
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (\ _lhsIconstMp ->
         (let _lhsOgathFviMp :: FvInfoMp
              _lhsOintroNmL :: ([HsName])
              _lhsOconstMp :: ConstMp
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonFreeVar.ag"(line 1, column 33)
              _lhsOgathFviMp =
                  Map.empty
              -- use rule "build/101/lib-ehc/EH101/GrinCode/Trf/CommonIntroName.ag"(line 1, column 30)
              _lhsOintroNmL =
                  []
              -- copy rule (chain)
              _lhsOconstMp =
                  _lhsIconstMp
          in  ( _lhsOconstMp,_lhsOgathFviMp,_lhsOintroNmL)))