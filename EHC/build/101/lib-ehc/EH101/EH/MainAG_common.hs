

module EH101.EH.MainAG_common where

import Data.Char
import Data.List as List
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts
import EH101.Gam.Full
import EH101.Error
import EH101.Error.Pretty
import EH101.EH
import EH101.Ty.Pretty
import EH101.Ty.FitsInCommon
import EH101.Ty.FitsIn
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import EH101.VarMp
import EH101.Substitutable
import Data.Maybe
import EH101.Ty.Utils1
import EH101.Ty.Trf.Quantify
import EH101.Ty.Trf.Instantiate
import EH101.Ty
import EH101.Base.Debug as Debug
import Debug.Trace
import EH101.Ty.FitsInCommon2
import EH101.Ty.FIEnv2
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import EH101.Ty.Trf.FreshVar
import EH101.Ty.Ftv
import EH.Util.Utils (groupSortOn)
import Control.Applicative ((<|>))
import EH101.AbstractCore
import EH101.AbstractCore.Utils
import EH101.Core
import EH101.Core.FFI
import EH101.Core.Utils
import EH101.Core.Pretty
import EH101.Foreign.Extract
import EH101.LamInfo
import Control.Monad.State
import EH101.Ty.Utils2
import EH101.Base.Target
import EH101.Core.Subst
import EH101.Core.Coercion
import EH101.Ty.Trf.MergePreds
import EH101.Ty.Trf.Canonic
import EH101.Pred
import EH101.Pred.RedGraph (redPruneReductionsUntil)
import EH101.CHR
import EH101.CHR.Constraint
import EH101.Pred.CHR
import EH101.Pred.ToCHR
import EH101.Pred.Heuristics
import EH101.CHR.Solve
import EH101.Pred.EvidenceToCore
import EH101.Gam.ClassDefaultGam
import EH101.Ty.Trf.BetaReduce (tyBetaRedFull)
import EH101.Module
import EH101.Ty.UsedNames
import EH101.BuiltinPrims
import EH101.Foreign
import EH101.Foreign
import EH101.Foreign.Pretty
import EH101.Deriving
import EH101.Generics
import EH101.VarMp.Utils





































































type RangeMp = Map.Map Pred [Range]

cnstrMpToRangeMp :: CHRPredOccCnstrMp -> RangeMp
cnstrMpToRangeMp m = Map.unionsWith (++) [ Map.singleton (cpoPr p) [cpoRange p] | (Prove p) <- Map.keys m ]



-- | split a list of predicates into non-ambiguous & ambiguous, using quantifications results tqoGam,
--   which (a.o.) administers which predicates could be merged for quantification, those not are then ambiguous
doPredAmbigSplit :: (x -> PredOcc) -> TQOGam -> [x] -> ([x],[x])
doPredAmbigSplit get tqoGam prOccL
  = partition (\o -> poPoi (get o) `Set.member` assumedByQuant) prOccL
  where assumedByQuant = Set.unions $ map tmpoInsPrIdSet $ gamElts tqoGam



type DoFit = FIOpts -> VarMp -> FIOut



mkFreshAppImTy :: UID -> (Ty -> Ty) -> (Ty,Ty,Ty)
mkFreshAppImTy u mkr
  = (im,rt,[im] `mkArrow` rt)
  where [i,r] = mkNewUIDL 2 u
        im = mkImplsVar i
        rt = mkr $ mkTyVar r



mkFreshLamImTy :: UID -> (Ty,Ty,Ty,Ty)
mkFreshLamImTy u
  = (im,at,rt,[im,at] `mkArrow` rt)
  where [i,a,r] = mkNewUIDL 3 u
        im = mkImplsVar i
        at = mkTyVar a
        rt = mkTyVar r



dfCheck :: UID -> FIOpts -> FIEnv -> HsName -> ValGam -> VarMp -> Ty -> (Ty,Ty,Ty,FIOut,[Err])
dfCheck uniq fiOpts fe fldNm valGam knTyVarMp knTy
  = (gTy,knDataTy,knFldTy,fo,nmErrs)
  where (gTy,nmErrs) = valGamLookupTy (hsnFldUpd fldNm) valGam
        (_,u1,u2)    = mkNewLevUID2 uniq
        (knFldTy,knDataTy,knFldUpdTy)
                     = (e,df,v `mkArrow` knTy)
                     where v@[e,df] = mkNewTyVarL 2 u1
        fo           = fitsIn fiOpts fe u2 knTyVarMp gTy knFldUpdTy



tyGamLookupOrAdd :: UID -> HsName -> TyGam -> TyGam -> (TyGamInfo,TyGam)
tyGamLookupOrAdd uniq nm tyGamLkup tyGam
  =  case tyGamLookup nm tyGamLkup of
       Nothing    ->  let  t    =  mkNewTyVar uniq
                           tgi  =  mkTGI t
                      in   (tgi,gamAdd nm tgi tyGam)
       Just tgi   ->  (tgi,tyGam)



tyKiGamLookupOrAdd :: UID -> Ty -> TyKiGam -> (TyKiGamInfo,TyKiGam,TyKiGam)
tyKiGamLookupOrAdd uniq t tyKiGam
  =  case tyKiGamLookup t tyKiGam of
       Nothing    ->  (tkgi, newg `gamUnion` tyKiGam, newg)
                  where tkgi  =  TyKiGamInfo (mkNewTyVar uniq) -- t
                        newg  =  tyKiGamSingleton t tkgi
       Just tkgi  ->  (tkgi,tyKiGam,emptyGam)



tvGathFlowOut :: Ord k => Gam k v -> Gam k v -> Gam k v
tvGathFlowOut lhsG insideG
  = lhsG'
  where  (l,g)     = gamPop insideG
         lhsG'     = gamAddGam l lhsG

tvGathFlowIn :: Ord k => Gam k v -> Gam k v -> Gam k v
tvGathFlowIn newG lhsG = gamPushNew (gamPushGam newG lhsG)



gamErrToL :: Range -> Gam HsName ErrL -> ErrL
gamErrToL r = concat . map (\(n,e) -> mkNestErr r (pp n) e) . gamToAssocL



ppErrsSq :: ErrSq -> PP_Doc
ppErrsSq = ppErrs . Seq.toList



checkClNms :: Range -> HsNameS -> [Err]
checkClNms r s = if Set.null s then [] else [mkErr_NamesNotIntrod r "class" (Set.toList s)]



mkRangedPred :: RangeMp -> Pred -> (Pred,[Range])
mkRangedPred rm pr = (pr,Map.findWithDefault [] pr rm)



mkPrvErr :: RangeMp -> Range -> CHRPredOccCnstrTraceMp -> [Err]
mkPrvErr rm r m
  = if Map.null m
    then []
    else [Err_NotProvenPreds r [ (mkRangedPred rm pr, vlist $ map pp t) | (c,((_,t):_)) <- Map.toList m, let pr = cpoPr (cnstrPred c) ]]



infixr 2 >###<      -- same as >-<

(>###<) :: PP_Doc -> PP_Doc -> PP_Doc
l >###< r = l >-< indent 2 r



mkWeaveExpr :: EHCOpts -> UID -> VarMp -> CSubst -> (CExpr->CExpr) -> CExpr -> [Coe] -> [Coe] -> (CExpr,CSubst)
mkWeaveExpr opts uniq finTyVarMp csubst postmk expr lCoeL rCoeL
  = (postmk e, s1 `cSubstApp` s2)
  where (_,u1,u2) = mkNewLevUID2 uniq
        (ww,s1)   = coeWipeWeaveAsSubst2 opts u1 finTyVarMp csubst lCoeL rCoeL
        (e ,s2)   = coeEvalOnAsSubst u2 ww expr



-- | Make bindings for constructors, given various bits of info
mkDataCBindL :: EHCOpts -> CTag -> Ty -> [DataConFldAnnInfo] -> HsName -> Bool -> [CBind]
mkDataCBindL opts ctag ty annL conNm isNewtype
  | isNewtype = [ acoreBind1Ty conNm ty (acoreVar $ ehcOptBuiltin opts ehbnId) ]
  | otherwise = [ acoreBind1Ty conNm ty
                    (acoreLam nms
                      (foldr (.) id mkStrict
                        (acoreApp (CExpr_Tup ctag)
                          (map acoreVar nms'
                    ) ) ) )
                ]
  where nms = (map (\i -> hsnFromString ("x" ++ show i)) [1 .. ctagArity ctag])
        (nms',mkStrict)
          = (nms, replicate (ctagArity ctag) id)
          where len = ctagArity ctag
{-
          = unzip [ if s then (n',\e -> acoreLet1StrictIn n' (acoreVar n) (const e)) else (n,id)
                  | (n,s) <- zip nms $ map ((==Strictness_Strict) . dcfaiStrictness) annL
                  , let n' = hsnUniqifyEval n
                  ]
-}



-- | Generate all bindings given functions to generate individual bindings.
mkDataBinds
  :: (Bool -> DataTagInfo -> [bind])
     -> DataGam
     -> [bind]
mkDataBinds mkCon dataGam
  = concat
      [ concat
          [ mkCon isNewtype dti
          | dti <- Map.elems constrMp
          ]
      | dgi@(DataGamInfo
              { dgiConstrTagMp  = constrMp
              }) <- gamElts dataGam
      , let isNewtype = isJust (dgiMbNewtype dgi)
      ]



fsL2PatOffsetL :: FieldSplitL -> AssocL RPatFld (Maybe Int)
fsL2PatOffsetL l = [ (RPatFld_Fld n oe n p,Just oi) | (o,(foff,p)) <- zip [0..] l, let (oi,oe) = foffMkOff foff o, let n = foffLabel foff ]



mkLetCBodyBindL :: Bool -> CBindCateg -> [(CBindCateg,CBindL)] -> [(CBindCateg,CBindL)] -> [(CBindCateg,CBindL)] -> CExpr -> ([(CBindCateg,CBindL)],CExpr)
mkLetCBodyBindL isStrict howToBind locBindL locOthBindL bodyBindL body
  = if isStrict
    then (locOthBindL,foldr (\(c,b) e -> acoreLet c b e) body (merge $ locBindL ++ bodyBindL))
    else (merge $ locOthBindL ++ locBindL ++ bodyBindL, body)
  where merge (cb1@(c1,b1) : tl)
          = case merge tl of
              (c2,b2) : tl' | c1 == c2 && c1 == CBindCateg_Rec
                  -> (c1,b1++b2) : tl'
              tl' -> cb1 : tl'
        merge [] = []



cdictOffset :: HsName -> Ty -> Int
cdictOffset n r = tyRecOffset n $ r



-- | Make the various names required for instance building
mkInstanceNames
  :: HsName
     -> Ty
     -> ( AssocL HsName Ty
        , [HsName]
        , [HsName]
        , [CExpr]
        )
mkInstanceNames
     dictNm instanceRecTy
  = (instanceFieldL,memberNames,memberNewNames,memberNewVars)
  where instanceFieldL  = snd $ tyRecExts instanceRecTy
        memberNames     = assocLKeys $ tyRowCanonOrder instanceFieldL
        memberNewNames  = [ hsnUniqifyStr HsNameUniqifier_DictField (show n) dictNm | n <- memberNames ]
        memberNewVars   = map acoreVar memberNewNames



-- | Make a dictionary corresponding to an instance declaration
mkNormalInstance
  :: EHCOpts
     -> ClGamInfo                           -- info about the class
     -> DataGam                             -- env/gamma for datas
     -> CTag                                -- tag of the dictionary (as data type)
     -> (HsName,HsName,HsName)              -- various names for the dictionary under construction at various stages of construction, also used externally
     -> Ty                                  -- instance dict as record
     -> ( [HsName]                          -- names of super class fields
        , [HsName]                          -- names of super class dicts, given as arg
        , [HsName]                          -- names of context class dicts, given as arg
        )
     -> [CBind]                             -- bindings for decls of the instance
     -> ([CBind]                            -- bindings for decls related context reduction, both Assume & Prove
        ,[CBind]                            -- bindings for super class related context reduction, only Prove
        ,[CBind]                            -- bindings for super + ctxt class related context reduction, only Assume
        )
     -> ( CExpr                             -- the instance
        , CMetaVal                          -- the meta info about the instance
        )
mkNormalInstance
      opts clgi dataGam dictTag
      (dictNm,_,dictBuildNm)
      instanceRecTy
      (superFldNmL,superNmL,contextNmL)
      declsCBindL
      (chrDeclsCBindL,chrSuperProveCBindL,chrSuperCtxtAssumeCBindL)
  = ( bind4
    , CMetaVal_DictInstance (TrackVarApply (ctagTyNm dictTag) [] : TrackVarApply dfltNm [] : instanceAndSuperTracks )
    )
  where dfltNm       = clgiDfltDictNm clgi
        rsltNm       = dictBuildNm
        rsltNmStrict = hsnUniqifyEval rsltNm
        dfltNmStrict = hsnUniqifyEval dfltNm

        dfltVar      = acoreVar dfltNm
        rsltVar      = acoreVar rsltNm
        rsltVarStrict= acoreVar rsltNmStrict
        dfltVarStrict= acoreVar dfltNmStrict

        (instanceFieldL,memberNames,memberNewNames,memberNewVars)
                            = mkInstanceNames dictNm instanceRecTy
        mkSetFlds           = mkInstanceSetFlds opts dataGam dictTag instanceRecTy

        instanceBindingPairs= [ (nm,v)
                              | CBind_Bind nm asps <- declsCBindL, CBound_Bind _ v <- asps
                              ]
        instanceMbBodies    = map (flip lookup instanceBindingPairs) memberNames
        instanceMbNewNames  = let f _ Nothing  = Nothing
                                  f n (Just e) = Just n
                              in zipWith f memberNewNames instanceMbBodies
        instanceMbNames     = let f _ Nothing  = Nothing
                                  f n (Just _) = Just n
                              in zipWith f memberNames instanceMbNewNames
        instanceNameMapping = [ (nm, CMetaVal_Val, v)
                              | (Just nm,v) <- zip instanceMbNames memberNewVars
                              ]
        instanceNewBindings = [ acoreBind1MetaTy nm CMetaVal_Val (acoreTyErr $ "EH.ToCore.mkNormalInstance.instanceNewBindings: " ++ show nm) v
                              | (nm,Just v) <- zip memberNewNames instanceMbBodies
                              ]

        superNewVars        = map acoreVar superNmL
        superNameMapping    = zip3 superFldNmL
                                   (repeat CMetaVal_Dict)
                                   superNewVars

        superNameMapping2   = zip superFldNmL
                                  superNmL
        superPairs          = map getBindLeftAndRightVar chrSuperProveCBindL

        doubleLookup :: HsName -> Maybe HsName
        doubleLookup nm   =  do { nm2 <- lookup nm superNameMapping2
                                -- ; nm3 <- lookup nm2 superPairs
                                ; return nm2 -- nm3
                                }

        superMbNewNames   =  map doubleLookup memberNames

        instanceAndSuperMbNewNames = let f (Just x) _ = Just x
                                         f _ (Just y) = Just y
                                         f _ _        = Nothing
                                     in zipWith f instanceMbNewNames superMbNewNames

        instanceAndSuperTracks = map mbNameToTrack instanceAndSuperMbNewNames

        instanceCHRBindings = chrSuperCtxtAssumeCBindL ++ chrSuperProveCBindL ++ chrDeclsCBindL

        dict1     = acoreApp1 dfltVar rsltVar
        bind1     = acoreBind1MetaTy dfltNmStrict CMetaVal_Dict (acoreTyErr $ "EH.ToCore.mkNormalInstance.bind1: " ++ show dfltNmStrict) dict1

        dict2     = acoreLetBase (acoreBindcategStrict) [bind1] (mkSetFlds (superNameMapping ++ instanceNameMapping) dfltVarStrict)
        bind2     = acoreBind1MetaTy rsltNmStrict (CMetaVal_Track TrackSelf) (acoreTyErr $ "EH.ToCore.mkNormalInstance.bind2: " ++ show rsltNmStrict) dict2

        dict3     = acoreLetBase (acoreBindcategPlain) [bind2] rsltVarStrict
        bind3     = acoreBind1MetaTy rsltNm       (CMetaVal_Track TrackSelf) (acoreTyErr $ "EH.ToCore.mkNormalInstance.bind3: " ++ show rsltNm) dict3

        dict4     = acoreLetRec ([bind3] ++ instanceNewBindings ++ instanceCHRBindings) rsltVar
        bind4     = acoreLam contextArguments dict4

        -- The above generates the following Core code as dict4:
        --
        -- letrec reslt = let rsltStrict = let! dfltStrict = dflt rslt
        --                                 in   case defltStrict of
        --                                        (Dict-C _1 _2 _3 _4) -> (Dict-C _1 inst2 _3 inst4)     -- in this example, field 2 and 4 are defined in the isntance, 1 and 3 are taken from the defaultdefinitions
        --                in  rsltStrict
        --        inst2 = ...
        --        inst4 = ...
        -- in reslt

        contextArguments = contextNmL



--  Make a dictionary for a normal instance, the default-definitions of a class, or for a derived instance
mkInstanceCBindL
  :: EHCOpts
     -> ClGamInfo                           -- info about the class
     -> DataGam                             -- env/gamma for datas
     -> InstVariant                         -- what kind of instance
     -> CTag                                -- tag of the dictionary (as data type)
     -> (HsName,HsName,HsName)              -- various names for the dictionary under construction at various stages of construction, also used externally
     -> Ty                                  -- instance dict as data type
     -> Ty                                  -- instance dict as record
     -> ( [HsName]                          -- names of super class fields
        , [HsName]                          -- names of super class dicts, given as arg
        , [HsName]                          -- names of context class dicts, given as arg
        )
     -> [CBind]                             -- bindings for decls of the instance
     -> ([CBind]                            -- bindings for decls related context reduction, both Assume & Prove
        ,[CBind]                            -- bindings for super class related context reduction, only Prove
        ,[CBind]                            -- bindings for super + ctxt class related context reduction, only Assume
        )
     -> [CBind]
mkInstanceCBindL
      opts clgi dataGam instVariant dictTag
      nms@(dictNm,dictSelfNm,dictBuildNm)
      instanceTy
      instanceRecTy
      nmLs@(superFldNmL,superNmL,contextNmL)
      declsCBindL
      chrBindLs@(chrDeclsCBindL,chrSuperProveCBindL,chrSuperCtxtAssumeCBindL)
  = [acoreBind1MetaTy dictNm generatedMeta instanceTy generatedDictionary]
  where (generatedDictionary, generatedMeta)
          = case instVariant of
               InstNormal     -> mkNormalD  declsCBindL
               InstDefault    -> mkDefaultD declsCBindL
               InstDeriving _ -> mkNormalD  declsCBindL
        rsltNm   = dictBuildNm
        rsltVar  = acoreVar rsltNm

        mkNormalD instanceBindings
          = mkNormalInstance
              opts clgi dataGam dictTag nms instanceRecTy nmLs
              instanceBindings chrBindLs

        -- Make a dictionary corresponding to (the default definitions in a) class definition
        mkDefaultD defaultBindings
          = let selfNm    = dictSelfNm

                (instanceFieldL,memberNames,memberNewNames,memberNewVars)
                                    = mkInstanceNames dictNm instanceRecTy

                lookupMeta n = if n `elem` superFldNmL then CMetaVal_Dict else CMetaVal_Val

                defaultBindingPairs = [ (nm,v)
                                      | CBind_Bind nm asps <- defaultBindings, CBound_Bind _ v <- asps
                                      ]
                defaultMbBodies     = map (flip lookup defaultBindingPairs) memberNames
                defaultMetas        = map lookupMeta                        memberNames
                defaultBodies       = map (maybe (acoreBuiltinUndefined opts) id) defaultMbBodies
                defaultMbNewNames   = let f _ Nothing  = Nothing
                                          f n (Just _) = Just n
                                      in zipWith f memberNewNames defaultMbBodies
                defaultTracks       = map mbNameToTrack defaultMbNewNames

                defaultNewBindings  = zipWith3 (\n m b -> acoreBind1MetaTy n m (acoreTyErr $ "EH.ToCore.mkInstanceCBindL.defaultNewBindings: " ++ show n) b)
                                                                memberNewNames
                                                                defaultMetas
                                                                defaultBodies
                defaultCHRBindings  = chrDeclsCBindL

                dict5     = acoreApp (CExpr_Tup dictTag) memberNewVars
                bind5     = acoreBind1MetaTy rsltNm CMetaVal_Dict (acoreTyErr $ "EH.ToCore.mkInstanceCBindL.bind5: " ++ show rsltNm) dict5

                dict6     = acoreLetRec ([bind5] ++ defaultNewBindings ++ defaultCHRBindings) rsltVar
                bind6     = acoreLam1 selfNm dict6

            in  ( bind6
                , CMetaVal_DictClass defaultTracks
                )



mkInstanceSetFlds :: EHCOpts -> DataGam -> CTag -> Ty -> [(HsName,CMetaVal,CExpr)] -> CExpr -> CExpr
mkInstanceSetFlds opts dataGam dictTag instanceRecTy
  = if   ehcCfgClassViaRec opts
    then mkSetFldsRec
    else mkSetFldsData
  where off n = cdictOffset n instanceRecTy
        mkSetFldsRec ts d0
          = foldl (\d (n,_,v) -> mkSet d n v) d0 ts
          where mkSet d n v = CExpr_TupUpd d dictTag n (acoreInt $ off n) v

        mkSetFldsData ts d0
          = acoreSatSelsCaseMetaTy env Nothing CMetaVal_Dict d0 dictTag nmLblOffL Nothing sel
          where env = (emptyRCEEnv opts) {rceDataGam = dataGam}
                fullListWithTrivialValues = [ (o, (o, (acoreVar n) ) )
                                            | (n,o) <- nmLblOffL
                                            ]
                sparseListWithInterestingValues = [ (off n, (v) )
                                                  | (n,m,v) <- ts
                                                  ]
                arity = ctagArity dictTag
                ns = take arity hsnLclSupply
                nmLblOffL = zip ns [0..]
                sel = acoreApp       (CExpr_Tup dictTag)
                                     (map snd
                                      $ listSaturateWith 0
                                                         (arity-1)
                                                         fst
                                                         fullListWithTrivialValues
                                                         sparseListWithInterestingValues
                                     )



getBindRightVar :: CBind -> Maybe HsName

getBindRightVar (CBind_Bind _ [CBound_Bind _ v]) = acoreExprMbVar v
getBindRightVar _ = Nothing


getBindLeftAndRightVar :: CBind -> (HsName,HsName)

getBindLeftAndRightVar (CBind_Bind nm1 [CBound_Bind _ (CExpr_Var ref2)]) = (nm1, acbrefNm ref2)
getBindLeftAndRightVar _ = error "getBindLeftAndRightVar: not a bind"

mbNameToTrack Nothing   = TrackNone
mbNameToTrack (Just nm) = TrackVarApply nm []




mkInstanceDerivingFldBind :: EHCOpts -> RCEEnv -> DataGam -> UID -> DataGamInfo -> Ty -> HsName -> CTag -> [[PredOcc]] -> [PredOcc] -> DerivClsFld -> CBind
mkInstanceDerivingFldBind opts rceEnv dataGam uniq derivDataDGI derivHdDataTy dictBuildNm ctag derivAltsPrOcc derivExtraPrOcc
  = mkf
  where rsltNm   = dictBuildNm
        rsltVar  = acoreVar rsltNm

        mkf :: DerivClsFld -> CBind
        mkf dcf
         = acoreBind1Ty (dcfNm dcf) (acoreTyErr $ "EH.ToCore.mkInstanceDerivingFldBind.mkf.dcfNm: " ++ show (dcfNm dcf))
             (if fArity == 0
              then acoreLam (dcfInitialArgL dcf)
                   $ dcfNoArgSubsCExpr dcf rceEnv
                       [ (dti,mkcSubs dti (repeat []) altPrOccL) | (dti,altPrOccL) <- zip dtis derivAltsPrOcc ]
                       nrOfAlts rsltVar extraCxL (dcfInitialArgL dcf)
              else acoreLam (dcfInitialArgL dcf ++ take fArity nsA)
                     (dcfWrapCase dcf opts derivDataDGI nrOfAlts n1 (dcfInitialArgL dcf)
                      $ acoreStrictSatCaseTy rceEnv (Just (hsnUniqifyEval n1,(acoreTyErr $ "EH.ToCore.mkInstanceDerivingFldBind.mkf.n1: " ++ show n1))) n1v
                        [ CAlt_Alt (dcfMkCPat dcf altInx ctag (ctagArity ctag) (Just $ n1subs ctag))
                                   (mkc splDTI altInx (fArity - 1) n1s [n1subs ctag] altPrOccL)
                        | (ctag,altPrOccL) <- zip (map dtiCTag dtis) derivAltsPrOcc
                        , let splDTI@(dtisLT,_,_) = splitDTIs ctag dtis
                              altInx = length dtisLT
                        ]
             )       )
         where n1v = acoreVar n1
               nsA@(n1:n1s) = hsnLclSupplyWith (mkHNm "x")
               dtis  = panicJust "Decl.Instance.Deriving.dataGamDTIsOfTy" $ dataGamDTIsOfTy derivHdDataTy dataGam
               nrOfAlts = length dtis
               splitDTIs t ts -- in (lt,eq,gt), according to list ordering
                 = (l,e,g)
                 where (l,(e:g)) = span ((/= t) . dtiCTag) ts
               fArity = (length $ tyArrowArgs $ dcfTy dcf) - dcfNrOmitTailArg dcf - length (dcfInitialArgL dcf) - 1 {- dictionary -}
               n1subs t = nsubs t n1
               nsubs t n = take (ctagArity t) $ hsnLclSupplyWith n
               extraCxL = [ acoreNmHolePred (poPoi prOcc) | prOcc <- derivExtraPrOcc ]
               mkcSubs dti nmFldLL altPrOccL
                 = [ acoreApp
                       (acoreVar (dcfNm dcf))
                       ([acoreNmHolePred (poPoi prOcc)]
                         ++ (dcfInitialSubArgL dcf dti ++ map acoreVar nmFldL))
                   | (nmFldL,prOcc) <- zip nmFldLL altPrOccL
                   ]
               mkc tg@(dtisLT,dti,dtisGT) altInx arity nms nmFldLL altPrOccL
                 = case nms of
                     (n:ns) | arity > 0
                       -> acoreStrictSatCaseTy rceEnv (Just (hsnUniqifyEval n,(acoreTyErr $ "EH.ToCore.mkInstanceDerivingFldBind.mkf.mkc.n: " ++ show n))) (acoreVar n)
                            (   [ mka (dtiCTag dti) tsubs (mkc tg altInx (arity - 1) ns (nmFldLL ++ [tsubs]) altPrOccL) ]
                             ++ [ mka t (nsubs t n) (dcfAllTagLtCExpr dcf) | t <- map dtiCTag dtisLT ]
                             ++ [ mka t (nsubs t n) (dcfAllTagGtCExpr dcf) | t <- map dtiCTag dtisGT ]
                            )
                       where tsubs = nsubs (dtiCTag dti) n
                             mka ctag nsubs e = CAlt_Alt (dcfMkCPat dcf altInx ctag (ctagArity ctag) (Just nsubs)) e
                     _ -> dcfFoldSubsCExpr dcf uniq rceEnv dti (altInx, nrOfAlts) (dcfInitialArgL dcf)
                          $ mkcSubs dti (transpose nmFldLL) altPrOccL



mkDataGenerBinds
  :: -- (AbstractCore e m b bcat mbind t) =>
        EHCOpts
     -> RCEEnv
     -> ClGam                               -- env/gamma for class
     -> DataGam                             -- env/gamma for datas
     -> UID                                 -- uniq
     -> [GenerDataInstInfo]                 -- inst infos
     -> ([CBind],[Err])
mkDataGenerBinds
     opts rceEnv clGam dataGam
     uniq instInfoL
  = (concat instBinds, concat errs)
  where (instBinds,errs) = unzip $ map mk instInfoL
        mk (orignm,dictNm,clNm,instKind,pr)
          = ( mkInstanceCBindL opts clgi dataGam InstNormal (clgiDictTag clgi) (dictNm,dictSelfNm,dictBuildNm) (pred2DataTy pr) recTy ([],[],[])
                               binds ([],[],[])
            , foErrL evFO
            )
          where (_,u1,u2) = mkNewLevUID2 uniq
                clgi  = panicJust ("mkDataGenerBinds.clGam: " ++ show dictNm ++ ": " ++ show clNm) $ gamLookup clNm clGam

                -- derived names required for building
                (dictSelfNm,dictBuildNm) = mkInstanceBuildNms dictNm

                -- match predicate to get the record type describing fields of dictionary
                evFO  = fitPredToEvid' u1 emptyVarMp (mkTyPr pr) (Left clgi)
                recTy = foVarMp evFO `varUpd` foTy evFO

                -- the bindings for datatypes acting as labels
                binds = case instKind of
                          GenerInstKind_Representable remArity proj
                            -> [ acoreBind1 (ehcOptBuiltin2 opts ehbnGenerClassRepresentableNFldFrom remArity) (projFrom opts rceEnv proj)
                               , acoreBind1 (ehcOptBuiltin2 opts ehbnGenerClassRepresentableNFldTo   remArity) (projTo   opts rceEnv proj)
                               ]

                          GenerInstKind_Datatype
                            -> [ mkBindStr ehbnGenerClassDatatypeFldName                         $ hsnQualified orignm
                               , mkBindStr ehbnGenerClassDatatypeFldModule $ maybe hsnUnknown id $ hsnQualifier orignm
                               ]

                          GenerInstKind_Selector
                            -> [ mkBindStr ehbnGenerClassSelectorFldName                         $ hsnQualified orignm
                               ]

                          GenerInstKind_Constructor tyNm
                            -> [ mkBindStr ehbnGenerClassConstructorFldName $ hsnQualified orignm ]
                               ++ (if Map.null (dtiFldMp dti) then [] else [mkBind ehbnGenerClassConstructorFldIsRec $ acoreVar $ biNm ehbnBoolTrue])
                               ++ (if isNothing mbInfix
                                   then []
                                   else [mkBind ehbnGenerClassConstructorFldFixity
                                         $ acoreApp (acoreVar $ biNm ehbnGenerDataFixityAltInfix)
                                         $ [ acoreVar (biNm (case fixity of
                                                               Fixity_Infix  -> ehbnGenerDataAssociativityAltNot
                                                               Fixity_Infixr -> ehbnGenerDataAssociativityAltRight
                                                               Fixity_Infixl -> ehbnGenerDataAssociativityAltLeft
                                                      )     )
                                           , acoreInt prio
                                           ]
                                        ]
                                  )
                            where dgi = panicJust ("mkDataGenerBinds.dataGam: " ++ show tyNm) $ dataGamLookup tyNm dataGam
                                  dti = panicJust ("mkDataGenerBinds.dgiConstrTagMp: " ++ show orignm) $ Map.lookup orignm $ dgiConstrTagMp dgi
                                  mbInfix@(~(Just (prio,fixity))) = dtiMbFixityPrio dti

                          -- _ -> []

        -- get a builtin name
        biNm          = ehcOptBuiltin opts

        -- utils for bindings
        argNm         = mkHNm "x"
        mkBindx a f x = acoreBind1 (biNm f) (acoreLam [a] x)
        mkBind        = mkBindx argNm
        mkBindStr f n = mkBind f $ acoreBuiltinString opts $ show n



tyPrLToSupNmL :: TyL -> [HsName]
tyPrLToSupNmL = zipWith (\i p -> hsnUniqifyInt HsNameUniqifier_SuperClass i (tyPredNm p)) [1..]



-- | given the name of an instance, return names of intermediate dictionaries being constructed
mkInstanceBuildNms :: HsName -> (HsName,HsName)
mkInstanceBuildNms dictNm
  = ( hsnUniqify HsNameUniqifier_ResultDict dictNm		-- the computed result, being returned in the end
    , hsnUniqify HsNameUniqifier_SelfDict   dictNm		-- the default, partially built dictionary, given as argument
    )



-- | Get info about datatype and class for which derivation is done, looking up in various environments
derivGetDataTyInfo :: HsName -> HsName -> TyGam -> TyKiGam -> DataGam -> (TyGamInfo,TyKiGamInfo,DataGamInfo,TyKiGamInfo,[Err])
derivGetDataTyInfo clNm tyNm tyGam tyKiGam dataGam
  = (tgi,tkgi,dgi,ctkgi,firstNotEmpty [errs123,errs4])
  where
        (tgi, tkgi, dgi, errs123)
          -- special case to detect records/tuples
          | hsnIsRec tyNm = ( emptyTGI, emptyTKGI
                            , emptyDataGamInfo {dgiVariant = DataGamInfoVariant_Rec}
                            , []
                            )
          | otherwise     = let (tgi, errs1) = tyGamLookupErr            tyNm                   tyGam                   -- lookup for type
                                (dgi, errs3) = dataGamLookupErr          tyNm                   dataGam                 -- lookup for structure info
                                (tkgi,errs2) = tyKiGamLookupByNameErr    tyNm                   tyKiGam                 -- lookup for kind signature
                            in  (tgi, tkgi, dgi, firstNotEmpty [errs1,errs2,errs3])
        (ctkgi,errs4)     =                    tyKiGamLookupByNameErr    (hsnClass2Kind clNm) 	tyKiGam               	-- lookup for class kind signature



-- | make pred from kind and tvar
mkDerivPredFromKi :: UID -> Int -> Ty -> Ty -> Ty -> Maybe (Ty,UID)
mkDerivPredFromKi uniq derivArity predTyCon ki tv
  = case tyArrowArgsRes ki of
      (as,_) -- (as,Ty_Con n) | n == hsnKindStar				-- ???? is this restriction indeed necessary?
        -> Just (mkTyPr $ Pred_Class $ predTyCon `mk1App` mkApp (tv : map mkTyVar u1s),u')
        where (u',u1) = mkNewLevUID uniq
              u1s = mkNewUIDL arity u1
              arity = length as - derivArity
      -- _ -> Nothing




-- | make context from the kinds of the datatype args and the actual datatype args
mkDerivPredContext :: UID -> Int -> Ty -> [Ty] -> [Ty] -> Maybe ([Ty],UID)
mkDerivPredContext uniq derivArity predTyCon dataTyKiL dataTyArgL
  = foldr (\(k,t) cxu
            -> do (cx,u) <- cxu
                  (cx',u') <- mkDerivPredFromKi u derivArity predTyCon k t
                  return (cx' : cx, u')
          )
          (Just ([],uniq))
          (zip dataTyKiL dataTyArgL)



type ScopeGam = Gam HsName PredScope



-- | behavior configuration of simplify
data SimplifyHow
  = SimplifyHow_Canonicalize            -- canonicalize predicates
  deriving Eq



data SimplifyState
  = SimplifyState
      { simpToProveHereCnstrMp          :: !CHRPredOccCnstrMp                               -- new constraints to prove
      , simpAllCnstrMp                  :: !CHRPredOccCnstrMp                               -- all constraints to prove, including previous
      , simpRemCnstrTraceMp             :: !CHRPredOccCnstrTraceMp                          -- remaining to be proven, as Assume's
      , simpRemOccL                     :: ![CHRIntermediateUntilAssume]                    -- same, but only occurrences (as list), with original trace mp split up per pred
      , simpCannotCnstrTraceMp          :: !CHRPredOccCnstrTraceMp                          -- cannot be proven
      , simpVarMp                       :: !VarMp                                           -- additional substitution
      , simpEvidMp                      :: !CHRPredOccEvidMp                                -- map from constraint/pred occurrence to evidence
      , simpEnv                         :: !FIIn
      , simpRes                         :: !(SimplifyResult CHRPredOcc RedHowAnnotation Guard VarMp)                                -- simplification work done at a previous call
      , simpErrs                        :: ![Err]                                           -- errors
      }

emptySimplifyState :: SimplifyState
emptySimplifyState = SimplifyState Map.empty Map.empty Map.empty [] Map.empty emptyVarMp Map.empty (emptyFI :: FIIn) emptySimplifyResult []

simplify'
  :: [SimplifyHow]
     -> ScopedPredStore                                                                     -- available CHR rules
     -> ClassDefaultGam                                                                     -- environment/gam for defaulting
     -> Heuristic CHRPredOcc RedHowAnnotation                                               -- graph reduction heuristic
     -> ([CHRIntermediateUntilAssume] -> ([CHRIntermediateUntilAssume],[CHRIntermediateUntilAssume]))   -- partition into ambiguous predicates (and others), for defaulting
     -> CHRPredOccCnstrMp                                                                   -- constraints already proven a previous call
     -> State SimplifyState ()
simplify' simplifyHow chrStore clDfGam heur partitionUnresolved2AmbigAndOthers toProveHereCnstrMpPrev
  = do { s <- get

       ; let -- unique seeds
             (_,u1,u2,u3) = mkNewLevUID3 $ fiUniq origEnv

             -- remember for later
             origEnv = simpEnv s

       -- canonicalize predicate occurrences (expand type signatures, record fields in same order)
       ; when (SimplifyHow_Canonicalize `elem` simplifyHow)
              (do { s <- get
                  ; let (pr,mp)
                          = Map.foldrWithKey (\p c (cm,m)
                                               -> let (p',m') = canon s p
                                                  in  (Map.insertWith (++) p' c cm, m' `varUpd` m)
                                            )
                                            (Map.empty,emptyVarMp :: VarMp) (simpToProveHereCnstrMp s)
                  ; put (s {simpToProveHereCnstrMp = pr, simpVarMp = mp `varUpd` simpVarMp s})
                  })

       -- adapt env to inhibit bindings for variables freely occurring in predicates, these are fixed (apart from improving substitutions, when resolving defaulting)
       ; modify (\s -> let e = simpEnv s
                           o = fiFIOpts e
                       in  s {simpEnv = e {fiFIOpts = o {fioDontBind = fixedFtv (simpToProveHereCnstrMp s), fioBindCategs = TyVarCateg_Meta : fioBindCategs o}}}
                )

       -- get base result, for backtracking to when restarting for defaulting
       ; s <- get
       ; let prevSimplifyRes = simpRes s

       -- 1st simplification run
       ; (cannotResCnstrTraceMp,chrSolveAssumablePrOccL) <- fullSimp u1 toProveHereCnstrMpPrev prevSimplifyRes
       ; modify (\s -> s {simpCannotCnstrTraceMp = cannotResCnstrTraceMp, simpRemOccL = chrSolveAssumablePrOccL})

       -- defaulting is done here very crudely, find defaulting candidates, apply the improving substitution, and restart. TBD: this is a hack, do this better, probably when improving subst are done right.
       ; s <- get
       ; let (chrSolveAmbigPrOccL,chrSolveAssumablePrOccL)
               = partitionUnresolved2AmbigAndOthers (simpRemOccL s)
       ; when ( -- Debug.tr "chrSolveAmbigPrOccL" (ppCommas (simpRemOccL s) >-< ppCommas chrSolveAmbigPrOccL >-< ppCommas chrSolveAssumablePrOccL) $
                not (null chrSolveAmbigPrOccL))
              (do { let (dfltPrOccL,dfltTyVarMp) = foldr cmb ([],emptyVarMp) chrSolveAmbigPrOccL
                          where cmb :: (CHRPredOcc,x) -> ([CHRPredOcc],VarMp) -> ([CHRPredOcc],VarMp)
                                cmb (o,_) r@(os,m) = maybe r (\m' -> (o:os,m' `varUpd` m)) $ clDfGamLookupDefault (origEnv {fiVarMp = m `varUpd` fiVarMp (simpEnv s)}) (cpoPr o) clDfGam
                  ; when ( -- Debug.tr "dfltPrOccL" (ppCommas (simpRemOccL s) >-< ppCommas dfltPrOccL >-< dfltTyVarMp) $
                           not (null dfltPrOccL))
                         -- when a match is found, apply the substitution and kick off proving again
                         (do { modify (\s -> s {simpToProveHereCnstrMp = dfltTyVarMp `varUpd` simpToProveHereCnstrMp s, simpVarMp = dfltTyVarMp `varUpd` simpVarMp s})
                             ; (cannotResCnstrTraceMp,chrSolveAssumablePrOccL) <- fullSimp u1 toProveHereCnstrMpPrev prevSimplifyRes
                             ; modify (\s -> s {simpCannotCnstrTraceMp = cannotResCnstrTraceMp, simpRemOccL = chrSolveAssumablePrOccL})
                             })
                  })

       -- (possibly) 2nd simplification run, to deal with unproven constraints which can be assumed or defaulted
       ; s <- get
       ; if   not (null (simpRemOccL s))
         then -- if not all could be proven, assumptions have to be made: clean up, prove again with additional assumptions
              do { s <- get
                 ; let -- remember for later use
                       chrSimplifyResult = simpRes s

                       -- prune the redgraph, remove that which depends on unresolved predicates
                       redGraphPruned
                         = redPruneReductionsUntil (map fst (simpRemOccL s))
                                                   (let m = Set.map cnstrPred $ Map.keysSet (simpToProveHereCnstrMp s) in \p -> Set.member p m)
                                                   (simpresRedGraph chrSimplifyResult)

                 ; modify (\s -> s {simpRes = chrSimplifyResult {simpresRedGraph = redGraphPruned, simpresRedGraphs = ("simplify pruned",redGraphPruned) : simpresRedGraphs chrSimplifyResult}})

                 -- (2nd time) RedGraph to Evidence etc
                 ; (_,chrSolveAssumablePrOccL) <- basicSimpRedGraph (simpAllCnstrMp s)
                 ; modify (\s -> s {simpRemOccL = chrSolveAssumablePrOccL})

                 -- convert to Assume constraints
                 ; s <- get
                 ; let (chrSolveRemCnstrTraceMp,chrSolveEvidMp)
                         = patchUnresolvedWithAssumption ((simpEnv s) {fiUniq = u3}) (simpRemOccL s) (simpresRedGraph chrSimplifyResult) (simpEvidMp s)
                 ; modify (\s -> s {simpRemCnstrTraceMp = chrSolveRemCnstrTraceMp, simpEvidMp = chrSolveEvidMp})
                 }
         else modify (\s -> s {simpRemCnstrTraceMp = Map.empty, simpRemOccL = []})

       ; return ()
       }
  where {-
        canon s (Prove p) = (Prove $ p {cpoPr = p'}, m)
                          where (p',m) = predCanonic (simpEnv s) $ cpoPr p
        canon s  c        = (c, emptyVarMp)
        -}
        canon s c
          = case cnstrReducablePart c of
              Just (_,p,mkc) -> (mkc $ p {cpoPr = p'}, m)
                             where (p',m) = predCanonic (emptyTyBetaRedEnv {tbredFI=simpEnv s}) $ cpoPr p
              _              -> (c, emptyVarMp)

        -- basic simplification of reduction graph
        basicSimpRedGraph chrSolveAllCnstrMp
          = do { s <- get
               ; let -- RedGraph to Evidence
                     ((chrSolveRemCnstrTraceMp,chrSolveEvidMp,errs),chrSimplifyResult2)
                       = chrSimplifyRedGraphToEvidence heur chrSolveAllCnstrMp (simpRes s)

                     -- split unresolved into those who can be resolved by assuming them (via qualified types) and the others
                     (chrSolveAssumablePrOccL,cannotResCnstrTraceMp)
                       = partitionUnresolved2AssumableAndOthers chrSolveRemCnstrTraceMp
                     -- TBD: propagation of unresolved traces for error reporting
                     -- cannotResCnstrMp   = cnstrTraceMpElimTrace cannotResCnstrTraceMp
                     -- chrSolveRemCnstrMp = cnstrTraceMpElimTrace chrSolveRemCnstrTraceMp

               ; put (s {simpRemCnstrTraceMp = chrSolveRemCnstrTraceMp, simpEvidMp = chrSolveEvidMp, simpErrs = errs, simpRes = chrSimplifyResult2})
               ; return (cannotResCnstrTraceMp,chrSolveAssumablePrOccL)
               }

        -- full simplification
        fullSimp u toProveHereCnstrMpPrev chrSimplifyResult
          = do { s <- get
               ; let -- simplification, to RedGraph
                     (chrSolveAllCnstrMp,chrSimplifyResult1)
                       = chrSimplifySolveToRedGraph ((simpEnv s) {fiUniq = u}) chrStore toProveHereCnstrMpPrev (simpToProveHereCnstrMp s) chrSimplifyResult

               ; put (s {simpRes = chrSimplifyResult1, simpAllCnstrMp = chrSolveAllCnstrMp})

               -- RedGraph to Evidence etc
               ; basicSimpRedGraph chrSolveAllCnstrMp
               }



simplify
  :: [SimplifyHow]                                                                          -- config
     -> FIIn                                                                                -- environment
     -> ScopedPredStore                                                                     -- available CHR rules
     -> ClassDefaultGam                                                                     -- environment/gam for defaulting
     -> Heuristic CHRPredOcc RedHowAnnotation                                               -- graph reduction heuristic
     -> ([CHRIntermediateUntilAssume] -> ([CHRIntermediateUntilAssume],[CHRIntermediateUntilAssume]))   -- partition into ambiguous predicates (and others), for defaulting
     -> CHRPredOccCnstrMp                                                                   -- constraints already proven a previous call
     -> CHRPredOccCnstrMp                                                                   -- constraints to prove
     -> SimplifyResult CHRPredOcc RedHowAnnotation Guard VarMp                              -- simplification work done at a previous call
     -> ( CHRPredOccCnstrMp                                                                 -- canonicalized to be proven constraints
        , CHRPredOccCnstrTraceMp                                                            -- remaining to be proven, as Assume's
        , CHRPredOccCnstrTraceMp                                                            -- cannot be proven
        , VarMp                                                                             -- additional substitution found during proving & simplification
        , [Err]                                                                             -- errors
        , SimplifyResult CHRPredOcc RedHowAnnotation Guard VarMp                            -- simplification work, for a next call
        , CHRPredOccEvidMp                                                                  -- map from constraint/pred occurrence to evidence
        , EvidKeyToCBindMap                                                                 -- map from occurrence to binding, globally
        , PredScopeToCBindMap                                                               -- map from predscope to binding, per predscope opening/intro
        , EvidKeyToCExprMap                                                                 -- map from occurrence to expr + additional nesting info, for debug only
        )
simplify simplifyHow env chrStore clDfGam heur partitionUnresolved2AmbigAndOthers toProveHereCnstrMpPrev toProveHereCnstrMp prevRes
  = {- Debug.tr "simplify"
      (   (if toProveHereCnstrMpCanon == toProveHereCnstrMpCanon' then empty else pp "toProveHereCnstrMpCanon wrong")
      >-< (if canonVarMp == canonVarMp' then empty else pp "canonVarMp wrong")
      >-< (if cannotResCnstrMp1 == cannotResCnstrMp1' then empty else pp "cannotResCnstrMp1 wrong")
      >-< (if chrSolveRemCnstrMp3 == chrSolveRemCnstrMp3' then empty else pp "chrSolveRemCnstrMp3 wrong" >-< show chrSolveRemCnstrMp3 >-< show chrSolveRemCnstrMp3')
      -- >-< (if chrSolveAssumablePrOccL3 == chrSolveAssumablePrOccL3' then empty else pp "chrSolveAssumablePrOccL3 wrong" >-< ppCommas chrSolveAssumablePrOccL3 >-< ppCommas chrSolveAssumablePrOccL3')
      ) $
    -}
    ( toProveHereCnstrMpCanon
    , chrSolveRemCnstrMp3, cannotResCnstrTraceMp
    , canonVarMp
    , firstNotEmpty
        [ chrSolveErrs2
        , overlapErrs
        ]
    , chrSimplifyResult4
        { simpresRemPredL = map fst chrSolveAssumablePrOccL3
        }
    , chrSolveEvidMp2
    , chrSolveEvidBindMp, chrSolveScopeBindMp
    , chrSolveEvidCoreMp        -- for debug only
    )
  where s = execState (simplify' simplifyHow chrStore clDfGam heur partitionUnresolved2AmbigAndOthers toProveHereCnstrMpPrev)
                      (emptySimplifyState {simpEnv = env {fiUniq = u1}, simpToProveHereCnstrMp = toProveHereCnstrMp, simpRes = prevRes})
        env'                        =   simpEnv s
        chrSimplifyResult4          =   simpRes s
        chrSolveAssumablePrOccL3    =   simpRemOccL s
        chrSolveRemCnstrMp3         =   simpRemCnstrTraceMp s
        chrSolveEvidMp2             =   simpEvidMp s
        chrSolveErrs2               =   simpErrs s
        toProveHereCnstrMpCanon     =   simpToProveHereCnstrMp s
        canonVarMp                  =   simpVarMp s
        cannotResCnstrTraceMp       =   simpCannotCnstrTraceMp s

        -- some unique seeds
        (_,u1,u2,u3) = mkNewLevUID3 $ fiUniq env

        -- (1) compute code (Core) for dictionaries/witness, (2) get the overlapping instances
        (chrSolveEvidCoreMp,overlapEvids)
          = evidMpToCore (env' {fiUniq = u3}) chrSolveEvidMp2

        -- get the bindings which must be introduced
        (chrSolveEvidBindMp,chrSolveScopeBindMp)
          = evidKeyCoreMpToBinds chrSolveEvidCoreMp

        -- overlapping errors; 20120209: is empty, detection done earlier
        -- overlapErrs = if null overlapEvids then [] else [rngLift range Err_OverlapPreds [ (cpoPr $ overlapevidPredOcc a,map pp $ overlapevidInfos a) | a <- overlapEvids]]
        overlapErrs = []
        -- TyCore variant
        range = feRange $ fiEnv env
        -- debugging info
        -- dbgp = ppParensCommas $ Map.keys toProveHereCnstrMpCanon
        -- dbg m= id -- Debug.tr m dbgp



fixedFtv mp = varFreeSet $ map cnstrPred $ Map.keys mp

debugInfo chrSimplifyResult
  = (s,rg,chrSolveStateDoneConstraints s,chrSolveStateTrace s,redAlts,redTrees,simpresRemPredL chrSimplifyResult)
  where s  = simpresSolveState chrSimplifyResult
        rg = simpresRedGraph chrSimplifyResult
        redAlts  = simpresRedAlts   chrSimplifyResult
        redTrees = simpresRedTrees  chrSimplifyResult




mkScopeBindings :: Bool -> PredScope -> PredScopeToCBindMap -> ([CBind],PredScopeToCBindMap)
mkScopeBindings alsoOuter sc mp
  = (concatMap f scs,foldr Map.delete mp scs)
  where f sc = Map.findWithDefault [] sc mp
        scs  = [sc] ++ (if alsoOuter then pscpParents sc else [])



mkAssumeBindings :: [UID] -> EvidKeyToCBindMap -> [CBind]
mkAssumeBindings ids mp
  = concat [ maybe [] id $ Map.lookup i mp | i <- ids ]



polGamLookupOrAdd :: UID -> HsName -> PolGam -> PolGam -> (PolGamInfo,PolGam)
polGamLookupOrAdd uniq nm polGamLkup polGam
  =  case polGamLookup nm polGamLkup of
       Nothing    ->  let  t    =  mkPolVar uniq
                           pgi  =  mkPGI t
                      in   (pgi,gamAdd nm pgi polGam)
       Just pgi   ->  (pgi,polGam)



gathMentrelFilterMpFromSimplifyResult :: HsName -> SimplifyResult p RedHowAnnotation g m -> ModEntRelFilterMp
gathMentrelFilterMpFromSimplifyResult moduleNm simpRes
  = mentrelFilterMpUnions
      [ mentrelFilterMpSingleton [moduleNm] IdOcc_Val nm
      | (Reduction {cnstrInfo=RedHow_ByInstance nm _ _}) <- chrSolveStateDoneConstraints $ simpresSolveState simpRes
      ]



-- | determine the orphans given a modulenm, of a nm
orphanNmS :: HsName -> HsName -> Set.Set HsName
orphanNmS moduleNm nm
  = case hsnQualifier nm of
      Just m | m /= moduleNm -> Set.singleton m
      _                      -> Set.empty



-- | The info for generics about datatypes, required higher in the syntax tree
type GenerDataInfo
  = (  ( HsName         -- orig type name
       , [TyVarId]      -- data type args
       , Ty             -- kind of type, without kiVarMap yet applied
       )
    , [( CTag           -- orig con tag
       , FldTyL         -- field types, with possible label
      )]
    , PredScope
    , Int               -- max kind arity over which genericity abstracts, currently \in {0,1}
    )

-- | what kind of instance should be generated
data GenerInstKind
  = GenerInstKind_Datatype
  | GenerInstKind_Selector
  | GenerInstKind_Constructor       HsName      -- for a type
  | GenerInstKind_Representable     Int         -- remaining arity
                                    Proj        -- projection info
  deriving (Show)

-- | Info for generics, for generating data instances
type GenerDataInstInfo
  = ( HsName            -- original name of type, data
    , HsName            -- instance name
    , HsName            -- class name
    , GenerInstKind     -- what kind of instance
    , Pred              -- the predicate
    )

-- | Info for generics, for generating repr instances, in particular projection descriptors
type GenerTySynProjInfo
  = ( HsName                -- original name of datatype
    , [TyVarId]             -- data type args
    , PredScope             -- predicate scope
    , [( HsName             -- representation type name
       , Ty                 -- type synonym itself, i.e. the type lambda
       , Proj               -- projection descriptor
       , Int                -- the arity of remaining type args
      )]
    )



mkGenerThing
  ::    thing
     -> (thing -> thing)
     -> (thing -> thing -> thing)
     -> [thing]
     -> thing
mkGenerThing zero one two things
  = mkSub things
  where mkSub [ ] = zero
        mkSub [s] = one s
        mkSub ss  = two (mkSub ss1) (mkSub ss2)
                  where (ss1,ss2) = splitAt (length ss `div` 2) ss



mkGenerRepresentableTypeSynTyProj
  :: EHCOpts
     -> Int                     -- the arity of to be supplied type paramaters
     -> [HsName]                -- all type names defined in this binding group (i.e. let)
     -> (HsName -> Int-> Bool)  -- name of type has Representable<remArity>?
     -> HsName                  -- type name
     -> [TyVarId]               -- type args
     -- -> ([Ty],Ty)               -- kind, already split up in args + res
     -> [(CTag,HsName,FldTyL)]    -- tags, constructor type names, with type args
     -> ( Ty                    -- type
        , Proj                  -- intermediate description of projection
        )
mkGenerRepresentableTypeSynTyProj
     opts remArity allTyNmL isRepresentable
     tyNm tvarArgL
     -- (kiArgL,kiRes)
     conNmArgsL
  = mkTop $ mkSm [ mkAlt tg c (mkPr [ mkLblArg p | p <- ps ]) | (tg,c,ps) <- conNmArgsL ]
  where lamArity      = length tvarArgL - remArity
        (lamTvarArgL,remTvarArgL)
                      = splitAt lamArity tvarArgL
        mkC   f       = Ty_Con $ ehcOptBuiltin  opts f
        mkC2  f i     = Ty_Con $ ehcOptBuiltin2 opts f i
        mkSub (u,pu) (f,pf)
                      = mkGenerThing (mkC u,pu) id (\(x,px) (y,py) -> (mkApp [mkC f, x, y], pf px py))
        mkPr          = mkSub (ehbnGenerDataUnit1, Proj_U1   ) (ehbnGenerDataProd, Proj_Prod                             )
        mkSm          = mkSub (ehbnGenerDataVoid1, Proj_Void ) (ehbnGenerDataSum , \x y -> Proj_L1 x `Proj_Sum` Proj_R1 y)
        mkLblArg (mbLbl,t)
                      = ( mkApp [ mkC ehbnGenerDataMetaS1, sel, t' ], Proj_M1_S1 p )
                      where (t',p) = mkArg t
                            sel    = maybe (mkC ehbnGenerDataNoSelector) (Ty_Con) mbLbl
        mkArg t       = (mkApp funarg, proj)
                      where (funarg,proj)
                              | isJust mbVar     = case elemIndex tvar remTvarArgL of       -- TBD: check for kind *, check for one of tvarArgL
                                                     Just i -> ([ mkC ehbnGenerDataPar1    ], Proj_Par1 [t] (i+1))
                                                     _      -> ([ mkC ehbnGenerDataPar0, t ], Proj_K1   [t]      )
                              | remArity > 0 && {- isJust mbCon && -} length args > 0 && isRepr
                                                 = case (initlast remTvarArgL, initlast args) of
                                                     (Just (_,tv), Just (firstargs,lastarg)) | maybe False (tv==) $ tyMbVar lastarg
                                                       -> ([ mkC ehbnGenerDataRec1, mkApp (fun : firstargs) ], Proj_Rec1 [t] remArity)
                                                     (_, Just (firstargs,lastarg)) | not $ Set.null $ varFreeSet lastarg
                                                       -> ( [ mkC ehbnGenerDataComp1, fmapTy, lastarg' ]
                                                          , Proj_Comp1 [t] fmapTy lastproj
                                                          )
                                                       where (lastarg',lastproj) = mkArg lastarg
                                                             fmapTy = mkApp (fun : firstargs)
                                                     _ -> dflt
                              | otherwise        = dflt
                              where mbVar    @(~(Just tvar      )) = tyMbVar        t
                                    funArgs  @(       (fun,args) ) = tyAppFunArgs   t
                                    isRepr = maybe (isJust $ tyMbVar fun) (\con -> isRepresentable con remArity) $ tyMbCon fun
                                    dflt   = ([ mkC ehbnGenerDataRec0, t ], Proj_K1 [t])
        mkAlt tg c (t,p)  = (mkApp [mkC2 ehbnGenerDataMetaCN 1, Ty_Con c, t], Proj_M1 $ Proj_Con tg p)
        mkTop (t,p)   = ( mkTyLam lamTvarArgL (mkApp [ mkC2 ehbnGenerDataMetaDN 1, semCon tyNm, t ])
                        , Proj $ Proj_M1 p
                        )
        -- eqLastTvar    = maybe (const False) (\(_,v) -> maybe False (v==) . tyMbVar) $ initlast tvarArgL
        -- isRec t       = not (null as) || tyConNm f `elem` allTyNmL
        --               where (f,as) = tyAppFunArgs t

mkGenerRepresentableTypeSynonymKi :: TyKiGam -> HsName -> Ty
mkGenerRepresentableTypeSynonymKi tyKiGam tyNm
  = maybe (acoreTyErr $ "Generics.mkGenerRepresentableTypeSynonymKi: " ++ show tyNm) (addStar . tkgiKi) $ tyKiGamLookupByName tyNm tyKiGam
  where addStar ki = mkArrow (as ++ [kiStar]) r
                   where (as,r) = tyArrowArgsRes ki

-- TBD: sort out additional pol param at end, just like for kinds
mkGenerRepresentableTypeSynonymPol :: PolGam -> HsName -> Ty
mkGenerRepresentableTypeSynonymPol polGam tyNm = maybe (acoreTyErr $ "Generics.mkGenerRepresentableTypeSynonymPol: " ++ show tyNm) pgiPol $ polGamLookup tyNm polGam



type GenerForFitsIn
  = ( HsName            -- field name
    , HsName            -- default value name
    , Ty                -- field type
    , Ty                -- default value type
    )

-- AGItf -------------------------------------------------------

-- semantic domain
type T_AGItf  = ScopedPredStore ->
                ClassDefaultGam ->
                ClGam ->
                DataGam ->
                UID ->
                IdQualGam ->
                Bool ->
                KiGam ->
                HsName ->
                EHCOpts ->
                PolGam ->
                TyGam ->
                TyKiGam ->
                ValGam ->
                ( ErrSq,CModule,UID,ScopedPredStore,ClassDefaultGam,ClGam,DataGam,(Seq.Seq (HsName,IdOccKind)),KiGam,LamMp,ModEntRelFilterMp,PolGam,TyGam,TyKiGam,ValGam,(Maybe (Set.Set HsName)),PP_Doc,PP_Doc)
-- CaseAlt -----------------------------------------------------

-- semantic domain
type T_CaseAlt  = ( Range,T_CaseAlt_1 )
type T_CaseAlt_1  = UID ->
                    ( UID,T_CaseAlt_2 )
type T_CaseAlt_2  = KiGam ->
                    Int ->
                    Int ->
                    ( Int,T_CaseAlt_3 )
type T_CaseAlt_3  = VarMp ->
                    EHCOpts ->
                    PolGam ->
                    VarMp ->
                    PredScope ->
                    TyGam ->
                    TyKiGam ->
                    TyVarIdS ->
                    ( VarMp,VarMp,T_CaseAlt_4 )
type T_CaseAlt_4  = ClGam ->
                    DataGam ->
                    VarMp ->
                    TyKiGam ->
                    TyVarIdS ->
                    ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),T_CaseAlt_5 )
type T_CaseAlt_5  = Ty ->
                    VarMp ->
                    ValGam ->
                    ( VarMp,T_CaseAlt_6 )
type T_CaseAlt_6  = ScopedPredStore ->
                    ClassDefaultGam ->
                    FIOpts ->
                    Ty ->
                    VarMp ->
                    TyVarIdS ->
                    VarMp ->
                    TyVarIdS ->
                    ( CHRPredOccCnstrMp,RangeMp,VarMp,T_CaseAlt_7 )
type T_CaseAlt_7  = CSubst ->
                    EvidKeyToCBindMap ->
                    PredScopeToCBindMap ->
                    VarMp ->
                    ValGam ->
                    HsName ->
                    RangeMp ->
                    ( ErrSq,CSubst,ErrSq,ModEntRelFilterMp,VarMp,Ty,PP_Doc,RAlt,RAlt,Ty)
-- CaseAlts ----------------------------------------------------

-- semantic domain
type T_CaseAlts  = ( Range,T_CaseAlts_1 )
type T_CaseAlts_1  = UID ->
                     ( UID,T_CaseAlts_2 )
type T_CaseAlts_2  = KiGam ->
                     Int ->
                     Int ->
                     ( Int,T_CaseAlts_3 )
type T_CaseAlts_3  = VarMp ->
                     EHCOpts ->
                     PolGam ->
                     VarMp ->
                     PredScope ->
                     TyGam ->
                     TyKiGam ->
                     TyVarIdS ->
                     ( VarMp,VarMp,T_CaseAlts_4 )
type T_CaseAlts_4  = ClGam ->
                     DataGam ->
                     VarMp ->
                     TyKiGam ->
                     TyVarIdS ->
                     ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),T_CaseAlts_5 )
type T_CaseAlts_5  = Ty ->
                     Ty ->
                     VarMp ->
                     ValGam ->
                     ( VarMp,Ty,T_CaseAlts_6 )
type T_CaseAlts_6  = ScopedPredStore ->
                     ClassDefaultGam ->
                     FIOpts ->
                     VarMp ->
                     TyVarIdS ->
                     VarMp ->
                     TyVarIdS ->
                     ( CHRPredOccCnstrMp,RangeMp,VarMp,T_CaseAlts_7 )
type T_CaseAlts_7  = CSubst ->
                     EvidKeyToCBindMap ->
                     PredScopeToCBindMap ->
                     VarMp ->
                     ValGam ->
                     HsName ->
                     RangeMp ->
                     ( ErrSq,CSubst,ErrSq,ModEntRelFilterMp,VarMp,Ty,PP_Doc,([PP_Doc]),([RAlt]),([RAlt]))
-- DataConstr --------------------------------------------------

-- semantic domain
type T_DataConstr  = ( Range,T_DataConstr_1 )
type T_DataConstr_1  = UID ->
                       ( UID,T_DataConstr_2 )
type T_DataConstr_2  = Ty ->
                       Polarity ->
                       PolGam ->
                       VarMp ->
                       TyGam ->
                       ( (AssocL HsName Ty),VarMp,T_DataConstr_3 )
type T_DataConstr_3  = VarMp ->
                       TyKiGam ->
                       ( Int,TyKiGam,VarMp,T_DataConstr_4 )
type T_DataConstr_4  = Ty ->
                       Int ->
                       HsName ->
                       ( Ty,([HsName]),DataConstrTagMp,T_DataConstr_5 )
type T_DataConstr_5  = TyKiGam ->
                       VarMp ->
                       ValGam ->
                       VarMp ->
                       ( ValGam,ValGam,CHRPredOccCnstrMp,RangeMp,VarMp,ValGam,T_DataConstr_6 )
type T_DataConstr_6  = EvidKeyToCBindMap ->
                       PredScopeToCBindMap ->
                       ScopedPredStore ->
                       ClassDefaultGam ->
                       ClGam ->
                       VarMp ->
                       VarMp ->
                       Bool ->
                       KiGam ->
                       Int ->
                       HsName ->
                       EHCOpts ->
                       PredScope ->
                       RangeMp ->
                       TyVarIdS ->
                       TyVarIdS ->
                       TyVarIdS ->
                       ValGam ->
                       TyVarIdS ->
                       ( ErrSq,CBindL,ErrSq,CBindL,CBindL,ModEntRelFilterMp,VarMp,PP_Doc)
-- DataConstrs -------------------------------------------------

-- semantic domain
type T_DataConstrs  = ( Range,T_DataConstrs_1 )
type T_DataConstrs_1  = UID ->
                        ( UID,T_DataConstrs_2 )
type T_DataConstrs_2  = Ty ->
                        Polarity ->
                        PolGam ->
                        VarMp ->
                        TyGam ->
                        ( (AssocL HsName Ty),VarMp,T_DataConstrs_3 )
type T_DataConstrs_3  = VarMp ->
                        TyKiGam ->
                        ( Int,TyKiGam,VarMp,T_DataConstrs_4 )
type T_DataConstrs_4  = Ty ->
                        Int ->
                        HsName ->
                        ( Ty,([HsName]),DataConstrTagMp,T_DataConstrs_5 )
type T_DataConstrs_5  = TyKiGam ->
                        VarMp ->
                        ValGam ->
                        VarMp ->
                        ( ValGam,ValGam,CHRPredOccCnstrMp,RangeMp,VarMp,ValGam,T_DataConstrs_6 )
type T_DataConstrs_6  = EvidKeyToCBindMap ->
                        PredScopeToCBindMap ->
                        ScopedPredStore ->
                        ClassDefaultGam ->
                        ClGam ->
                        VarMp ->
                        VarMp ->
                        Bool ->
                        KiGam ->
                        Int ->
                        HsName ->
                        EHCOpts ->
                        PredScope ->
                        RangeMp ->
                        TyVarIdS ->
                        TyVarIdS ->
                        TyVarIdS ->
                        ValGam ->
                        TyVarIdS ->
                        ( ErrSq,CBindL,ErrSq,CBindL,CBindL,ModEntRelFilterMp,VarMp,PP_Doc,([PP_Doc]))
-- DataField ---------------------------------------------------

-- semantic domain
type T_DataField  = ( Range,T_DataField_1 )
type T_DataField_1  = UID ->
                      ( UID,T_DataField_2 )
type T_DataField_2  = Polarity ->
                      PolGam ->
                      VarMp ->
                      TyGam ->
                      ( ([DataConFldAnnInfo]),FldTyL,VarMp,TyGam,T_DataField_3 )
type T_DataField_3  = TyKiGam ->
                      ( TyKiGam,TyKiGam,T_DataField_4 )
type T_DataField_4  = Ty ->
                      VarMp ->
                      ( ValGam,ValGam,CHRPredOccCnstrMp,RangeMp,TyL,VarMp,T_DataField_5 )
type T_DataField_5  = EvidKeyToCBindMap ->
                      PredScopeToCBindMap ->
                      ScopedPredStore ->
                      ClassDefaultGam ->
                      ClGam ->
                      VarMp ->
                      TyKiGam ->
                      VarMp ->
                      KiGam ->
                      Int ->
                      HsName ->
                      EHCOpts ->
                      VarMp ->
                      PredScope ->
                      RangeMp ->
                      VarMp ->
                      TyVarIdS ->
                      TyVarIdS ->
                      TyVarIdS ->
                      TyVarIdS ->
                      ( ErrSq,ErrSq,ModEntRelFilterMp,VarMp,VarMp,PP_Doc)
-- DataFieldExpr -----------------------------------------------

-- semantic domain
type T_DataFieldExpr  = ( Range,T_DataFieldExpr_1 )
type T_DataFieldExpr_1  = UID ->
                          ( UID,T_DataFieldExpr_2 )
type T_DataFieldExpr_2  = KiGam ->
                          Int ->
                          Int ->
                          ( Int,T_DataFieldExpr_3 )
type T_DataFieldExpr_3  = VarMp ->
                          EHCOpts ->
                          PolGam ->
                          VarMp ->
                          PredScope ->
                          TyGam ->
                          TyKiGam ->
                          TyVarIdS ->
                          ( VarMp,VarMp,T_DataFieldExpr_4 )
type T_DataFieldExpr_4  = ClGam ->
                          VarMp ->
                          TyKiGam ->
                          DataGam ->
                          TyVarIdS ->
                          ( DataGam,T_DataFieldExpr_5 )
type T_DataFieldExpr_5  = DataGam ->
                          ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),T_DataFieldExpr_6 )
type T_DataFieldExpr_6  = ScopedPredStore ->
                          ClassDefaultGam ->
                          FIOpts ->
                          VarMp ->
                          TyVarIdS ->
                          VarMp ->
                          ValGam ->
                          TyVarIdS ->
                          ( ([HsName]),(Maybe HsName),Ty,VarMp,T_DataFieldExpr_7 )
type T_DataFieldExpr_7  = FIOpts ->
                          Ty ->
                          VarMp ->
                          ( CHRPredOccCnstrMp,RangeMp,TyVarIdS,Ty,VarMp,T_DataFieldExpr_8 )
type T_DataFieldExpr_8  = CSubst ->
                          EvidKeyToCBindMap ->
                          PredScopeToCBindMap ->
                          DataGamInfo ->
                          VarMp ->
                          ValGam ->
                          HsName ->
                          RangeMp ->
                          ( ErrSq,CSubst,CBindL,CExpr,ErrSq,(FieldUpdateL (DataTagInfo -> Int -> CExpr)),ModEntRelFilterMp,VarMp,(Maybe DataTagInfo),PP_Doc,([PP_Doc]))
-- DataFieldPatExpr --------------------------------------------

-- semantic domain
type T_DataFieldPatExpr  = ( Range,T_DataFieldPatExpr_1 )
type T_DataFieldPatExpr_1  = UID ->
                             ( UID,T_DataFieldPatExpr_2 )
type T_DataFieldPatExpr_2  = KiGam ->
                             Int ->
                             Int ->
                             ( Int,T_DataFieldPatExpr_3 )
type T_DataFieldPatExpr_3  = VarMp ->
                             EHCOpts ->
                             PolGam ->
                             VarMp ->
                             PredScope ->
                             TyGam ->
                             TyKiGam ->
                             TyVarIdS ->
                             ( VarMp,VarMp,TyGam,TyKiGam,T_DataFieldPatExpr_4 )
type T_DataFieldPatExpr_4  = ClGam ->
                             DataGam ->
                             VarMp ->
                             TyKiGam ->
                             TyVarIdS ->
                             ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),T_DataFieldPatExpr_5 )
type T_DataFieldPatExpr_5  = FIOpts ->
                             Ty ->
                             VarMp ->
                             ValGam ->
                             ValGam ->
                             ( VarMp,ValGam,T_DataFieldPatExpr_6 )
type T_DataFieldPatExpr_6  = ScopedPredStore ->
                             ClassDefaultGam ->
                             VarMp ->
                             TyVarIdS ->
                             VarMp ->
                             TyVarIdS ->
                             ( CHRPredOccCnstrMp,RangeMp,ScopeGam,Ty,VarMp,T_DataFieldPatExpr_7 )
type T_DataFieldPatExpr_7  = CSubst ->
                             HsName ->
                             EvidKeyToCBindMap ->
                             PredScopeToCBindMap ->
                             VarMp ->
                             ValGam ->
                             HsName ->
                             RangeMp ->
                             ( ErrSq,CSubst,CBindL,DataTagInfo,ErrSq,([HsName]),FieldSplitL,ModEntRelFilterMp,VarMp,CPatRest,PP_Doc,([PP_Doc]))
-- DataFields --------------------------------------------------

-- semantic domain
type T_DataFields  = ( Range,T_DataFields_1 )
type T_DataFields_1  = UID ->
                       ( UID,T_DataFields_2 )
type T_DataFields_2  = Polarity ->
                       PolGam ->
                       VarMp ->
                       TyGam ->
                       ( ([DataConFldAnnInfo]),FldTyL,VarMp,TyGam,T_DataFields_3 )
type T_DataFields_3  = TyKiGam ->
                       ( TyKiGam,TyKiGam,T_DataFields_4 )
type T_DataFields_4  = Ty ->
                       VarMp ->
                       ( ValGam,ValGam,CHRPredOccCnstrMp,RangeMp,TyL,VarMp,T_DataFields_5 )
type T_DataFields_5  = EvidKeyToCBindMap ->
                       PredScopeToCBindMap ->
                       ScopedPredStore ->
                       ClassDefaultGam ->
                       ClGam ->
                       VarMp ->
                       TyKiGam ->
                       VarMp ->
                       KiGam ->
                       Int ->
                       HsName ->
                       EHCOpts ->
                       VarMp ->
                       PredScope ->
                       RangeMp ->
                       VarMp ->
                       TyVarIdS ->
                       TyVarIdS ->
                       TyVarIdS ->
                       TyVarIdS ->
                       ( ErrSq,ErrSq,ModEntRelFilterMp,VarMp,VarMp,PP_Doc,([PP_Doc]))
-- Decl --------------------------------------------------------

-- semantic domain
type T_Decl  = ( Range,T_Decl_1 )
type T_Decl_1  = UID ->
                 TyGam ->
                 ( UID,TyGam,Bool,T_Decl_2 )
type T_Decl_2  = KiGam ->
                 ( TyKiGam,T_Decl_3 )
type T_Decl_3  = Int ->
                 Int ->
                 ( Int,T_Decl_4 )
type T_Decl_4  = TyGam ->
                 TyKiGam ->
                 ( TyGam,T_Decl_5 )
type T_Decl_5  = TyKiGam ->
                 TyGam ->
                 VarMp ->
                 ( TyKiGam,VarMp,T_Decl_6 )
type T_Decl_6  = VarMp ->
                 PolGam ->
                 TyKiGam ->
                 ( VarMp,PolGam,T_Decl_7 )
type T_Decl_7  = VarMp ->
                 (Maybe Polarity) ->
                 EHCOpts ->
                 PolGam ->
                 VarMp ->
                 PredScope ->
                 TyVarIdS ->
                 ( ([GenerDataInfo]),VarMp,VarMp,T_Decl_8 )
type T_Decl_8  = ClGam ->
                 ( ClGam,T_Decl_9 )
type T_Decl_9  = VarMp ->
                 TyKiGam ->
                 ClGam ->
                 TyVarIdS ->
                 ( ClGam,T_Decl_10 )
type T_Decl_10  = ClGam ->
                  DataGam ->
                  ( DataGam,DataGam,ValGam,T_Decl_11 )
type T_Decl_11  = DataGam ->
                  ( (Seq.FastSeq (CHRClassDecl Pred RedHowAnnotation)),(Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),ClassDefaultGam,T_Decl_12 )
type T_Decl_12  = VarMp ->
                  ValGam ->
                  VarMp ->
                  ValGam ->
                  ( VarMp,ValGam,T_Decl_13 )
type T_Decl_13  = ScopedPredStore ->
                  ClassDefaultGam ->
                  TyVarIdS ->
                  VarMp ->
                  ValGam ->
                  TyVarIdS ->
                  ( CHRPredOccCnstrMp,RangeMp,TyKiGam,TyVarIdS,ScopeGam,VarMp,T_Decl_14 )
type T_Decl_14  = CSubst ->
                  EvidKeyToCBindMap ->
                  PredScopeToCBindMap ->
                  VarMp ->
                  ValGam ->
                  TyGam ->
                  HsName ->
                  RangeMp ->
                  ( ErrSq,([CExpr -> CExpr]),CSubst,CBindL,ErrSq,CBindL,CBindL,(Seq.Seq (HsName,IdOccKind)),LamMp,ModEntRelFilterMp,VarMp,(Set.Set HsName),PP_Doc)
-- Decls -------------------------------------------------------

-- semantic domain
type T_Decls  = ( Range,T_Decls_1 )
type T_Decls_1  = UID ->
                  TyGam ->
                  ( UID,TyGam,Bool,T_Decls_2 )
type T_Decls_2  = KiGam ->
                  ( TyKiGam,T_Decls_3 )
type T_Decls_3  = Int ->
                  Int ->
                  ( Int,T_Decls_4 )
type T_Decls_4  = TyGam ->
                  TyKiGam ->
                  ( TyGam,T_Decls_5 )
type T_Decls_5  = TyKiGam ->
                  TyGam ->
                  VarMp ->
                  ( TyKiGam,VarMp,T_Decls_6 )
type T_Decls_6  = VarMp ->
                  PolGam ->
                  TyKiGam ->
                  ( VarMp,PolGam,T_Decls_7 )
type T_Decls_7  = VarMp ->
                  (Maybe Polarity) ->
                  EHCOpts ->
                  PolGam ->
                  VarMp ->
                  PredScope ->
                  TyVarIdS ->
                  ( ([GenerDataInfo]),VarMp,VarMp,T_Decls_8 )
type T_Decls_8  = ClGam ->
                  ( ClGam,T_Decls_9 )
type T_Decls_9  = VarMp ->
                  TyKiGam ->
                  ClGam ->
                  TyVarIdS ->
                  ( ClGam,T_Decls_10 )
type T_Decls_10  = ClGam ->
                   DataGam ->
                   ( DataGam,DataGam,ValGam,T_Decls_11 )
type T_Decls_11  = DataGam ->
                   ( (Seq.FastSeq (CHRClassDecl Pred RedHowAnnotation)),(Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),ClassDefaultGam,T_Decls_12 )
type T_Decls_12  = VarMp ->
                   ValGam ->
                   VarMp ->
                   ValGam ->
                   ( VarMp,ValGam,T_Decls_13 )
type T_Decls_13  = ScopedPredStore ->
                   ClassDefaultGam ->
                   TyVarIdS ->
                   VarMp ->
                   ValGam ->
                   TyVarIdS ->
                   ( CHRPredOccCnstrMp,RangeMp,TyKiGam,TyVarIdS,ScopeGam,VarMp,T_Decls_14 )
type T_Decls_14  = CSubst ->
                   EvidKeyToCBindMap ->
                   PredScopeToCBindMap ->
                   VarMp ->
                   ValGam ->
                   TyGam ->
                   HsName ->
                   RangeMp ->
                   ( ErrSq,([CExpr -> CExpr]),CSubst,CBindL,ErrSq,CBindL,CBindL,(Seq.Seq (HsName,IdOccKind)),LamMp,ModEntRelFilterMp,VarMp,(Set.Set HsName),PP_Doc)
-- Expr --------------------------------------------------------

-- semantic domain
type T_Expr  = ( Range,T_Expr_1 )
type T_Expr_1  = UID ->
                 ( UID,Bool,T_Expr_2 )
type T_Expr_2  = Bool ->
                 KiGam ->
                 Int ->
                 Int ->
                 ( Int,T_Expr_3 )
type T_Expr_3  = VarMp ->
                 EHCOpts ->
                 PolGam ->
                 VarMp ->
                 PredScope ->
                 TyGam ->
                 TyKiGam ->
                 TyVarIdS ->
                 ( VarMp,VarMp,T_Expr_4 )
type T_Expr_4  = ClGam ->
                 VarMp ->
                 TyKiGam ->
                 DataGam ->
                 TyVarIdS ->
                 ( DataGam,T_Expr_5 )
type T_Expr_5  = DataGam ->
                 ( (Seq.FastSeq (CHRClassDecl Pred RedHowAnnotation)),FIIn,(Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),ClassDefaultGam,T_Expr_6 )
type T_Expr_6  = ScopedPredStore ->
                 ClassDefaultGam ->
                 FIOpts ->
                 Ty ->
                 VarMp ->
                 TyVarIdS ->
                 VarMp ->
                 ValGam ->
                 TyVarIdS ->
                 ( CHRPredOccCnstrMp,RangeMp,ValGam,TyVarIdS,Ty,VarMp,T_Expr_7 )
type T_Expr_7  = CSubst ->
                 EvidKeyToCBindMap ->
                 PredScopeToCBindMap ->
                 VarMp ->
                 ValGam ->
                 Bool ->
                 HsName ->
                 RangeMp ->
                 ( ErrSq,([Coe]),([PP_Doc]),CExpr,HsName,PP_Doc,([(CBindCateg,CBindL)]),CSubst,UIDS,CExpr,ErrSq,([(CBindCateg,CBindL)]),(FieldUpdateL CExpr),ClGam,(Seq.Seq (HsName,IdOccKind)),KiGam,LamMp,ModEntRelFilterMp,PolGam,VarMp,TyGam,TyKiGam,Bool,([PP_Doc]),PP_Doc,([(CBindCateg,CBindL)]),CExpr,(Set.Set HsName),PP_Doc)
-- ExprAnn -----------------------------------------------------

-- semantic domain
type T_ExprAnn  = ( PP_Doc)
-- FuncDep -----------------------------------------------------

-- semantic domain
type T_FuncDep  = UID ->
                  ( UID,T_FuncDep_1 )
type T_FuncDep_1  = TyGam ->
                    ( TyGam,T_FuncDep_2 )
type T_FuncDep_2  = TyKiGam ->
                    ( TyKiGam,T_FuncDep_3 )
type T_FuncDep_3  = TyL ->
                    VarMp ->
                    TyKiGam ->
                    VarMp ->
                    HsName ->
                    EHCOpts ->
                    TyVarIdS ->
                    TyVarIdS ->
                    TyVarIdS ->
                    TyVarIdS ->
                    ( ErrSq,ErrSq,([ClsFuncDep]),ModEntRelFilterMp,PP_Doc,Range)
-- FuncDeps ----------------------------------------------------

-- semantic domain
type T_FuncDeps  = UID ->
                   ( UID,T_FuncDeps_1 )
type T_FuncDeps_1  = TyGam ->
                     ( TyGam,T_FuncDeps_2 )
type T_FuncDeps_2  = TyKiGam ->
                     ( TyKiGam,T_FuncDeps_3 )
type T_FuncDeps_3  = TyL ->
                     VarMp ->
                     TyKiGam ->
                     VarMp ->
                     HsName ->
                     EHCOpts ->
                     TyVarIdS ->
                     TyVarIdS ->
                     TyVarIdS ->
                     TyVarIdS ->
                     ( ErrSq,ErrSq,([ClsFuncDep]),ModEntRelFilterMp,PP_Doc,([PP_Doc]),Range)
-- KiExpr ------------------------------------------------------

-- semantic domain
type T_KiExpr  = ( Range,T_KiExpr_1 )
type T_KiExpr_1  = UID ->
                   ( UID,T_KiExpr_2 )
type T_KiExpr_2  = KiGam ->
                   ( Ty,KiGam,T_KiExpr_3 )
type T_KiExpr_3  = VarMp ->
                   TyKiGam ->
                   VarMp ->
                   HsName ->
                   EHCOpts ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   ( ErrSq,([PP_Doc]),HsName,PP_Doc,ErrSq,ModEntRelFilterMp,PP_Doc)
-- KiExprAnn ---------------------------------------------------

-- semantic domain
type T_KiExprAnn  = ( PP_Doc)
-- MbTyExpr ----------------------------------------------------

-- semantic domain
type T_MbTyExpr  = ( Range,T_MbTyExpr_1 )
type T_MbTyExpr_1  = UID ->
                     ( UID,T_MbTyExpr_2 )
type T_MbTyExpr_2  = Polarity ->
                     PolGam ->
                     VarMp ->
                     TyGam ->
                     ( VarMp,(Maybe Ty),T_MbTyExpr_3 )
type T_MbTyExpr_3  = VarMp ->
                     TyKiGam ->
                     ( TyKiGam,VarMp,T_MbTyExpr_4 )
type T_MbTyExpr_4  = ClGam ->
                     VarMp ->
                     TyKiGam ->
                     VarMp ->
                     KiGam ->
                     HsName ->
                     EHCOpts ->
                     TyVarIdS ->
                     TyVarIdS ->
                     TyVarIdS ->
                     TyVarIdS ->
                     ( ErrSq,HsNameS,HsNameS,ErrSq,ModEntRelFilterMp,PP_Doc,(Maybe PP_Doc),TyGam,TyKiGam,TyVarWildMp)
-- PatExpr -----------------------------------------------------

-- semantic domain
type T_PatExpr  = ( Range,T_PatExpr_1 )
type T_PatExpr_1  = UID ->
                    ( UID,T_PatExpr_2 )
type T_PatExpr_2  = KiGam ->
                    Int ->
                    Int ->
                    ( Int,T_PatExpr_3 )
type T_PatExpr_3  = VarMp ->
                    EHCOpts ->
                    PolGam ->
                    VarMp ->
                    PredScope ->
                    TyGam ->
                    TyKiGam ->
                    TyVarIdS ->
                    ( VarMp,VarMp,TyGam,TyKiGam,T_PatExpr_4 )
type T_PatExpr_4  = ClGam ->
                    DataGam ->
                    VarMp ->
                    TyKiGam ->
                    TyVarIdS ->
                    ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),HsName,T_PatExpr_5 )
type T_PatExpr_5  = ValGam ->
                    ( Int,(Maybe HsName),Ty,T_PatExpr_6 )
type T_PatExpr_6  = FIOpts ->
                    Bool ->
                    Ty ->
                    TyL ->
                    VarMp ->
                    ValGam ->
                    ( VarMp,ValGam,T_PatExpr_7 )
type T_PatExpr_7  = ScopedPredStore ->
                    ClassDefaultGam ->
                    VarMp ->
                    TyVarIdS ->
                    VarMp ->
                    TyVarIdS ->
                    ( RPatNm,CHRPredOccCnstrMp,RangeMp,ScopeGam,Ty,VarMp,T_PatExpr_8 )
type T_PatExpr_8  = CSubst ->
                    HsName ->
                    EvidKeyToCBindMap ->
                    PredScopeToCBindMap ->
                    VarMp ->
                    ValGam ->
                    HsName ->
                    RangeMp ->
                    ( ErrSq,([PP_Doc]),HsName,PP_Doc,CSubst,CBindL,ErrSq,FieldSplitL,ModEntRelFilterMp,VarMp,Bool,CPatRest,PP_Doc,RPat)
-- PatExprAnn --------------------------------------------------

-- semantic domain
type T_PatExprAnn  = ( PP_Doc)
-- PrExpr ------------------------------------------------------

-- semantic domain
type T_PrExpr  = ( Range,T_PrExpr_1 )
type T_PrExpr_1  = UID ->
                   ( UID,T_PrExpr_2 )
type T_PrExpr_2  = TyGam ->
                   ( Ty,Ty,TyGam,T_PrExpr_3 )
type T_PrExpr_3  = Polarity ->
                   ( ([Polarity]),T_PrExpr_4 )
type T_PrExpr_4  = PolGam ->
                   VarMp ->
                   ( VarMp,T_PrExpr_5 )
type T_PrExpr_5  = TyKiGam ->
                   ( TyKiGam,TyKiGam,T_PrExpr_6 )
type T_PrExpr_6  = VarMp ->
                   ( PolGam,Ty,VarMp,TyVarWildMp,T_PrExpr_7 )
type T_PrExpr_7  = ClGam ->
                   ( Ty,T_PrExpr_8 )
type T_PrExpr_8  = VarMp ->
                   TyKiGam ->
                   VarMp ->
                   KiGam ->
                   HsName ->
                   EHCOpts ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   ( ErrSq,HsNameS,HsNameS,ErrSq,ModEntRelFilterMp,PP_Doc,Pred)
-- PrExprs -----------------------------------------------------

-- semantic domain
type T_PrExprs  = ClGam ->
                  VarMp ->
                  TyKiGam ->
                  VarMp ->
                  UID ->
                  KiGam ->
                  VarMp ->
                  Polarity ->
                  HsName ->
                  EHCOpts ->
                  PolGam ->
                  VarMp ->
                  TyGam ->
                  TyKiGam ->
                  TyVarIdS ->
                  TyVarIdS ->
                  TyVarIdS ->
                  TyVarIdS ->
                  ( ErrSq,HsNameS,HsNameS,ErrSq,UID,ModEntRelFilterMp,TyKiGam,VarMp,VarMp,PP_Doc,([PP_Doc]),([Pred]),Range,TyGam,TyKiGam,TyL,TyVarWildMp)
-- RecExpr -----------------------------------------------------

-- semantic domain
type T_RecExpr  = ( Range,T_RecExpr_1 )
type T_RecExpr_1  = UID ->
                    ( UID,T_RecExpr_2 )
type T_RecExpr_2  = KiGam ->
                    Int ->
                    Int ->
                    ( Int,T_RecExpr_3 )
type T_RecExpr_3  = VarMp ->
                    EHCOpts ->
                    PolGam ->
                    VarMp ->
                    PredScope ->
                    TyGam ->
                    TyKiGam ->
                    TyVarIdS ->
                    ( VarMp,VarMp,T_RecExpr_4 )
type T_RecExpr_4  = ClGam ->
                    VarMp ->
                    TyKiGam ->
                    DataGam ->
                    TyVarIdS ->
                    ( DataGam,T_RecExpr_5 )
type T_RecExpr_5  = DataGam ->
                    ([HsName]) ->
                    ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),([HsName]),T_RecExpr_6 )
type T_RecExpr_6  = ScopedPredStore ->
                    ClassDefaultGam ->
                    FIOpts ->
                    Ty ->
                    VarMp ->
                    TyVarIdS ->
                    VarMp ->
                    ValGam ->
                    TyVarIdS ->
                    ( CHRPredOccCnstrMp,RangeMp,TyVarIdS,VarMp,T_RecExpr_7 )
type T_RecExpr_7  = CSubst ->
                    EvidKeyToCBindMap ->
                    PredScopeToCBindMap ->
                    VarMp ->
                    ValGam ->
                    HsName ->
                    RangeMp ->
                    ( ErrSq,CSubst,ErrSq,HsName,(FieldUpdateL CExpr),ModEntRelFilterMp,VarMp,Bool,PP_Doc,([PP_Doc]),CExpr,Ty)
-- RecPatExpr --------------------------------------------------

-- semantic domain
type T_RecPatExpr  = ( Range,T_RecPatExpr_1 )
type T_RecPatExpr_1  = UID ->
                       ( UID,T_RecPatExpr_2 )
type T_RecPatExpr_2  = KiGam ->
                       Int ->
                       Int ->
                       ( Int,T_RecPatExpr_3 )
type T_RecPatExpr_3  = VarMp ->
                       EHCOpts ->
                       PolGam ->
                       VarMp ->
                       PredScope ->
                       TyGam ->
                       TyKiGam ->
                       TyVarIdS ->
                       ( VarMp,VarMp,TyGam,TyKiGam,T_RecPatExpr_4 )
type T_RecPatExpr_4  = ClGam ->
                       DataGam ->
                       VarMp ->
                       TyKiGam ->
                       ([HsName]) ->
                       TyVarIdS ->
                       ( (Seq.FastSeq (CHRScopedInstanceDecl Pred RedHowAnnotation PredScope)),Bool,([HsName]),T_RecPatExpr_5 )
type T_RecPatExpr_5  = FIOpts ->
                       Ty ->
                       Bool ->
                       VarMp ->
                       ValGam ->
                       ValGam ->
                       ( VarMp,ValGam,T_RecPatExpr_6 )
type T_RecPatExpr_6  = ScopedPredStore ->
                       ClassDefaultGam ->
                       VarMp ->
                       TyVarIdS ->
                       VarMp ->
                       TyVarIdS ->
                       ( CHRPredOccCnstrMp,RangeMp,ScopeGam,Ty,VarMp,T_RecPatExpr_7 )
type T_RecPatExpr_7  = CSubst ->
                       HsName ->
                       EvidKeyToCBindMap ->
                       PredScopeToCBindMap ->
                       VarMp ->
                       ValGam ->
                       HsName ->
                       RangeMp ->
                       ( ErrSq,CSubst,CBindL,ErrSq,HsName,FieldSplitL,ModEntRelFilterMp,VarMp,CPatRest,PP_Doc,([PP_Doc]))
-- RowTyExpr ---------------------------------------------------

-- semantic domain
type T_RowTyExpr  = ( Range,T_RowTyExpr_1 )
type T_RowTyExpr_1  = UID ->
                      ([HsName]) ->
                      ( UID,([HsName]),T_RowTyExpr_2 )
type T_RowTyExpr_2  = TyGam ->
                      ( TyGam,Ty,T_RowTyExpr_3 )
type T_RowTyExpr_3  = Polarity ->
                      PolGam ->
                      VarMp ->
                      ( VarMp,T_RowTyExpr_4 )
type T_RowTyExpr_4  = TyKiGam ->
                      ( TyKiGam,TyKiGam,T_RowTyExpr_5 )
type T_RowTyExpr_5  = VarMp ->
                      ( VarMp,TyVarWildMp,T_RowTyExpr_6 )
type T_RowTyExpr_6  = ClGam ->
                      VarMp ->
                      TyKiGam ->
                      VarMp ->
                      KiGam ->
                      HsName ->
                      EHCOpts ->
                      TyVarIdS ->
                      TyVarIdS ->
                      TyVarIdS ->
                      TyVarIdS ->
                      ( ErrSq,HsNameS,HsNameS,ErrSq,HsName,ModEntRelFilterMp,PP_Doc,([PP_Doc]))
-- TyExpr ------------------------------------------------------

-- semantic domain
type T_TyExpr  = ( Range,T_TyExpr_1 )
type T_TyExpr_1  = UID ->
                   ( UID,T_TyExpr_2 )
type T_TyExpr_2  = TyGam ->
                   ( Ty,TyGam,T_TyExpr_3 )
type T_TyExpr_3  = Polarity ->
                   ( ([Polarity]),T_TyExpr_4 )
type T_TyExpr_4  = PolGam ->
                   VarMp ->
                   ( (Maybe Strictness),VarMp,T_TyExpr_5 )
type T_TyExpr_5  = TyKiGam ->
                   ( TyKiGam,TyKiGam,T_TyExpr_6 )
type T_TyExpr_6  = VarMp ->
                   ( PolGam,Ty,VarMp,Polarity,TyVarWildMp,T_TyExpr_7 )
type T_TyExpr_7  = ClGam ->
                   ( Ty,T_TyExpr_8 )
type T_TyExpr_8  = VarMp ->
                   TyKiGam ->
                   VarMp ->
                   KiGam ->
                   HsName ->
                   EHCOpts ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   ( ErrSq,([PP_Doc]),HsName,PP_Doc,HsNameS,HsNameS,ErrSq,ModEntRelFilterMp,PP_Doc,TyL)
-- TyExprAnn ---------------------------------------------------

-- semantic domain
type T_TyExprAnn  = ( TyAnn,Bool,(Maybe Strictness),PP_Doc)
-- TyExprs -----------------------------------------------------

-- semantic domain
type T_TyExprs  = ( Range,T_TyExprs_1 )
type T_TyExprs_1  = UID ->
                    ( UID,T_TyExprs_2 )
type T_TyExprs_2  = TyGam ->
                    ( ([Polarity]),TyGam,TyL,T_TyExprs_3 )
type T_TyExprs_3  = PolGam ->
                    VarMp ->
                    ( VarMp,T_TyExprs_4 )
type T_TyExprs_4  = TyKiGam ->
                    ( TyKiGam,TyKiGam,T_TyExprs_5 )
type T_TyExprs_5  = VarMp ->
                    ( PolGam,TyL,VarMp,TyVarWildMp,T_TyExprs_6 )
type T_TyExprs_6  = ClGam ->
                    VarMp ->
                    TyKiGam ->
                    VarMp ->
                    KiGam ->
                    Polarity ->
                    HsName ->
                    EHCOpts ->
                    TyVarIdS ->
                    TyVarIdS ->
                    TyVarIdS ->
                    TyVarIdS ->
                    ( ErrSq,HsNameS,HsNameS,ErrSq,ModEntRelFilterMp,PP_Doc,([PP_Doc]))
-- TyVar -------------------------------------------------------

-- semantic domain
type T_TyVar  = ( Range,T_TyVar_1 )
type T_TyVar_1  = UID ->
                  ( UID,T_TyVar_2 )
type T_TyVar_2  = TyGam ->
                  ( TyGam,T_TyVar_3 )
type T_TyVar_3  = TyKiGam ->
                  ( PolGam,TyKiGam,Ty,([Polarity]),Ty,TyKiGam,T_TyVar_4 )
type T_TyVar_4  = VarMp ->
                  TyKiGam ->
                  VarMp ->
                  HsName ->
                  EHCOpts ->
                  TyVarIdS ->
                  TyVarIdS ->
                  TyVarIdS ->
                  TyVarIdS ->
                  ( ErrSq,ErrSq,ModEntRelFilterMp,HsName,PP_Doc)
-- TyVars ------------------------------------------------------

-- semantic domain
type T_TyVars  = ( Range,T_TyVars_1 )
type T_TyVars_1  = UID ->
                   ( UID,T_TyVars_2 )
type T_TyVars_2  = TyGam ->
                   ( TyGam,T_TyVars_3 )
type T_TyVars_3  = TyKiGam ->
                   ( PolGam,TyKiGam,TyL,([Polarity]),TyKiGam,TyL,T_TyVars_4 )
type T_TyVars_4  = VarMp ->
                   TyKiGam ->
                   VarMp ->
                   HsName ->
                   EHCOpts ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   TyVarIdS ->
                   ( ErrSq,ErrSq,ModEntRelFilterMp,([HsName]),PP_Doc,([PP_Doc]))
