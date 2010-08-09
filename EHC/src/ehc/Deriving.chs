%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deriving info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(91 codegen) module {%{EH}Deriving} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Gam.Full},{%{EH}Ty})
%%]

%%[(91 codegen) import({%{EH}Core},{%{EH}Core.Utils})
%%]

%%[(91 codegen) import({%{EH}AbstractCore})
%%]

%%[(91 codegen) import(qualified Data.Map as Map,Data.List,EH.Util.Utils)
%%]

%%[(91 codegen hmtyinfer) import({%{EH}Ty.FitsInCommon2}, {%{EH}Ty.Trf.Canonic})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table for each derivable field of each derivable class: type as well as codegen info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(91 codegen) export(DerivCls(..),DerivClsFld(..),DerivClsMp)
data DerivClsExtraCtxt
  = DerivClsExtraCtxt_Fixed                                                     -- fixed predicate, independent of type over which is derived
      { dccClsClNm                  :: !HsName                                  -- class name
      , dccClsTyNm                  :: !HsName                                  -- and its type arg
      }

data DerivCls
  = DerivCls
      { dclNm                       :: !HsName                                  -- name of class
      , dclFldL                     :: ![DerivClsFld]                           -- field info
      , dclExtraCtxt                :: ![DerivClsExtraCtxt]                     -- additional instances to prove (and use)
      }

data DerivClsFld
  = DerivClsFld
      { dcfNm                       :: !HsName                                  -- name of class field
      , dcfTy                       :: !Ty                                      -- type of class field, expanded w.r.t. type synonyms
      , dcfMkCPat                   ::                                          -- make the pattern for each alternative
                                       Int                                      -- index of alternative (the actual Enum ordering)
                                       -> CTag                                  -- tag
                                       -> Int                                   -- arity
                                       -> Maybe [HsName]                        -- optional list of names for constituents
                                       -> CPat
      , dcfInitialArgL              :: ![HsName]                                -- initial arguments for function, passed to dcfFoldSubsCExpr
      , dcfInitialSubArgL           :: DataTagInfo -> [CExpr]                   -- initial arguments for constituents
      , dcfNrOmitTailArg            :: !Int                                     -- nr of args not passed, i.e. how much to omit for partial app
      , dcfFoldSubsCExpr            :: UID                                      -- construct out of constituents
                                       -> RCEEnv
                                       -> DataTagInfo                           -- this tag info
                                       -> (Int,Int)                             -- (index (a la toEnum), nr of alts)
                                       -> [HsName]                              -- names of initial args (usually same as dcfInitialArgL)
                                       -> [CExpr]                               -- constituents
                                       -> CExpr
      , dcfNoArgSubsCExpr           :: RCEEnv
                                       -> [(DataTagInfo,[CExpr])]               -- all tag info + sub calls parameterized with appropriate instance
                                       -> Int                                   -- nr of alts
                                       -> CExpr                                 -- instance under construction
                                       -> [CExpr]                               -- instances corresponding to dclExtraCtxt
                                       -> [HsName]                              -- names of initial args (usually same as dcfInitialArgL)
                                       -> CExpr                                 -- when class member takes no args
      , dcfAllTagLtCExpr            :: !CExpr                                   -- when tags are < previous
      , dcfAllTagGtCExpr            :: !CExpr                                   -- when tags are > previous
      , dcfWrapCase                 ::                                          -- wrap case expr in derived function
                                       EHCOpts
                                       -> DataGamInfo                           -- info of data ty
                                       -> Int                                   -- nr of alts
                                       -> HsName                                -- name of expr inspected by case expr
                                       -> [HsName]                              -- names of initial args (usually same as dcfInitialArgL)
                                       -> CExpr                                 -- case expr
                                       -> CExpr
      }

type DerivClsMp = Map.Map HsName DerivCls
%%]

%%[(91 codegen) export(emptyDerivCls)
emptyDerivCls :: DerivCls
emptyDerivCls = DerivCls hsnUnknown [] []
%%]

%%[(91 codegen) export(dccMkTy)
dccMkTy :: DerivClsExtraCtxt -> Ty
dccMkTy (DerivClsExtraCtxt_Fixed clNm tyNm) = mkConApp clNm [semCon tyNm]
%%]

The table is dependent on builtin names, stored in EHCOpts in FIEnv.

%%[(91 codegen) export(mkDerivClsMp)
mkDerivClsMp :: FIEnv -> ValGam -> DataGam -> DerivClsMp
mkDerivClsMp fe valGam dataGam
  = Map.unions
    $ map (uncurry Map.singleton)
    $ [
      -- Eq
         mkc ehbnClassEq []
           [
           -- Eq((==))
             mkf ehbnClassEqFldEq
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ _ _ _ _ vs
                     -> case vs of
                          [] -> true
                          _  -> foldr1 and vs
                )
                Nothing -- no zero arg
                false false
                nowrap
           ]
           
      -- Ord
      ,  mkc ehbnClassOrd []
           [
           -- Ord(compare)
             mkf ehbnClassOrdFldCompare
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\uniq env _ _ _ vs
                     -> case vs of
                          [] -> eq
                          _  -> foldr1 (\l r -> acoreStrictSatCase env (Just nStrict) l
                                                  [ CAlt_Alt (acorePatTagArityMbNms (orderingTag eqNm) 0 Nothing) r
                                                  , CAlt_Alt (acorePatTagArityMbNms (orderingTag ltNm) 0 Nothing) lt
                                                  , CAlt_Alt (acorePatTagArityMbNms (orderingTag gtNm) 0 Nothing) gt
                                                  ]
                                       ) vs
                             where n = mkHNm uniq
                                   nStrict = hsnUniqifyEval n
                )
                Nothing -- no zero arg
                gt lt
                nowrap
           ]
           
      -- Enum
      ,  mkc ehbnClassEnum [DerivClsExtraCtxt_Fixed (fn ehbnClassEnum) hsnInt]
           [
           -- Enum(enumFrom)
             mkf ehbnClassEnumFldEnumFrom
                Nothing -- only data constructor patterns
                (take 1 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \_ dtiSubs nrOfAlts self _ [argFrom]
                     -> let (dti,_) = last dtiSubs
                        in  acoreBuiltinApp opts ehbnClassEnumFldEnumFromTo [self,acoreVar argFrom,CExpr_Tup (dtiCTag dti)]
                )
                undef undef
                nowrap
           -- Enum(enumFromThen)
           , mkf ehbnClassEnumFldEnumFromThen
                Nothing -- only data constructor patterns
                (take 2 hsnLclSupply)
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \_ dtiSubs nrOfAlts self _ [argFrom,argThen]
                     -> let (dti1,_) = head dtiSubs
                            (dti2,_) = last dtiSubs
                        in  acoreBuiltinApp opts ehbnClassEnumFldEnumFromThenTo
                              [ self
                              , acoreVar argFrom
                              , acoreVar argThen
                              , acoreIf opts Nothing
                                  (acoreBuiltinApp opts ehbnPrimGtInt
                                     [ acoreBuiltinApp opts ehbnClassEnumFldFromEnum [self, acoreVar argFrom]
                                     , acoreBuiltinApp opts ehbnClassEnumFldFromEnum [self, acoreVar argThen]
                                     ]
                                  )
                                  (CExpr_Tup (dtiCTag dti1))
                                  (CExpr_Tup (dtiCTag dti2))
                              ]
                )
                undef undef
                nowrap
           -- Enum(fromEnum)
           , mkf ehbnClassEnumFldFromEnum
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env _ (altInx,_) _ []
                     -> acoreInt altInx
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           -- Enum(toEnum)
           , mkf ehbnClassEnumFldToEnum
                (Just $ \altInx _ _ _ -> CPat_Int altInx)
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ _ dti _ _ []
                     -> CExpr_Tup (dtiCTag dti)
                )
                Nothing -- no zero arg
                undef undef
                (\opts dgi nrOfAlts cNm _ body
                  -> let cNmv = acoreVar cNm
                         cNm1 = hsnUniqifyStr HsNameUniqifier_Evaluated "boundCheck" cNm
                     in  acoreIf opts (Just cNm1)
                           (acoreBuiltinApp opts ehbnPrimGtInt [cNmv,acoreInt (nrOfAlts-1)])
                           (acoreBuiltinError opts $ "too high for toEnum to " ++ show (dgiTyNm dgi))
                           (acoreIf opts (Just cNm1)
                             (acoreBuiltinApp opts ehbnPrimGtInt [acoreInt 0,cNmv])
                             (acoreBuiltinError opts $ "too low for toEnum to " ++ show (dgiTyNm dgi))
                             body
                           )
                )
           -- Enum(succ)
           , mkf ehbnClassEnumFldSucc
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env dti (altInx,nrOfAlts) _ []
                     -> if altInx < nrOfAlts  - 1
                        then acoreInt $ altInx + 1
                        else acoreBuiltinError opts $ "cannot succ last constructor: " ++ show (dtiConNm dti)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           -- Enum(pred)
           , mkf ehbnClassEnumFldPred
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                (\_ env dti (altInx,_) _ []
                     -> if altInx > 0
                        then acoreInt $ altInx - 1
                        else acoreBuiltinError opts $ "cannot pred first constructor: " ++ show (dtiConNm dti)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
           
      -- Bounded
      {-
      ,  mkc ehbnClassBounded []
           [
           -- Bounded(maxBound)
             mkf ehbnClassBoundedFldMaxBound
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \_ dtiSubs _ _ _ _
                     -> let (dti,subs) = last dtiSubs
                        in  acoreTagTup (dtiCTag dti) subs
                )
                undef undef
                nowrap
           -- Bounded(minBound)
           , mkf ehbnClassBoundedFldMinBound
                Nothing -- only data constructor patterns
                []
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \_ dtiSubs _ _ _ _
                     -> let (dti,subs) = head dtiSubs
                        in  acoreTagTup (dtiCTag dti) subs
                )
                undef undef
                nowrap
           ]
      -}
      
      -- Show
      ,  mkc ehbnClassShow []
           [
           -- Show(showsPrec)
             mkf ehbnClassShowFldShowsPrec
                Nothing -- only data constructor patterns
                [precDepthNm]
                (Just $ \dti -> [acoreInt $ maybe (fixityAppPrio + 1) ((+1) . fst) $ dtiMbFixityPrio dti])
                1
                (\_ _ dti _ [dNm] vs
                     -> let mk needParen v = acoreApp (acoreVar $ fn ehbnPrelShowParen) [needParen,v]
                        in  case (vs,dtiMbFixityPrio dti) of
                              ([],_)
                                -> showString $ tag2str (dtiCTag dti)
                              ([v1,v2],Just (p,_))
                                -> mk (acoreBuiltinGtInt opts (acoreVar dNm) p)
                                   $ foldr1 compose ([v1] ++ [ showString $ " " ++ tag2str (dtiCTag dti) ++ " " ] ++ [v2])
                              _ -> mk (acoreBuiltinGtInt opts (acoreVar dNm) fixityAppPrio)
                                   $ foldr1 compose ([showString $ tag2str (dtiCTag dti) ++ " "] ++ intersperse (showString " ") vs)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
           
%%[[99
      -- Ix
      {-
      -}
      ,  mkc ehbnClassIx []
           [
           -- Ix(range)
           -- Ix(index)
           -- Ix(inRange)
           ]
      
      -- Read
      ,  mkc ehbnClassRead []
           [
           -- Read(readsPrec)
             mkf ehbnClassReadFldReadsPrec
                Nothing -- only data constructor patterns
                [precDepthNm,remInputNm]
                Nothing -- no extra args for recursion on constituents
                0
                nononzeroargs
                (Just $
                  \env dtiSubs _ _ _ [dNm,rNm]
                     -> let mkSub (dti,subCalls)
                              = acoreApp (acoreVar $ fn ehbnPrelReadParen) [needParen,v,acoreVar rNm]
                              where nSub = length subCalls
                                    nms seed = take (nSub+1) $ hsnLclSupplyWith $ mkHNmHidden seed
                                    remNmL  = nms "v"
                                    nmL = zip4 (nms "uv") (rNm:remNmL) (nms "u") remNmL
                                    nmLRemFinal = last remNmL
                                    mkLamTup     x res rem cont = acoreLam1 x $ acoreMatchTuple env [res,rem] cont (acoreVar x)
                                    mkLamStr str x res rem cont = mkLamTup x res rem $ acoreMatchString env str cont nil (acoreVar res)
                                    mkConcatMapTup lam prio rem subCall = acoreApp (acoreVar $ fn ehbnPrelConcatMap) [lam,acoreApp subCall [prio,acoreVar rem]]
                                    mkConcatMapStr lam      rem         = acoreApp (acoreVar $ fn ehbnPrelConcatMap) [lam,acoreApp (acoreVar $ fn ehbnPrelLex) [acoreVar rem]]
                                    mkPrio prio = acoreInt (prio + 1)
                                    mkRes  resL rem = acoreBuiltinListSingleton opts $ acoreTup [acoreTagTup (dtiCTag dti) $ map acoreVar resL,acoreVar rem]
                                    mkConRead (resrem,remprev,res,rem) = \cont -> mkConcatMapStr (mkLamStr (tag2str (dtiCTag dti)) resrem res rem cont) remprev
                                    mkSubReads tlNmL subCalls = [ (\cont -> mkConcatMapTup (mkLamTup resrem res rem cont) (mkPrio fixityAppPrio) remPrev subCall, res) | ((resrem,remPrev,res,rem),subCall) <- zip tlNmL subCalls ]
                                    (needParen,v)
                                      = case (subCalls,dtiMbFixityPrio dti,nmL) of
                                          ([subCall1,subCall2],Just (p,_),(subNms1:conNms:subNms2:_))
                                            -> ( acoreBuiltinGtInt opts (acoreVar dNm) p
                                               , acoreLam1 rNm $ sub1 $ mkConRead conNms $ sub2 $ mkRes subResNmL nmLRemFinal
                                               )
                                            where ([sub1,sub2],subResNmL) = unzip $ mkSubReads [subNms1,subNms2] subCalls
                                          (_,_,(conNms:subNmsL))
                                            -> ( acoreBuiltinGtInt opts (acoreVar dNm) fixityAppPrio
                                               , acoreLam1 rNm $ foldr ($) (mkRes subResNmL nmLRemFinal) (mkConRead conNms : subs)
                                               )
                                            where (subs,subResNmL) = unzip $ mkSubReads subNmsL subCalls
                                          
                        in  foldr1 (\ds e -> acoreApp (acoreVar $ fn ehbnPrelConcat2) [ds,e])
                            $ map mkSub dtiSubs
                )
                undef undef
                nowrap
           ]
%%]]
      ]
  where mkc c extraCtxt flds
          = (c', DerivCls c' flds extraCtxt)
          where c' = fn c
        mkf f mbMkPat as mbAsSubs omTl cAllMatch mbCNoArg cLT cGT wrap
          = DerivClsFld f' (tyCanonic (emptyFI {fiEnv = fe}) t) mkPat as asSubs omTl cAllMatch cNoArg cLT cGT wrap
          where f' = fn f
                (t,_) = valGamLookupTy f' valGam
                mkPat  = maybe (const acorePatTagArityMbNms) id mbMkPat
                asSubs = maybe (const []) id mbAsSubs
                cNoArg = maybe (\_ _ _ _ _ _ -> undef) id mbCNoArg
        fn f  = ehcOptBuiltin opts f
        false = acoreVar $ fn ehbnBoolFalse
        true  = acoreVar $ fn ehbnBoolTrue
        nil = CExpr_Tup $ ctagNil opts
        and l r = (acoreVar $ fn ehbnBoolAnd) `acoreApp` [l,r]
        compose l r = (acoreVar $ fn ehbnPrelCompose) `acoreApp` [l,r]
        showString s = (acoreVar $ fn ehbnPrelShowString) `acoreApp` [acoreBuiltinString opts s]
        eq = acoreVar eqNm
        lt = acoreVar ltNm
        gt = acoreVar gtNm
        undef = acoreBuiltinUndefined opts
        tag2str = show . hsnQualified . ctagNm
        orderingTag conNm
          = dtiCTag
            $ dgiDtiOfCon conNm
            $ panicJust "mkDerivClsMp.dataGamLookup"
            $ dataGamLookup (fn ehbnDataOrdering) dataGam
        nowrap _ _ _ _ _ x = x
        nononzeroargs _ _ _ _ _ _ = undef
        opts = feEHCOpts fe
        eqNm = fn ehbnDataOrderingAltEQ
        ltNm = fn ehbnDataOrderingAltLT
        gtNm = fn ehbnDataOrderingAltGT
        precDepthNm = mkHNm "d"
%%[[99
        remInputNm = mkHNm "r"
%%]]
%%]

