%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deriving info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(95 codegen) module {%{EH}Deriving} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Gam.Full},{%{EH}Ty})
%%]

%%[(95 codegen) import({%{EH}Core},{%{EH}Core.Utils})
%%]

%%[(95 codegen) import(qualified Data.Map as Map,Data.List,EH.Util.Utils)
%%]

%%[(95 codegen hmtyinfer) import({%{EH}Ty.FitsInCommon2}, {%{EH}Ty.Trf.Canonic})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table for each derivable field of each derivable class: type as well as codegen info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(95 codegen) export(DerivCls(..),DerivClsFld(..),DerivClsMp)
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

%%[(95 codegen) export(emptyDerivCls)
emptyDerivCls :: DerivCls
emptyDerivCls = DerivCls hsnUnknown [] []
%%]

%%[(95 codegen) export(dccMkTy)
dccMkTy :: DerivClsExtraCtxt -> Ty
dccMkTy (DerivClsExtraCtxt_Fixed clNm tyNm) = mkConApp clNm [semCon tyNm]
%%]

The table is dependent on builtin names, stored in EHCOpts in FIEnv.

%%[(95 codegen) export(mkDerivClsMp)
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
                          _  -> foldr1 (\l r -> mkCExprStrictSatCase env (Just nStrict) l
                                                  [ CAlt_Alt (mkCPatCon (orderingTag eqNm) 0 Nothing) r
                                                  , CAlt_Alt (mkCPatCon (orderingTag ltNm) 0 Nothing) lt
                                                  , CAlt_Alt (mkCPatCon (orderingTag gtNm) 0 Nothing) gt
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
                        in  cbuiltinApp opts ehbnClassEnumFldEnumFromTo [self,CExpr_Var argFrom,CExpr_Tup (dtiCTag dti)]
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
                        in  cbuiltinApp opts ehbnClassEnumFldEnumFromThenTo
                              [ self
                              , CExpr_Var argFrom
                              , CExpr_Var argThen
                              , mkCIf opts Nothing
                                  (cbuiltinApp opts ehbnPrimGtInt
                                     [ cbuiltinApp opts ehbnClassEnumFldFromEnum [self, CExpr_Var argFrom]
                                     , cbuiltinApp opts ehbnClassEnumFldFromEnum [self, CExpr_Var argThen]
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
                     -> CExpr_Int altInx
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
                  -> let cNmv = CExpr_Var cNm
                         cNm1 = hsnUniqifyStr HsNameUniqifier_Evaluated "boundCheck" cNm
                     in  mkCIf opts (Just cNm1)
                           (cbuiltinApp opts ehbnPrimGtInt [cNmv,CExpr_Int (nrOfAlts-1)])
                           (cerror opts $ "too high for toEnum to " ++ show (dgiTyNm dgi))
                           (mkCIf opts (Just cNm1)
                             (cbuiltinApp opts ehbnPrimGtInt [CExpr_Int 0,cNmv])
                             (cerror opts $ "too low for toEnum to " ++ show (dgiTyNm dgi))
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
                        then CExpr_Int $ altInx + 1
                        else cerror opts $ "cannot succ last constructor: " ++ show (dtiConNm dti)
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
                        then CExpr_Int $ altInx - 1
                        else cerror opts $ "cannot pred first constructor: " ++ show (dtiConNm dti)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
           
      -- Bounded
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
                        in  mkCExprTuple' (dtiCTag dti) subs
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
                        in  mkCExprTuple' (dtiCTag dti) subs
                )
                undef undef
                nowrap
           ]
           
      -- Show
      ,  mkc ehbnClassShow []
           [
           -- Show(showsPrec)
             mkf ehbnClassShowFldShowsPrec
                Nothing -- only data constructor patterns
                [precDepthNm]
                (Just $ \dti -> [CExpr_Int $ maybe (fixityAppPrio + 1) (+1) $ dtiMbFixityPrio $ dti])
                1
                (\_ _ dti _ [dNm] vs
                     -> let mk needParen v = mkCExprApp (CExpr_Var $ fn ehbnPrelShowParen) [needParen,v]
                        in  case (vs,dtiMbFixityPrio dti) of
                              ([],_)
                                -> showString $ tag2str (dtiCTag dti)
                              ([v1,v2],Just p)
                                -> mk (cgtint opts (CExpr_Var dNm) p)
                                   $ foldr1 compose ([v1] ++ [ showString $ " " ++ tag2str (dtiCTag dti) ++ " " ] ++ [v2])
                              _ -> mk (cgtint opts (CExpr_Var dNm) fixityAppPrio)
                                   $ foldr1 compose ([showString $ tag2str (dtiCTag dti) ++ " "] ++ intersperse (showString " ") vs)
                )
                Nothing -- no zero arg
                undef undef
                nowrap
           ]
           
%%[[99
      -- Ix
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
                              = mkCExprApp (CExpr_Var $ fn ehbnPrelReadParen) [needParen,v,CExpr_Var rNm]
                              where nSub = length subCalls
                                    nms seed = take (nSub+1) $ hsnLclSupplyWith $ mkHNmHidden seed
                                    remNmL  = nms "v"
                                    nmL = zip4 (nms "uv") (rNm:remNmL) (nms "u") remNmL
                                    nmLRemFinal = last remNmL
                                    mkLamTup     x res rem cont = mkCExprLam1 x $ mkCMatchTuple env [res,rem] cont (CExpr_Var x)
                                    mkLamStr str x res rem cont = mkLamTup x res rem $ mkCMatchString env str cont nil (CExpr_Var res)
                                    mkConcatMapTup lam prio rem subCall = mkCExprApp (CExpr_Var $ fn ehbnPrelConcatMap) [lam,mkCExprApp subCall [prio,CExpr_Var rem]]
                                    mkConcatMapStr lam      rem         = mkCExprApp (CExpr_Var $ fn ehbnPrelConcatMap) [lam,mkCExprApp (CExpr_Var $ fn ehbnPrelLex) [CExpr_Var rem]]
                                    mkPrio prio = CExpr_Int (prio + 1)
                                    mkRes  resL rem = mkCListSingleton opts $ mkCExprTuple [mkCExprTuple' (dtiCTag dti) $ map CExpr_Var resL,CExpr_Var rem]
                                    mkConRead (resrem,remprev,res,rem) = \cont -> mkConcatMapStr (mkLamStr (tag2str (dtiCTag dti)) resrem res rem cont) remprev
                                    mkSubReads tlNmL subCalls = [ (\cont -> mkConcatMapTup (mkLamTup resrem res rem cont) (mkPrio fixityAppPrio) remPrev subCall, res) | ((resrem,remPrev,res,rem),subCall) <- zip tlNmL subCalls ]
                                    (needParen,v)
                                      = case (subCalls,dtiMbFixityPrio dti,nmL) of
                                          ([subCall1,subCall2],Just p,(subNms1:conNms:subNms2:_))
                                            -> ( cgtint opts (CExpr_Var dNm) p
                                               , mkCExprLam1 rNm $ sub1 $ mkConRead conNms $ sub2 $ mkRes subResNmL nmLRemFinal
                                               )
                                            where ([sub1,sub2],subResNmL) = unzip $ mkSubReads [subNms1,subNms2] subCalls
                                          (_,_,(conNms:subNmsL))
                                            -> ( cgtint opts (CExpr_Var dNm) fixityAppPrio
                                               , mkCExprLam1 rNm $ foldr ($) (mkRes subResNmL nmLRemFinal) (mkConRead conNms : subs)
                                               )
                                            where (subs,subResNmL) = unzip $ mkSubReads subNmsL subCalls
                                          
                        in  foldr1 (\ds e -> mkCExprApp (CExpr_Var $ fn ehbnPrelConcat2) [ds,e])
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
                mkPat = maybe (const mkCPatCon) id mbMkPat
                asSubs = maybe (const []) id mbAsSubs
                cNoArg = maybe (\_ _ _ _ _ _ -> undef) id mbCNoArg
        fn f  = f $ ehcOptBuiltinNames opts
        false = CExpr_Var $ fn ehbnBoolFalse
        true  = CExpr_Var $ fn ehbnBoolTrue
        nil = CExpr_Tup $ ctagNil opts
        and l r = (CExpr_Var $ fn ehbnBoolAnd) `mkCExprApp` [l,r]
        compose l r = (CExpr_Var $ fn ehbnPrelCompose) `mkCExprApp` [l,r]
        showString s = (CExpr_Var $ fn ehbnPrelShowString) `mkCExprApp` [cstring opts s]
        eq = CExpr_Var eqNm
        lt = CExpr_Var ltNm
        gt = CExpr_Var gtNm
        undef = cundefined opts
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

