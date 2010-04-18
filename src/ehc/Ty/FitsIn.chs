%%[0
%include lhs2TeX.fmt
%include afp.fmt

%if style == poly
%format t1
%format t2
%format tf1
%format tf2
%format tr1
%format tr2
%format ta1
%format ta2
%format ty1
%format ty2
%format fi1
%format fi2
%format fo1
%format fo2
%format uqt1
%format uqt2
%endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption (fitting in) for types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(1 hmtyinfer) module {%{EH}Ty.FitsIn} import({%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty}, {%{EH}Error}) export (fitsIn)
%%]

%%[(2 hmtyinfer) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(4 hmtyinfer) import({%{EH}Ty.Trf.Instantiate}, {%{EH}Ty.FitsInCommon2}, {%{EH}Base.Opts}, {%{EH}Gam.Full}, Data.Maybe,Data.List as List)
%%]
%%[(4 hmtyinfer) import({%{EH}Ty.AppSpineGam})
%%]
%%[(4 hmtyinfer) import(qualified Data.Set as Set)
%%]
%%[(4 hmtyinfer) import(EH.Util.Utils)
%%]

%%[(8 codegen hmtyinfer) import(qualified {%{EH}TyCore.Full0} as C)
%%]

%%[(9 hmtyinfer) import({%{EH}Ty.Trf.Canonic})
%%]
%%[(9 hmtyinfer) import(qualified Data.Map as Map,EH.Util.Pretty,{%{EH}Pred})
%%]
%%[(9 codegen hmtyinfer) import({%{EH}Core.Pretty},{%{EH}Core},{%{EH}Core.Subst},{%{EH}Core.Coercion})
%%]
%%[(9 hmtyinfer) import({%{EH}Pred.CommonCHR})
%%]
%%[(9 hmtyinfer) export(fitsIn')
%%]

%%[(10 codegen hmtyinfer) import({%{EH}Core.Utils})
%%]

%%[(11 hmtyinfer) import({%{EH}Ty.Trf.BetaReduce})
%%]

%%[(99 hmtyinfer hmtyinfer).DerivationTree import({%{EH}DerivationTree})
%%]

%%[(100 hmtyinfer hmtyinfer) -99.DerivationTree
%%]

For debug/trace:
%%[(4 hmtyinfer) import(EH.Util.Pretty,{%{EH}Ty.Pretty},{%{EH}Error.Pretty},{%{EH}Ty.Utils1})
%%]

%%[(4 hmtyinfer) import({%{EH}Base.Debug} as Debug)
%%]

%%[(16 hmtyinfer) import(Debug.Trace)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer)
fiAppVarMp :: FIIn -> Ty -> Ty
fiAppVarMp fi x = fiVarMpLoc fi |=> fiVarMp fi |=> x
%%]

%%[(9 hmtyinfer)
instance Show FIIn where
  show _ = "FIIn"

instance PP FIIn where
  pp fi = "FIIn:" >#< pp (fiEnv fi)
%%]

%%[(4 hmtyinfer).fiUpdOpts
fiUpdOpts :: (FIOpts -> FIOpts) -> FIIn -> FIIn
fiUpdOpts upd fi = fi {fiFIOpts = upd (fiFIOpts fi)}
%%]

%%[(4 hmtyinfer)
fiInhibitVarExpandL :: TyVarId -> FIIn -> FIIn
fiInhibitVarExpandL v fi = fi {fiExpLTvS = v `Set.insert` fiExpLTvS fi}

fiVarIsExpandedL :: TyVarId -> FIIn -> Bool
fiVarIsExpandedL v fi = v `Set.member` fiExpLTvS fi

fiInhibitVarExpandR :: TyVarId -> FIIn -> FIIn
fiInhibitVarExpandR v fi = fi {fiExpRTvS = v `Set.insert` fiExpRTvS fi}

fiVarIsExpandedR :: TyVarId -> FIIn -> Bool
fiVarIsExpandedR v fi = v `Set.member` fiExpRTvS fi
%%]

%%[(4 hmtyinfer)
fiSwapCoCo :: FIIn -> FIIn
fiSwapCoCo fi = fi {fiExpLTvS = fiExpRTvS fi, fiExpRTvS = fiExpLTvS fi}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup of AppSpine + Polarity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Get the type level spine info, in particular how co/contra variance should propagate from type application to its arguments.
The polarity for a type constructor is used for that purpose.
The implementation matches the polarity against a -> b -> Covariant, and observes values for a and b.
In case of failure, the worst is assumed and all is invariant.
TBD: failure should not happen, the encoding of polarity is too strict by not matching Invariant <= Covariant, thus failing.

%%[(4 hmtyinfer)
fiAppSpineLookup :: FIIn -> HsName -> AppSpineGam -> Maybe AppSpineInfo
%%[[4
fiAppSpineLookup fi n gappSpineGam = asGamLookup n $ feAppSpineGam $ fiEnv fi
%%][17
fiAppSpineLookup fi n gappSpineGam
  = case (asGamLookup n $ feAppSpineGam $ fiEnv fi,polGamLookup n (fePolGam $ fiEnv fi)) of
      (Just asi, Just pgi)
        -> Just $ upd pgi asi
      (_,Just pgi)
        -> Just $ upd pgi emptyAppSpineInfo
      (mbasi,_)
        -> mbasi
  where upd pgi asi
          | foHasErrs fo = asi
          | otherwise    = asi {asgiVertebraeL = zipWith asUpdateByPolarity (tyArrowArgs $ tyCanonic emptyFI $ foVarMp fo |=> foTy fo) (asgiVertebraeL asi)}
          where pol = pgiPol pgi
                (polargs,polres) = tyArrowArgsRes pol
                (_,u1,u2) = mkNewLevUID2 uidStart
                fo = fitsIn weakFIOpts emptyFE u1 emptyVarMp pol (map mkPolVar (mkNewUIDL (length polargs) u2) `mkArrow` polCovariant)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[fitsInHead.1
fitsIn :: Ty -> Ty -> FIOut
fitsIn ty1 ty2
  =  f ty1 ty2
  where
            res t                                   = emptyFO {foTy = t}
%%]

%%[fitsInBotCon.1
            f  Ty_Any               t2              = res t2                                -- m.any.l
            f  t1                   Ty_Any          = res t1                                -- m.any.r
            f  t1@(Ty_Con s1)                                                               -- m.con
               t2@(Ty_Con s2)
                 | s1 == s2                         = res t2
%%]

%%[fitsInBind.2
            bind tv t                               = (res t) {foVarMp = tv `varmpTyUnit` t}
            occurBind v t       | v `elem` ftv t    = err [Err_UnifyOccurs ty1 ty2 v t]
                                | otherwise         = bind v t
%%]

%%[fitsInapp.1
            comp tf1 ta1 tf2 ta2 mkComp
                 = foldr1  (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)
                           [ffo,afo,res rt]
                 where  ffo  = f tf1 tf2
                        afo  = f ta1 ta2
                        rt   = mkComp (foTy ffo) (foTy afo)
%%]

%%[fitsInapp.2
            comp tf1 ta1 tf2 ta2 mkComp
                 = foldr1  (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)
                           [ffo,afo,rfo]
                 where  ffo  =   f tf1 tf2
                        fs   =   foVarMp ffo
                        afo  =   f (fs |=> ta1) (fs |=> ta2)
                        as   =   foVarMp afo
                        rt   =   mkComp (as |=> foTy ffo) (foTy afo)
                        rfo  =   emptyFO {foTy = rt, foVarMp = as |=> fs}
%%]

%%[fitsInApp.1
            f  t1@(Ty_App (Ty_App (Ty_Con c1) ta1) tr1)                                     -- m.arrow
               t2@(Ty_App (Ty_App (Ty_Con c2) ta2) tr2)
                 | hsnIsArrow c1 && c1 == c2
                 = comp ta2 tr1 ta1 tr2 (\a r -> [a] `mkArrow` r)
            f  t1@(Ty_App tf1 ta1)                                                          -- m.prod
               t2@(Ty_App tf2 ta2)
                 = comp tf1 ta1 tf2 ta2 Ty_App
%%]

%%[fitsInRest.1
            f  t1                   t2              = err [Err_UnifyClash ty1 ty2 t1 t2]
            err e                                   = emptyFO {foErrL = e}
%%]

%%[(1 hmtyinfer).fitsIn.Base
%%@fitsInHead.1
%%@fitsInBotCon.1
%%]

%%[(1 hmtyinfer).fitsIn.AppRest
%%@fitsInApp.1
%%@fitsInRest.1
%%@fitsInapp.1
%%]

%%[(2 hmtyinfer).fitsIn.Base -(1.fitsIn.Base 1.fitsIn.AppRest)
%%@fitsInHead.1
%%]

%%[(2 hmtyinfer).fitsIn.Bind
%%@fitsInBind.2
%%]

%%[(2 hmtyinfer).fitsIn.app
%%@fitsInapp.2
%%]

%%[(2 hmtyinfer).fitsIn.BotCon
%%@fitsInBotCon.1
%%]

%%[(2 hmtyinfer).fitsIn.Var
            f  t1@(Ty_Var v1)       (Ty_Var v2)
                 | v1 == v2                         = res t1
            f  t1@(Ty_Var v1)       t2              = occurBind v1 t2
            f  t1                   t2@(Ty_Var v2)  = occurBind v2 t1
%%]

%%[(2 hmtyinfer).fitsIn.AppRest
%%@fitsInApp.1
%%@fitsInRest.1
%%]

%%[fitsInVar.3
            f  t1@(Ty_Var v1 f1)    (Ty_Var v2 f2)
                 | v1 == v2 && f1 == f2             = res t1
            f  t1@(Ty_Var v1 f)     t2              
                 | f == TyVarCateg_Plain            = occurBind v1 t2
            f  t1                   t2@(Ty_Var v2 f)
                 | f == TyVarCateg_Plain            = occurBind v2 t1
%%]

%%[(3 hmtyinfer).fitsIn -(2.fitsIn.Base 2.fitsIn.Bind 2.fitsIn.app 2.fitsIn.BotCon 2.fitsIn.Var 2.fitsIn.AppRest)
%%@fitsInHead.1
%%@fitsInBind.2
%%@fitsInapp.2
%%@fitsInBotCon.1
%%@fitsInVar.3
%%@fitsInApp.1
%%@fitsInRest.1
%%]

%%[(4 hmtyinfer).fitsIn.Prelim -3.fitsIn
manyFO :: [FIOut] -> FIOut
manyFO = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)

fitsIn :: FIOpts -> FIEnv -> UID -> VarMp -> Ty -> Ty -> FIOut
fitsIn opts env uniq varmp
  =  fitsInFI
       (emptyFI
          { fiUniq = uniq
          , fiFIOpts = opts
          , fiVarMp = varmp
%%[[8
          , fiEnv = env
%%]]
          }
       )
%%]

%%[(4 hmtyinfer).fitsInFI
fitsInFI :: FIIn -> Ty -> Ty -> FIOut
fitsInFI fi ty1 ty2
  =  foRes {foTrace = reverse $ foTrace foRes}
  where
%%[[4
            appSpineGam             =  feAppSpineGam $ fiEnv fi
%%]]
%%[[8
            -- options
            globOpts                =  feEHCOpts $ fiEnv fi
%%]]
%%[[10
            -- additional functionality
            fiReqs                  =  feFIReqs $ fiEnv fi
%%]]
            -- range where fitsIn takes place
%%[[1
            range                   =  emptyRange
%%][99
            range                   =  feRange $ fiEnv fi
%%]]

            -- tracing
%%[[4
            trfiAdd  tr   fi        =  fi {fiTrace = tr ++ fiTrace fi}
            trfi msg rest fi        =  trfiAdd [trfitIn msg rest] fi
            trfoAdd  tr   fo        =  fo {foTrace = tr ++ foTrace fo}
            trfo msg rest fo        =  trfoAdd [trfitOu msg rest] fo
%%][100
            trfiAdd  tr   fi        =  fi
            trfi msg rest fi        =  fi
            trfoAdd  tr   fo        =  fo
            trfo msg rest fo        =  fo
%%]]

            -- derivation tree
%%[[4
            dtfo _ _ _ _ _ _ fo     =  fo
%%][99
            dtfo rlNm fi t1 t2 subfos mbind fo
                                    =  fo {foMkDT = mk}
                                    where mk mbTop fmt m dm1 = ( dtRule False fmt ("m." ++ rlNm) (reverse subs) (dtJdgMatch opts fiopts t1' t2' t3 mbnd), dm5 )
                                            where (t1' ,dm2) = dtEltTy (dtChooseDT opts m mfi) dm1 t1
                                                  (t2' ,dm3) = dtEltTy (dtChooseDT opts m mfi) dm2 t2
                                                  (subs,dm4) = foldl (\(subs,dm) (fo,fmt) -> let (sub,dm') = foMkDT fo Nothing fmt m dm in (sub:subs,dm')) ([],dm3) subfos
                                                  (t3  ,dm5) = dtEltTy (dtChooseDT opts m mfo) dm4 (foTy fo)
                                                  (mbnd,dm6) = maybe (dtEltVarMp (dtChooseDT opts m mfo) dm5 mbind) (\x -> (x,emptyVarMp)) mbTop
                                                  mfi        = fiVarMpLoc fi |=> fiVarMp fi
                                                  mfo        = foVarMp fo |=> fiVarMp fi
                                                  opts       = feEHCOpts $ fiEnv fi
                                                  fiopts     = fiFIOpts fi
%%][100
            dtfo _ _ _ _ _ _ fo     =  fo
%%]]

            -- results
            res' fi tv t            =  (\fo -> trfo "res" (ppTyWithFI fi tv >|< ", spine" >#< (tyConNm t) >|< ":" >#< pp (foAppSpineInfo fo) {- >-< "polgam:" >#< ppGam (fePolGam $ fiEnv fi) -}) fo)
                                       $ (fifo fi emptyFO) {foTy = tv, foMbAppSpineInfo = fiAppSpineLookup fi (tyConNm t) appSpineGam}
            res  fi    t            =  res' fi t t

            -- errors
            err  fi e               =  trfo "err" (ppErrL e)
                                       $ emptyFO {foUniq = fioUniq (fiFIOpts fi), foErrL = e, foTrace = fiTrace fi}
            errClash fiErr t1 t2    =  err fiErr [rngLift range Err_UnifyClash (fiAppVarMp fiErr ty1) (fiAppVarMp fiErr ty2) (fioMode (fiFIOpts fi)) (fiAppVarMp fiErr t1) (fiAppVarMp fiErr t2) (fioMode (fiFIOpts fiErr))]

            -- binding
            occurBind fi v t        =  bind fi v t
%%]
            occurBind fi v t
                | v `elem` ftv t    =  err fi [rngLift range Err_UnifyOccurs (fiAppVarMp fi ty1) (fiAppVarMp fi ty2) (fioMode (fiFIOpts fi)) v t (fioMode (fiFIOpts fi))]
                | otherwise         =  bind fi v t

%%[(4 hmtyinfer)
            -- 20080309, AD: naming of function is not right, type info neither, error yes. Should indicate a double expansion of tyvar, indicating infinite type.
            errInfinite fi v t      =  err fi [rngLift range Err_UnifyOccurs (fiAppVarMp fi ty1) (fiAppVarMp fi ty2) (fioMode (fiFIOpts fi)) v t (fioMode (fiFIOpts fi))]
%%]

%%[(9 hmtyinfer).fitsIn.lookupImplsVar
            lookupImplsVarCyc fi v  =  fiLookupVar' varmpImplsLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)
%%]
%%[(10 hmtyinfer).fitsIn.lookupLabelVarCyc
            lookupLabelCyc    fi v  =  fiLookupVar' varmpLabelLookupLabelCyc v (fiVarMpLoc fi) (fiVarMp fi)
%%]
            tyVarIsBound tv fi      =  isJust $ lookupTyVar fi tv

%%[(4 hmtyinfer).fitsIn.bind
            bind fi tv t            =  dtfo "bind" fi tv' t [] (tv `varmpTyUnit` t)
                                       $ trfo "bind" ("tv:" >#< tv >-< "ty:" >#< ppTyWithFI fi t)
                                       $ (res' (fiBindTyVar tv t fi) tv' t)
                                    where tv' = mkTyVar tv
%%]

%%[(4 hmtyinfer).fitsIn.allowImpredTVBind
            allowImpredTVBindL fi t _
                = fioBindLFirst (fiFIOpts fi) && fiAllowTyVarBind fi t
            allowImpredTVBindR fi t _
                = fioBindRFirst (fiFIOpts fi) && fiAllowTyVarBind fi t
%%]

%%[(4 hmtyinfer).fitsIn.unquant
            -- removal of quantifier
            unquant fi t hide howToInst
                = ( fi { fiUniq = u
%%[[6
                       , fiVarMpLoc = instToL1VarMp instto |=> fiVarMpLoc fi
%%]]
                       }
                  , uqt,back,instto
                  )
                where  (u,uq)            = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs,instto) = tyInst1Quants uq howToInst t
                       back              = if hide  then  \fo -> foSetVarMp (varmpDel rtvs (foVarMp fo)) $ foUpdTy t fo
                                                    else  id
%%]

%%[(16 hmtyinfer).fitsIn.eqProofAssume
            eqAssume p fi t1 t2 isRec isSum
              = out { foGathCnstrMp = foGathCnstrMp out `Map.union` mp }
              where
                mp    = cnstrMpFromList [cnstr]
                cnstr = rngLift range mkAssumeConstraint p lUniq scope
                scope = fePredScope $ fiEnv fi
                (gUniq,lUniq) = mkNewLevUID (fiUniq fi)
                fi' = fi { fiUniq = gUniq }
                out = fRow fi' t1 t2 isRec isSum
%%]

%%[(16 hmtyinfer).fitsIn.eqProofObligation
            eqProofObligation tRes fi tL tR
                = (res fi tRes) { foGathCnstrMp = mp }
                where
                  mp    = cnstrMpFromList [cnstr]
                  cnstr = rngLift range mkProveConstraint (Pred_Eq tL tR) uid scope
                  scope = fePredScope $ fiEnv fi
                  uid   = fiUniq fi
%%]

%%[(16 hmtyinfer).fitsIn.isSkVar
            -- is skolemnized tyvar?
            isSkVar = isSkVar' . show
            
            isSkVar' ('C':'_':_) = True
            isSkVar' _           = False
%%]

%%[(4 hmtyinfer).fitsIn.FOUtils
            foUpdVarMp  c fo = fo {foVarMp = c |=> foVarMp fo}
            fifo       fi fo = fo { foVarMp    = fiVarMpLoc fi, foUniq = fiUniq fi, foTrace = fiTrace fi -- ++ foTrace fo
                                  }
            fofi       fo fi = fi { fiVarMpLoc = foVarMp    fo, fiUniq = foUniq fo, fiTrace = foTrace fo -- ++ fiTrace fi
                                  }
%%]
%%[(9 hmtyinfer)
            fiInhibitBind v fi = fi {fiFIOpts = o {fioDontBind = v `Set.insert` fioDontBind o}}
                               where o  = fiFIOpts fi
%%]

%%[(4 hmtyinfer).fitsIn.FOUtils
            foUpdTy  t   fo  = fo {foTy = t}
%%]

%%[(4 hmtyinfer).fitsIn.foCmb
            foCmbAppTy   ffo afo  = afo {foTy = Ty_App (foTy ffo) (foTy afo)}
            foCmbVarMp   ffo afo  = afo -- {foVarMp = foVarMp afo |=> foVarMp ffo}
            foCmbCoCon   ffo afo  = afo {foMbAppSpineInfo = fmap asgiShift1SpinePos $ foMbAppSpineInfo ffo}
%%]

%%[(9 hmtyinfer)
            foCmbPrL     ffo afo  = afo {foPredOccL = foPredOccL afo ++ foPredOccL ffo, foGathCnstrMp = foGathCnstrMp afo `cnstrMpUnion` foGathCnstrMp ffo}
%%]
%%[(9 codegen hmtyinfer)
            foCmbCSubst  ffo afo  = afo {foCSubst = cSubstApp (foCSubst afo) (foCSubst ffo)}
            foCmbTCSubst ffo afo  = afo {foTCSubst = C.cSubstAppSubst (foTCSubst afo) (foTCSubst ffo)}
%%]

%%[(4 hmtyinfer).fitsIn.foCmbApp
            foCmbApp     ffo      = 
%%[[6
                                    -- foCmbTvKiVarMp ffo .
%%]]
%%[[9
                                    foCmbPrfRes ffo .
%%]]
                                    foCmbCoCon ffo . foCmbVarMp ffo . foCmbAppTy ffo
%%]

%%[(7 hmtyinfer).fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo afo  = afo
%%]

%%[(9 hmtyinfer).fitsIn.foCmbPrfRes -7.fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo      = foCmbPrL ffo
%%[[(9 codegen)
                                    . foCmbCSubst ffo
                                    . foCmbTCSubst ffo
%%]]
%%]

%%[(9 hmtyinfer)
            fiAddPr n i prTy fi
                =  let  e                   = fiEnv fi
                        (_,assumePredScope) = pscpEnter 0 $ fePredScope (fiEnv fi)
                        pr                  = tyPred prTy
                   in   (fi { fiEnv = e {fePredScope = assumePredScope} },gathPredLToAssumeCnstrMp [rngLift range mkPredOccRng pr i assumePredScope])
            foUpdErrs e fo = fo {foErrL = e ++ foErrL fo}
            foUpdCnstrMp m fo = fo {foGathCnstrMp = m `cnstrMpUnion` foGathCnstrMp fo}
            foUpdPrL prL prMp fo = foUpdCnstrMp prMp $ fo {foPredOccL = prL ++ foPredOccL fo}
            foUpdImplExpl iv im tpr fo
                            = foUpdVarMp (iv `varmpImplsUnit` im)
                            $ foUpdTy ([tpr] `mkArrow` foTy fo)
                            $ fo
%%[[(9 codegen)
            foUpdLRCoe lrcoe fo = fo {foLRCoe = lrcoe `lrcoeUnion` foLRCoe fo}
%%]]
%%[[(8 codegen)
            foUpdLRTCoe lrcoe fo = fo {foLRTCoe = lrcoe `C.lrcoeUnion` foLRTCoe fo}
%%]]
%%[[(8 codegen)
            foUpdImplExplCoe iv im tpr       tlrcoe fo
                            = foUpdImplExpl iv im tpr $                    foUpdLRTCoe tlrcoe fo
%%][(9 codegen)
            foUpdImplExplCoe iv im tpr lrcoe tlrcoe fo
                            = foUpdImplExpl iv im tpr $ foUpdLRCoe lrcoe $ foUpdLRTCoe tlrcoe fo
%%][8
            foUpdImplExplCoe
                            = foUpdImplExpl
%%]]
%%]

A counterpart type to enforce deep quantifier instantiation.
20080606, AD: Omitting the check on hsnPolNegation breaks polarity matching; this has to be sorted out.
%%[(4 hmtyinfer)
            deepInstMatchTy fi t
              = case t of
                  _ | not (null as
%%[[17
                          || tyConNm f == hsnPolNegation
%%]]
                          )
                                  -> Just (mkApp $ mkNewTyVarL (length as + 1) u1, fi')
                    | otherwise   -> Nothing
                    where (f,as) = tyAppFunArgs t
              where (u,u1) = mkNewLevUID (fiUniq fi)
                    fi' = fi {fiUniq = u}
%%]

%%[(7 hmtyinfer)
            fPairWise fi tL1 tL2
              =  foldr  (\(t1,t2) (foL,fii)
                           -> let  fo = fVar' ff fii id t1 t2
                              in   (fo:foL,fofi fo fii))
                        ([],fi)
                        (zip tL1 tL2)
%%]

GADT: when encountering a product with eq-constraints on the outset, remove them and bring them in scope as assume constraints
%%[(16 hmtyinfer).fitsIn.fRow.StripPreds
            fRow fi (Ty_Ext t1 _ (Ty_Pred p)) t2 isRec isSum = eqAssume p fi t1 t2 isRec isSum
            fRow fi t1 (Ty_Ext t2 _ (Ty_Pred p)) isRec isSum = eqAssume p fi t1 t2 isRec isSum
%%]

%%[(7 hmtyinfer).fitsIn.fRow.Base
            fRow fi tr1 tr2 isRec isSum
                = foR
                where  (r1,exts1) = tyRowExtsUnAnn $ tyRowExtsWithLkup (fiLookupTyVarCyc fi) tr1
                       (r2,exts2) = tyRowExtsUnAnn $ tyRowExtsWithLkup (fiLookupTyVarCyc fi) tr2
                       (extsIn1,extsIn12,extsIn2) = split (tyRowCanonOrder exts1) (tyRowCanonOrder exts2)
                       split ees1@(e1:es1) ees2@(e2:es2)
                         = case e1 `rowExtCmp` e2 of
                               EQ -> let (es1',es12,es2') = split es1  es2  in (es1',(e1,e2):es12,es2')
                               LT -> let (es1',es12,es2') = split es1  ees2 in (e1:es1',es12,es2')
                               GT -> let (es1',es12,es2') = split ees1 es2  in (es1',es12,e2:es2')
                       split ees1 ees2
                         = (ees1,[],ees2)
                       mkTv fi    = (fi',mkTyVar u)
                         where  (u',u) = mkNewUID (fiUniq fi)
                                fi' = fi {fiUniq = u'}
                       bind fo v r e = manyFO [fo,foUpdTy (foTy fo `mkTyRow` e) $ foUpdVarMp (v `varmpTyUnit` mkTyRow r e) $ fo]
                       (u',u1)    = mkNewLevUID (fiUniq fi)
                       fi2        = fi {fiUniq = u'}
                       
                       fR fi r1 r2@(Ty_Var v2 f2) e1@(_:_) e12 e2
                         | tvCatIsPlain f2
                         = bind (fR fi2 r1 rv [] e12 e2) v2 rv e1
                         where  (fi2,rv) = mkTv fi
                       fR fi r1@(Ty_Var v1 f1) r2 e1 e12 e2@(_:_)
                         | tvCatIsPlain f1
                         = bind (fR fi2 rv r2 e1 e12 []) v1 rv e2
                         where (fi2,rv) = mkTv fi
                       fR fi r1@(Ty_Con n1) _ _ _ e2@(_:_)
                         | n1 == hsnRowEmpty && isRec
                         = err fi [rngLift range Err_MissingRowLabels (assocLKeys e2) (fiAppVarMp fi tr1)]
{-
                       fR fi r1 r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isRec && not (null labs)
                         = err fi [rngLift range Err_MissingRowLabels labs (fiAppVarMp fi tr2)]
                         where labs = fioNoLLabElimFor (fiFIOpts fi) `List.intersect` assocLKeys e1
-}
                       fR fi r1 r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isRec
                         =  if null labs
                            then fR fi r1 r2 [] e12 e2
                            else err fi [rngLift range Err_TooManyRowLabels (assocLKeys e1) (fiAppVarMp fi tr2)]
                         where labs = fioNoRLabElimFor (fiFIOpts fi) `List.intersect` assocLKeys e1
                       fR fi r1@(Ty_Con n1) r2 e1 e12 e2@(_:_)
                         | n1 == hsnRowEmpty && isSum
                         = fR fi r1 r2 e1 e12 []
                       fR fi r1 r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isSum
                         = err fi [rngLift range Err_MissingRowLabels (assocLKeys e1) (fiAppVarMp fi tr2)]
                       fR fi r1 r2 e1 e12@(_:_) e2
                         = foR
                         where (e1L,e2L) = unzip e12
                               (foL,fi2) = fPairWise (fiUpdOpts fioMkStrong fi) (assocLElts e1L) (assocLElts e2L)
                               eKeys = assocLKeys e1L
                               eL = zip eKeys (map foTy foL)
                               fo = fR fi2 r1 r2 e1 [] e2
                               foR = manyFO ([fo] ++ foL ++ [foRes])
                               foRes = (\fo -> foldr foCmbPrfRes fo foL)
%%[[(10 codegen)
                                       $ foUpdRecFldsCoe eKeys foL tr1
%%]]
                                       $ foUpdTy (foTy fo `mkTyRow` eL) fo
%%]

%%[(7 hmtyinfer).fitsIn.fRow.fRFinal
                       fR fi r1 r2 [] [] []
                         = f fi r1 r2
%%]

%%[(10 hmtyinfer).fitsIn.fRow.fRFinal -7.fitsIn.fRow.fRFinal
                       fR fi r1@(Ty_Var _ f1) r2@(Ty_Con n2) [] [] []
                         | tvCatIsFixed f1 && n2 == hsnRowEmpty && isRec
                         = res fi r2
                       fR fi r1@(Ty_Var v1 f1) r2@(Ty_Con n2) [] [] []
                         | tvCatIsPlain f1 && n2 == hsnRowEmpty && isRec
                         = occurBind fi v1 r2
                       fR fi r1 r2 [] [] []
                         = (fUpd fi id r1 r2)
%%[[(10 codegen)
                              { foLRCoe = emptyLRCoe
                              , foLRTCoe = C.emptyLRCoe
                              }
%%]]
%%]
                       fR fi r1@(Ty_Var _ cat) r2@(Ty_Con n2) [] [] []
                         | tvCatIsFixed cat && n2 == hsnRowEmpty && isRec
                         = res fi r2
                       fR fi r1 r2 [] [] []
                         = (f fi r1 r2)
                             { foLRCoe = emptyLRCoe
                             , foLRTCoe = C.emptyLRCoe
                             }

%%[(7 hmtyinfer).fitsIn.fRow.Final1
                       fR fi _ _ _ _ _
                         = errClash fi tr1 tr2
%%]

%%[(7 hmtyinfer).fitsIn.fRow.foR
                       foR        = fR fi2 r1 r2 extsIn1 extsIn12 extsIn2
%%]

%%[(10 hmtyinfer).fitsIn.fRow.foR -7.fitsIn.fRow.foR
                       fo         = fR fi2 r1 r2 extsIn1 extsIn12 extsIn2
                       foR        = (if isRec then foUpdRecCoe tr1 r1 r2 extsIn1 extsIn12 extsIn2 else id) fo 
                       foUpdRecCoe tr1 r1 r2 e1 e12 e2 fo
                         =  let  rn = uidHNm u1
                                 predScope = fePredScope (fiEnv fi)
                                 -- tr1s = foVarMp fo |=> tr1
                                 fi3 = fofi fo fi2
                                 tr1s = uncurry mkTyRow $ tyRowExtsUnAnn $ tyRowExtsWithLkup (fiLookupTyVarCyc fi3) tr1
                                 (u',u2,u3,u4) = mkNewLevUID3 (foUniq fo)
%%[[(10 codegen)
                                 r = CExpr_Var rn
                                 tr = C.Expr_Var rn
                                 mkLSel n u = mkCExprSelCase (emptyRCEEnv globOpts) (Just $ hsnUniqifyEval rn) r CTagRec n n (mkCExprHole globOpts u) Nothing
                                 mkLTSel n u = C.mkExprSelCase (C.emptyRCEEnv globOpts) (Just (hsnUniqifyEval rn,C.tyErr ("fitsIn.mkLTSel: " ++ show n ++ ":" ++ show u))) tr CTagRec n (C.mkExprHole globOpts u) Nothing
%%]]
                                 mkLPred' r l u
                                   =  let  r' = maybe Ty_Any fst $ tyRowExtr l r
                                      in   (rngLift range mkPredOccRng (Pred_Lacks r' (Label_Lab l)) (mkPrIdCHR u) predScope,r')
                                 mkLPred r l u = fst (mkLPred' r l u)
%%[[(10 codegen)
                                 rowCoeL = sortByOn rowLabCmp fst (foRowCoeL fo)
                                 rowTCoeL = sortByOn rowLabCmp fst (foRowTCoeL fo)
%%][10
                                 rowCoeL = sortByOn rowLabCmp fst $ map fst extsIn12
                                 rowTCoeL = sortByOn rowLabCmp fst $ map fst extsIn12
%%]]
                                 (fuUpdL,prUpdL,tr1s',csubstUpd,_)
                                   =  foldr  (\(l,c) (fuL,prL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel) = coeEvalOnAsSubst u2 c (mkLSel l u1)
                                                    in  ( ( l
%%[[(10 codegen)
                                                          , (CExpr_TupUpd (cundefined globOpts) CTagRec l (mkCExprHole globOpts u) sel,Nothing)
%%]]
                                                          ) : fuL
                                                        , mkLPred r l u1 : prL
                                                        , r
                                                        , csubst `cSubstApp` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([],[],tr1s,emptyCSubst,u2) rowCoeL
                                 (tfuUpdL,tprUpdL,ttr1s',tcsubstUpd,_)
                                   =  foldr  (\(l,c) (fuL,prL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel) = {- C.coeEvalOnAsSubst -} fireqCoeEvalOnAsSubst fiReqs u2 c (mkLTSel l u1)
                                                    in  ( ( l
%%[[(10 codegen)
                                                          , (C.Expr_TupUpd (C.tcUndefined globOpts) CTagRec l (C.mkExprHole globOpts u) sel,Nothing)
%%]]
                                                          ) : fuL
                                                        , mkLPred r l u1 : prL
                                                        , r
                                                        , csubst `C.cSubstAppSubst` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([],[],tr1s,C.emptyCSubst,u2) rowTCoeL
                                 (fuDelL,prDelL,_,_)
                                   =  foldl  (\(fuL,prL,r,u) l
                                                  ->  let  (pr,r') = mkLPred' r l u
                                                      in   ( ( l
%%[[(10 codegen)
                                                             , (CExpr_TupDel (CExpr_Var hsnWild) CTagRec l (mkCExprHole globOpts u),Nothing)
%%]]
                                                             ) : fuL
                                                           , pr:prL,r',uidNext u
                                                           )
                                             )
                                             ([],[],tr1s',u3) (sortBy rowLabCmp (assocLKeys e1))
                                 (tfuDelL,tprDelL,_,_)
                                   =  foldl  (\(fuL,prL,r,u) l
                                                  ->  let  (pr,r') = mkLPred' r l u
                                                      in   ( ( l
%%[[(10 codegen)
                                                             , (C.Expr_TupDel (C.Expr_Var hsnWild) CTagRec l (C.mkExprHole globOpts u),Nothing)
%%]]
                                                             ) : fuL
                                                           , pr:prL,r',uidNext u
                                                           )
                                             )
                                             ([],[],tr1s',u3) (sortBy rowLabCmp (assocLKeys e1))
                                 fuL = fuUpdL ++ reverse fuDelL
                                 tfuL = tfuUpdL ++ reverse tfuDelL
                                 (prBldL, fBldL, _, csubstBld, _) 
                                   =  foldr  (\l (prL,fL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel)
                                                          = maybe (s,emptyCSubst) (\c -> coeEvalOnAsSubst u2 c s) (lookup l rowCoeL)
                                                          where s = mkLSel l u1
                                                    in  ( mkLPred r l u1 : prL,
%%[[(10 codegen)
                                                          sel :
%%]]
                                                          fL
                                                        , r
                                                        , csubst `cSubstApp` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([], [], tr1s, emptyCSubst, u3)
                                             (sortBy rowLabCmp ((assocLKeys . map fst $ e12) ++ assocLKeys e2))
                                 (tprBldL, tfBldL, _, tcsubstBld, _) 
                                   =  foldr  (\l (prL,fL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel)
                                                          = maybe (s,C.emptyCSubst) (\c -> {- C.coeEvalOnAsSubst -} fireqCoeEvalOnAsSubst fiReqs u2 c s) (lookup l rowTCoeL)
                                                          where s = mkLTSel l u1
                                                    in  ( mkLPred r l u1 : prL,
%%[[(10 codegen)
                                                          sel :
%%]]
                                                          fL
                                                        , r
                                                        , csubst `C.cSubstAppSubst` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([], [], tr1s, C.emptyCSubst, u3)
                                             (sortBy rowLabCmp ((assocLKeys . map fst $ e12) ++ assocLKeys e2))
                            in   case r2 of
                                   Ty_Con n2
                                     | n2 == hsnRowEmpty && null fuL && null e2
                                     ->  fo
%%[[(10 codegen)
                                             { foLRCoe = emptyLRCoe
                                             , foLRTCoe = C.emptyLRCoe
                                             , foCSubst  = foCSubst fo `cSubstApp` csubstUpd `cSubstApp` csubstBld
                                             , foTCSubst = foTCSubst fo `C.cSubstAppSubst` tcsubstUpd `C.cSubstAppSubst` tcsubstBld
                                             }
%%]]
{- -- when ext rec deletes are implemented
                                     | n2 == hsnRowEmpty && null fuUpdL && not (null fuDelL) && null e2
                                     ->  let coe = Coe (\e -> mkCExprLet CBindings_Plain [CBind_Bind rn e] (fuMkCExpr globOpts u4 fuDelL r))
                                         in  fo  {  foLRCoe = lrcoeLSingleton coe
                                                 ,  foPredOccL = prDelL ++ foPredOccL fo
                                                 ,  foGathCnstrMp = gathPredLToProveCnstrMp prDelL `cnstrMpUnion` foGathCnstrMp fo
                                                 ,  foUniq = u'
                                                 }
-}
                                     | n2 == hsnRowEmpty && not (null prBldL)
                                     ->  let
%%[[(10 codegen)
                                             coe = Coe (\e -> mkCExprLet CBindings_Plain [mkCBind1 rn e] (CExpr_Tup CTagRec `mkCExprApp` fBldL))
                                             tcoe = C.Coe_Map (\e -> C.mkExprLet C.ValBindCateg_Plain [C.mkValBind1 rn (C.tyErr ("fitsIn.coe: " ++ show rn)) e] (C.Expr_Tup CTagRec `C.mkExprApp` tfBldL))
%%]]
                                         in  fo  {  foPredOccL = prBldL ++ foPredOccL fo
                                                 ,  foGathCnstrMp = gathPredLToProveCnstrMp prBldL `cnstrMpUnion` foGathCnstrMp fo
                                                 ,  foUniq = u'
%%[[(10 codegen)
                                                 ,  foLRCoe = lrcoeLSingleton coe
                                                 ,  foLRTCoe = C.lrcoeLSingleton tcoe
%%]]
                                                 }
                                   Ty_Var _ cat
                                     | tvCatIsFixed cat && not (null fuL)
                                     ->  fo  {  foPredOccL = prUpdL ++ prDelL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp (prUpdL ++ prDelL) `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
%%[[(10 codegen)
                                             ,  foLRCoe = lrcoeLSingleton coe
                                             ,  foLRTCoe = C.lrcoeLSingleton tcoe
%%]]
                                             }
%%[[(10 codegen)
                                     where coe = Coe (\e -> mkCExprLet CBindings_Plain [mkCBind1 rn e] (fuMkCExpr globOpts u4 fuL r))
                                           tcoe = C.Coe_Map (\e -> C.mkExprLet C.ValBindCateg_Plain [C.mkValBind1 rn (C.tyErr ("fitsIn.coe: " ++ show rn)) e] (C.fuMkExpr globOpts u4 tfuL tr))
%%]]
                                   _ |  not (null fuUpdL)
                                     ->  fo  {  foPredOccL = prUpdL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp prUpdL `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
%%[[(10 codegen)
                                             ,  foLRCoe = lrcoeLSingleton coe
                                             ,  foLRTCoe = C.lrcoeLSingleton tcoe
%%]]
                                             }
                                     |  otherwise
                                     ->  fo
%%[[(10 codegen)
                                             {  foLRCoe = emptyLRCoe
                                             ,  foLRTCoe = C.emptyLRCoe
                                             }
%%]]
%%[[(10 codegen)
                                     where coe = Coe (\e -> mkCExprLet CBindings_Plain [mkCBind1 rn e] (fuMkCExpr globOpts u4 fuUpdL r))
                                           tcoe = C.Coe_Map (\e -> C.mkExprLet C.ValBindCateg_Plain [C.mkValBind1 rn (C.tyErr ("fitsIn.coe: " ++ show rn)) e] (C.fuMkExpr globOpts u4 tfuUpdL tr))
%%]]
%%]

%%[(10 codegen hmtyinfer).fitsIn.fRow.Coe
                       foUpdRecFldsCoe eKeys foL tr1 foR
                         =  let (u',u1) = mkNewLevUID (foUniq foR)
                                us = mkNewLevUIDL (length foL) u1
                                (cL,sL)
                                   = unzip
                                       [  ((l,c),s)
                                       |  (l,fo,u) <- zip3 eKeys foL us
                                       ,  let (c,s) = lrcoeWipeWeaveAsSubst globOpts u (foVarMp foR) (foLRCoe fo)
                                       ,  not (coeIsId c)
                                       ]
                                (tcL,tsL)
                                   = unzip
                                       [  ((l,c),s)
                                       |  (l,fo,u) <- zip3 eKeys foL us
                                       ,  let (c,s) = {- C.lrcoeWipeWeaveAsSubst -} fireqLRCoeWipeWeaveAsSubst fiReqs globOpts u (foVarMp foR) (foLRTCoe fo)
                                       ,  not (C.coeIsId c)
                                       ]
                            in  foR { foUniq = u'
                                    , foRowCoeL = cL
                                    , foRowTCoeL = tcL
                                    , foCSubst = foldr cSubstApp (foCSubst foR) sL
                                    , foTCSubst = foldr C.cSubstAppSubst (foTCSubst foR) tsL
                                    }
%%]

%%[(4 hmtyinfer).fitsIn.ff
            f fi t1 t2
              = fUpd fi id t1 t2
%%]
%%[(4 hmtyinfer).fitsIn.ff
            ff fi updTy t1 t2
              = fUpd fi updTy t1 t2
%%]
%%[(11 hmtyinfer) -4.fitsIn.ff
            ff fi updTy t1 t2
              = case filter (not . foHasErrs) tries of
                  (fo:_) -> fo
                  _      -> case (drop limit rt1, drop limit rt2) of
                              (((t,tr):_),_) -> err (trfiAdd tr fi2) [rngLift range Err_TyBetaRedLimit (fiAppVarMp fi2 t1) (fiAppVarMp fi2 t) limit]
                              (_,((t,tr):_)) -> err (trfiAdd tr fi2) [rngLift range Err_TyBetaRedLimit (fiAppVarMp fi2 t2) (fiAppVarMp fi2 t) limit]
                              _              -> last tries
              where fi2   = trfi "ff" ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2) fi
                    limit = ehcOptTyBetaRedCutOffAt globOpts
                    rt1   = tyBetaRed fi2 betaRedTyLookup t1
                    rt2   = tyBetaRed fi2 betaRedTyLookup t2
                    tries = take (limit+1) $ try fi2 ((t1,[]) : rt1) ((t2,[]) : rt2)
                          where try fi ((t1,tr1):ts1@(_:_)) ((t2,tr2):ts2@(_:_)) = fUpd fi' updTy t1 t2 : try fi' ts1 ts2
                                                                                 where fi' = trfiAdd tr1 $ trfiAdd tr2 fi
                                try fi ts1@[(t1,tr1)]       ((t2,tr2):ts2@(_:_)) = fUpd fi' updTy t1 t2 : try fi' ts1 ts2
                                                                                 where fi' = trfiAdd tr2 fi
                                try fi ((t1,tr1):ts1@(_:_)) ts2@[(t2,tr2)]       = fUpd fi' updTy t1 t2 : try fi' ts1 ts2
                                                                                 where fi' = trfiAdd tr1 fi
                                try fi [(t1,tr1)]           [(t2,tr2)]           = [fUpd fi' updTy t1 t2]
                                                                                 where fi' = fi
%%]

%%[(4 hmtyinfer).fitsIn.fVar
            fVar' f fi updTy t1@(Ty_Var v1 f1)     t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2
%%[[8
                  && not (fioExpandEqTyVar (fiFIOpts fi))
%%]]
                                                              = res fi t1
            fVar' f fi updTy t1@(Ty_Var v1 f1)     t2
                | isJust mbTy1                                = if fiVarIsExpandedL v1 fi
                                                                then errInfinite fi v1 t1'
                                                                else fVar' f (fiInhibitVarExpandL v1 fi2) updTy t1' t2
%%[[9
                | not ((fioBindIsYes mbvs || v1 `Set.member` fioBindNoSet mbvs) || v1 `Set.member` fioDontBind (fiFIOpts fi))
                                                              = fVar' f (fiInhibitBind v1 fi2) updTy t1 t2
%%]]
                where mbTy1   = fiLookupTyVarCyc fi v1
                      t1'     = fromJust mbTy1
                      fi2     = trfi "fVar L"
                                     ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2
%%[[9999
                                            >-< "fioDontBind:" >#< show (fioDontBind (fiFIOpts fi)) >-< "fioBindNoSet:" >#< show (fioBindNoSet mbvs) >-< "fioBindIsYes:" >#< show (fioBindIsYes mbvs)
%%]]
                                     ) fi
%%[[9
                      mbvs    = fioBindLVars (fiFIOpts fi)
%%]]
            fVar' f fi updTy t1                    t2@(Ty_Var v2 f2)
                | isJust mbTy2                                = if fiVarIsExpandedR v2 fi
                                                                then errInfinite fi v2 t2'
                                                                else fVar' f (fiInhibitVarExpandR v2 fi2) updTy t1 t2'
%%[[9
                | not ((fioBindIsYes mbvs || v2 `Set.member` fioBindNoSet mbvs) || v2 `Set.member` fioDontBind (fiFIOpts fi))
                                                              = fVar' f (fiInhibitBind v2 fi2) updTy t1 t2
%%]]
                where mbTy2   = fiLookupTyVarCyc fi v2
                      t2'     = fromJust mbTy2
                      fi2     = trfi "fVar R"
                                     ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2
%%[[9999
                                            >-< "fioDontBind:" >#< show (fioDontBind (fiFIOpts fi)) >-< "fioBindNoSet:" >#< show (fioBindNoSet mbvs) >-< "fioBindIsYes:" >#< show (fioBindIsYes mbvs)
%%]]
                                     ) fi
%%[[9
                      mbvs    = fioBindRVars (fiFIOpts fi)
%%]]
            fVar' f fi updTy t1                    t2                = fAnn f fi2 updTy t1 t2
                where fi2     = trfi "fVar Dflt"
                                     ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2
%%[[9999
                                            >-< "fioDontBind    :" >#< show (fioDontBind (fiFIOpts fi))
                                            >-< "fioBindNoSet(L):" >#< show (fioBindNoSet (fioBindLVars (fiFIOpts fi)))
                                            >-< "fioBindIsYes(L):" >#< show (fioBindIsYes (fioBindLVars (fiFIOpts fi)))
                                            >-< "tyIsA       (L):" >#< tyIsA t1
                                            >-< "fioBindNoSet(R):" >#< show (fioBindNoSet (fioBindRVars (fiFIOpts fi)))
                                            >-< "fioBindIsYes(R):" >#< show (fioBindIsYes (fioBindRVars (fiFIOpts fi)))
                                            >-< "tyIsA       (R):" >#< tyIsA t2
%%]]
                                     ) fi

            fVar f fi        t1                    t2                = fVar' f fi id t1 t2
%%]

%%[(4 hmtyinfer)
			-- | tvar binding part 1: 2 tvars
            varBind1  fi updTy t1@(Ty_Var v1 f1)      t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2                  = Just $ res fi t1
                | lBefR && fiAllowTyVarBind fi t1       = Just $ bind fi v1 (updTy t2)
                | not lBefR && fiAllowTyVarBind fi t2   = Just $ bind fi v2 (updTy t1)
                where lBefR = fioBindLBeforeR (fiFIOpts fi)
            varBind1  _  _     _                      _ = Nothing       

			-- | tvar binding part 2: 1 of 2 tvars, impredicatively
            varBind2  fi updTy t1@(Ty_Var v1 _)       t2
                | allowImpredTVBindL fi t1 t2           = Just $ occurBind fi v1 (updTy t2)
            varBind2  fi updTy t1                     t2@(Ty_Var v2 _)
                | allowImpredTVBindR fi t2 t1           = Just $ occurBind fi v2 (updTy t1)
            varBind2  _  _     _                      _ = Nothing       

			-- | tvar binding part 3: 1 of 2 tvars, non impredicatively
            varBind3  fi updTy t1@(Ty_Var v1 _)       t2
                | fiAllowTyVarBind fi t1                = case deepInstMatchTy fi t2 of
                                                            Just (t1',fi') | fiRankEqInstRank fi
                                                              -> Just $ fVar' fUpd (fiInitInstRank $ fiBindTyVar v1 t1' fi') updTy t1 t2
                                                            _ -> Just $ occurBind fi v1 t2
            varBind3  fi updTy t1                     t2@(Ty_Var v2 _)
                | fiAllowTyVarBind fi t2                = case deepInstMatchTy fi t1 of
                                                            Just (t2',fi') | fiRankEqInstRank fi
                                                              -> Just $ fVar' fUpd (fiInitInstRank $ fiBindTyVar v2 t2' fi') updTy t1 t2
                                                            _ -> Just $ occurBind fi v2 t1
            varBind3  _  _     _                      _ = Nothing       
%%]

%%[(4 hmtyinfer)
            fAnn f fi updTy t1@(Ty_Var _ _) t2                = case tyAnnDecomposeMk t2 of
                                                                    (t2@(Ty_Var _ _), (_:_), mk2)
                                                                      -> fVar' f fi2 (updTy . mk2) t1 t2
                                                                      where fi2 = trfi "fAnn L" ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2) fi
                                                                    _ -> f fi updTy t1 t2
            fAnn f fi updTy t1              t2@(Ty_Var _ _)   = case tyAnnDecomposeMk t1 of
                                                                    (t1@(Ty_Var _ _), (_:_), mk1)
                                                                      -> fVar' f fi2 (updTy . mk1) t1 t2
                                                                      where fi2 = trfi "fAnn R" ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2) fi
                                                                    _ -> f fi updTy t1 t2
            fAnn f fi updTy t1              t2                = f fi updTy t1 t2
%%]

%%[(9 hmtyinfer)
            fVarPred2 f fi tpr1                             (Ty_Impls (Impls_Tail iv2 _))
                | isJust mbTl                                 = f fi tpr1 (Ty_Impls (fromJust mbTl))
                where mbTl = lookupImplsVarCyc fi iv2
            fVarPred2 f fi (Ty_Impls (Impls_Tail iv1 _))    tpr2
                | isJust mbTl                                 = f fi (Ty_Impls (fromJust mbTl)) tpr2
                where mbTl = lookupImplsVarCyc fi iv1
            fVarPred2 f fi tpr1                             tpr2
                = f fi tpr1 tpr2
            fVarPred1 f fi (Ty_Impls (Impls_Tail iv1 _))
                | isJust mbTl                                 = f fi (Ty_Impls (fromJust mbTl))
                where mbTl = lookupImplsVarCyc fi iv1
            fVarPred1 f fi tpr1
                = f fi tpr1
%%]

%%[(4 hmtyinfer).fitsIn.Base
            fUpd fi updTy t1                     t2
                | fioMode (fiFIOpts fi) == FitSubRL          = fUpd  fi' updTy t2 t1
                where  fi'       = fiSwapCoCo $ fi  {fiFIOpts = fioSwapOpts $ fioSwapPolarity polContravariant $ fiFIOpts fi}
            fUpd fi updTy Ty_Any                 t2          = res fi t2
            fUpd fi updTy t1                     Ty_Any      = res fi t1
            fUpd fi updTy t1@(Ty_Con s1)         t2@(Ty_Con s2)
                | s1 == s2                                   = dtfo "con" fi t1 t2 [] emptyVarMp
                                                               $ res fi t2

            fUpd fi updTy t1                     t2
                | isJust mbVarBind					         = fromJust mbVarBind
                where  mbVarBind = varBind1 fi updTy t1 t2
%%]

%%[(4 hmtyinfer).fitsIn.Ann
            fUpd fi updTy t1@(Ty_Ann TyAnn_Mono at1)     	t2          = fo
                where fi2 = fi { fiFIOpts = (fiFIOpts fi) {fioBindLFirst = False} }
                      fo  = fVar' fUpd fi2 (updTy . tyAnnMono) at1 t2
            fUpd fi updTy t1                     			t2@(Ty_Ann TyAnn_Mono at2)
                                                    			        = fo
                where fi2 = fi { fiFIOpts = (fiFIOpts fi) {fioBindRFirst = False} }
                      fo  = fVar' fUpd fi2 (updTy . tyAnnMono) t1 at2

            fUpd fi updTy t1@(Ty_Ann a1 at1)     			t2          = fVar' fUpd fi updTy at1 t2
            fUpd fi updTy t1                     			t2@(Ty_Ann a2 at2)
                                                    			        = fVar' fUpd fi updTy t1 at2
%%]

%%[(4 hmtyinfer).fitsIn.Ann
            fUpd fi updTy t1                     t2
                | isJust mbVarBind					         = fromJust mbVarBind
                where  mbVarBind = varBind2 fi updTy t1 t2
%%]

%%[(9 hmtyinfer)
            fUpd fi updTy t1@(Ty_Pred pr1) t2@(Ty_Pred pr2)
                | fioPredAsTy (fiFIOpts fi) && isJust mbfp
                = let (fo,pr) = fromJust mbfp in foUpdTy (Ty_Pred pr) fo
                where  mbfp = fP pr1 pr2
                       fP (Pred_Class ct1)          (Pred_Class ct2)
                            = Just (fo,Pred_Class (foTy fo))
                            where fo = fVar' ff fi id ct1 ct2
                       fP (Pred_Pred prt1)          (Pred_Pred prt2)
                            = Just (fo,Pred_Pred (foTy fo))
                            where fo = fVar' ff fi id prt1 prt2
%%]
%%[(10 hmtyinfer)
                       fP (Pred_Lacks lt1 l1)       (Pred_Lacks lt2 l2)
                            | l1' == l2'
                            = Just (fo,Pred_Lacks (foTy fo) l1')
                            where fo = fVar' ff fi id lt1 lt2
                                  l1' = maybe l1 id $ lookupLabelCyc fi l1
                                  l2' = maybe l2 id $ lookupLabelCyc fi l2
%%]
%%[(9 hmtyinfer)
                       fP _                         _
                            = Nothing
%%]

%%[(4 hmtyinfer).fitsIn.QLR
%%[[4
            fUpd fi updTy t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
%%][6
            fUpd fi updTy t1@(Ty_Quant q1 _ _ _) t2@(Ty_Quant q2 _ _ _)
%%]]
                | fioMode (fiFIOpts fi) == FitUnify && q1 == q2
                                                    = fVar' ff fi2 id uqt1 uqt2
                where  (fi1,uqt1,_,_) = unquant fi   t1 False instCoConst
                       (fi2,uqt2,_,_) = unquant fi1  t2 False instCoConst
%%]

%%[(4 hmtyinfer).fitsIn.QR
%%[[4
            fUpd fi updTy t1                     t2@(Ty_Quant _ _ _)
%%][6
            fUpd fi updTy t1                     t2@(Ty_Quant _ _ _ _)
%%]]
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (fo { foRInstToL = instto2 ++ foRInstToL fo
                                                                })
                where (fi2,uqt2,back2,instto2) = unquant fi t2 False instCoConst
                      fo = fVar' ff fi2 id t1 uqt2
%%[[4
            fUpd fi updTy t1                     t2@(Ty_Quant _ _ _)
%%][6
            fUpd fi updTy t1                     t2@(Ty_Quant _ _ _ _)
%%]]
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (fo { foRInstToL = instto2 ++ foRInstToL fo
                                                                })
                where (fi2,uqt2,back2,instto2) = unquant fi t2 False instContra
                      fo = fVar' ff fi2 id t1 uqt2
%%]

%%[(4 hmtyinfer).fitsIn.QL
%%[[4
            fUpd fi updTy t1@(Ty_Quant _ _ _)    t2
%%][6
            fUpd fi updTy t1@(Ty_Quant _ _ _ _)  t2
%%]]
                | fioIsSubsume (fiFIOpts fi)        = fo { foLInstToL = instto1 ++ foLInstToL fo
                                                         }
                where (fi1,uqt1,back1,instto1) = unquant fi t1 False instCoConst
                      fo = fVar' ff fi1 id uqt1 t2
%%]

%%[(9 hmtyinfer)
            fUpd fi updTy  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                           t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c1 && c1 == c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  -- decompose
                       -- the work
                       (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfPredScope     = fePredScope (fiEnv fi)
                       mbfp             = fVarPred2 fP (fi {fiUniq = u'}) tpr1 tpr2
                       mberr            = Just (errClash fi t1 t2)
                       {-
                       fSub fi prv tr1 tr2
                            = 
                            where prn = poiHNm prv
                                  fi2 = fiAddPr prn prv tpr2 fi
                                  fo  = fVar ff fi2 tr1 tr2
                       -}
                       fP fi tpr1@(Ty_Pred _)              tpr2@(Ty_Pred _)
                            =  if foHasErrs pfo
                               then Nothing
                               else Just  ( foUpdTy (updTy $ [foTy pfo] `mkArrow` foTy fo)
%%[[(9 codegen)
                                          $ foUpdLRCoe (mkIdLRCoeWith n (CMetaVal_Dict Nothing))
                                          $ foUpdLRTCoe (C.mkIdLRCoeWith n (C.MetaVal_Dict Nothing) (C.tyErr ("fitsIn.fP.mkIdLRCoeWith.1: " ++ show n)))
%%]]
                                          $ fo)
                            where  pfo   = fVar' fUpd (fi {fiFIOpts = predFIOpts}) id tpr2 tpr1
                                   n     = uidHNm u2
                                   fo    = fVar' ff (fofi pfo fi) id tr1 tr2
                       fP fi tpr1@(Ty_Pred pr1)            (Ty_Impls (Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv2
%%[[9
                                                      (Impls_Cons iv2 pr1 (mkPrIdCHR u2) ipo2 im2)
%%][99
                                                      (Impls_Cons iv2 pr1 (mkPrIdCHR u2) range ipo2 im2)
%%]]
                                                      tpr1
%%[[(9 codegen)
                                                      (mkIdLRCoeWith n (CMetaVal_Dict Nothing))
                                                      (C.mkIdLRCoeWith n (C.MetaVal_Dict Nothing) (C.tyErr ("fitsIn.fP.mkIdLRCoeWith.2: " ++ show n)))
%%]]
                                                      fo)
                            where  im2   = Impls_Tail u1 ipo2
                                   n     = uidHNm u2
                                   fo    = fVar' ff fi updTy tr1 ([Ty_Impls im2] `mkArrow` tr2)
                       fP fi (Ty_Impls (Impls_Tail iv1 ipo1)) tpr2@(Ty_Pred pr2)
                            =  Just (foUpdImplExplCoe iv1
%%[[9
                                                      (Impls_Cons iv1 pr2 (mkPrIdCHR u2) ipo1 im1)
%%][99
                                                      (Impls_Cons iv1 pr2 (mkPrIdCHR u2) range ipo1 im1)
%%]]
                                                      tpr2
%%[[(9 codegen)
                                                      (mkIdLRCoeWith n (CMetaVal_Dict Nothing))
                                                      (C.mkIdLRCoeWith n (C.MetaVal_Dict Nothing) (C.tyErr ("fitsIn.fP.mkIdLRCoeWith.3: " ++ show n)))
%%]]
                                                      fo)
                            where  im1   = Impls_Tail u1 ipo1
                                   n     = uidHNm u2
                                   fo    = fVar' ff fi updTy ([Ty_Impls im1] `mkArrow` tr1) tr2
                       fP fi (Ty_Impls (Impls_Tail iv1 _)) tpr2@(Ty_Impls im2@(Impls_Nil))
                            =  Just (foUpdImplExpl iv1 im2 tpr2 (fVar' ff fi id tr1 tr2))
                       fP fi (Ty_Impls (Impls_Nil))   tpr2@(Ty_Impls im2@(Impls_Tail iv2 _))
                            =  Just (foUpdImplExpl iv2 Impls_Nil (Ty_Impls Impls_Nil) (fVar' ff fi id tr1 tr2))
                       fP fi tpr1@(Ty_Impls (Impls_Tail iv1 _)) (Ty_Impls im2@(Impls_Tail iv2 _)) | iv1 == iv2
                            =  Just (res fi tpr1)
                       fP fi (Ty_Impls (Impls_Tail iv1 ipo1)) (Ty_Impls im2@(Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv1 im2' (Ty_Impls im2')
%%[[(9 codegen)
                                                      (mkLRCoe (CoeImplApp iv2) (CoeImplLam iv2))
                                                      (C.mkLRCoe (C.Coe_ImplApp iv2) (C.Coe_ImplLam iv2))
%%]]
                                                      (fVar' ff fi updTy tr1 tr2))
                            where im2' = Impls_Tail iv2 ({- [ipo] ++ -} ipo1 ++ ipo2)
                                  -- ipo  = mkImplsProveOcc u1 (fePredScope (fiEnv fi))
                       fP fi (Ty_Impls Impls_Nil)          (Ty_Impls Impls_Nil)
                            =  Just (fVar' ff fi updTy tr1 tr2)
                       fP fi (Ty_Impls Impls_Nil)          (Ty_Impls _)
                            =  mberr
                       fP fi (Ty_Impls Impls_Nil)          (Ty_Pred _)
                            =  mberr
                       fP fi (Ty_Impls _)                  (Ty_Impls Impls_Nil)
                            =  mberr
                       -- fP fi (Ty_Pred _)                   (Ty_Impls Impls_Nil)
                       --      =  mberr
                       fP fi _                             _
                            =  Nothing
%%]

%%[(9 hmtyinfer)
            fUpd fi updTy  t1
                           t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  -- decompose
                       -- the work
                       (u',u1)          = mkNewLevUID (fiUniq fi)
                       mbfp             = fVarPred1 fP (fi {fiUniq = u'}) tpr2
                       mkPrTy pr2 fo    = [Ty_Pred ({- foVarMp fo |=> -} pr2)] `mkArrow` foTy fo
                       fSub fi updTy pr2v pr2 tr2
                            =  let  pr2n  = poiHNm pr2v
                                    (fi3,cnstrMp)
                                          = fiAddPr pr2n pr2v tpr2 fi
                                    fo    = fVar' ff fi3 updTy t1 tr2
%%[[(9 codegen)
                                    rCoe  = mkLamLetCoe pr2n (poiId pr2v)
                                    trCoe  = C.mkLamLetCoe pr2n (C.tyErr ("fitsIn.fSub.mkLamLetCoe: " ++ show pr2n)) (poiId pr2v)
%%]]
                               in   ( foUpdCnstrMp cnstrMp fo
%%[[(9 codegen)
                                    , rCoe
                                    , trCoe
%%]]
                                    )
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just fo
                            where fo = fVar' ff fi updTy t1 tr2
                       fP fi (Ty_Impls (Impls_Tail iv2 _))
                            =  Just (foUpdVarMp (iv2 `varmpImplsUnit` Impls_Nil) fo)
                            where fo = fVar' ff fi updTy t1 tr2
%%[[9
                       fP fi (Ty_Impls (Impls_Cons _ pr2 pv2 _ im2))
%%][99
                       fP fi (Ty_Impls (Impls_Cons _ pr2 pv2 _ _ im2))
%%]]
                            =  Just ( foUpdTy (updTy $ mkPrTy pr2 fo)
%%[[(9 codegen)
                                    $ foUpdLRCoe (lrcoeRSingleton rCoe)
                                    $ foUpdLRTCoe (C.lrcoeRSingleton trCoe)
%%]]
                                    $ fo )
%%[[(9 codegen)
                            where (fo,rCoe,trCoe)
%%][9
                            where fo
%%]]
                                    = fSub fi id pv2 pr2 ([Ty_Impls im2] `mkArrow` tr2)
                       fP fi (Ty_Pred pr2)  | fioAllowRPredElim (fiFIOpts fi)
                            =  Just ( foUpdTy (updTy $ mkPrTy pr2 fo)
%%[[(9 codegen)
                                    $ foUpdLRCoe (lrcoeRSingleton rCoe)
                                    $ foUpdLRTCoe (C.lrcoeRSingleton trCoe)
%%]]
                                    $ fo )
%%[[(9 codegen)
                            where (fo,rCoe,trCoe)
%%][9
                            where fo
%%]]
                                    = fSub fi id (mkPrIdCHR u1) pr2 tr2
                       fP fi _ =  Nothing
%%]

%%[(9 hmtyinfer)
            fUpd fi updTy  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                           t2
                    | hsnIsArrow c1 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  -- decompose
                       -- the work
                       (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfPredScope     = fePredScope (fiEnv fi)
                       mbfp             = fVarPred1 fP (fi {fiUniq = u'}) tpr1
                       fSub fi updTy pv1 psc1 pr1 tr1
                            =  let  fo    = fVar' ff fi updTy tr1 t2
                                    fs    = foVarMp fo
                                    prfPrL= [rngLift range mkPredOccRng pr1 pv1 psc1]
%%[[(9 codegen)
                                    coe   = mkAppCoe1With (mkCExprPrHole globOpts pv1) (CMetaVal_Dict Nothing)
                                    tcoe  = C.mkAppCoe1With (C.mkExprPrHole globOpts pv1) (C.MetaVal_Dict Nothing)
%%]]
                               in   ( fo
%%[[(9 codegen)
                                    , coe
                                    , tcoe
%%]]
                                    , gathPredLToProveCnstrMp prfPrL
                                    )
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just (fVar' ff fi updTy tr1 t2)
                       fP fi (Ty_Impls (Impls_Tail iv1 _))
                            =  Just (foUpdVarMp (iv1 `varmpImplsUnit` Impls_Nil) (fVar' ff fi updTy tr1 t2))
%%[[9
                       fP fi (Ty_Impls (Impls_Cons _ pr1 pv1 _ im1))
%%][99
                       fP fi (Ty_Impls (Impls_Cons _ pr1 pv1 _ _ im1))
%%]]
                            =  Just ( foUpdPrL [] cnstrMp
%%[[(9 codegen)
                                    $ foUpdLRCoe (lrcoeLSingleton lCoe)
                                    $ foUpdLRTCoe (C.lrcoeLSingleton tlCoe)
%%]]
                                    $ fo )
                            where ( fo
%%[[(9 codegen)
                                   , lCoe
                                   , tlCoe
%%]]
                                   , cnstrMp ) = fSub fi updTy pv1 prfPredScope pr1 ([Ty_Impls im1] `mkArrow` tr1)
                       fP fi (Ty_Pred pr1)
                            =  Just ( foUpdPrL [] cnstrMp
%%[[(9 codegen)
                                    $ foUpdLRCoe (lrcoeLSingleton lCoe)
                                    $ foUpdLRTCoe (C.lrcoeLSingleton tlCoe)
%%]]
                                    $ fo )
                            where ( fo
%%[[(9 codegen)
                                   , lCoe
                                   , tlCoe
%%]]
                                   , cnstrMp ) = fSub fi updTy (mkPrIdCHR u1) prfPredScope pr1 tr1
                       fP fi _ =  Nothing
%%]

                       fP fi im2@(Ty_Impls (Impls_Nil))
                            =  Just (foUpdTy ([im2] `mkArrow` foTy fo) $ fo)
                            where fo = fVar' ff fi id t1 tr2
                       fP fi (Ty_Impls (Impls_Tail iv2 _))
                            =  Just (foUpdVarMp (iv2 `varmpImplsUnit` Impls_Nil) $ foUpdTy ([Ty_Impls (Impls_Nil)] `mkArrow` foTy fo) $ fo)
                            where fo = fVar' ff fi id t1 tr2

%%[(7 hmtyinfer)
            fUpd fi updTy  t1@(Ty_App (Ty_Con n1) tr1)
                           t2@(Ty_App (Ty_Con n2) tr2)
                | n1 == n2 && (isRec || isSum)
                = foUpdTy (updTy $ n1 `mkConApp` [foTy fo]) fo
                where  -- decompose
                       -- the work
                       isRec = hsnIsRec n1
                       isSum = hsnIsSum n1
                       fo = fRow fi tr1 tr2 isRec isSum
%%]
            fUpd fi updTy  t1@(Ty_App ta1 tr1)
                           t2@(Ty_App ta2 tr2)
                | isJust mbC1 && isJust mbC2 && n1 == n2 && (isRec || isSum)
                = foUpdTy (updTy $ n1 `mkConApp` [foTy fo]) fo
                where  -- decompose
                       mbC1 = tyMbCon ta1
                       mbC2 = tyMbCon ta2
                       n1 = fromJust mbC1
                       n2 = fromJust mbC2
                       -- the work
                       isRec = hsnIsRec n1
                       isSum = hsnIsSum n1
                       fo = fRow fi tr1 tr2 isRec isSum

%%[(4 hmtyinfer).fitsIn.Var2
            fUpd fi updTy t1                     t2
                | isJust mbVarBind					= fromJust mbVarBind
                where  mbVarBind = varBind3 fi updTy t1 t2
%%]

%%[(4 hmtyinfer).fitsIn.App
            fUpd fi updTy t1@(Ty_App tf1 ta1)
                          t2@(Ty_App tf2 ta2)
                = manyFO [ ffo, afo
                         , dtfo "app" fi t1 t2 [(ffo,"l"),(afo,"r")] emptyVarMp
                           $ trfo "comp" ("ty:" >#< ppTyWithFIFO fi rfo (foTy rfo))
                           $ foUpdTy (updTy $ foTy rfo) rfo
                         ]
                where  -- decompose
                       -- the work
                       fi2    = trfi "decomp" ("t1:" >#< ppTyWithFI fi t1 >-< "t2:" >#< ppTyWithFI fi t2) fi
                       ffo    = fVar' fUpd fi2 id tf1 tf2
                       spine  = asgiSpine $ foAppSpineInfo ffo
%%[[4
                       (as,_) = hdAndTl' unknownAppSpineVertebraeInfo spine
                       -- (as,_) = hdAndTl' (Debug.tr "Ty.FitsIn: trace" (vlist $ reverse $ foTrace ffo) $ panic ("Ty.FitsIn: no spine info")) spine
%%][100
                       (as,_) = hdAndTl' unknownAppSpineVertebraeInfo spine
%%]]
                       pol    = asPolarity as
                       fi3    = trfi "spine" ("f tf1 tf2:" >#< ppTyWithFI fi2 (foTy ffo) >-< "spine:" >#< ppCommas spine) fi2
                       fi4    = (fofi ffo $ fiUpdRankByPolarity pol $ fiSwapCoCo fi3) {fiFIOpts = asFIO as $ fioSwapPolarity pol $ fiFIOpts fi}
                       afo    = fVar' ff fi4 id ta1 ta2
%%[[4
                       rfo    = asFO as ffo $ foCmbApp ffo afo
%%][(8 codegen)
                       rfo    = case (foMbAppSpineInfo ffo,asMbFOUpdCoe as) of
                                  (Nothing,_) | hasSubCoerce
                                    -> errCoerce
                                  (Just _,Nothing) | hasSubCoerce
                                    -> errCoerce
                                  _ -> asFOUpdCoe as globOpts [ffo, asFO as ffo $ foCmbApp ffo afo]
                              where errCoerce = err fi4 [rngLift range Err_NoCoerceDerivation (foVarMp afo |=> foTy ffo) (foVarMp afo |=> foTy afo)]
%%[[8
                                    hasSubCoerce = not $ C.lrcoeIsId $ foLRTCoe afo
%%][9
                                    hasSubCoerce = not $ lrcoeIsId $ foLRCoe afo
%%]]
%%]]
%%]

%%[(7 hmtyinfer).fitsIn.Ext
            fUpd fi updTy t1@(Ty_Ext _ _ _)   t2@(Ty_Ext _ _ _)
                =  fRow fi t1 t2 False False
%%]

FitsIn type clashes

GADT: type clash between fixed type variable and some other type results in a equality proof constraint
%%[(16 hmtyinfer).fitsIn.EqProve
            fUpd fi updTy t1@(Ty_Var v1 TyVarCateg_Fixed) t2 | fioFitVarFailureToProveObl (fiFIOpts fi)  = eqProofObligation t2 fi t1 t2
            fUpd fi updTy t1 t2@(Ty_Var v2 TyVarCateg_Fixed) | fioFitVarFailureToProveObl (fiFIOpts fi)  = eqProofObligation t2 fi t2 t1
            fUpd fi updTy t1@(Ty_Con cstr) t2 | isSkVar cstr && fioFitVarFailureToProveObl (fiFIOpts fi) = eqProofObligation t2 fi t1 t2
            fUpd fi updTy t1 t2@(Ty_Con cstr) | isSkVar cstr && fioFitVarFailureToProveObl (fiFIOpts fi) = eqProofObligation t2 fi t2 t1
            
            fUpd fi updTy t1 t2
              | fioFitFailureToProveObl (fiFIOpts fi)
                  && t1 /= ty1 && t2 /= ty2  -- only generate proof obligations for type clashes when there is at least a partial match
              = eqProofObligation t1 fi t1 t2
%%]

%%[(17 hmtyinfer).fitsIn.PolaritySubtyping
            -- N.B. hsnInvariant is a unique name which cannot be written by a programmer. In other words,
            -- this pattern match cannot trigger during other type inferences.
            -- Weaken Co/Contravariant polarity to Invariant polarity
            fUpd fi updTy t1@(Ty_Con _) t2@(Ty_Con s2)
                | s2 == hsnInvariant                = res fi t2
            -- Invariance propagates through Negate. A bit tricky because this bit of evaluation means unclarity what the return value is
            fUpd fi updTy t1@(Ty_Con s1) t2@(Ty_App (Ty_Con sf2) ta2)
                | s1 == hsnInvariant && sf2 == hsnPolNegation
                                                    = fVar' fUpd fi id t1 ta2
%%]

%%[(4 hmtyinfer).fitsIn.DefaultCase
            fUpd fi updTy t1                     t2          = errClash fi t1 t2
%%]

%%[(4 hmtyinfer).fitsIn.SetupAndResult
            foRes  = fVar' ff fi id ty1 ty2
%%]

%%[(9 hmtyinfer)
fitsIn' :: String -> FIOpts -> FIEnv -> UID -> VarMp -> Ty -> Ty -> FIOut
fitsIn' msg opts env uniq varmp ty1 ty2
  =  fitsIn opts (trPP (msg ++ "-env") env) (trPP (msg ++ "-uniq") uniq) varmp (trPP (msg ++ "-ty1") ty1) (trPP (msg ++ "-ty2") ty2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption for lists of types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer) export(fitsInL)
fitsInL :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> TyL -> (TyL,FIOut)
fitsInL opts env uniq varmp tyl1 tyl2
  = (map foTy foL,fo)
  where (fo,foL)
          = fitsInLWith (\fo1 fo2 -> fo2 {foVarMp = foVarMp fo1 |+> foVarMp fo2, foErrL = foErrL fo1 ++ foErrL fo2})
                        (mkFitsInWrap' env) opts uniq varmp tyl1 tyl2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iterative fitsIn, for now just a simple one (no constr prop, no new uniq, ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(7 hmtyinfer) export(fitsInFold)
fitsInFold :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> FIOut
fitsInFold opts env uniq varmp tyl
  = foldl (\fo t -> if foHasErrs fo then fo else fitsIn opts env uniq varmp (foTy fo) t)
          emptyFO tyl
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(fitPredIntoPred)
fitPredIntoPred :: FIIn -> Pred -> Pred -> Maybe (Pred,VarMp)
fitPredIntoPred fi pr1 pr2
  = f pr1 pr2
  where f (Pred_Var pv1)        pr2@(Pred_Var pv2) | pv1 == pv2     = Just (pr2,emptyVarMp)
        f (Pred_Var pv1)        pr2                | isJust mbPr    = f (fromJust mbPr) pr2
                                                                    where mbPr = varmpPredLookup pv1 (fiVarMp fi)
        f pr1                   (Pred_Var pv2)     | isJust mbPr    = f pr1 (fromJust mbPr)
                                                                    where mbPr = varmpPredLookup pv2 (fiVarMp fi)
        f (Pred_Var pv1)        pr2@(Pred_Var pv2)                  = Just (pr2,pv1 `varmpPredUnit` pr2)
        f pr1                   (Pred_Var pv2)                      = Nothing
        f (Pred_Var pv1)        pr2                                 = Just (pr2,pv1 `varmpPredUnit` pr2)
%%[[10
        f (Pred_Lacks (Ty_Var rv1 TyVarCateg_Plain) l1) pr2 | isJust mbTy
          = f (Pred_Lacks (fromJust mbTy) l1) pr2
          where mbTy = varmpTyLookup rv1 (fiVarMp fi)
        f (Pred_Lacks t1 (Label_Var lv1)) pr2 | isJust mbLb
          = f (Pred_Lacks t1 (fromJust mbLb)) pr2
          where mbLb = varmpLabelLookup lv1 (fiVarMp fi)
        f (Pred_Lacks (Ty_Var rv1 TyVarCateg_Plain)    (Label_Var lv1))
          (Pred_Lacks ty2                           l2@(Label_Lab lb2))
          = Just (Pred_Lacks ty2 l2, (rv1 `varmpTyUnit` ty2) |=> (lv1 `varmpLabelUnit` l2))
        f (Pred_Lacks ty1                              (Label_Var lv1))
          (Pred_Lacks ty2                           l2@(Label_Lab lb2))
          | tyIsEmptyRow ty1 && tyIsEmptyRow ty2
          = Just (Pred_Lacks ty2 l2, lv1 `varmpLabelUnit` l2)
%%]]
%%[[13
        -- assumption: a PredSeq_Var can only occur as a tail in a PredSeq
        f (Pred_Preds ps1) (Pred_Preds ps2)
          = do (ps, varMp) <- fPreds ps1 ps2
               return (Pred_Preds ps, varMp)
          where
            fPreds ps@(PredSeq_Var v1) (PredSeq_Var v2)
              | v1 == v2
              = Just (ps, emptyVarMp)
            fPreds (PredSeq_Var v1) ps
              = Just (ps, v1 `varmpPredSeqUnit` ps)
            fPreds ps (PredSeq_Var v1)
              = Just (ps, v1 `varmpPredSeqUnit` ps)
            fPreds (PredSeq_Cons pr1 ps1) (PredSeq_Cons pr2 ps2)
              = do (pr', s1) <- f pr1 pr2
                   (ps', s2) <- fPreds (s1 |=> ps1) (s1 |=> ps2)
                   return (PredSeq_Cons pr' ps', s2 |=> s1)
            fPreds PredSeq_Nil PredSeq_Nil
              = Just (PredSeq_Nil, emptyVarMp)
            fPreds _ _
              = Nothing
%%]]
%%[[16
        f (Pred_Eq tlA trA) (Pred_Eq tlB trB)
          = if foHasErrs foL || foHasErrs foR
            then Nothing
            else Just $ (Pred_Eq tlOut trOut, varMpOut)
          where
            (u1, u2, u3, u4) = mkNewLevUID3 (fiUniq fi)

            fiOptsL = unifyFIOpts { fioUniq = u3, fioBindRVars = FIOBindNoBut Set.empty, fioDontBind = fioDontBind (fiFIOpts fi), fioPredAsTy = True, fioLeaveRInst = True }
            fiOptsR = unifyFIOpts { fioUniq = u4, fioBindRVars = FIOBindNoBut Set.empty, fioDontBind = fioDontBind (fiFIOpts fi), fioPredAsTy = True, fioLeaveRInst = True }

            foL = fitsIn fiOptsL (fiEnv fi) u1 varMp1In tlA tlB
            foR = fitsIn fiOptsR (fiEnv fi) u2 varMp2In trA trB

            varMp1In = fiVarMp fi
            varMp2In = varMp1Out |=> fiVarMp fi

            varMp1Out = foVarMp foL
            varMp2Out = foVarMp foR
            varMpOut  = varMp2Out |=> varMp1Out

            tlOut = varMpOut |=> foTy foL
            trOut = varMpOut |=> foTy foR
%%]]
        f pr1                   pr2
          = if foHasErrs fo
            then Nothing
            else Just (tyPred $ foTy fo,foVarMp fo)
          where fo = fitsIn (predFIOpts {fioBindRVars = FIOBindNoBut Set.empty, fioDontBind = fioDontBind (fiFIOpts fi)})
                            (fiEnv fi) (fiUniq fi) (fiVarMp fi)
                            (Ty_Pred pr1) (Ty_Pred pr2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Retrieving evidence type for predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(fitPredToEvid)
fitPredToEvid :: UID -> VarMp -> Ty -> ClGam -> FIOut
fitPredToEvid u varmp prTy g
  =  case prTy of
       Ty_Any  ->  emptyFO
       _       ->  fPr u prTy
  where  fPr u prTy
            =  case tyUnAnn prTy of -- TBD: necessary?
                 Ty_Pred p@(Pred_Class _)
                    ->  case gamLookup (predMatchNm p) g of
                           Just clgi
                             -> let (u',u1,u2) = mkNewLevUID2 u
                                    fo = fitsIn (predFIOpts {fioBindRVars = FIOBindNoBut $ Set.singleton u2}) emptyFE u1 varmp (clgiPrToEvidTy clgi) ([prTy] `mkArrow` mkTyVar u2)
                                in  fo {foTy = snd (tyArrowArgRes (foTy fo))}
                           _ -> emptyFO {foErrL = [rngLift emptyRange mkErr_NamesNotIntrod "class" [tyPredMatchNm prTy]]}
                 Ty_Pred (Pred_Pred t)
                    ->  let  (aL,r) = tyArrowArgsRes t
                             (_,aLr'@(r':aL')) = foldr (\t (u,ar) -> let (u',u1) = mkNewLevUID u in (u',fPr u1 t : ar)) (u,[]) (r : aL)
                        in   manyFO (aLr' ++ [emptyFO {foTy = map foTy aL' `mkArrow` foTy r'}])
%%]
         fOpts = predFIOpts {fioDontBind = ftvClosureSet varmp prTy}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrapper for fitsIn (as a solution for module dependency cycle)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer)
mkFitsInWrap' :: FIEnv -> FitsIn'
mkFitsInWrap' env
  =  \opt u varmp t1 t2
        -> let  fo = fitsIn opt env u varmp t1 t2
           in   fo

mkFitsInWrap :: FIEnv -> FitsIn
mkFitsInWrap env
  =  \opt u varmp t1 t2
        -> let  fo = fitsIn opt env u varmp t1 t2
           in   (foTy fo, foVarMp fo, foErrL fo)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For use in ToTyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(fitsInForToTyCore)
-- fitsInForToTyCore :: C.KiFitsIn
fitsInForToTyCore uniq t1 t2
  = foLInstToL fo
  where fo = fitsIn (strongFIOpts {fioExpandEqTyVar=True}) emptyFE uniq emptyVarMp t1 t2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Deep type instantiation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Preferably done
  - in Ty.Trf.Instantiate, but leads to module cycle
  - or directly as part of fitsIn, but is difficult to fit in pairwise type matching structure

