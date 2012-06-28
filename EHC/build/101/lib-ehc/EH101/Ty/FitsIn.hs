module EH101.Ty.FitsIn
( fitsIn
, fitsInL', fitsInL
, fitsInFold
, fitsInForToTyCore
, fitsIn'
, fitPredIntoPred
, fitPredToEvid'
, fitPredToEvid )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Ty.FitsInCommon
import EH101.Ty
import EH101.Error
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.Trf.Instantiate
import EH101.Ty.FitsInCommon2
import EH101.Opts
import EH101.Gam.Full
import Data.Maybe
import Data.List as List
import EH101.Ty.AppSpineGam
import qualified Data.Set as Set
import EH.Util.Utils
import EH.Util.Pretty
import EH101.Ty.Pretty
import EH101.Error.Pretty
import EH101.Ty.Utils1
import EH101.Base.Debug as Debug
import EH101.AbstractCore
import EH101.Ty.Trf.Canonic
import qualified Data.Map as Map
import EH.Util.Pretty
import EH101.Pred
import EH101.Core.Pretty
import EH101.Core
import EH101.Core.Subst
import EH101.Core.Coercion
import EH101.Pred.CommonCHR
import EH101.Core.Utils
import EH101.Ty.Trf.BetaReduce










{-# LINE 85 "src/ehc/Ty/FitsIn.chs" #-}
fiAppVarMp :: VarUpdatable Ty gm => FIIn' gm -> Ty -> Ty
fiAppVarMp fi x = fiVarMpLoc fi `varUpd` (fiVarMp fi `varUpd` x)

{-# LINE 94 "src/ehc/Ty/FitsIn.chs" #-}
instance Show (FIIn' gm) where
  show _ = "FIIn"

instance PP (FIIn' gm) where
  pp fi = "FIIn:" >#< pp (fiEnv fi)

{-# LINE 102 "src/ehc/Ty/FitsIn.chs" #-}
fiUpdOpts :: (FIOpts -> FIOpts) -> FIIn' gm -> FIIn' gm
fiUpdOpts upd fi = fi {fiFIOpts = upd (fiFIOpts fi)}

{-# LINE 107 "src/ehc/Ty/FitsIn.chs" #-}
fiInhibitVarExpandL :: TyVarId -> FIIn' gm -> FIIn' gm
fiInhibitVarExpandL v fi = fi {fiExpLTvS = v `Set.insert` fiExpLTvS fi}

fiVarIsExpandedL :: TyVarId -> FIIn' gm -> Bool
fiVarIsExpandedL v fi = v `Set.member` fiExpLTvS fi

fiInhibitVarExpandR :: TyVarId -> FIIn' gm -> FIIn' gm
fiInhibitVarExpandR v fi = fi {fiExpRTvS = v `Set.insert` fiExpRTvS fi}

fiVarIsExpandedR :: TyVarId -> FIIn' gm -> Bool
fiVarIsExpandedR v fi = v `Set.member` fiExpRTvS fi

{-# LINE 121 "src/ehc/Ty/FitsIn.chs" #-}
fiSwapCoCo :: FIIn' gm -> FIIn' gm
fiSwapCoCo fi = fi {fiExpLTvS = fiExpRTvS fi, fiExpRTvS = fiExpLTvS fi}

{-# LINE 136 "src/ehc/Ty/FitsIn.chs" #-}
fiAppSpineLookup
  :: forall gm .
     ( VarLookupCmb VarMp gm
     , VarLookup gm TyVarId VarMpInfo
     )
     => FIIn' gm -> HsName -> AppSpineGam -> Maybe AppSpineInfo
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
          | otherwise    = asi {asgiVertebraeL = zipWith asUpdateByPolarity (tyArrowArgs $ tyCanonic (emptyTyBetaRedEnv' emptyFE) $ foVarMp fo `varUpd` foTy fo) (asgiVertebraeL asi)}
          where pol = pgiPol pgi
                (polargs,polres) = tyArrowArgsRes pol
                (_,u1,u2) = mkNewLevUID2 uidStart
                fo = fitsIn weakFIOpts emptyFE u1 (emptyVarMp :: VarMp) pol (map mkPolVar (mkNewUIDL (length polargs) u2) `mkArrow` polCovariant)

{-# LINE 286 "src/ehc/Ty/FitsIn.chs" #-}
manyFO :: [FIOut] -> FIOut
manyFO = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)

fitsIn
  :: forall gm .
     {- ( VarUpdatable Ty gm
     , VarLookupCmb VarMp gm
     , VarLookupCmb gm gm
     )
     => -}
     ( VarLookup gm TyVarId VarMpInfo
     , VarLookupCmb VarMp gm
     )
     => FIOpts -> FIEnv -> UID -> gm -> Ty -> Ty
     -> FIOut
fitsIn opts env uniq varmp
  =  fitsInFI
       ((emptyFI
          { fiUniq = uniq
          , fiFIOpts = opts
          , fiVarMp = varmp
          , fiEnv = env
          }
        ) :: FIIn' gm
       )

{-# LINE 316 "src/ehc/Ty/FitsIn.chs" #-}
fitsInFI
  :: forall gm .
     {- ( VarUpdatable Ty gm
     , VarLookupCmb VarMp gm
     , VarLookupCmb gm gm
     )
     => -}
     ( VarLookup gm TyVarId VarMpInfo
     , VarLookupCmb VarMp gm
     )
     => FIIn' gm -> Ty -> Ty
     -> FIOut
fitsInFI fi ty1 ty2
  =  foRes {foTrace = reverse $ foTrace foRes}
  where
            appSpineGam             =  feAppSpineGam $ fiEnv fi
            -- options
            globOpts                =  feEHCOpts $ fiEnv fi
            -- range where fitsIn takes place
            range                   =  feRange $ fiEnv fi

            -- tracing
            trfiAdd  tr   fi        =  fi
            trfi msg rest fi        =  fi
            trfoAdd  tr   fo        =  fo
            trfo msg rest fo        =  fo

            -- derivation tree
            dtfo _ _ _ _ _ _ fo     =  fo

            -- results
            res' fi tv t            =  updtr $ (fifo fi emptyFO) {foTy = tv, foMbAppSpineInfo = fiAppSpineLookup fi (tyConNm t) appSpineGam}
                                    where updtr    = id
            res  fi    t            =  res' fi t t

            -- errors
            err  fi e               =  trfo "err" (ppErrL e)
                                       $ emptyFO {foUniq = fioUniq (fiFIOpts fi), foErrL = e, foTrace = fiTrace fi}
            errClash fiErr t1 t2    =  maybe dflt (\mk -> err fiErr [mk ty1 ty2]) $ fiMbMkErrClash $ fiFIOpts fiErr
                                    where dflt = err fiErr [rngLift range Err_UnifyClash (fiAppVarMp fiErr ty1) (fiAppVarMp fiErr ty2) (fioMode (fiFIOpts fi)) (fiAppVarMp fiErr t1) (fiAppVarMp fiErr t2) (fioMode (fiFIOpts fiErr))]


            -- binding
            occurBind fi isLBind v t=  bind fi isLBind v t

{-# LINE 413 "src/ehc/Ty/FitsIn.chs" #-}
            -- 20080309, AD: naming of function is not right, type info neither, error yes. Should indicate a double expansion of tyvar, indicating infinite type.
            errInfinite fi v t      =  err fi [rngLift range Err_UnifyOccurs (fiAppVarMp fi ty1) (fiAppVarMp fi ty2) (fioMode (fiFIOpts fi)) v t (fioMode (fiFIOpts fi))]

{-# LINE 418 "src/ehc/Ty/FitsIn.chs" #-}
            lookupImplsVarCyc fi v  =  fiLookupVar' varmpImplsLookupCyc varmpImplsLookupCyc v (fiVarMpLoc fi) (fiVarMp fi)
{-# LINE 421 "src/ehc/Ty/FitsIn.chs" #-}
            lookupLabelCyc    fi v  =  fiLookupVar' varmpLabelLookupLabelCyc varmpLabelLookupLabelCyc v (fiVarMpLoc fi) (fiVarMp fi)

{-# LINE 425 "src/ehc/Ty/FitsIn.chs" #-}
            bind fi isLBind tv t    =  dtfo "bind" fi tv' t [] (tv `varmpTyUnit` t)
                                       $ res' (fiBindTyVar tv t fi2) tv' t
                                    where tv' = mkTyVar tv
                                          fi2 = case (tyMbVar t, (if isLBind then fioBindRVars else fioBindLVars) (fiFIOpts fi)) of
                                                  (Just v, FIOBindNoBut but) | not (v `Set.member` but)
                                                    -> -- (\x -> let o = fiFIOpts x in Debug.tr "fitsIn.bind.fi2" (isLBind >#< tv >#< t >-< show (fioBindRVars o) >#< show (fioBindLVars o) >-< show (fioDontBind $ fiFIOpts fi) >#< show (fioDontBind o)) x) $
                                                       fiInhibitBind v fi
                                                  _ -> fi

{-# LINE 444 "src/ehc/Ty/FitsIn.chs" #-}
            allowImpredTVBindL fi t _
                = fioBindLFirst (fiFIOpts fi) && fiAllowTyVarBind fi t
            allowImpredTVBindR fi t _
                = fioBindRFirst (fiFIOpts fi) && fiAllowTyVarBind fi t

{-# LINE 451 "src/ehc/Ty/FitsIn.chs" #-}
            -- removal of quantifier
            unquant fi t hide howToInst
                = ( fi { fiUniq = u
                       , fiVarMpLoc = instToL1VarMp instto |+> fiVarMpLoc fi
                       }
                  , uqt,back,instto
                  )
                where  (u,uq)            = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs,instto) = tyInst1Quants uq howToInst t
                       back              = if hide  then  \fo -> foSetVarMp (varmpDel rtvs (foVarMp fo)) $ foUpdTy t fo
                                                    else  id

{-# LINE 497 "src/ehc/Ty/FitsIn.chs" #-}
            foUpdVarMp  c fo = fo {foVarMp = c |+> foVarMp fo}
            fifo       fi fo = fo { foVarMp    = fiVarMpLoc fi, foUniq = fiUniq fi, foTrace = fiTrace fi
                                  , foDontBind = fioDontBind (fiFIOpts fi)
                                  }
            fofi       fo fi = -- (\x -> Debug.tr "fofi" ((pp $ show $ fioDontBind o) >-< (pp $ show $ foDontBind fo) >-< (pp $ show $ fioDontBind $ fiFIOpts x)) x)
                               fi { fiVarMpLoc = foVarMp    fo, fiUniq = foUniq fo, fiTrace = foTrace fo
                                  , fiFIOpts   = o {fioDontBind = foDontBind fo}
                                  }
                               where o  = fiFIOpts fi

{-# LINE 513 "src/ehc/Ty/FitsIn.chs" #-}
            fiInhibitBind v fi = fi {fiFIOpts = o {fioDontBind = v `Set.insert` fioDontBind o}}
                               where o  = fiFIOpts fi

{-# LINE 518 "src/ehc/Ty/FitsIn.chs" #-}
            foUpdTy  t   fo  = fo {foTy = t}

{-# LINE 522 "src/ehc/Ty/FitsIn.chs" #-}
            foCmbAppTy   ffo afo  = afo {foTy = Ty_App (foTy ffo) (foTy afo)}
            foCmbVarMp   ffo afo  = afo -- {foVarMp = foVarMp afo `varUpd` foVarMp ffo}
            foCmbCoCon   ffo afo  = afo {foMbAppSpineInfo = fmap asgiShift1SpinePos $ foMbAppSpineInfo ffo}

{-# LINE 528 "src/ehc/Ty/FitsIn.chs" #-}
            foCmbPrL     ffo afo  = afo {foPredOccL = foPredOccL afo ++ foPredOccL ffo, foGathCnstrMp = foGathCnstrMp afo `cnstrMpUnion` foGathCnstrMp ffo}
{-# LINE 531 "src/ehc/Ty/FitsIn.chs" #-}
            foCmbCSubst  ffo afo  = afo {foCSubst = cSubstApp (foCSubst afo) (foCSubst ffo)}
{-# LINE 538 "src/ehc/Ty/FitsIn.chs" #-}
            foCmbApp     ffo      =
                                    -- foCmbTvKiVarMp ffo .
                                    -- (\afo -> afo {foDontBind = ((\x -> Debug.tr "foCmbApp.ffo" (pp $ show x) x) $ foDontBind ffo) `Set.union` ((\x -> Debug.tr "foCmbApp.afo" (pp $ show x) x) $ foDontBind afo)}) .
                                    foCmbPrfRes ffo .
                                    foCmbCoCon ffo . foCmbVarMp ffo . foCmbAppTy ffo

{-# LINE 556 "src/ehc/Ty/FitsIn.chs" #-}
            foCmbPrfRes  ffo      = foCmbPrL ffo
                                    . foCmbCSubst ffo

{-# LINE 566 "src/ehc/Ty/FitsIn.chs" #-}
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
            foUpdLRCoe lrcoe fo = fo {foLRCoe = lrcoe `lrcoeUnion` foLRCoe fo}
            foUpdImplExplCoe iv im tpr
                                   lrcoe
                                   fo
                            = foUpdImplExpl iv im tpr
                              $ foUpdLRCoe lrcoe
                                fo

{-# LINE 607 "src/ehc/Ty/FitsIn.chs" #-}
            deepInstMatchTy fi t
              = case t of
                  _ | not (null as
                          || tyConNm f == hsnPolNegation
                          )
                                  -> Just (mkApp $ mkNewTyVarL (length as + 1) u1, fi')
                    | otherwise   -> Nothing
                    where (f,as) = tyAppFunArgs t
              where (u,u1) = mkNewLevUID (fiUniq fi)
                    fi' = fi {fiUniq = u}

{-# LINE 622 "src/ehc/Ty/FitsIn.chs" #-}
            fPairWise fi tL1 tL2
              =  foldr  (\(t1,t2) (foL,fii)
                           -> let  fo = fVar' fTySyn fii id t1 t2
                              in   (fo:foL,fofi fo fii))
                        ([],fi)
                        (zip tL1 tL2)

{-# LINE 637 "src/ehc/Ty/FitsIn.chs" #-}
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
                         | fiAllowTyVarBind fi r2
                         = bind (fR fi2 r1 rv [] e12 e2) v2 rv e1
                         where  (fi2,rv) = mkTv fi
                       fR fi r1@(Ty_Var v1 f1) r2 e1 e12 e2@(_:_)
                         | fiAllowTyVarBind fi r1
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
                               (foL,fi2) = fPairWise ({- fiUpdOpts fioMkStrong -} fi) (assocLElts e1L) (assocLElts e2L)
                               eKeys = assocLKeys e1L
                               eL = zip eKeys (map foTy foL)
                               fo = fR fi2 r1 r2 e1 [] e2
                               foR = manyFO ([fo] ++ foL ++ [foRes])
                               foRes = (\fo -> foldr foCmbPrfRes fo foL)
                                       $ foUpdRecFldsCoe eKeys foL tr1
                                       $ foUpdTy (foTy fo `mkTyRow` eL) fo

{-# LINE 706 "src/ehc/Ty/FitsIn.chs" #-}
                       fR fi r1@(Ty_Var _ f1) r2@(Ty_Con n2) [] [] []
                         | tvCatIsFixed f1 && n2 == hsnRowEmpty && isRec
                         = res fi r2
                       fR fi r1@(Ty_Var v1 f1) r2@(Ty_Con n2) [] [] []
                         | f1 `elem` fioBindCategs (fiFIOpts fi) {- tvCatIsPlain f1 -} && n2 == hsnRowEmpty && isRec
                         = occurBind fi True v1 r2
                       fR fi r1 r2 [] [] []
                         = (fBase fi id r1 r2)
                              { foLRCoe = emptyLRCoe
                              }

{-# LINE 732 "src/ehc/Ty/FitsIn.chs" #-}
                       fR fi _ _ _ _ _
                         = errClash fi tr1 tr2

{-# LINE 741 "src/ehc/Ty/FitsIn.chs" #-}
                       fo         = fR fi2 r1 r2 extsIn1 extsIn12 extsIn2
                       foR        = (if isRec then foUpdRecCoe tr1 r1 r2 extsIn1 extsIn12 extsIn2 else id) fo
                       foUpdRecCoe tr1 r1 r2 e1 e12 e2 fo
                         =  let  rn = uidHNm u1
                                 predScope = fePredScope (fiEnv fi)
                                 -- tr1s = foVarMp fo `varUpd` tr1
                                 fi3 = fofi fo fi2
                                 tr1s = uncurry mkTyRow $ tyRowExtsUnAnn $ tyRowExtsWithLkup (fiLookupTyVarCyc fi3) tr1
                                 (u',u2,u3,u4) = mkNewLevUID3 (foUniq fo)
                                 r = acoreVar rn
                                 mkLSel n u = acoreSelCaseTy (emptyRCEEnv globOpts) (Just (hsnUniqifyEval rn,Ty_Any)) r CTagRec n {-n-} (acoreNmHole u) Nothing
                                 mkLPred' r l u
                                   =  let  r' = maybe Ty_Any fst $ tyRowExtr l r
                                      in   (rngLift range mkPredOccRng (Pred_Lacks r' (Label_Lab l)) (mkPrIdCHR u) predScope,r')
                                 mkLPred r l u = fst (mkLPred' r l u)
                                 rowCoeL = sortByOn rowLabCmp fst (foRowCoeL fo)
                                 (fuUpdL,prUpdL,tr1s',csubstUpd,_)
                                   =  foldr  (\(l,c) (fuL,prL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel) = coeEvalOnAsSubst u2 c (mkLSel l u1)
                                                    in  ( ( l
                                                          , (CExpr_TupUpd (acoreBuiltinUndefined globOpts) CTagRec l (acoreNmHole u) sel,Nothing)
                                                          ) : fuL
                                                        , mkLPred r l u1 : prL
                                                        , r
                                                        , csubst `cSubstApp` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([],[],tr1s,emptyCSubst,u2) rowCoeL
                                 (fuDelL,prDelL,_,_)
                                   =  foldl  (\(fuL,prL,r,u) l
                                                  ->  let  (pr,r') = mkLPred' r l u
                                                      in   ( ( l
                                                             , (CExpr_TupDel (acoreVar hsnWild) CTagRec l (acoreNmHole u),Nothing)
                                                             ) : fuL
                                                           , pr:prL,r',uidNext u
                                                           )
                                             )
                                             ([],[],tr1s',u3) (sortBy rowLabCmp (assocLKeys e1))
                                 fuL = fuUpdL ++ reverse fuDelL
                                 (prBldL, fBldL, _, csubstBld, _)
                                   =  foldr  (\l (prL,fL,r,csubst,u)
                                                ->  let (u',u1,u2) = mkNewLevUID2 u
                                                        (sel,csubstSel)
                                                          = maybe (s,emptyCSubst) (\c -> coeEvalOnAsSubst u2 c s) (lookup l rowCoeL)
                                                          where s = mkLSel l u1
                                                    in  ( mkLPred r l u1 : prL,
                                                          sel :
                                                          fL
                                                        , r
                                                        , csubst `cSubstApp` csubstSel
                                                        , u'
                                                        )
                                             )
                                             ([], [], tr1s, emptyCSubst, u3)
                                             (sortBy rowLabCmp ((assocLKeys . map fst $ e12) ++ assocLKeys e2))
                            in   case r2 of
                                   Ty_Con n2
                                     | n2 == hsnRowEmpty && null fuL && null e2
                                     ->  fo
                                             { foLRCoe = emptyLRCoe
                                             , foCSubst  = foCSubst fo `cSubstApp` csubstUpd `cSubstApp` csubstBld
                                             }
{- -- when ext rec deletes are implemented
                                     | n2 == hsnRowEmpty && null fuUpdL && not (null fuDelL) && null e2
                                     ->  let coe = Coe_Map (\e -> acoreLet CBindCateg_Plain [CBind_Bind rn e] (fuMkCExpr globOpts u4 fuDelL r))
                                         in  fo  {  foLRCoe = lrcoeLSingleton coe
                                                 ,  foPredOccL = prDelL ++ foPredOccL fo
                                                 ,  foGathCnstrMp = gathPredLToProveCnstrMp prDelL `cnstrMpUnion` foGathCnstrMp fo
                                                 ,  foUniq = u'
                                                 }
-}
                                     | n2 == hsnRowEmpty && not (null prBldL)
                                     ->  let
                                             coe = Coe_Map (\e -> acoreLet1Plain rn e (CExpr_Tup CTagRec `acoreApp` fBldL))
                                         in  fo  {  foPredOccL = prBldL ++ foPredOccL fo
                                                 ,  foGathCnstrMp = gathPredLToProveCnstrMp prBldL `cnstrMpUnion` foGathCnstrMp fo
                                                 ,  foUniq = u'
                                                 ,  foLRCoe = lrcoeLSingleton coe
                                                 }
                                   Ty_Var _ cat
                                     | tvCatIsFixed cat && not (null fuL)
                                     ->  fo  {  foPredOccL = prUpdL ++ prDelL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp (prUpdL ++ prDelL) `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
                                             ,  foLRCoe = lrcoeLSingleton coe
                                             }
                                     where coe = Coe_Map (\e -> acoreLet1Plain rn e (fuMkCExpr globOpts u4 fuL r))
                                   _ |  not (null fuUpdL)
                                     ->  fo  {  foPredOccL = prUpdL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp prUpdL `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
                                             ,  foLRCoe = lrcoeLSingleton coe
                                             }
                                     |  otherwise
                                     ->  fo
                                             {  foLRCoe = emptyLRCoe
                                             }
                                     where coe = Coe_Map (\e -> acoreLet1Plain rn e (fuMkCExpr globOpts u4 fuUpdL r))

{-# LINE 954 "src/ehc/Ty/FitsIn.chs" #-}
                       foUpdRecFldsCoe eKeys foL tr1 foR
                         =  let (u',u1) = mkNewLevUID (foUniq foR)
                                us = mkNewLevUIDL (length foL) u1
                                (cL,sL)
                                   = unzip
                                       [  ((l,c),s)
                                       |  (l,fo,u) <- zip3 eKeys foL us
                                       ,  let (c,s) = lrcoeWipeWeaveAsSubst globOpts u (foVarMp foR) (foLRCoe fo)
                                       ,  not (acoreCoeIsId c)
                                       ]
                            in  foR { foUniq = u'
                                    , foRowCoeL = cL
                                    , foCSubst = foldr cSubstApp (foCSubst foR) sL
                                    }

{-# LINE 984 "src/ehc/Ty/FitsIn.chs" #-}
            f fi t1 t2
              = fBase fi id t1 t2
{-# LINE 992 "src/ehc/Ty/FitsIn.chs" #-}
            fTySyn fi updTy t1 t2
              = case filter (not . foHasErrs) tries of
                  (fo:_) -> fo
                  _      -> case (drop limit rt1, drop limit rt2, tries) of
                              ((t:_),_    ,_       ) -> err (trfiAdd (tbroutTracePPL t) fi2) [rngLift range Err_TyBetaRedLimit (fiAppVarMp fi2 t1) (fiAppVarMp fi2 (tbroutRes t)) limit]
                              (_    ,(t:_),_       ) -> err (trfiAdd (tbroutTracePPL t) fi2) [rngLift range Err_TyBetaRedLimit (fiAppVarMp fi2 t2) (fiAppVarMp fi2 (tbroutRes t)) limit]
                              (_    ,_    ,ts@(_:_)) -> last ts
                              (_    ,_    ,_       ) -> errClash fi2 t1 t2
              where limit = ehcOptTyBetaRedCutOffAt globOpts
                    fi2   = fi
                    rt1   = tyBetaRedAndInit (emptyTyBetaRedEnv {tbredFI=fi2}) betaRedTyLookup t1
                    rt2   = tyBetaRedAndInit (emptyTyBetaRedEnv {tbredFI=fi2}) betaRedTyLookup t2
                    tries = take (limit+1) $ try fi2 (rt1) (rt2)
                          where -- get the pairwise fitsIn of further and further expanded synonyms
                                try fi (t1:ts1@(_:_)) (t2:ts2@(_:_)) = (ok t1 t2 $ fBase fi' updTy (tbroutRes t1) (tbroutRes t2)) ++ try fi' ts1 ts2
                                                                     where fi' = trfiAdd (tbroutTracePPL t1) $ trfiAdd (tbroutTracePPL t2) fi
                                try fi ts1@[t1]       (t2:ts2@(_:_)) = (ok t1 t2 $ fBase fi' updTy (tbroutRes t1) (tbroutRes t2)) ++ try fi' ts1 ts2
                                                                     where fi' = trfiAdd (tbroutTracePPL t2) fi
                                try fi (t1:ts1@(_:_)) ts2@[t2]       = (ok t1 t2 $ fBase fi' updTy (tbroutRes t1) (tbroutRes t2)) ++ try fi' ts1 ts2
                                                                     where fi' = trfiAdd (tbroutTracePPL t1) fi
                                try fi [t1]           [t2]           =  ok t1 t2 $ fBase fi' updTy (tbroutRes t1) (tbroutRes t2)
                                                                     where fi' = fi
                                -- check for a valid combi using lookahead info of next expansion
                                ok e1 e2 f | betaRedIsOkFitsinCombi (fiAllowTyVarBind fi)
                                                                    e1 e2 = [f]
                                           | otherwise              =       []

{-# LINE 1025 "src/ehc/Ty/FitsIn.chs" #-}
            varMayFit isL fi t@(Ty_Var v f)
              = f `elem` fioBindCategs (fiFIOpts fi) && not (v `Set.member` fioDontBind (fiFIOpts fi))
              -- where mbvs = if isL then fioBindLVars (fiFIOpts fi) else fioBindRVars (fiFIOpts fi)
{-# LINE 1030 "src/ehc/Ty/FitsIn.chs" #-}
            varMayExpand mbvs fi t@(Ty_Var v f)
              = not ((fioBindIsYes mbvs || v `Set.member` fioBindNoSet mbvs) || v `Set.member` fioDontBind (fiFIOpts fi))

{-# LINE 1035 "src/ehc/Ty/FitsIn.chs" #-}
            fVar' f fi updTy t1@(Ty_Var v1 f1)     t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2
                  && not (fioExpandEqTyVar (fiFIOpts fi))
                                                              = res fi t1
            fVar' f fi updTy t1@(Ty_Var v1 f1)     t2
                | isJust mbTy1                                = if fiVarIsExpandedL v1 fi
                                                                then errInfinite fi v1 t1'
                                                                else fVar' f (fiInhibitVarExpandL v1 fi2) updTy t1' t2
                | varMayExpand mbvs fi t1                     = fVar' f (fiInhibitBind v1 fi2) updTy t1 t2
                where mbTy1   = fiLookupTyVarCyc fi v1
                      t1'     = fromJust mbTy1
                      fi2     = fi
                      mbvs    = fioBindLVars (fiFIOpts fi)
            fVar' f fi updTy t1                    t2@(Ty_Var v2 f2)
                | isJust mbTy2                                = if fiVarIsExpandedR v2 fi
                                                                then errInfinite fi v2 t2'
                                                                else fVar' f (fiInhibitVarExpandR v2 fi2) updTy t1 t2'
                | varMayExpand mbvs fi t2                     = fVar' f (fiInhibitBind v2 fi2) updTy t1 t2
                where mbTy2   = fiLookupTyVarCyc fi v2
                      t2'     = fromJust mbTy2
                      fi2     = fi
                      mbvs    = fioBindRVars (fiFIOpts fi)
            fVar' f fi updTy t1                    t2                = fAnn f fi2 updTy t1 t2
                where fi2     = fi

            fVar f fi        t1                    t2                = fVar' f fi id t1 t2

{-# LINE 1109 "src/ehc/Ty/FitsIn.chs" #-}
			-- | tvar binding part 1: 2 tvars
            varBind1  fi updTy t1@(Ty_Var v1 f1)      t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2                  = Just $ res  fi       t1
                |     lBefR && fiAllowTyVarBind fi t1   = Just $ bind fi True  v1 (updTy t2)
                | not lBefR && fiAllowTyVarBind fi t2   = Just $ bind fi False v2 (updTy t1)
                where lBefR = fioBindLBeforeR (fiFIOpts fi)
            {-
            varBind1  fi updTy t1@(Ty_Var v1 f1)      t2
                | isJust mbNoise                        = case fromJust mbNoise of
                                                            (Ty_Var v2 f2) | v1 == v2 && f1 == f2 -> Just $ res fi t1
                where mbNoise = tyUnNoiseForVarBind t2
            varBind1  fi updTy t1                     t2@(Ty_Var v2 f2)
                | isJust mbNoise                        = case fromJust mbNoise of
                                                            (Ty_Var v1 f1) | v1 == v2 && f1 == f2 -> Just $ res fi t2
                where mbNoise = tyUnNoiseForVarBind t1
            -}
            varBind1  _  _     _                      _ = Nothing

			-- | tvar binding part 2: 1 of 2 tvars, impredicatively
            varBind2  fi updTy t1@(Ty_Var v1 _)       t2
                | allowImpredTVBindL fi t1 t2           = Just $ occurBind fi True  v1 (updTy t2)
            varBind2  fi updTy t1                     t2@(Ty_Var v2 _)
                | allowImpredTVBindR fi t2 t1           = Just $ occurBind fi False v2 (updTy t1)
            {-
            varBind2  fi updTy t1@(Ty_Var v1 f1)      t2
                | isJust mbNoise                        = case fromJust mbNoise of
                                                            (Ty_Var v2 f2) | v1 == v2 && f1 == f2 -> Just $ res fi t1
                where mbNoise = tyUnNoiseForVarBind t2
            varBind2  fi updTy t1                     t2@(Ty_Var v2 f2)
                | isJust mbNoise                        = case fromJust mbNoise of
                                                            (Ty_Var v1 f1) | v1 == v2 && f1 == f2 -> Just $ res fi t2
                where mbNoise = tyUnNoiseForVarBind t1
            -}
            varBind2  _  _     _                      _ = Nothing

			-- | tvar binding part 3: 1 of 2 tvars, non impredicatively
            varBind3  fi updTy t1@(Ty_Var v1 _)       t2
                | fiAllowTyVarBind fi t1                = case deepInstMatchTy fi t2 of
                                                            Just (t1',fi') | fiRankEqInstRank fi
                                                              -> Just $ fVar' fBase (fiInitInstRank $ fiBindTyVar v1 t1' fi') updTy t1 t2
                                                            _ -> Just $ occurBind fi True v1 t2
            varBind3  fi updTy t1                     t2@(Ty_Var v2 _)
                | fiAllowTyVarBind fi t2                = case deepInstMatchTy fi t1 of
                                                            Just (t2',fi') | fiRankEqInstRank fi
                                                              -> Just $ fVar' fBase (fiInitInstRank $ fiBindTyVar v2 t2' fi') updTy t1 t2
                                                            _ -> Just $ occurBind fi False v2 t1
            varBind3  _  _     _                      _ = Nothing

{-# LINE 1159 "src/ehc/Ty/FitsIn.chs" #-}
            fAnn f fi updTy t1@(Ty_Var _ _) t2                = case tyAnnDecomposeMk t2 of
                                                                    (t2@(Ty_Var _ _), (_:_), mk2)
                                                                      -> fVar' f fi2 (updTy . mk2) t1 t2
                                                                      where fi2 = fi
                                                                    _ -> f fi updTy t1 t2
            fAnn f fi updTy t1              t2@(Ty_Var _ _)   = case tyAnnDecomposeMk t1 of
                                                                    (t1@(Ty_Var _ _), (_:_), mk1)
                                                                      -> fVar' f fi2 (updTy . mk1) t1 t2
                                                                      where fi2 = fi
                                                                    _ -> f fi updTy t1 t2
            fAnn f fi updTy t1              t2                = f fi updTy t1 t2

{-# LINE 1181 "src/ehc/Ty/FitsIn.chs" #-}
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

{-# LINE 1197 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1                    t2
                | fioMode (fiFIOpts fi) == FitSubRL          = fBase  fi' updTy t2 t1
                where  fi'       = fiSwapCoCo $ fi  {fiFIOpts = fioSwapOpts $ fioSwapPolarity polContravariant $ fiFIOpts fi}
            fBase fi updTy Ty_Any                t2          = res fi t2
            fBase fi updTy t1                    Ty_Any      = res fi t1
            fBase fi updTy t1@(Ty_Con s1)        t2@(Ty_Con s2)
                | s1 == s2                                   = dtfo "con" fi t1 t2 [] emptyVarMp
                                                               $ res fi t2

            fBase fi updTy t1                    t2
                | isJust mbVarBind					         = fromJust mbVarBind
                where  mbVarBind = varBind1 fi updTy t1 t2

{-# LINE 1212 "src/ehc/Ty/FitsIn.chs" #-}
            -- get rid of annotation for fitsIn, but preserve as result
            fBase fi updTy t1@(Ty_Ann TyAnn_Mono at1)     	t2          = fo
                where fi2 = fi { fiFIOpts = (fiFIOpts fi) {fioBindLFirst = False} }
                      fo  = fVar' fBase fi2 (updTy . tyAnnMono) at1 t2
            fBase fi updTy t1                     			t2@(Ty_Ann TyAnn_Mono at2)
                                                    			        = fo
                where fi2 = fi { fiFIOpts = (fiFIOpts fi) {fioBindRFirst = False} }
                      fo  = fVar' fBase fi2 (updTy . tyAnnMono) t1 at2

            fBase fi updTy t1@(Ty_Ann a1 at1)     			t2          = fVar' fBase fi updTy at1 t2
            fBase fi updTy t1                     			t2@(Ty_Ann a2 at2)
                                                    			        = fVar' fBase fi updTy t1 at2

{-# LINE 1227 "src/ehc/Ty/FitsIn.chs" #-}
            -- always get rid of empty implicits
            fBase fi updTy t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                           t2
                    | hsnIsArrow c1 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  mbfp             = fVarPred1 fP fi tpr1
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just (fVar' fTySyn fi updTy tr1 t2)
                       fP fi _ =  Nothing
            fBase fi updTy t1
                           t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  mbfp             = fVarPred1 fP fi tpr2
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just (fVar' fTySyn fi updTy t1 tr2)
                       fP fi _ =  Nothing

{-# LINE 1247 "src/ehc/Ty/FitsIn.chs" #-}
            -- here we decide whether to bind impredicatively, anything not to be bounded as such must be dealt with before here
            fBase fi updTy t1                    t2
                | isJust mbVarBind					         = fromJust mbVarBind
                where  mbVarBind = varBind2 fi updTy t1 t2

{-# LINE 1254 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_Pred pr1) t2@(Ty_Pred pr2)
                | fioPredAsTy (fiFIOpts fi) && isJust mbfp
                = let (fo,pr) = fromJust mbfp in foUpdTy (Ty_Pred pr) fo
                where  mbfp = fP pr1 pr2
                       fP (Pred_Class ct1)          (Pred_Class ct2)
                            = Just (fo,Pred_Class (foTy fo))
                            where fo = fVar' fTySyn fi id ct1 ct2
                       fP (Pred_Pred prt1)          (Pred_Pred prt2)
                            = Just (fo,Pred_Pred (foTy fo))
                            where fo = fVar' fTySyn fi id prt1 prt2
{-# LINE 1266 "src/ehc/Ty/FitsIn.chs" #-}
                       fP (Pred_Lacks lt1 l1)       (Pred_Lacks lt2 l2)
                            | l1' == l2'
                            = Just (fo,Pred_Lacks (foTy fo) l1')
                            where fo = fVar' fTySyn fi id lt1 lt2
                                  l1' = maybe l1 id $ lookupLabelCyc fi l1
                                  l2' = maybe l2 id $ lookupLabelCyc fi l2
{-# LINE 1274 "src/ehc/Ty/FitsIn.chs" #-}
                       fP _                         _
                            = Nothing

{-# LINE 1279 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_TBind q1 _ _ _) t2@(Ty_TBind q2 _ _ _)
                | fioMode (fiFIOpts fi) == FitUnify && q1 == q2
                                                    = fVar' fTySyn fi2 id uqt1 uqt2
                where  (fi1,uqt1,_,_) = unquant fi   t1 False instCoConst
                       (fi2,uqt2,_,_) = unquant fi1  t2 False instCoConst

{-# LINE 1291 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1                     t2@(Ty_TBind _ _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (fo { foRInstToL = instto2 ++ foRInstToL fo
                                                                })
                where (fi2,uqt2,back2,instto2) = unquant fi t2 False instCoConst
                      fo = fVar' fTySyn fi2 id t1 uqt2
            fBase fi updTy t1                     t2@(Ty_TBind _ _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (fo { foRInstToL = instto2 ++ foRInstToL fo
                                                                })
                where (fi2,uqt2,back2,instto2) = unquant fi t2 False instContra
                      fo = fVar' fTySyn fi2 id t1 uqt2

{-# LINE 1314 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_TBind _ _ _ _)  t2
                | fioIsSubsume (fiFIOpts fi)        = fo { foLInstToL = instto1 ++ foLInstToL fo
                                                         }
                where (fi1,uqt1,back1,instto1) = unquant fi t1 False instCoConst
                      fo = fVar' fTySyn fi1 id uqt1 t2

{-# LINE 1326 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
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
                                  fo  = fVar fTySyn fi2 tr1 tr2
                       -}
                       fP fi tpr1@(Ty_Pred _)              tpr2@(Ty_Pred _)
                            =  if foHasErrs pfo
                               then Nothing
                               else Just  ( foUpdTy (updTy $ [foTy pfo] `mkArrow` foTy fo)
                                          $ foUpdLRCoe (mkIdLRCoeWith n CMetaVal_Dict)
                                          $ fo)
                            where  pfo   = fVar' fBase (fi {fiFIOpts = predFIOpts}) id tpr2 tpr1
                                   n     = uidHNm u2
                                   fo    = fVar' fTySyn (fofi pfo fi) id tr1 tr2
                       fP fi tpr1@(Ty_Pred pr1)            (Ty_Impls (Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv2
                                                      (Impls_Cons iv2 pr1 (mkPrIdCHR u2) range ipo2 im2)
                                                      tpr1
                                                      (mkIdLRCoeWith n CMetaVal_Dict)
                                                      fo)
                            where  im2   = Impls_Tail u1 ipo2
                                   n     = uidHNm u2
                                   fo    = fVar' fTySyn fi updTy tr1 ([Ty_Impls im2] `mkArrow` tr2)
                       fP fi (Ty_Impls (Impls_Tail iv1 ipo1)) tpr2@(Ty_Pred pr2)
                            =  Just (foUpdImplExplCoe iv1
                                                      (Impls_Cons iv1 pr2 (mkPrIdCHR u2) range ipo1 im1)
                                                      tpr2
                                                      (mkIdLRCoeWith n CMetaVal_Dict)
                                                      fo)
                            where  im1   = Impls_Tail u1 ipo1
                                   n     = uidHNm u2
                                   fo    = fVar' fTySyn fi updTy ([Ty_Impls im1] `mkArrow` tr1) tr2
                       fP fi (Ty_Impls (Impls_Tail iv1 _)) tpr2@(Ty_Impls im2@(Impls_Nil))
                            =  Just (foUpdImplExpl iv1 im2 tpr2 (fVar' fTySyn fi id tr1 tr2))
                       fP fi (Ty_Impls (Impls_Nil))   tpr2@(Ty_Impls im2@(Impls_Tail iv2 _))
                            =  Just (foUpdImplExpl iv2 Impls_Nil (Ty_Impls Impls_Nil) (fVar' fTySyn fi id tr1 tr2))
                       fP fi tpr1@(Ty_Impls (Impls_Tail iv1 _)) (Ty_Impls im2@(Impls_Tail iv2 _)) | iv1 == iv2
                            =  Just (res fi tpr1)
                       fP fi (Ty_Impls (Impls_Tail iv1 ipo1)) (Ty_Impls im2@(Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv1 im2' (Ty_Impls im2')
                                                      (mkLRCoe (Coe_ImplApp iv2) (Coe_ImplLam iv2))
                                                      (fVar' fTySyn fi updTy tr1 tr2))
                            where im2' = Impls_Tail iv2 ({- [ipo] ++ -} ipo1 ++ ipo2)
                                  -- ipo  = mkImplsProveOcc u1 (fePredScope (fiEnv fi))
                       fP fi (Ty_Impls Impls_Nil)          (Ty_Impls Impls_Nil)
                            =  Just (fVar' fTySyn fi updTy tr1 tr2)
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

{-# LINE 1425 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1
                           t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  -- decompose
                       -- the work
                       (u',u1)          = mkNewLevUID (fiUniq fi)
                       mbfp             = fVarPred1 fP (fi {fiUniq = u'}) tpr2
                       mkPrTy pr2 fo    = [Ty_Pred ({- foVarMp fo `varUpd` -} pr2)] `mkArrow` foTy fo
                       fSub fi updTy pr2v pr2 tr2
                            =  let  pr2n  = poiHNm pr2v
                                    (fi3,cnstrMp)
                                          = fiAddPr pr2n pr2v tpr2 fi
                                    fo    = fVar' fTySyn fi3 updTy t1 tr2
                                    rCoe  = acoreCoeLamLet pr2n (poiId pr2v)
                               in   ( foUpdCnstrMp cnstrMp fo
                                    , rCoe
                                    )
                       {-
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just fo
                            where fo = fVar' fTySyn fi updTy t1 tr2
                       -}
                       fP fi (Ty_Impls (Impls_Tail iv2 _))
                            =  Just (foUpdVarMp (iv2 `varmpImplsUnit` Impls_Nil) fo)
                            where fo = fVar' fTySyn fi updTy t1 tr2
                       fP fi (Ty_Impls (Impls_Cons _ pr2 pv2 _ _ im2))
                            =  Just ( foUpdTy (updTy $ mkPrTy pr2 fo)
                                    $ foUpdLRCoe (lrcoeRSingleton rCoe)
                                    $ fo )
                            where ( fo
                                   , rCoe
                                   )
                                    = fSub fi id pv2 pr2 ([Ty_Impls im2] `mkArrow` tr2)
                       fP fi (Ty_Pred pr2)  | fioAllowRPredElim (fiFIOpts fi)
                            =  Just ( foUpdTy (updTy $ mkPrTy pr2 fo)
                                    $ foUpdLRCoe (lrcoeRSingleton rCoe)
                                    $ fo )
                            where ( fo
                                   , rCoe
                                   )
                                    = fSub fi id (mkPrIdCHR u1) pr2 tr2
                       fP fi _ =  Nothing

{-# LINE 1509 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                           t2
                    | hsnIsArrow c1 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  -- decompose
                       -- the work
                       (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfPredScope     = fePredScope (fiEnv fi)
                       mbfp             = fVarPred1 fP (fi {fiUniq = u'}) tpr1
                       fSub fi updTy pv1 psc1 pr1 tr1
                            =  let  fo    = fVar' fTySyn fi updTy tr1 t2
                                    fs    = foVarMp fo
                                    prfPrL= [rngLift range mkPredOccRng pr1 pv1 psc1]
                                    coe   = acoreCoeApp1 (acoreNmHolePred pv1)
                               in   ( fo
                                    , coe
                                    , gathPredLToProveCnstrMp prfPrL
                                    )
                       {-
                       fP fi (Ty_Impls (Impls_Nil))
                            =  Just (fVar' fTySyn fi updTy tr1 t2)
                       -}
                       fP fi (Ty_Impls (Impls_Tail iv1 _))
                            =  Just (foUpdVarMp (iv1 `varmpImplsUnit` Impls_Nil) (fVar' fTySyn fi updTy tr1 t2))
                       fP fi (Ty_Impls (Impls_Cons _ pr1 pv1 _ _ im1))
                            =  Just ( foUpdPrL [] cnstrMp
                                    $ foUpdLRCoe (lrcoeLSingleton lCoe)
                                    $ fo )
                            where ( fo
                                   , lCoe
                                   , cnstrMp ) = fSub fi updTy pv1 prfPredScope pr1 ([Ty_Impls im1] `mkArrow` tr1)
                       fP fi (Ty_Pred pr1)
                            =  Just ( foUpdPrL [] cnstrMp
                                    $ foUpdLRCoe (lrcoeLSingleton lCoe)
                                    $ fo )
                            where ( fo
                                   , lCoe
                                   , cnstrMp ) = fSub fi updTy (mkPrIdCHR u1) prfPredScope pr1 tr1
                       fP fi _ =  Nothing

{-# LINE 1592 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_App (Ty_Con n1) tr1)
                           t2@(Ty_App (Ty_Con n2) tr2)
                | n1 == n2 && (isRec || isSum)
                = foUpdTy (updTy $ n1 `mkConApp` [foTy fo]) fo
                where  -- decompose
                       -- the work
                       isRec = hsnIsRec n1
                       isSum = hsnIsSum n1
                       fo = fRow fi tr1 tr2 isRec isSum

{-# LINE 1617 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1                    t2
                | isJust mbVarBind					= fromJust mbVarBind
                where  mbVarBind = varBind3 fi updTy t1 t2

{-# LINE 1623 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_App tf1 ta1)
                           t2@(Ty_App tf2 ta2)
                = manyFO [ ffo, afo
                         , dtfo "app" fi t1 t2 [(ffo,"l"),(afo,"r")] emptyVarMp
                           $ foUpdTy (updTy $ foTy rfo) rfo
                         ]
                where  -- decompose
                       -- the work
                       fi2    = fi
                       ffo    = fVar' fTySyn fi2 id tf1 tf2
                       spine  = asgiSpine $ foAppSpineInfo ffo
                       (as,_) = hdAndTl' unknownAppSpineVertebraeInfo spine
                       pol    = asPolarity as
                       fi3    = fi2
                       fi4    = -- (\x -> Debug.tr "fBase.fi4" ((pp $ show $ fioDontBind $ fiFIOpts fi) >-< (pp $ show $ foDontBind ffo) >-< (pp $ show $ fioDontBind $ fiFIOpts x) ) x) $
                                -- (fofi ffo $ fiUpdRankByPolarity pol $ fiSwapCoCo fi3) {fiFIOpts = asFIO as $ fioSwapPolarity pol $ fiFIOpts fi}
                                (fofi ffo $ fiUpdRankByPolarity pol $ fiSwapCoCo (fi3 {fiFIOpts = asFIO as $ fioSwapPolarity pol $ fiFIOpts fi}))
                       afo    = fVar' fTySyn fi4 id ta1 ta2
                       rfo    = case (foMbAppSpineInfo ffo,asMbFOUpdCoe as) of
                                  (Nothing,_) | hasSubCoerce
                                    -> errCoerce
                                  (Just _,Nothing) | hasSubCoerce
                                    -> errCoerce
                                  _ -> asFOUpdCoe as globOpts [ffo, asFO as ffo $ foCmbApp ffo afo]
                              where errCoerce = err fi4 [rngLift range Err_NoCoerceDerivation (foVarMp afo `varUpd` foTy ffo) (foVarMp afo `varUpd` foTy afo)]
                                    hasSubCoerce = not $ lrcoeIsId $ foLRCoe afo

{-# LINE 1679 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1@(Ty_Ext _ _ _)   t2@(Ty_Ext _ _ _)
                =  fRow fi t1 t2 False False

{-# LINE 1699 "src/ehc/Ty/FitsIn.chs" #-}
            -- N.B. hsnInvariant is a unique name which cannot be written by a programmer. In other words,
            -- this pattern match cannot trigger during other type inferences.
            -- Weaken Co/Contravariant polarity to Invariant polarity
            fBase fi updTy t1@(Ty_Con _) t2@(Ty_Con s2)
                | s2 == hsnInvariant                = res fi t2
            -- Invariance propagates through Negate. A bit tricky because this bit of evaluation means unclarity what the return value is
            fBase fi updTy t1@(Ty_Con s1) t2@(Ty_App (Ty_Con sf2) ta2)
                | s1 == hsnInvariant && sf2 == hsnPolNegation
                                                    = fVar' fBase fi id t1 ta2

{-# LINE 1711 "src/ehc/Ty/FitsIn.chs" #-}
            fBase fi updTy t1                     t2          = errClash fi t1 t2

{-# LINE 1715 "src/ehc/Ty/FitsIn.chs" #-}
            foRes  = fVar' fTySyn fi id ty1 ty2

{-# LINE 1719 "src/ehc/Ty/FitsIn.chs" #-}
fitsIn' :: String -> FIOpts -> FIEnv -> UID -> VarMp -> Ty -> Ty -> FIOut
fitsIn' msg opts env uniq varmp ty1 ty2
  =  fitsIn opts (trPP (msg ++ "-env") env) (trPP (msg ++ "-uniq") uniq) varmp (trPP (msg ++ "-ty1") ty1) (trPP (msg ++ "-ty2") ty2)

{-# LINE 1729 "src/ehc/Ty/FitsIn.chs" #-}
fitsInL' :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> TyL -> ([FIOut],FIOut)
fitsInL' opts env uniq varmp tyl1 tyl2
  = fitsInLWith (\fo1 fo2 -> fo2 {foVarMp = foVarMp fo1 |+> foVarMp fo2, foErrL = foErrL fo1 ++ foErrL fo2})
                (mkFitsInWrap' env) opts uniq varmp tyl1 tyl2

fitsInL :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> TyL -> (TyL,FIOut)
fitsInL opts env uniq varmp tyl1 tyl2
  = (map foTy foL,fo)
  where (foL,fo) = fitsInL' opts env uniq varmp tyl1 tyl2

{-# LINE 1745 "src/ehc/Ty/FitsIn.chs" #-}
fitsInFold :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> FIOut
fitsInFold opts env uniq varmp tyl
  = foldl (\fo t -> if foHasErrs fo then fo else fitsIn opts env uniq varmp (foTy fo) t)
          emptyFO tyl

{-# LINE 1756 "src/ehc/Ty/FitsIn.chs" #-}
fitPredIntoPred
  :: ( VarLookupCmb VarMp gm
     , VarLookup gm TyVarId VarMpInfo
     )
     => FIIn' gm -> Pred -> Pred
     -> Maybe (Pred,VarMp)
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
        f (Pred_Lacks r1@(Ty_Var rv1 _) l1) pr2
          | fiAllowTyVarBind fi r1 && isJust mbTy
          = f (Pred_Lacks (fromJust mbTy) l1) pr2
          where mbTy = varmpTyLookup rv1 (fiVarMp fi)
        f (Pred_Lacks t1 (Label_Var lv1)) pr2 | isJust mbLb
          = f (Pred_Lacks t1 (fromJust mbLb)) pr2
          where mbLb = varmpLabelLookup lv1 (fiVarMp fi)
        f (Pred_Lacks ty1@(Ty_Var rv1 _)    (Label_Var lv1))
          (Pred_Lacks ty2                l2@(Label_Lab lb2))
          | fiAllowTyVarBind fi ty1
          = Just (Pred_Lacks ty2 l2, (rv1 `varmpTyUnit` ty2) `varUpd` (lv1 `varmpLabelUnit` l2))
        f (Pred_Lacks ty1                              (Label_Var lv1))
          (Pred_Lacks ty2                           l2@(Label_Lab lb2))
          | tyIsEmptyRow ty1 && tyIsEmptyRow ty2
          = Just (Pred_Lacks ty2 l2, lv1 `varmpLabelUnit` l2)
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
                   (ps', s2) <- fPreds (s1 `varUpd` ps1) (s1 `varUpd` ps2)
                   return (PredSeq_Cons pr' ps', s2 |+> s1)
            fPreds PredSeq_Nil PredSeq_Nil
              = Just (PredSeq_Nil, emptyVarMp)
            fPreds _ _
              = Nothing
        f pr1                   pr2
          = if foHasErrs fo
            then {- tr "fitPredIntoPred"
                            (pr1 >-< pr2 >-< ppErrL (foErrL fo)
                             >-< "fiVarMp" >#< fiVarMp fi >-< "foVarMp" >#< foVarMp fo
                             >-< "fioDontBind" >#< show (fioDontBind (fiFIOpts fi))
                             >-< "fioBindLVars" >#< show (fioBindLVars (fiFIOpts fi)) >-< "fioBindRVars" >#< show (fioBindRVars (fiFIOpts fi))
                             >-< "foTrace" >#< vlist (foTrace fo)
                            ) $ -}
                 Nothing
            else Just (tyPred $ foTy fo,foVarMp fo)
          where fo = fitsIn (predFIOpts
                               { fioBindRVars = FIOBindNoBut Set.empty
                               , fioDontBind = fioDontBind (fiFIOpts fi)
                               , fioBindCategs = fioBindCategs (fiFIOpts fi)
                               })
                            (fiEnv fi) (fiUniq fi) (fiVarMp fi)
                            (Ty_Pred pr1) (Ty_Pred pr2)

{-# LINE 1860 "src/ehc/Ty/FitsIn.chs" #-}
fitPredToEvid' :: UID -> VarMp -> Ty -> Either ClGamInfo ClGam -> FIOut
fitPredToEvid' u varmp prTy gg
  =  case prTy of
       Ty_Any  ->  emptyFO
       _       ->  fPr u prTy
  where  fPr u prTy
            =  case tyUnAnn prTy of -- TBD: necessary?
                 Ty_Pred p@(Pred_Class _)
                    ->  case gg of
                          Left clgi -> fClgi u clgi prTy
                          Right g   -> maybe err (\clgi -> fClgi u clgi prTy) $ gamLookup (fst $ predMatchNmArgs p) g
                                    where err = emptyFO {foErrL = [rngLift emptyRange mkErr_NamesNotIntrod "class" [fst $ tyPredMatchNmArgs prTy]]}
                    where fClgi u clgi prTy
                            = fo {foTy = snd (tyArrowArgRes (foTy fo))}
                            where (u',u1,u2) = mkNewLevUID2 u
                                  fo = fitsIn (predFIOpts {fioBindRVars = FIOBindNoBut $ Set.singleton u2}) emptyFE u1 varmp (clgiPrToEvidTy clgi) ([prTy] `mkArrow` mkTyVar u2)
                 Ty_Pred (Pred_Pred t)
                    ->  let  (aL,r) = tyArrowArgsRes t
                             (_,aLr'@(r':aL')) = foldr (\t (u,ar) -> let (u',u1) = mkNewLevUID u in (u',fPr u1 t : ar)) (u,[]) (r : aL)
                        in   manyFO (aLr' ++ [emptyFO {foTy = map foTy aL' `mkArrow` foTy r'}])

{-# LINE 1883 "src/ehc/Ty/FitsIn.chs" #-}
fitPredToEvid :: UID -> VarMp -> Ty -> ClGam -> FIOut
fitPredToEvid u varmp prTy g = fitPredToEvid' u varmp prTy (Right g)

{-# LINE 1892 "src/ehc/Ty/FitsIn.chs" #-}
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

{-# LINE 1910 "src/ehc/Ty/FitsIn.chs" #-}
-- fitsInForToTyCore :: C.KiFitsIn
fitsInForToTyCore uniq t1 t2
  = foLInstToL fo
  where fo = fitsIn (strongFIOpts {fioExpandEqTyVar=True}) emptyFE uniq (emptyVarMp :: VarMp) t1 t2

