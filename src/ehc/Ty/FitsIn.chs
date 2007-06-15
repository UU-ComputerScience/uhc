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

%%[1 module {%{EH}Ty.FitsIn} import({%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Ty.FitsInCommon}, {%{EH}Ty}, {%{EH}Error}) export (fitsIn)
%%]

%%[2 import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[4 import({%{EH}Ty.Trf.Instantiate}, {%{EH}Base.Opts}, {%{EH}Gam}, Data.Maybe,Data.List as List)
%%]

%%[4 export(FIEnv(..),emptyFE)
%%]

%%[4 import({%{EH}Base.Debug})
%%]

%%[4_2 import({%{EH}Ty.Trf.ElimAlts}) export(mkFitsInWrap,mkFitsInWrap')
%%]

%%[4_2 import({%{EH}Ty.Trf.ElimBoth}) export(foHasErrs)
%%]

%%[6 export(fitsInL)
%%]

%%[9 import(EH.Util.Utils)
%%]

%%[9 import(qualified Data.Map as Map,qualified Data.Set as Set,EH.Util.Pretty,{%{EH}Core.Pretty},{%{EH}Pred},{%{EH}Core},{%{EH}Core.Subst})
%%]

%%[9 import({%{EH}Pred.CommonCHR})
%%]

%%[9 export(fitsIn')
%%]

%%[10 import({%{EH}Core.Utils})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(foAppLRCoe)
foAppLRCoe :: EHCOpts -> FIOut -> VarMp -> CSubst -> CExpr -> CExpr
foAppLRCoe opts fo c cs ce = foAppLRCoe' opts (foCSubst fo,foLRCoe fo) c cs ce
%%]

-- for use by Ruler
%%[9 export(foAppLRCoe')
foAppLRCoe' :: EHCOpts -> (CSubst,LRCoe) -> VarMp -> CSubst -> CExpr -> CExpr
foAppLRCoe' opts (fCS,fLRCoe) c cs ce
  =  let  s = cs `cSubstApp` fCS
     in   cSubstApp s (lrcoeWipeWeave opts c s fLRCoe `coeEvalOn` ce)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIIn export(FIIn(..))
data FIIn   =  FIIn     {  fiFIOpts          :: !FIOpts
                        ,  fiUniq            :: !UID
                        ,  fiVarMp           :: !VarMp
%%[[9
                        ,  fiEnv             :: !FIEnv
%%]]
                        }
%%]

%%[4.FIn.emptyFI export(emptyFI)
emptyFI     =  FIIn     {  fiFIOpts          =   strongFIOpts
                        ,  fiUniq            =   uidStart
                        ,  fiVarMp           =   emptyVarMp
%%[[9
                        ,  fiEnv             =   emptyFE
%%]]
                        }
%%]

%%[9
instance Show FIIn where
  show _ = "FIIn"

instance PP FIIn where
  pp fi = "FIIn:" >#< pp (fiEnv fi)
%%]

%%[4.fiUpdOpts
fiUpdOpts :: (FIOpts -> FIOpts) -> FIIn -> FIIn
fiUpdOpts upd fi = fi {fiFIOpts = upd (fiFIOpts fi)}
%%]

%%[4.FIEnv
data FIEnv
  =   FIEnv
%%[[9
        {   feEHCOpts       :: !EHCOpts
        ,   feDontBind      :: !TyVarIdS
        ,   fePredScope     :: !PredScope
%%[[11
        ,   feTyGam         :: !TyGam
%%]]
%%[[99
        ,   feRange         :: !Range
%%]]
        }
%%]]

emptyFE
  =   FIEnv
%%[[9
        {   feEHCOpts       =   defaultEHCOpts
        ,   feDontBind      =   Set.empty
        ,   fePredScope     =   initPredScope
%%[[11
        ,   feTyGam         =   emptyGam
%%]]
%%[[99
        ,   feRange         =   emptyRange
%%]]
        }
%%]]

instance Show FIEnv where
  show _ = "FIEnv"
%%]

%%[9
instance PP FIEnv where
  pp e = "FIEnv"
         >#< (empty
%%[[11
             >-< pp (feTyGam e)
%%]]
             )
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

%%[1.fitsIn.Base
%%@fitsInHead.1
%%@fitsInBotCon.1
%%]

%%[1.fitsIn.AppRest
%%@fitsInApp.1
%%@fitsInRest.1
%%@fitsInapp.1
%%]

%%[2.fitsIn.Base -(1.fitsIn.Base 1.fitsIn.AppRest)
%%@fitsInHead.1
%%]

%%[2.fitsIn.Bind
%%@fitsInBind.2
%%]

%%[2.fitsIn.app
%%@fitsInapp.2
%%]

%%[2.fitsIn.BotCon
%%@fitsInBotCon.1
%%]

%%[2.fitsIn.Var
            f  t1@(Ty_Var v1)       (Ty_Var v2)
                 | v1 == v2                         = res t1
            f  t1@(Ty_Var v1)       t2              = occurBind v1 t2
            f  t1                   t2@(Ty_Var v2)  = occurBind v2 t1
%%]

%%[2.fitsIn.AppRest
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

%%[3.fitsIn -(2.fitsIn.Base 2.fitsIn.Bind 2.fitsIn.app 2.fitsIn.BotCon 2.fitsIn.Var 2.fitsIn.AppRest)
%%@fitsInHead.1
%%@fitsInBind.2
%%@fitsInapp.2
%%@fitsInBotCon.1
%%@fitsInVar.3
%%@fitsInApp.1
%%@fitsInRest.1
%%]

%%[4.fitsIn.Prelim -3.fitsIn
manyFO :: [FIOut] -> FIOut
manyFO = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)

fitsIn :: FIOpts -> FIEnv -> UID -> VarMp -> Ty -> Ty -> FIOut
fitsIn opts env uniq varmp
  =  fitsInFI (emptyFI  { fiUniq = uniq, fiFIOpts = opts, fiVarMp = varmp
%%[[9
                        , fiEnv = env
%%]]
                        }
              )

fitsInFI :: FIIn -> Ty -> Ty -> FIOut
fitsInFI fi ty1 ty2
  =  fo
  where
%%[[9
            globOpts                =  feEHCOpts $ fiEnv fi
%%]]
%%[[1
            range                   =  emptyRange
%%][99
            range                   =  feRange $ fiEnv fi
%%]]
            res fi t                =  emptyFO  { foUniq = fiUniq fi, foTy = t
                                                , foMbAppSpineInfo = asGamLookup (tyConNm t) appSpineGam}
            err    e                =  emptyFO {foUniq = fioUniq (fiFIOpts fi), foErrL = e}
            errClash fi t1 t2       =  err [rngLift range Err_UnifyClash ty1 ty2 (fioMode (fiFIOpts fi)) t1 t2 (fioMode (fiFIOpts fi))]
            occurBind fi v t
                | v `elem` ftv t    =  err [rngLift range Err_UnifyOccurs ty1 ty2 (fioMode (fiFIOpts fi)) v t (fioMode (fiFIOpts fi))]
                | otherwise         =  bind fi v t
%%]

%%[4.fitsIn.bind
            bind fi tv t            =  (res fi t) {foVarMp = tv `varmpTyUnit` t}
%%]

%%[4_2.fitsIn.bind
            occurBindAlt fi isR tv t=  occurBind fi tv (mkTyAlts fi isR tv t)
%%]
            occurBindAlt fi isR tv t=  case t of
                                         Ty_Quant q _ _ | tyquIsForall q
                                            -> occurBind fi tv t
                                         _  -> occurBind fi tv (mkTyAlts fi isR tv t)

            f fi t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
                |  q1 == q2
                   &&  (   fioMode (fiFIOpts fi) == FitMeet && tyquIsExists q1
                       ||  fioMode (fiFIOpts fi) == FitJoin && tyquIsForall q1
                       )                            = manyFO [fo,rfo]

%%[9_1.fitsIn.bind -4_1.fitsIn.bind
            occurBindAlt fi isR tv t
                                    =  occurBind (fi {fiUniq = u'}) tv (mkTyAlts fi tv t u)
                                       where (u',u)  = mkNewUID (fiUniq fi)
%%]

%%[4_2.fitsIn.mkTyPlus
            mkTyPlusHard fi isR v t =  (fi,TyPlus_Ty t h o)
                                       where h =  if fioIsMeetJoin (fiFIOpts fi)
                                                  then TyHard
                                                  else TySoft v
                                             o =  if fioIsSubsume (fiFIOpts fi)
                                                  then (if isR then TyRequired else TyOffered)
                                                  else if fioIsMeetJoin (fiFIOpts fi)
                                                  then (if fioMode (fiFIOpts fi) == FitMeet then TyRequired else TyOffered)
                                                  else TyNoNeed
%%]

%%[9_1.fitsIn.mkTyPlus -4_1.fitsIn.mkTyPlus
            mkTyPlus fi t           =  (fi {fiUniq = u'},TyPlus_Ty t u)
                                       where (u',u)  = mkNewUID (fiUniq fi)
%%]

%%[4_2.fitsIn.mkTyAlts
            mkTyAlts fi isR tv t    =  if fioBindToTyAlts (fiFIOpts fi)
                                       then Ty_Alts tv [snd (mkTyPlusHard fi isR tv t)]
                                       else t
%%]

%%[9_1.fitsIn.mkTyAlts -4_1.fitsIn.mkTyAlts
            mkTyAlts fi tv t pl     =  if fioBindToTyAlts (fiFIOpts fi)
                                       then Ty_Alts tv [TyPlus_Ty t pl]
                                       else t
%%]

%%[4_2
            bindMany fi tvL t       =  (res fi t) {foVarMp = mkVarMp tvL t}
            mkVarMp tvL t           =  assocLToVarMp .  map (flip (,) t) $ tvL
            cmbTyAltsL t1L t2L      =  nub (q1 ++ q2) ++ nub (r1 ++ r2)
                                       where  p = partition (tyIsQu . tyPlusTy)
                                              (q1,r1) = p t1L
                                              (q2,r2) = p t2L
            foBindMany tvL t fo     =  foUpdVarMp (mkVarMp tvL t) . foUpdTy t $ fo
            foBind tv               =  foBindMany [tv]
%%]

%%[4.fitsIn.allowBind
            allowBind fi (Ty_Var v f)   =  f == TyVarCateg_Plain
%%]

%%[4_2.fitsIn.allowBind -4.fitsIn.allowBind
            allowBind fi (Ty_Var v f)   =  f == TyVarCateg_Plain && v `notElem` fioDontBind (fiFIOpts fi)
%%]

%%[9.fitsIn.allowBind -4.fitsIn.allowBind
            allowBind fi (Ty_Var v f)   =  f == TyVarCateg_Plain && v `notElem` fioDontBind (fiFIOpts fi)
%%]

%%[4.fitsIn.allowImpredTVBind
            allowImpredTVBindL fi t _
                = fioBindLFirst (fiFIOpts fi) && allowBind fi t
            allowImpredTVBindR fi t _
                = fioBindRFirst (fiFIOpts fi) && allowBind fi t
%%]

%%[90.fitsIn.allowImpredTVBind -4.fitsIn.allowImpredTVBind
            allowImpredTVBindL fi (Ty_Var _ f) t
                = fioBindLFirst (fiFIOpts fi) && f == TyVarCateg_Plain && not (tyIsImplsTail . fst . tyArrowArgRes $ t)
            allowImpredTVBindR fi (Ty_Var _ f) t
                = fioBindRFirst (fiFIOpts fi) && f == TyVarCateg_Plain && not (tyIsImplsTail . fst . tyArrowArgRes $ t)
%%]

%%[4.fitsIn.unquant
            unquant fi t hide howToInst
                =   (fi {fiUniq = u},uqt,back)
                where  (u,uq)         = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs)     = tyInst1Quants uq howToInst t
                       back           = if hide  then  \fo ->  let  s = varmpDel rtvs (foVarMp fo)
                                                               in   fo {foVarMp = s, foTy = s |=> t}
                                                 else  id
%%]
%%[4_2.fitsIn.unquant -4.fitsIn.unquant
            unquant fi t hide howToInst
                =   (fi2,uqt,back)
                where  (fi2,uqt,rtvs) = unquant' fi t howToInst
                       back           = if hide  then  \fo ->  let  s = varmpDel rtvs (foVarMp fo)
                                                               in   fo {foVarMp = s, foTy = s |=> t}
                                                 else  id
            unquant' fi t howToInst
                =   (fi {fiUniq = u},uqt,rtvs)
                where  (u,uq)         = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs)     = tyInst1Quants uq howToInst t
%%]

%%[4.fitsIn.FOUtils
            foUpdTy  t   fo  = fo {foTy = t}
            foUpdVarMp c fo  = fo {foVarMp = c |=> foVarMp fo}
%%]

%%[4_2.fitsIn.FOUtils
            foNoErr      fo  = fo {foErrL = []}
%%]

%%[4.fitsIn.foCmb
            foCmbAppTy   ffo afo  = afo {foTy = Ty_App (foVarMp afo |=> foTy ffo) (foTy afo)}
            foCmbVarMp   ffo afo  = afo {foVarMp = foVarMp afo |=> foVarMp ffo}
            foCmbCoCon   ffo afo  = afo {foMbAppSpineInfo = fmap asgiShift1SpinePos $ foMbAppSpineInfo ffo}
%%]

%%[9
            foCmbPrL     ffo afo  = afo {foPredOccL = foPredOccL afo ++ foPredOccL ffo, foGathCnstrMp = foGathCnstrMp afo `cnstrMpUnion` foGathCnstrMp ffo}
            foCmbCSubst  ffo afo  = afo {foCSubst = cSubstOptApp globOpts (foCSubst afo) (foCSubst ffo)}
%%]

%%[50.fitsIn.foCmb
            foCmbEqVarMp ffo afo  = afo {foEqVarMp = foEqVarMp afo |=> foEqVarMp ffo}
%%]

%%[4.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbCoCon ffo . foCmbVarMp ffo . foCmbAppTy ffo
%%]

%%[9.fitsIn.foCmbApp -4.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbPrfRes ffo . foCmbCoCon ffo . foCmbVarMp ffo . foCmbAppTy ffo
%%]

%%[50.fitsIn.foCmbApp -9.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbEqVarMp ffo . foCmbPrfRes ffo . foCmbCoCon ffo . foCmbVarMp ffo . foCmbAppTy ffo
%%]

%%[7.fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo afo  = afo
%%]

%%[9.fitsIn.foCmbPrfRes -7.fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo      = foCmbCSubst ffo . foCmbPrL ffo
%%]

%%[9
            fiAddPr n i prTy fi
                =  let  e                   = fiEnv fi
                        (_,assumePredScope) = pscpEnter 0 $ fePredScope (fiEnv fi)
                        pr                  = tyPred prTy
                   in   (fi { fiEnv = e {fePredScope = assumePredScope} },gathPredLToAssumeCnstrMp [mkPredOcc pr i assumePredScope])
            foUpdErrs e fo = fo {foErrL = e ++ foErrL fo}
            foUpdLRCoe lrcoe fo = fo {foLRCoe = lrcoe `lrcoeUnion` foLRCoe fo}
            foUpdCnstrMp m fo = fo {foGathCnstrMp = m `cnstrMpUnion` foGathCnstrMp fo}
            foUpdPrL prL prMp fo = foUpdCnstrMp prMp $ fo {foPredOccL = prL ++ foPredOccL fo}
            foUpdCSubst s fo = fo {foCSubst = cSubstOptApp globOpts s (foCSubst fo)}
            foUpdImplExpl iv im tpr fo
                            = foUpdVarMp (iv `varmpImplsUnit` (foVarMp fo |=> im))
                            $ foUpdTy ([foVarMp fo |=> tpr] `mkArrow` foTy fo)
                            $ fo
            foUpdImplExplCoe iv im tpr lrcoe fo
                            = foUpdImplExpl iv im tpr . foUpdLRCoe lrcoe $ fo
%%]

%%[4
            fPairWise' cCmb fi tL1 tL2
              =  foldr  (\(t1,t2) (foL,fii,c)
                           -> let  fo = ff (fii) (c |=> t1) (c |=> t2)
                              in   (fo:foL,fii {fiUniq = foUniq fo},fo `cCmb` c))
                        ([],fi,emptyVarMp)
                        (zip tL1 tL2)
            fPairWise = fPairWise' (\fo c -> foVarMp fo |=> c)
%%]

%%[50
            instCoConst = fioInstCoConst (fiFIOpts fi)
%%]

%%[7.fitsIn.fRow.Base
            fRow fi tr1 tr2 isRec isSum
                = foR
                where  (r1,exts1) = tyRowExts tr1
                       (r2,exts2) = tyRowExts tr2
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
                       bind fo v r e = manyFO [fo,foUpdTy (foTy fo `mkTyRow` e') . foUpdVarMp (v `varmpTyUnit` mkTyRow r' e') $ fo]
                         where  e' = foVarMp fo |=> e
                                r' = foVarMp fo |=> r
                       (u',u1)    = mkNewLevUID (fiUniq fi)
                       fi2        = fi {fiUniq = u'}
                       
                       fR fi r1 r2@(Ty_Var v2 f2) e1@(_:_) e12 e2
                         | f2 == TyVarCateg_Plain
                         = bind (fR fi2 r1 rv [] e12 e2) v2 rv e1
                         where  (fi2,rv) = mkTv fi
                       fR fi r1@(Ty_Var v1 f1) r2 e1 e12 e2@(_:_)
                         | f1 == TyVarCateg_Plain
                         = bind (fR fi2 rv r2 e1 e12 []) v1 rv e2
                         where  (fi2,rv) = mkTv fi
                       fR fi r1@(Ty_Con n1) _ _ _ e2@(_:_)
                         | n1 == hsnRowEmpty && isRec
                         = err [rngLift range Err_MissingRowLabels (assocLKeys e2) tr1]
                       fR fi _ r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isRec
                         =  if null (fioNoRLabElimFor (fiFIOpts fi) `List.intersect` assocLKeys e1)
                            then fR fi r2 r2 [] e12 e2
                            else err [rngLift range Err_TooManyRowLabels (assocLKeys e1) tr2]
                       fR fi r1@(Ty_Con n1) _ e1 e12 e2@(_:_)
                         | n1 == hsnRowEmpty && isSum
                         = fR fi r1 r1 e1 e12 []
                       fR fi r1 r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isSum
                         = err [rngLift range Err_MissingRowLabels (assocLKeys e1) tr2]
                       fR fi r1 r2 e1 e12@(_:_) e2
                         = foR
                         where (e1L,e2L) = unzip e12
                               (foL,fi2,fVarMp) = fPairWise (fiUpdOpts fioMkStrong fi) (assocLElts e1L) (assocLElts e2L)
                               eKeys = assocLKeys e1L
                               eL = zip eKeys (map ((fVarMp |=>) . foTy) foL)
                               fo = fR fi2 r1 r2 e1 [] e2
                               foR = manyFO ([fo] ++ foL ++ [foRes])
                               foRes = (\fo -> foldr foCmbPrfRes fo foL)
%%]
%%[10
                                       $ foUpdRecFldsCoe eKeys foL tr1
%%]
%%[7
                                       $ foUpdTy (foTy fo `mkTyRow` eL)
                                       $ foUpdVarMp fVarMp fo
%%]

%%[7.fitsIn.fRow.fRFinal
                       fR fi r1 r2 [] [] []
                         = f fi r1 r2
%%]

%%[10.fitsIn.fRow.fRFinal -7.fitsIn.fRow.fRFinal
                       fR fi r1@(Ty_Var _ cat) r2@(Ty_Con n2) [] [] []
                         | tvCatIsFixed cat && n2 == hsnRowEmpty && isRec
                         = res fi r2
                       fR fi r1 r2 [] [] []
                         = (f fi r1 r2) {foLRCoe = emptyLRCoe}
%%]

%%[7.fitsIn.fRow.Final1
                       fR fi _ _ _ _ _
                         = errClash fi tr1 tr2
%%]

%%[7.fitsIn.fRow.foR
                       foR        = fR fi2 r1 r2 extsIn1 extsIn12 extsIn2
%%]

%%[10.fitsIn.fRow.foR -7.fitsIn.fRow.foR
                       fo         = fR fi2 r1 r2 extsIn1 extsIn12 extsIn2
                       foR        = (if isRec then foUpdRecCoe (foVarMp fo |=> r1) (foVarMp fo |=> r2) extsIn1 extsIn12 extsIn2 else id) fo 
                       foUpdRecCoe r1 r2 e1 e12 e2 fo
                         =  let  rn = uidHNm u1
                                 predScope = fePredScope (fiEnv fi)
                                 r = CExpr_Var rn
                                 tr1s = foVarMp fo |=> tr1
                                 (u',u2,u3,u4) = mkNewLevUID3 (foUniq fo)
                                 mkLSel n u = mkCExprSelCase (emptyRCEEnv globOpts) (Just $ hsnSuffix rn "!") r CTagRec n n (mkCExprHole globOpts u) Nothing
                                 mkLPred' r l u
                                   =  let  r' = maybe Ty_Any fst $ tyRowExtr l r
                                      in   (mkPredOcc (Pred_Lacks r' (Label_Lab l)) (mkPrId basePrfCtxtId u) predScope,r')
                                 mkLPred r l u = fst (mkLPred' r l u)
                                 rowCoeL = [ rc | rc@(_,c) <- sortByOn rowLabCmp fst (foRowCoeL fo) {-, not (coeIsId c) -} ]
                                 (fuUpdL,prUpdL,tr1s',_)
                                   =  foldr  (\(l,c) (fuL,prL,r,u)
                                                ->  ( (l,(CExpr_TupUpd (cundefined globOpts) CTagRec l (mkCExprHole globOpts u) (c `coeEvalOn` mkLSel l u),Nothing)) : fuL
                                                    , mkLPred r l u : prL,r,uidNext u
                                                    )
                                             )
                                             ([],[],tr1s,u2) rowCoeL
                                 (fuDelL,prDelL,_,_)
                                   =  foldl  (\(fuL,prL,r,u) l
                                                  ->  let  (pr,r') = mkLPred' r l u
                                                      in   ( (l,(CExpr_TupDel (CExpr_Var hsnWild) CTagRec l (mkCExprHole globOpts u),Nothing)) : fuL
                                                           , pr:prL,r',uidNext u
                                                           )
                                             )
                                             ([],[],tr1s',u3) (sortBy rowLabCmp (assocLKeys e1))
                                 fuL = fuUpdL ++ reverse fuDelL
                                 (fBldL,prBldL,_,_)
                                   =  foldr  (\l (fL,prL,r,u)
                                                ->  ( (maybe id coeEvalOn (lookup l rowCoeL) $ mkLSel l u) : fL
                                                    , mkLPred r l u : prL,r,uidNext u
                                                    )
                                             )
                                             ([],[],tr1s,u3) (sortBy rowLabCmp ((assocLKeys . map fst $ e12) ++ assocLKeys e2))
                            in   case r2 of
                                   Ty_Con n2
                                     | n2 == hsnRowEmpty && null fuUpdL && null e2
                                     ->  fo  {  foLRCoe = emptyLRCoe }
                                     | n2 == hsnRowEmpty && not (null fBldL)
                                     ->  fo  {  foLRCoe = lrcoeLSingleton coe
                                             ,  foPredOccL = prBldL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp prBldL `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
                                             }
                                     where coe = Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (CExpr_Tup CTagRec `mkCExprApp` fBldL))
                                   Ty_Var _ cat
                                     | tvCatIsFixed cat && not (null fuL)
                                     ->  fo  {  foLRCoe = lrcoeLSingleton coe
                                             ,  foPredOccL = prUpdL ++ prDelL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp (prUpdL ++ prDelL) `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
                                             }
                                     where coe = Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (fuMkCExpr globOpts u4 fuL r))
                                   _ |  not (null fuUpdL)
                                     ->  fo  {  foLRCoe = lrcoeLSingleton coe
                                             ,  foPredOccL = prUpdL ++ foPredOccL fo
                                             ,  foGathCnstrMp = gathPredLToProveCnstrMp prUpdL `cnstrMpUnion` foGathCnstrMp fo
                                             ,  foUniq = u'
                                             }
                                     |  otherwise
                                     ->  fo  {  foLRCoe = emptyLRCoe }
                                     where coe = Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (fuMkCExpr globOpts u4 fuUpdL r))
%%]

%%[10.fitsIn.fRow.Coe
                       foUpdRecFldsCoe eKeys foL tr1 foR
                         =  let cL =   [  (l,c)
                                       |  (l,fo) <- zip eKeys foL
                                       ,  let c = lrcoeWipeWeave globOpts (foVarMp foR) (foCSubst foR) (foLRCoe fo)
                                       ,  not (coeIsId c)
                                       ]
                            in  foR {foRowCoeL = cL}
%%]

%%[4.fitsIn.ff
            ff fi t1 t2
              = f fi t1 t2
%%]
%%[11 -4.fitsIn.ff
            ff fi t1 t2
              = case filter (not . foHasErrs) tries of
                  (f:_) -> f
                  _     -> case (drop limit rt1, drop limit rt2) of
                             ((t:_),_) -> err [rngLift range Err_TyBetaRedLimit t1 t limit]
                             (_,(t:_)) -> err [rngLift range Err_TyBetaRedLimit t2 t limit]
                             _         -> head tries
              where limit = ehcOptTyBetaRedCutOffAt globOpts
                    reduc = tyBetaRed (feTyGam $ fiEnv fi)
                    rt1   = reduc t1
                    rt2   = reduc t2
                    tries = take (limit+1) $ try (t1 : rt1) (t2 : rt2)
                          where try (t1:ts1@(_:_)) (t2:ts2@(_:_)) = f fi t1 t2 : try ts1 ts2
                                try ts1@[t1]       (t2:ts2@(_:_)) = f fi t1 t2 : try ts1 ts2
                                try (t1:ts1@(_:_)) ts2@[t2]       = f fi t1 t2 : try ts1 ts2
                                try [t1]           [t2]           = [f fi t1 t2]
%%]

%%[4.fitsIn.Base
            f fi t1                     t2
                | fioMode (fiFIOpts fi) == FitSubRL = f  fi' t2 t1
                where  fi'       = fi  {fiFIOpts = fioSwapOpts . fioSwapCoCo ContraVariant . fiFIOpts $ fi}
            f fi Ty_Any                 t2          = res fi t2
            f fi t1                     Ty_Any      = res fi t1
            f fi t1@(Ty_Con s1)         t2@(Ty_Con s2)
                | s1 == s2                          = res fi t2
            f fi t1@(Ty_Var v1 f1)      t2@(Ty_Var v2 f2)
                | v1 == v2 && f1 == f2              = res fi t1
                | lBefR && allowBind fi t1          = bind fi v1 t2
                | not lBefR && allowBind fi t2      = bind fi v2 t1
                where lBefR = fioBindLBeforeR (fiFIOpts fi)
%%]

%%[4_2.fitsIn.Both
            f fi t1@(Ty_Both _ _)       t2@(Ty_Alts _ _)
                                                    = res fi t1
            f fi t1@(Ty_Alts _ _)       t2@(Ty_Both _ _)
                                                    = res fi t2
            f fi t1@(Ty_Both v1 [t1b])  t2@(Ty_Both v2 [t2b])
                                                    = manyFO [fo,foBindMany [v1,v2] (Ty_Both v2 [foTy fo]) fo]
                where  fo = f fi t1b t2b
            f fi t1@(Ty_Both v1 [])     t2@(Ty_Both v2 _)
                                                    = bind fi v1 t2
            f fi t1@(Ty_Both v1 _)      t2@(Ty_Both v2 [])
                                                    = bind fi v2 t1
            f fi t1@(Ty_Both v1 [])     t2          = bind fi v1 (Ty_Both v1 [t2])
            f fi t1@(Ty_Both v1 [t1b])  t2          = manyFO [fo,foBind v1 (Ty_Both v1 [foTy fo]) fo]
                where  fo = f fi t1b t2
            f fi t1     t2@(Ty_Both v2 [])          = bind fi v2 (Ty_Both v2 [t1])
            f fi t1     t2@(Ty_Both v2 [t2b])       = manyFO [fo,foBind v2 (Ty_Both v2 [foTy fo]) fo]
                where  fo = f fi t1 t2b
%%]

%%[50.fitsIn.EqualVar
            f fi t1@(Ty_Var v1 _) t2@(Ty_Equal v2 t2e)
                | fioAllowEqOpen (fiFIOpts fi)      = (bind fi v2 t2e) {foEqVarMp = ce}
                where  ce = v1 `varmpTyUnit` Ty_Equal v1 t2e
%%]

%%[4.fitsIn.Var1
            f fi t1@(Ty_Var v1 _)       t2
                | allowImpredTVBindL fi t1 t2       = occurBind fi v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowImpredTVBindR fi t2 t1       = occurBind fi v2 t1
%%]

%%[4_2.fitsIn.Var1Alts1 -4.fitsIn.Var1
            f fi t1@(Ty_Var v1 _)       t2@(Ty_Var v2 _)
                | allowBind fi t1                   = bind fi v1 t2
                | allowBind fi t2                   = bind fi v2 t1
            f fi t1@(Ty_Var v1 _)       t2@(Ty_Alts _ _)
                | allowBind fi t1                   = bind fi v1 t2
            f fi t1@(Ty_Alts _ _)       t2@(Ty_Var v2 _)
                | allowBind fi t2                   = bind fi v2 t1
            f fi t1@(Ty_Var v1 _)       t2
                | allowImpredTVBindL fi t1 t2       = occurBindAlt fi True v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowImpredTVBindR fi t2 t1       = occurBindAlt fi False v2 t1
%%]

%%[4_2.fitsIn.Alts
            f fi t1@(Ty_Alts v1 t1L)    t2@(Ty_Alts v2 t2L)
                                                    = bindMany fi [v1,v2] (Ty_Alts v1 (t1L `cmbTyAltsL` t2L))
            f fi t1@(Ty_Alts v1 t1L)    t2
                                                    = bind fipl v1 (Ty_Alts v1 (t1L `cmbTyAltsL` [t2pl]))
                where  (fipl,t2pl) = mkTyPlusHard fi True v1 t2
            f fi t1                     t2@(Ty_Alts v2 t2L)
                                                    = bind fipl v2 (Ty_Alts v2 ([t1pl] `cmbTyAltsL` t2L))
                where  (fipl,t1pl) = mkTyPlusHard fi False v2 t1
%%]

%%[50.fitsIn.Equal
            f fi t1@(Ty_Equal v1 t1e)   t2@(Ty_Equal v2 t2e)
                | v1 == v2                          = rfo
                where  fo = f fi t1e t2e
                       rfo = if foHasErrs fo
                             then res fi (Ty_Var v1 TyVarCateg_Fixed)
                             else foUpdTy (Ty_Equal v2 (foTy fo)) fo
            f fi t1@(Ty_Equal v1 t1e)   t2          = f fi t1e t2
            f fi t1                     t2@(Ty_Equal v2 t2e)
                                                    = manyFO [fo,rfo]
                where  fo = f fi t1 t2e
                       rfo = foUpdTy (Ty_Equal v2 (foTy fo)) fo
%%]
            f fi t1@(Ty_Equal v1 t1e)   t2@(Ty_Equal v2 t2e)
                | v1 /= v2                          = manyFO [fo,rfo]
                where  fo = f fi t1e t2e
                       rfo = foUpdTy (Ty_Equal v2 (foTy fo)) fo

%%[9
            f fi t1@(Ty_Pred pr1) t2@(Ty_Pred pr2)
                | fioPredAsTy (fiFIOpts fi) && isJust mbfp
                = let (fo,pr) = fromJust mbfp in foUpdTy (Ty_Pred pr) fo
                where  mbfp = fP pr1 pr2
                       fP (Pred_Class ct1)          (Pred_Class ct2)
                            = Just (fo,Pred_Class (foTy fo))
                            where fo = ff fi ct1 ct2
                       fP (Pred_Pred prt1)          (Pred_Pred prt2)
                            = Just (fo,Pred_Pred (foTy fo))
                            where fo = ff fi prt1 prt2
%%]
%%[10
                       fP (Pred_Lacks lt1 (Label_Lab l1))       (Pred_Lacks lt2 (Label_Lab l2))
                            | l1 == l2
                            = Just (fo,Pred_Lacks (foTy fo) (Label_Lab l1))
                            where fo = ff fi lt1 lt2
%%]
%%[9
                       fP _                         _
                            = Nothing
%%]

%%[4.fitsIn.QLR
            f fi t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
                | fioMode (fiFIOpts fi) == FitUnify && q1 == q2
                                                    = ff fi2 uqt1 uqt2
                where  (fi1,uqt1,_) = unquant fi   t1 False instCoConst
                       (fi2,uqt2,_) = unquant fi1  t2 False instCoConst
%%]

%%[4.fitsIn.QR
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoConst
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra
%%]

%%[6_4.fitsIn.QR -4.fitsIn.QR
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoExist
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (ff fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra
%%]

%%[4.fitsIn.QL
            f fi t1@(Ty_Quant _ _ _)    t2
                | fioIsSubsume (fiFIOpts fi)        = ff fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoConst
%%]

%%[6_4.fitsIn.QL -4.fitsIn.QL
            f fi t1@(Ty_Quant _ _ _)    t2
                | fioIsSubsume (fiFIOpts fi)        = ff fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoExist
%%]

%%[4_2.fitsIn.QL1
            f fi t1@(Ty_Quant q1 _ _)   t2
                | m == FitMeet || m == FitJoin      = manyFO [fo,fo2]
                where  m = fioMode (fiFIOpts fi)
                       (u',u1,u2) = mkNewLevUID2 (fiUniq fi)
                       (fi1,uqt1,rtvs1) = unquant' (fi {fiUniq = u'}) t1 instMeet
                       fo = ff fi1 uqt1 t2
                       (ebTy,ebVarMp) = tyElimBoth rtvs1 (foTy fo)
                       ebTy' =  if     m == FitMeet && tyquIsExists q1
                                   ||  m == FitJoin && tyquIsForall q1
                                then ebVarMp |=> ebTy
                                else ebTy
                       tvs = rtvs1 `List.intersect` ftv ebTy'
                       fo2 = foUpdTy (mkTyQu q1 tvs ebTy') . foUpdVarMp ebVarMp $ fo
%%]

%%[4_2.fitsIn.QR
            f fi t1                     t2@(Ty_Quant q2 _ _)
                | fioIsMeetJoin (fiFIOpts fi)       = ff (fi  {fiFIOpts = fioSwapOpts (fiFIOpts fi)})
                                                         t2 t1
%%]

%%[9
            f fi  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                  t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c1 && c1 == c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfPredScope     = fePredScope (fiEnv fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr1 tpr2
                       mberr            = Just (errClash fi t1 t2)
                       fP tpr1@(Ty_Pred _)              tpr2@(Ty_Pred _)
                            =  if foHasErrs pfo
                               then Nothing
                               else Just  ( foUpdTy ([foVarMp fo |=> foTy pfo] `mkArrow` foTy fo)
                                          $ foUpdLRCoe (mkIdLRCoe n)
                                          $ fo)
                            where  pfo   = f (fi2 {fiFIOpts = predFIOpts}) tpr2 tpr1
                                   n     = uidHNm u2
                                   fo    = ff (fi2 {fiUniq = foUniq pfo}) (foVarMp pfo |=> tr1) (foVarMp pfo |=> tr2)
                       fP tpr1@(Ty_Pred pr1)            (Ty_Impls (Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv2 (Impls_Cons iv2 pr1 (mkPrId basePrfCtxtId u2) ipo2 im2) tpr1 (mkIdLRCoe n) fo)
                            where  im2   = Impls_Tail u1 ipo2
                                   n     = uidHNm u2
                                   fo    = f fi2 tr1 ([Ty_Impls im2] `mkArrow` tr2)
                       fP (Ty_Impls (Impls_Tail iv1 ipo1)) tpr2@(Ty_Pred pr2)
                            =  Just (foUpdImplExplCoe iv1 (Impls_Cons iv1 pr2 (mkPrId basePrfCtxtId u2) ipo1 im1) tpr2 (mkIdLRCoe n) fo)
                            where  im1   = Impls_Tail u1 ipo1
                                   n     = uidHNm u2
                                   fo    = f fi2 ([Ty_Impls im1] `mkArrow` tr1) tr2
                       fP (Ty_Impls (Impls_Tail iv1 _)) tpr2@(Ty_Impls im2@(Impls_Nil))
                            =  Just (foUpdImplExpl iv1 im2 tpr2 (f fi2 tr1 tr2))
                       fP (Ty_Impls (Impls_Nil))   tpr2@(Ty_Impls im2@(Impls_Tail iv2 _))
                            =  Just (foUpdImplExpl iv2 Impls_Nil (Ty_Impls Impls_Nil) (f fi2 tr1 tr2))
                       fP (Ty_Impls (Impls_Tail iv1 ipo1)) (Ty_Impls im2@(Impls_Tail iv2 ipo2))
                            =  Just (foUpdImplExplCoe iv1 im2' (Ty_Impls im2') (mkLRCoe (CoeImplApp iv2) (CoeImplLam iv2)) (f fi2 tr1 tr2))
                            where im2' = Impls_Tail iv2 (ipo1 ++ ipo2)
                       fP (Ty_Impls Impls_Nil)          (Ty_Impls Impls_Nil)
                            =  Just (f fi2 tr1 tr2)
                       fP (Ty_Impls Impls_Nil)          (Ty_Impls _)
                            =  mberr
                       fP (Ty_Impls Impls_Nil)          (Ty_Pred _)
                            =  mberr
                       fP (Ty_Impls _)                  (Ty_Impls Impls_Nil)
                            =  mberr
                       fP (Ty_Pred _)                   (Ty_Impls Impls_Nil)
                            =  mberr
                       fP _                             _
                            =  Nothing
            f fi  t1
                  t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  (u',u1)          = mkNewLevUID (fiUniq fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr2
                       mkPrTy pr2 fo    = [Ty_Pred (foVarMp fo |=> pr2)] `mkArrow` foTy fo
                       fSub pr2v pr2 tr2
                            =  let  pr2n  = poiHNm pr2v
                                    (fi3,cnstrMp)
                                          = fiAddPr pr2n pr2v tpr2 fi2
                                    fo    = f fi3 t1 tr2
                                    rCoe  = mkLamLetCoe pr2n (poiId pr2v)
                               in   (foUpdCnstrMp cnstrMp fo,rCoe)
                       fP (Ty_Impls (Impls_Nil))
                            =  Just fo
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Tail iv2 _))
                            =  Just (foUpdVarMp (iv2 `varmpImplsUnit` Impls_Nil) fo)
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Cons _ pr2 pv2 _ im2))
                            =  Just (foUpdLRCoe (lrcoeRSingleton rCoe) $ foUpdTy (mkPrTy pr2 fo) $ fo)
                            where (fo,rCoe) = fSub pv2 pr2 ([Ty_Impls im2] `mkArrow` tr2)
                       fP (Ty_Pred pr2)  | fioAllowRPredElim (fiFIOpts fi)
                            =  Just (foUpdLRCoe (lrcoeRSingleton rCoe) $ foUpdTy (mkPrTy pr2 fo) $ fo)
                            where (fo,rCoe) = fSub (mkPrId basePrfCtxtId u1) pr2 tr2
                       fP _ =  Nothing
            f fi  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                  t2
                    | hsnIsArrow c1 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfPredScope     = fePredScope (fiEnv fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr1
                       fSub pv1 psc1 pr1 tr1
                            =  let  fo    = f fi2 tr1 t2
                                    fs    = foVarMp fo
                                    prfPrL= [mkPredOcc (fs |=> pr1) pv1 psc1]
                                    coe   = mkAppCoe [mkCExprPrHole globOpts pv1]
                               in   (fo,coe,gathPredLToProveCnstrMp prfPrL)
                       fP (Ty_Impls (Impls_Nil))
                            =  Just (f fi2 tr1 t2)
                       fP (Ty_Impls (Impls_Tail iv1 _))
                            =  Just (foUpdVarMp (iv1 `varmpImplsUnit` Impls_Nil) (f fi2 tr1 t2))
                       fP (Ty_Impls (Impls_Cons _ pr1 pv1 _ im1))
                            =  Just (foUpdPrL [] cnstrMp $ foUpdLRCoe (lrcoeLSingleton lCoe) $ fo)
                            where (fo,lCoe,cnstrMp) = fSub pv1 prfPredScope pr1 ([Ty_Impls im1] `mkArrow` tr1)
                       fP (Ty_Pred pr1)
                            =  Just (foUpdPrL [] cnstrMp $ foUpdLRCoe (lrcoeLSingleton lCoe) $ fo)
                            where (fo,lCoe,cnstrMp) = fSub (mkPrId basePrfCtxtId u1) prfPredScope pr1 tr1
                       fP _ =  Nothing
%%]

                       fP im2@(Ty_Impls (Impls_Nil))
                            =  Just (foUpdTy ([im2] `mkArrow` foTy fo) $ fo)
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Tail iv2 _))
                            =  Just (foUpdVarMp (iv2 `varmpImplsUnit` Impls_Nil) $ foUpdTy ([Ty_Impls (Impls_Nil)] `mkArrow` foTy fo) $ fo)
                            where fo = f fi2 t1 tr2

%%[7
            f fi  t1@(Ty_App (Ty_Con n1) tr1)
                  t2@(Ty_App (Ty_Con n2) tr2)
                | n1 == n2 && (isRec || isSum)
                = foUpdTy (n1 `mkConApp` [foTy fo]) fo
                where  isRec = hsnIsRec n1
                       isSum = hsnIsSum n1
                       fo = fRow fi tr1 tr2 isRec isSum
%%]

%%[4.fitsIn.Var2
            f fi t1@(Ty_Var v1 _)       t2
                | allowBind fi t1                   = occurBind fi v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowBind fi t2                   = occurBind fi v2 t1
%%]

%%[4_2.fitsIn.Var2 -4.fitsIn.Var2
            f fi t1@(Ty_Var v1 _)       t2
                | allowBind fi t1                   = occurBindAlt fi True v1 t2
            f fi t1                     t2@(Ty_Var v2 _)
                | allowBind fi t2                   = occurBindAlt fi False v2 t1
%%]

%%[4.fitsIn.App
            f fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,foCmbApp ffo afo]
                where  ffo  = f fi tf1 tf2
                       fs   = foVarMp ffo
                       (as:_) = asgiSpine $ foAppSpineInfo ffo
                       fi'  = fi  { fiFIOpts  = asFIO as $ fioSwapCoCo (asCoCo as) $ fiFIOpts $ fi
                                  , fiUniq    = foUniq ffo }
                       afo  = f fi' (fs |=> ta1) (fs |=> ta2)
%%]

%%[9.fitsIn.App -4.fitsIn.App
            f fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,rfo]
                where  ffo  = f fi tf1 tf2
                       fs   = foVarMp ffo
                       (as:_) = asgiSpine $ foAppSpineInfo ffo
                       fi'  = fi  { fiFIOpts  = asFIO as $ fioSwapCoCo (asCoCo as) $ fiFIOpts $ fi
                                  , fiUniq    = foUniq ffo }
                       afo  = ff fi' (fs |=> ta1) (fs |=> ta2)
                       rfo  = case foMbAppSpineInfo ffo of
                                Nothing | not $ lrcoeIsId $ foLRCoe afo
                                  -> err [rngLift range Err_NoCoerceDerivation (foTy ffo) (foTy afo)]
                                _ -> asFOUpdCoe as globOpts [ffo, foCmbApp ffo afo]
%%]

%%[7.fitsIn.Ext
            f fi t1@(Ty_Ext _ _ _)   t2@(Ty_Ext _ _ _)
                =  fRow fi t1 t2 False False
%%]

%%[4.fitsIn.DefaultCase
            f fi t1                     t2          = errClash fi t1 t2
%%]

%%[4.fitsIn.SetupAndResult
            fo  = ff fi ty1 ty2
%%]

%%[9
%%]
            f' msg fi t1 t2 = let fo = f (trPP ("FIT" ++ "-" ++ msg ++ "-" ++ "fi") fi) (m "t1" t1) (m "t2" t2)
                                  m mm x = trPP ("FIT" ++ "-" ++ msg ++ "-" ++ mm) x
                              in  tr ("FIT" ++ "-" ++ msg) (pp (foTy fo)) fo

%%[9
fitsIn' :: String -> FIOpts -> FIEnv -> UID -> VarMp -> Ty -> Ty -> FIOut
fitsIn' msg opts env uniq varmp ty1 ty2
  =  fitsIn opts (trPP (msg ++ "-env") env) (trPP (msg ++ "-uniq") uniq) varmp (trPP (msg ++ "-ty1") ty1) (trPP (msg ++ "-ty2") ty2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption for lists of types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
fitsInL :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> TyL -> (TyL,FIOut)
fitsInL opts env uniq varmp tyl1 tyl2
  = (map foTy foL,fo)
  where (fo,foL)
          = fitsInLWith (\fo1 fo2 -> fo2 {foVarMp = foVarMp fo1 |=> foVarMp fo2, foErrL = foErrL fo1 ++ foErrL fo2})
                        (mkFitsInWrap' env) opts uniq varmp tyl1 tyl2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iterative fitsIn, for now just a simple one (no constr prop, no new uniq, ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(fitsInFold)
fitsInFold :: FIOpts -> FIEnv -> UID -> VarMp -> TyL -> FIOut
fitsInFold opts env uniq varmp tyl
  = foldl (\fo t -> if foHasErrs fo then fo else fitsIn opts env uniq varmp (foTy fo) t)
          emptyFO tyl
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(fitPredIntoPred)
fitPredIntoPred :: FIIn -> Pred -> Pred -> Maybe (Pred,VarMp)
fitPredIntoPred fi pr1' pr2
  = f ({- fiVarMp fi |=> -} pr1') pr2
  where f (Pred_Var pv1)    	pr2@(Pred_Var pv2) | pv1 /= pv2     = Just (pr2,pv1 `varmpPredUnit` pr2)
                                                   | otherwise      = Just (pr2,emptyVarMp)
        f pr1               	(Pred_Var pv2)                      = Nothing
        f (Pred_Var pv1)    	pr2                                 = Just (pr2,pv1 `varmpPredUnit` pr2)
%%[[10
        f (Pred_Lacks (Ty_Var rv1 TyVarCateg_Plain)    (Label_Var lv1))
          (Pred_Lacks ty2                           l2@(Label_Lab lb2))
          = Just (Pred_Lacks ty2 l2, (rv1 `varmpTyUnit` ty2) |=> (lv1 `varmpLabelUnit` l2))
        f (Pred_Lacks ty1                              (Label_Var lv1))
          (Pred_Lacks ty2                           l2@(Label_Lab lb2))
          | tyIsEmptyRow ty1 && tyIsEmptyRow ty2
          = Just (Pred_Lacks ty2 l2, lv1 `varmpLabelUnit` l2)
%%]]
        f pr1               	pr2
          = if foHasErrs fo
            then Nothing
            else Just (tyPred $ foTy fo,foVarMp fo)
          where fo = fitsIn (predFIOpts {fioDontBind = ftv pr2 ++ fioDontBind (fiFIOpts fi)}) fe u (fiVarMp fi) (Ty_Pred pr1') (Ty_Pred pr2)
                fe = fiEnv fi
                u  = fiUniq fi
%%]
        f (Pred_RowSplit (Ty_Var rv1) (RowExts_Var ev1))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Retrieving evidence type for predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(fitPredToEvid)
fitPredToEvid :: UID -> VarMp -> Ty -> ClGam -> FIOut
fitPredToEvid u varmp prTy g
  =  case prTy of
       Ty_Any  ->  emptyFO
       _       ->  fPr u prTy
  where  fPr u prTy
            =  case prTy of
                 Ty_Pred p@(Pred_Class _)
                    ->  case gamLookup (predMatchNm p) g of
                           Just clgi
                             -> let (u',u1,u2) = mkNewLevUID2 u
                                    fo = fitsIn fOpts emptyFE u1 varmp (clgiPrToEvidTy clgi) ([prTy] `mkArrow` mkTyVar u2)
                                in  fo {foTy = snd (tyArrowArgRes (foTy fo))}
                           _ -> emptyFO {foErrL = [rngLift emptyRange mkErr_NamesNotIntrod "class" [tyPredMatchNm prTy]]}
                 Ty_Pred (Pred_Pred t)
                    ->  let  (aL,r) = tyArrowArgsRes t
                             (_,aLr'@(r':aL')) = foldr (\t (u,ar) -> let (u',u1) = mkNewLevUID u in (u',fPr u1 t : ar)) (u,[]) (r : aL)
                        in   manyFO (aLr' ++ [emptyFO {foTy = map foTy aL' `mkArrow` foTy r'}])
         fOpts = predFIOpts {fioDontBind = ftv prTy}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrapper for fitsIn (as a solution for module dependency cycle)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
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
%%% Beta reduction for type, only saturated applications are expanded
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[11
tyBetaRed1 :: TyGam -> Ty -> Maybe Ty
tyBetaRed1 tyGam tp
  = eval fun args
  where (fun,args) = tyAppFunArgs tp
        eval lam@(Ty_Lam fa b) args
          | lamLen <= argLen
              = Just (mkApp (subst |=> lamBody : drop lamLen args))
          | otherwise
              = Nothing
          where (lamArgs,lamBody) = tyLamArgsRes lam
                lamLen = length lamArgs
                argLen = length args
                subst  = assocLToVarMp (zip lamArgs args)
        eval (Ty_Con nm) aa
              = case tyGamLookup nm tyGam of
                  Just tgi -> case tgiTy tgi of
                                Ty_Con nm' | nm == nm' -> Nothing
                                f                      -> Just (mkApp (f:aa))
                  Nothing  -> Nothing
        eval _ _
              = Nothing

tyBetaRed :: TyGam -> Ty -> [Ty]
tyBetaRed tyGam = unfoldr (fmap (\t -> (t,t)) . tyBetaRed1 tyGam)
%%]

%%[11 export(tyBetaRedFull)
tyBetaRedFull :: FIIn -> Ty -> Ty
tyBetaRedFull fi ty
  = red ty
  where env = fiEnv fi
        lim     = ehcOptTyBetaRedCutOffAt $ feEHCOpts env
        redl ty = take lim $ tyBetaRed (feTyGam env) ty
        -- red  ty = reda $ choose ty $ redl ty
        red  ty = choose ty $ redl ty
        reda ty
            = if all null as' then ty else mk f (zipWith choose as as')
            where (f,as,mk) = tyAppFunArgs' ty
                  as' = map redl as
        choose a [] = a
        choose a as = last as
%%]
