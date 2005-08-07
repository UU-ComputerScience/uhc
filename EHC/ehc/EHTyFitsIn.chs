% $Id$

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

%%[1 import(EHCommon, EHTyFitsInCommon, EHTy, EHError) export (fitsIn)
%%]

%%[2 import(EHCnstr,EHSubstitutable)
%%]

%%[4 import(EHTyInstantiate, EHOpts, EHGam, Data.Maybe,Data.List as List)
%%]

%%[4 export(FIEnv(..),emptyFE)
%%]

%%[4 import(EHDebug)
%%]

%%[4_2 import(EHTyElimBoth) export(foHasErrs)
%%]

%%[4_2 import(EHTyElimAlts) export(mkElimAltsWrap)
%%]

%%[6 export(fitsInL)
%%]

%%[9 import(qualified Data.Map as Map,qualified Data.Set as Set,UU.Pretty,EHCorePretty,EHPred,EHCore,EHCoreSubst) export(foAppCoe,foAppCoe',fitPredToEvid)
%%]

%%[9 export(prfPreds,prfPredsDbg)
%%]

%%[9 export(fitsIn')
%%]

%%[10 import(EHCoreUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn opts related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}
%%]

%%[4_2
fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App spine dependent info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App spine Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
foAppCoe :: FIOut -> Cnstr -> CSubst -> CExpr -> CExpr
foAppCoe fo c cs ce = foAppCoe' (foCSubst fo,foLCoeL fo,foRCoeL fo) c cs ce
%%]

-- for use by Ruler
%%[9
foAppCoe' :: (CSubst,[Coe],[Coe]) -> Cnstr -> CSubst -> CExpr -> CExpr
foAppCoe' (fCS,fLC,fRC) c cs ce
  =  let  s = cs `cSubstApp` fCS
     in   cSubstApp s (coeWipeWeave c s fLC fRC `coeEvalOn` ce)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIIn.hd
data FIIn   =  FIIn     {  fiFIOpts          ::  FIOpts              ,  fiUniq            ::  UID
%%]
%%[9
                        ,  fiEnv             ::  FIEnv
%%]
%%[4.FIn.tl
                        }
%%]

%%[4.FIn.emptyFI.hd
emptyFI     =  FIIn     {  fiFIOpts          =   strongFIOpts        ,  fiUniq            =   uidStart
%%]
%%[9
                        ,  fiEnv             =   emptyFE
%%]
%%[4.FIn.emptyFI.tl
                        }
%%]

%%[9
instance Substitutable FIIn where
  s |=> fi  =  let  e = fiEnv fi
               in   fi {fiEnv = e { fePrElimTGam = s |=> fePrElimTGam e
                                  }}
  ftv _     =  []

instance Show FIIn where
  show _ = "FIIn"

instance PP FIIn where
  pp fi = "FIIn:" >#< pp (fiEnv fi)
%%]

%%[4.FIEnv
data FIEnv  =   FIEnv

emptyFE     =   FIEnv
%%]

%%[4.fiUpdOpts
fiUpdOpts :: (FIOpts -> FIOpts) -> FIIn -> FIIn
fiUpdOpts upd fi = fi {fiFIOpts = upd (fiFIOpts fi)}
%%]

%%[9.FIEnv -4.FIEnv
data FIEnv
  =   FIEnv   {   fePrElimTGam    ::  PrElimTGam          ,   feEHCOpts       ::  EHCOpts
              ,   fePrfCtxtId     ::  PrfCtxtId           ,   feDontBind      ::  TyVarIdL
              }

emptyFE
  =   FIEnv   {   fePrElimTGam    =   emptyTGam uidStart  ,   feEHCOpts       =   defaultEHCOpts
              ,   fePrfCtxtId     =   uidStart            ,   feDontBind      =   []
              }

instance Show FIEnv where
  show _ = "FIEnv"

instance PP FIEnv where
  pp e = pp (fePrElimTGam e)
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
            bind tv t                               = (res t) {foCnstr = tv `cnstrTyUnit` t}
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
                        fs   =   foCnstr ffo
                        afo  =   f (fs |=> ta1) (fs |=> ta2)
                        as   =   foCnstr afo
                        rt   =   mkComp (as |=> foTy ffo) (foTy afo)
                        rfo  =   emptyFO {foTy = rt, foCnstr = as |=> fs}
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

fitsIn :: FIOpts -> FIEnv -> UID -> Ty -> Ty -> FIOut
fitsIn opts env uniq ty1 ty2
  =  fo
  where
            res fi t                =  emptyFO  { foUniq = fiUniq fi, foTy = t
                                                , foAppSpineL = asGamLookup appSpineGam (tyConNm t)}
            err    e                =  emptyFO {foUniq = fioUniq opts, foErrL = e}
            errClash fi t1 t2       =  err [Err_UnifyClash ty1 ty2 (fioMode opts) t1 t2 (fioMode (fiFIOpts fi))]
            occurBind fi v t
                | v `elem` ftv t    =  err [Err_UnifyOccurs ty1 ty2 (fioMode opts) v t (fioMode (fiFIOpts fi))]
                | otherwise         =  bind fi v t
%%]

%%[4.fitsIn.bind
            bind fi tv t            =  (res fi t) {foCnstr = tv `cnstrTyUnit` t}
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
            mkTyPlusHard fi isR v t =  (fi,TyPlus_Ty t h)
                                       where h =  if fioIsMeetJoin (fiFIOpts fi) then TyHard
                                                  else if isR && fioIsSubsume (fiFIOpts fi) then TyUpBound v
                                                  else TySoft v
%%]
            mkTyPlus     fi t       =  (fi,TyPlus_Ty t TySoft)

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
            bindMany fi tvL t       =  (res fi t) {foCnstr = mkCnstr tvL t}
            mkCnstr tvL t           =  assocLToCnstr .  map (flip (,) t) $ tvL
            cmbTyAltsL t1L t2L      =  q1 ++ q2 ++ r1 ++ r2
                                       where  p = partition (tyIsQu . tyPlusTy)
                                              (q1,r1) = p t1L
                                              (q2,r2) = p t2L
            foBindMany tvL t fo     =  foUpdCnstr (mkCnstr tvL t) . foUpdTy t $ fo
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

%%[99.fitsIn.allowImpredTVBind -4.fitsIn.allowImpredTVBind
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
                       back           = if hide  then  \fo ->  let  s = cnstrDel rtvs (foCnstr fo)
                                                               in   fo {foCnstr = s, foTy = s |=> t}
                                                 else  id
%%]
%%[4_2.fitsIn.unquant -4.fitsIn.unquant
            unquant fi t hide howToInst
                =   (fi2,uqt,back)
                where  (fi2,uqt,rtvs) = unquant' fi t howToInst
                       back           = if hide  then  \fo ->  let  s = cnstrDel rtvs (foCnstr fo)
                                                               in   fo {foCnstr = s, foTy = s |=> t}
                                                 else  id
            unquant' fi t howToInst
                =   (fi {fiUniq = u},uqt,rtvs)
                where  (u,uq)         = mkNewLevUID (fiUniq fi)
                       (uqt,rtvs)     = tyInst1Quants uq howToInst t
%%]

%%[4.fitsIn.FOUtils
            foUpdTy  t   fo  = fo {foTy = t}
            foUpdCnstr c fo  = fo {foCnstr = c |=> foCnstr fo}
%%]

%%[4_2.fitsIn.FOUtils
            foNoErr      fo  = fo {foErrL = []}
%%]

%%[4.fitsIn.foCmb
            foCmbAppTy   ffo afo  = afo {foTy = Ty_App (foCnstr afo |=> foTy ffo) (foTy afo)}
            foCmbCnstr   ffo afo  = afo {foCnstr = foCnstr afo |=> foCnstr ffo}
            foCmbCoCon   ffo afo  = afo {foAppSpineL = tail (foAppSpineL ffo)}
%%]

%%[9
            foCmbPrL     ffo afo  = afo {foPredOccL = foPredOccL afo ++ foPredOccL ffo}
            foCmbCSubst  ffo afo  = afo {foCSubst = foCSubst afo `cSubstApp` foCSubst ffo}
%%]

%%[11.fitsIn.foCmb
            foCmbEqCnstr ffo afo  = afo {foEqCnstr = foEqCnstr afo |=> foEqCnstr ffo}
%%]

%%[4.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbCoCon ffo . foCmbCnstr ffo . foCmbAppTy ffo
%%]

%%[9.fitsIn.foCmbApp -4.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbPrfRes ffo . foCmbCoCon ffo . foCmbCnstr ffo . foCmbAppTy ffo
%%]

%%[11.fitsIn.foCmbApp -9.fitsIn.foCmbApp
            foCmbApp     ffo      = foCmbEqCnstr ffo . foCmbPrfRes ffo . foCmbCoCon ffo . foCmbCnstr ffo . foCmbAppTy ffo
%%]

%%[7.fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo afo  = afo
%%]

%%[9.fitsIn.foCmbPrfRes -7.fitsIn.foCmbPrfRes
            foCmbPrfRes  ffo      = foCmbCSubst ffo . foCmbPrL ffo
%%]

%%[9
            fiAddPr ci n i prTy fi
                =  let  e = fiEnv fi
                        tg = peTGamAddKnPr ci n i (tyPred prTy) (tgamPushNew (fePrfCtxtId e) ci (fePrElimTGam e))
                   in   fi { fiEnv = e {fePrElimTGam = tg, fePrfCtxtId = ci} }
            foUpdErrs e fo = fo {foErrL = e ++ foErrL fo}
            foUpdLCoe l fo = fo {foLCoeL = l : foLCoeL fo}
            foUpdRCoe r fo = fo {foRCoeL = r : foRCoeL fo}
            foUpdLRCoe l r = foUpdLCoe l . foUpdRCoe r
            foUpdPrL prL fo = fo {foPredOccL = prL ++ foPredOccL fo}
            foUpdCSubst s fo = fo {foCSubst = s `cSubstApp` foCSubst fo}
            foUpdImplExpl iv im tpr fo
                            = foUpdCnstr (iv `cnstrImplsUnit` (foCnstr fo |=> im))
                            . foUpdTy ([foCnstr fo |=> tpr] `mkArrow` foTy fo)
                            $ fo
            foUpdImplExplCoe iv im tpr lCoe rCoe fo
                            = foUpdImplExpl iv im tpr . foUpdLRCoe lCoe rCoe $ fo
%%]

%%[4
            fPairWise' cCmb fi tL1 tL2
              =  foldr  (\(t1,t2) (foL,fii,c)
                           -> let  fo = f (fii) (c |=> t1) (c |=> t2)
                              in   (fo:foL,fii {fiUniq = foUniq fo},fo `cCmb` c))
                        ([],fi,emptyCnstr)
                        (zip tL1 tL2)
            fPairWise = fPairWise' (\fo c -> foCnstr fo |=> c)
%%]

%%[11
            instCoConst = fioInstCoConst opts
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
                       bind fo v r e = manyFO [fo,foUpdTy (foTy fo `mkTyRow` e') . foUpdCnstr (v `cnstrTyUnit` mkTyRow r' e') $ fo]
                         where  e' = foCnstr fo |=> e
                                r' = foCnstr fo |=> r
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
                         = err [Err_MissingRowLabels (assocLKeys e2) tr1]
                       fR fi _ r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isRec
                         =  if null (fioNoRLabElimFor (fiFIOpts fi) `List.intersect` assocLKeys e1)
                            then fR fi r2 r2 [] e12 e2
                            else err [Err_TooManyRowLabels (assocLKeys e1) tr2]
                       fR fi r1@(Ty_Con n1) _ e1 e12 e2@(_:_)
                         | n1 == hsnRowEmpty && isSum
                         = fR fi r1 r1 e1 e12 []
                       fR fi r1 r2@(Ty_Con n2) e1@(_:_) e12 e2
                         | n2 == hsnRowEmpty && isSum
                         = err [Err_MissingRowLabels (assocLKeys e1) tr2]
                       fR fi r1 r2 e1 e12@(_:_) e2
                         = foR
                         where (e1L,e2L) = unzip e12
                               (foL,fi2,fCnstr) = fPairWise (fiUpdOpts fioMkStrong fi) (assocLElts e1L) (assocLElts e2L)
                               eKeys = assocLKeys e1L
                               eL = zip eKeys (map ((fCnstr |=>) . foTy) foL)
                               fo = fR fi2 r1 r2 e1 [] e2
                               foR = manyFO ([fo] ++ foL ++ [foRes])
                               foRes = (\fo -> foldr foCmbPrfRes fo foL)
%%]
%%[10
                                       . foUpdRecFldsCoe eKeys foL tr1
%%]
%%[7
                                       . foUpdTy (foTy fo `mkTyRow` eL)
                                       . foUpdCnstr fCnstr $ fo
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
                         = (f fi r1 r2) {foLCoeL = [], foRCoeL = []}
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
                       foR        = (if isRec then foUpdRecCoe (foCnstr fo |=> r1) (foCnstr fo |=> r2) extsIn1 extsIn12 extsIn2 else id) fo 
                       foUpdRecCoe r1 r2 e1 e12 e2 fo
                         =  let  rn = uidHNm u1
                                 prfCxId = fePrfCtxtId (fiEnv fi)
                                 r = CExpr_Var rn
                                 tr1s = foCnstr fo |=> tr1
                                 (u',u2,u3,u4) = mkNewLevUID3 (foUniq fo)
                                 mkLSel n u = mkCExprSelCase emptyRCEEnv (hsnSuffix rn "!") r CTagRec n n (CExpr_Hole u)
                                 mkLPred' r l u
                                   =  let  r' = maybe Ty_Any fst . tyRowExtr l $ r
                                      in   (PredOcc (Pred_Lacks r' l) (mkPrId prfCxId u),r')
                                 mkLPred r l u = fst (mkLPred' r l u)
                                 rowCoeL = [ rc | rc@(_,c) <- sortByOn rowLabCmp fst (foRowCoeL fo), not (coeIsId c) ]
                                 (fuUpdL,prUpdL,tr1s',_)
                                   =  foldr  (\(l,c) (fuL,prL,r,u)
                                                ->  ((l,CExpr_TupUpd cvarUndefined CTagRec l (CExpr_Hole u) (c `coeEvalOn` mkLSel l u)):fuL
                                                    ,mkLPred r l u : prL,r,uidNext u
                                                    )
                                             )
                                             ([],[],tr1s,u2) rowCoeL
                                 (fuDelL,prDelL,_,_)
                                   =  foldl  (\(fuL,prL,r,u) l
                                                  ->  let  (pr,r') = mkLPred' r l u
                                                      in   ((l,CExpr_TupDel (CExpr_Var hsnWild) CTagRec l (CExpr_Hole u)):fuL
                                                           ,pr:prL,r',uidNext u
                                                           )
                                             )
                                             ([],[],tr1s',u3) (sortBy rowLabCmp (assocLKeys e1))
                                 fuL = fuUpdL ++ reverse fuDelL
                                 (fBldL,prBldL,_,_)
                                   =  foldr  (\l (fL,prL,r,u)
                                                ->  ((maybe id coeEvalOn (lookup l rowCoeL) $ mkLSel l u):fL
                                                    ,mkLPred r l u : prL,r,uidNext u
                                                    )
                                             )
                                             ([],[],tr1s,u3) (sortBy rowLabCmp ((assocLKeys . map fst $ e12) ++ assocLKeys e2))
                            in   case r2 of
                                   Ty_Con n2 | n2 == hsnRowEmpty
                                     ->  fo  {  foLCoeL = [Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (CExpr_Tup CTagRec `mkCExprApp` fBldL))]
                                             ,  foPredOccL = prBldL ++ foPredOccL fo
                                             ,  foUniq = u'
                                             }
                                   Ty_Var _ cat | tvCatIsFixed cat && not (null fuL)
                                     ->  fo  {  foLCoeL = [Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (fuMkCExpr u4 fuL r))]
                                             ,  foPredOccL = prUpdL ++ prDelL ++ foPredOccL fo
                                             ,  foUniq = u'
                                             }
                                   _ |  not (null fuUpdL)
                                     ->  fo  {  foLCoeL = [Coe (\e -> mkCExprLet CBindPlain [CBind_Bind rn e] (fuMkCExpr u4 fuUpdL r))]
                                             ,  foPredOccL = prUpdL ++ foPredOccL fo
                                             ,  foUniq = u'
                                             }
                                     |  otherwise
                                     ->  fo  {  foLCoeL = [] }
%%]

%%[10.fitsIn.fRow.Coe
                       foUpdRecFldsCoe eKeys foL tr1 foR
                         =  let cL =   [  (l,c)
                                       |  (l,fo) <- zip eKeys foL
                                       ,  let c = coeWipeWeave (foCnstr foR) (foCSubst foR) (foLCoeL fo) (foRCoeL fo)
                                       ,  not (coeIsId c)
                                       ]
                            in  foR {foRowCoeL = cL}
%%]

%%[4.fitsIn.Base
            f fi t1                     t2
                | fioMode (fiFIOpts fi) == FitSubRL = f  fi' t2 t1
                where  fi'       = fi  {fiFIOpts = fioSwapOpts . fioSwapCoCo ContraVariant . fiFIOpts $ fi}
            f fi Ty_Any                 t2          = res fi t2
            f fi t1                     Ty_Any      = res fi t1
            f fi t1@(Ty_Con s1)         t2@(Ty_Con s2)
                | s1 == s2                          = res fi t2
            f fi t1@(Ty_Var v1 f1)      (Ty_Var v2 f2)
                | v1 == v2 && f1 == f2              = res fi t1
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

%%[11.fitsIn.EqualVar
            f fi t1@(Ty_Var v1 _) t2@(Ty_Equal v2 t2e)
                | fioAllowEqOpen (fiFIOpts fi)      = (bind fi v2 t2e) {foEqCnstr = ce}
                where  ce = v1 `cnstrTyUnit` Ty_Equal v1 t2e
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

%%[11.fitsIn.Equal
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
                            where fo = f fi ct1 ct2
                       fP (Pred_Pred prt1)          (Pred_Pred prt2)
                            = Just (fo,Pred_Pred (foTy fo))
                            where fo = f fi prt1 prt2
%%]
%%[10
                       fP (Pred_Lacks lt1 l1)       (Pred_Lacks lt2 l2)
                            | l1 == l2
                            = Just (fo,Pred_Lacks (foTy fo) l1)
                            where fo = f fi lt1 lt2
%%]
%%[9
                       fP _                         _
                            = Nothing
%%]

%%[4.fitsIn.QLR
            f fi t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
                | fioMode (fiFIOpts fi) == FitUnify && q1 == q2
                                                    = f fi2 uqt1 uqt2
                where  (fi1,uqt1,_) = unquant fi   t1 False instCoConst
                       (fi2,uqt2,_) = unquant fi1  t2 False instCoConst
%%]

%%[4.fitsIn.QR
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (f fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoConst
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (f fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra
%%]

%%[6_4.fitsIn.QR -4.fitsIn.QR
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (f fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoExist
            f fi t1                     t2@(Ty_Quant _ _ _)
                | fioIsSubsume (fiFIOpts fi) && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (f fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra
%%]

%%[4.fitsIn.QL
            f fi t1@(Ty_Quant _ _ _)    t2
                | fioIsSubsume (fiFIOpts fi)        = f fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoConst
%%]

%%[6_4.fitsIn.QL -4.fitsIn.QL
            f fi t1@(Ty_Quant _ _ _)    t2
                | fioIsSubsume (fiFIOpts fi)        = f fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoExist
%%]

%%[4_2.fitsIn.QL1
            f fi t1@(Ty_Quant q1 _ _)   t2
                | m == FitMeet || m == FitJoin      = manyFO [fo,fo2]
                where  m = fioMode (fiFIOpts fi)
                       (u',u1,u2) = mkNewLevUID2 (fiUniq fi)
                       (fi1,uqt1,rtvs1) = unquant' (fi {fiUniq = u'}) t1 instMeet
                       fo = f fi1 uqt1 t2
                       (ebTy,ebCnstr) = tyElimBoth rtvs1 (foTy fo)
                       ebTy' =  if     m == FitMeet && tyquIsExists q1
                                   ||  m == FitJoin && tyquIsForall q1
                                then ebCnstr |=> ebTy
                                else ebTy
                       tvs = rtvs1 `List.intersect` ftv ebTy'
                       fo2 = foUpdTy (mkTyQu q1 tvs ebTy') . foUpdCnstr ebCnstr $ fo
%%]

%%[4_2.fitsIn.QR
            f fi t1                     t2@(Ty_Quant q2 _ _)
                | fioIsMeetJoin (fiFIOpts fi)       = f  (fi  {fiFIOpts = fioSwapOpts (fiFIOpts fi)})
                                                         t2 t1
%%]

%%[9
            f fi  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                  t2@(Ty_App (Ty_App (Ty_Con c2) tpr2) tr2)
                    | hsnIsArrow c1 && c1 == c2 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfCxId          = fePrfCtxtId (fiEnv fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr1 tpr2
                       mberr            = Just (errClash fi t1 t2)
                       fP tpr1@(Ty_Pred _)              tpr2@(Ty_Pred _)
                            =  if foHasErrs pfo
                               then Nothing
                               else Just  ( foUpdTy ([foCnstr fo |=> foTy pfo] `mkArrow` foTy fo)
                                          . foUpdLRCoe (mkAppCoe [CExpr_Var n]) (mkLamCoe n)
                                          $ fo)
                            where  pfo   = f (fi2 {fiFIOpts = predFIOpts}) tpr2 tpr1
                                   n     = uidHNm u2
                                   fo    = f (fi2 {fiUniq = foUniq pfo}) (foCnstr pfo |=> tr1) (foCnstr pfo |=> tr2)
                       fP tpr1@(Ty_Pred pr1)            (Ty_Impls (Impls_Tail iv2))
                            =  Just (foUpdImplExplCoe iv2 (Impls_Cons iv2 pr1 (mkPrId prfCxId u2) im2) tpr1 (mkAppCoe [CExpr_Var n]) (mkLamCoe n) fo)
                            where  im2   = Impls_Tail u1
                                   n     = uidHNm u2
                                   fo    = f fi2 tr1 ([Ty_Impls im2] `mkArrow` tr2)
                       fP (Ty_Impls (Impls_Tail iv1))   tpr2@(Ty_Pred pr2)
                            =  Just (foUpdImplExplCoe iv1 (Impls_Cons iv1 pr2 (mkPrId prfCxId u2) im1) tpr2 (mkAppCoe [CExpr_Var n]) (mkLamCoe n) fo)
                            where  im1   = Impls_Tail u1
                                   n     = uidHNm u2
                                   fo    = f fi2 ([Ty_Impls im1] `mkArrow` tr1) tr2
                       fP (Ty_Impls (Impls_Tail iv1))   tpr2@(Ty_Impls im2@(Impls_Nil))
                            =  Just (foUpdImplExpl iv1 im2 tpr2 (f fi2 tr1 tr2))
                       fP (Ty_Impls (Impls_Nil))   tpr2@(Ty_Impls im2@(Impls_Tail iv2))
                            =  Just (foUpdImplExpl iv2 Impls_Nil (Ty_Impls Impls_Nil) (f fi2 tr1 tr2))
                       fP (Ty_Impls (Impls_Tail iv1))   tpr2@(Ty_Impls im2@(Impls_Tail iv2))
                            =  Just (foUpdImplExplCoe iv1 im2 tpr2 (CoeImplApp iv2) (CoeImplLam iv2) (f fi2 tr1 tr2))
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
                       prfCxId          = fePrfCtxtId (fiEnv fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr2
                       mkPrTy pr2 fo    = [Ty_Pred (foCnstr fo |=> pr2)] `mkArrow` foTy fo
                       fSub pr2v pr2 tr2
                            =  let  ci    = u1
                                    pr2n  = poiHNm pr2v
                                    fo    = f (fiAddPr ci pr2n pr2v tpr2 fi2) t1 tr2
                               in   (fo,mkLamCoe pr2n)
                       fP (Ty_Impls (Impls_Nil))
                            =  Just fo
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Tail iv2))
                            =  Just (foUpdCnstr (iv2 `cnstrImplsUnit` Impls_Nil) fo)
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Cons _ pr2 pv2 im2))
                            =  Just (foUpdRCoe rCoe . foUpdTy (mkPrTy pr2 fo) $ fo)
                            where (fo,rCoe) = fSub pv2 pr2 ([Ty_Impls im2] `mkArrow` tr2)
                       fP (Ty_Pred pr2)  | fioAllowRPredElim (fiFIOpts fi)
                            =  Just (foUpdRCoe rCoe . foUpdTy (mkPrTy pr2 fo) $ fo)
                            where (fo,rCoe) = fSub (mkPrId prfCxId u1) pr2 tr2
                       fP _ =  Nothing
            f fi  t1@(Ty_App (Ty_App (Ty_Con c1) tpr1) tr1)
                  t2
                    | hsnIsArrow c1 && not (fioPredAsTy (fiFIOpts fi)) && isJust mbfp
                = fromJust mbfp
                where  (u',u1,u2,u3)    = mkNewLevUID3 (fiUniq fi)
                       prfCxId          = fePrfCtxtId (fiEnv fi)
                       fi2              = fi {fiUniq = u'}
                       mbfp             = fP tpr1
                       fSub pv1 pr1 tr1
                            =  let  fo    = f fi2 tr1 t2
                                    fs    = foCnstr fo
                                    (cxbindLMap,introCBindL,csubst,remPrfPrL,evidL,prfErrs)
                                          = prfPreds u3 (fiEnv (fs |=> fi2)) [PredOcc (fs |=> pr1) pv1]
                                    coe   = mkCoe (\e -> (csubst `cSubstApp` introCBindL) `mkCExprLetRec` (e `mkCExprApp` evidL))
                               in   (foUpdErrs prfErrs fo,coe,csubst,remPrfPrL)
                       fP (Ty_Impls (Impls_Nil))
                            =  Just (f fi2 tr1 t2)
                       fP (Ty_Impls (Impls_Tail iv1))
                            =  Just (foUpdCnstr (iv1 `cnstrImplsUnit` Impls_Nil) (f fi2 tr1 t2))
                       fP (Ty_Impls (Impls_Cons _ pr1 pv1 im1))
                            =  Just (foUpdPrL remPrfPrL . foUpdLCoe lCoe . foUpdCSubst csubst $ fo)
                            where (fo,lCoe,csubst,remPrfPrL) = fSub pv1 pr1 ([Ty_Impls im1] `mkArrow` tr1)
                       fP (Ty_Pred pr1)  | fioAllowRPredElim (fiFIOpts fi)
                            =  Just (foUpdPrL remPrfPrL . foUpdLCoe lCoe . foUpdCSubst csubst $ fo)
                            where (fo,lCoe,csubst,remPrfPrL) = fSub (mkPrId prfCxId u1) pr1 tr1
                       fP _ =  Nothing
%%]

                       fP im2@(Ty_Impls (Impls_Nil))
                            =  Just (foUpdTy ([im2] `mkArrow` foTy fo) $ fo)
                            where fo = f fi2 t1 tr2
                       fP (Ty_Impls (Impls_Tail iv2))
                            =  Just (foUpdCnstr (iv2 `cnstrImplsUnit` Impls_Nil) . foUpdTy ([Ty_Impls (Impls_Nil)] `mkArrow` foTy fo) $ fo)
                            where fo = f fi2 t1 tr2

(bodyPrfFrPoiCxBindLM,bodyPrfIntroCBindL,bodyPrfCSubst,bodyPrfArgPrOccL,_,_)
                                                    =   prfPreds @lUniq5 (@letFE {fePrElimTGam = @body.prElimTGam}) @gath2BodySubsPredL

                                    (cbindLMap,csubst,remPrfPrL,evidL,prfErrs,_) = prfPreds u3 (fiEnv (fs |=> fi2)) [PredOcc (fs |=> pr1) pv1]

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
                       fs   = foCnstr ffo
                       (as:_) = foAppSpineL ffo
                       fi'  = fi  { fiFIOpts  = asFIO as . fioSwapCoCo (asCoCo as) . fiFIOpts $ fi
                                  , fiUniq    = foUniq ffo }
                       afo  = f fi' (fs |=> ta1) (fs |=> ta2)
%%]

%%[9.fitsIn.App -4.fitsIn.App
            f fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,rfo]
                where  ffo  = f fi tf1 tf2
                       fs   = foCnstr ffo
                       (as:_) = foAppSpineL ffo
                       fi'  = fi  { fiFIOpts  = asFIO as . fioSwapCoCo (asCoCo as) . fiFIOpts $ fi
                                  , fiUniq    = foUniq ffo }
                       afo  = f fi' (fs |=> ta1) (fs |=> ta2)
                       rfo  = asFOUpdCoe as ffo . foCmbApp ffo $ afo
%%]

%%[7.fitsIn.Ext
            f fi t1@(Ty_Ext _ _ _)   t2@(Ty_Ext _ _ _)
                =  fRow fi t1 t2 False False
%%]

%%[4.fitsIn.DefaultCase
            f fi t1                     t2          = errClash fi t1 t2
%%]

%%[4.fitsIn.SetupAndResult
            fo  = f (emptyFI {fiUniq = uniq, fiFIOpts = opts}) ty1 ty2
%%]

%%[9.fitsIn.SetupAndResult -4.fitsIn.SetupAndResult
            fo  = f  (emptyFI  { fiUniq = uniq, fiFIOpts = opts
                               , fiEnv = env
                               }
                     ) ty1 ty2
%%]

%%[9
            f' msg fi t1 t2 = let fo = f (trPP ("FIT" ++ "-" ++ msg ++ "-" ++ "fi") fi) (m "t1" t1) (m "t2" t2)
                                  m mm x = trPP ("FIT" ++ "-" ++ msg ++ "-" ++ mm) x
                              in  tr ("FIT" ++ "-" ++ msg) (pp (foTy fo)) fo
%%]

%%[9
fitsIn' :: String -> FIOpts -> FIEnv -> UID -> Ty -> Ty -> FIOut
fitsIn' msg opts env uniq ty1 ty2
  =  fitsIn opts (trPP (msg ++ "-env") env) (trPP (msg ++ "-uniq") uniq) (trPP (msg ++ "-ty1") ty1) (trPP (msg ++ "-ty2") ty2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption for lists of types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
fitsInL :: FIOpts -> FIEnv -> UID -> TyL -> TyL -> (TyL,FIOut)
fitsInL opts env uniq tyl1 tyl2
  =  snd
     .  foldr  (\(t1,t2) (u,(ts,fos))
                  -> let  (u',ue) = mkNewLevUID u
                          fo = fitsIn opts env u (foCnstr fos |=> t1) (foCnstr fos |=> t2)
                     in   (u',(foTy fo:ts, fos {foCnstr = foCnstr fo |=> foCnstr fos, foErrL = foErrL fo ++ foErrL fos}))
               )
               (uniq,([],emptyFO))
     .  zip tyl1 $ tyl2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Proof of predicates, must be here because of mutual dep with fitsIn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prfPreds  :: UID -> FIEnv -> [PredOcc] -> (CxBindLMap,CBindL,CSubst,[PredOcc],[CExpr],[Err])
prfPreds u fe prOccL
  = (prfFrPoiCxBindLM, prfIntroCBindL, cSubst, argPrOccL, prvnOrigEvidL, prvnErrs)
  where  (prfFrPoiCxBindLM, prfIntroCBindL, cSubst, argPrOccL, prvnOrigEvidL, prvnErrs, _)
            = prfPredsDbg u fe prOccL
%%]

%%[9
prfPredsDbg  :: UID -> FIEnv -> [PredOcc]
                    ->  (CxBindLMap,CBindL,CSubst,[PredOcc],[CExpr],[Err]
                        ,(ProvenGraph,ProvenGraph,ProvenGraph,ProvenGraph,ProvenGraph,PrElimTGam,[Pred]
                         ,Map.Map PredOccId [PredOccId],Map.Map PredOccId PredOccId,PP_DocL
                        ))
prfPredsDbg u fe prOccL
  =  case prvnErrs of
       []  ->    ( prfFrPoiCxBindLM, prfIntroCBindL, cSubstLetHole `cSubstApp` cSubst, argPrOccL, prvnOrigEvidL
				 , overlErrs ++ prvnErrs
				 , (g,gPrune,gOr,gInterm1,gInterm2,eGam,prLeaves, fwdMp, backMp,[])
				 )
       _   ->    ( emptyCxBindLMap, [], emptyCSubst, prOccL, [], prvnErrs, (g,g,g,g,g,fePrElimTGam fe,[],Map.empty,Map.empty,[]))
  where   (g,prvnErrs,eGam)             = prfPredsToProvenGraph u fe prOccL
          prOrigs                       = prvgOrigs g
          (_,revTopSort)                = prvg2ReachableFrom g prOrigs
          topSort                       = reverse revTopSort
          prLeaves                      = prvgArgLeaves g topSort
          (gPrune@(ProvenGraph i2n _ _ _),gOr,backMp,(gInterm1,gInterm2))
                                        = prfPredsPruneProvenGraph (\pr cx -> pr `elem` prLeaves) g
          factPoiS                      = Set.fromList (prvgFactL gPrune)
          isFact                        = (`Set.member` factPoiS)
          cSubst                        = poiCExprLToCSubst . assocLMapElt (CExpr_Var . poiHNm) . Map.toList $ backMp
          argPrOccL                     = [ PredOcc p i | (i,ProvenArg p _) <- Map.toList i2n ]
          overl                         = [ (pr,map (\e -> case Map.lookup e (prvgIdNdMp gOr) of {Just (ProvenAnd _ _ _ _ ev) -> ev; _ -> mkCExprPrHole e}) es)
                                          | (ProvenOr pr es _) <- Map.elems (prvgIdNdMp gOr)
                                          ]
          overlErrs                     = if null overl then [] else [Err_OverlapPreds overl]
          prfFrPoiCxBindLM              = prvgCxBindLMap gPrune
          prfIntroCBindL                = prvgIntroBindL poiIsGlob topSort gPrune
                                        where poiIsGlob poi = tgamIsInScope (fePrfCtxtId fe) (poiCxId poi) (fePrElimTGam fe)
          cSubstLetHole                 = prvgLetHoleCSubst gPrune eGam prfFrPoiCxBindLM
          fwdMp                         = Map.fromList . map (\l@((_,i):_) -> (i,map fst l)) . groupSortOn snd . Map.toList $ backMp
          prvnOrigEvidL                 = map (\po -> mkCExprPrHole . poPoi $ po) prOccL
%%]

%%[9
prvgAddUsedAsFact :: PredOccId -> Pred -> ProvenGraph -> ProvenGraph
prvgAddUsedAsFact uid pr g = g {prvgPrUsedFactIdMp = Map.insertWith (++) pr [uid] (prvgPrUsedFactIdMp g)}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Proving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prfPredsToProvenGraph :: UID -> FIEnv -> [PredOcc] -> (ProvenGraph,[Err],PrElimTGam)
prfPredsToProvenGraph u fe prOccL
  =  let  initState1 = ProofState
                        (ProvenGraph Map.empty Map.empty
                            (foldr (\p m -> Map.insertWith (++) (poPr p) [poPoi p] m) (Map.empty) prOccL)
                            Map.empty)
                        u [] Map.empty (fePrElimTGam fe) []
          initState2 = prfsAddPrOccL prOccL 0 initState1
          resolve st@(ProofState _ _ (po:poL) _ peg [])
            =  let  st' = prfOneStep (fe {fePrElimTGam = peg}) po (st {prfs2PredsToProve = poL})
               in   resolve st'
          resolve st@(ProofState _ _ _ _ _ _) = st
          prvnState = resolve initState2
     in   (prfs2ProvenGraph prvnState, prfs2ErrL prvnState,prfs2PrElimTGam prvnState)
%%]
            =  let  st' = prfOneStep (fe {fePrElimTGam = peg}) (trPP "XX" po) (st {prfs2PredsToProve = poL})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One proof step
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prfOneStep :: FIEnv -> PredOcc -> ProofState -> ProofState
prfOneStep fe prOcc@(PredOcc pr prPoi) st@(ProofState g@(ProvenGraph _ p2i _ _) _ _ i2d _ errL)
  =  case Map.lookup prPoi i2d of
        Just depth
          | depth < ehcoptPrfCutOffAt (feEHCOpts fe)
              ->  case Map.lookup pr p2i of
                    Just poiL@(poi:_)
                      | poiCxId prPoi `notElem` map poiCxId poiL
                          ->  prf pr
                      | otherwise
                          ->  st {prfs2ProvenGraph = prvgAddPrNd pr [prPoi] (ProvenShare pr poi) g}
                    Nothing
                      ->  prf pr
                    _ ->  st
          | otherwise
              ->  st { prfs2ErrL = [Err_PrfCutOffReached prOcc depth] ++ errL }
          where  prf pr
                   =  case pr of
                        Pred_Class  _    ->  prfOneStepClass  fe prOcc depth st
                        Pred_Pred   _    ->  prfOneStepPred   fe prOcc depth st
%%]
%%[10
                        Pred_Lacks  _ _  ->  prfOneStepLacks  fe prOcc depth st
%%]
%%[9
%%]
                        _                ->  st
%%[9
        _     ->  st
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One proof step for class predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prfRuleMatches :: UID -> PredOcc -> [Rule] -> [([PredOcc],(Pred,PredOccId,CExpr),CExpr,ProofCost)]
prfRuleMatches u prOcc rules
  = catMaybes . zipWith (\u r -> matchRule u prOcc r) (mkNewUIDL (length rules) u) $ rules
%%]

%%[9
pegiLRuleL :: [PrElimGamInfo] -> [Rule]
pegiLRuleL
  = concat . map pegiRuleL

pegiLScopedCostRuleL :: [PrElimGamInfo] -> [Rule]
pegiLScopedCostRuleL
  =  concat
  .  zipWith  (\lev pegi
                 -> map (\r -> r {rulCost = rulCost r `costAddHi` lev})
                        (pegiRuleL pegi)
              ) [0..]
%%]

%%[9
prfOneStepClass :: FIEnv -> PredOcc -> Int -> ProofState -> ProofState
prfOneStepClass  fe prOcc@(PredOcc pr@(Pred_Class t) prPoi) depth
                 st@(ProofState g@(ProvenGraph i2n p2i p2oi p2fi) u _ _ _ _)
  =  let  nm = tyAppFunConNm t
          ndFail = ProvenArg pr costVeryMuch
     in   case tgamLookupAll (poiCxId prPoi) nm (fePrElimTGam fe) of
             pegis@(_:_)
                 ->  let  (u',u1,u2)    = mkNewLevUID2 u
                          rules         = pegiLScopedCostRuleL pegis
                          ruleMatches   = prfRuleMatches u1 prOcc rules
                          ((g',newPrOccL),depth')
                             = case ruleMatches of
                                   [] ->  ((prvgAddPrNd pr [prPoi] ndFail g,[]),depth)
                                   ms ->  let  orPoiL@(poiFail:poiMatchL) = map (mkPrId (poiCxId prPoi)) . mkNewUIDL (length ms + 1) $ u2
                                          in   (foldr
                                                   (\(poi,(needPrOccL,(matchPr,matchPoi,matchEv),evid,cost))
                                                     (g,newPrOccL)
                                                       ->  let  hasNoPre   =  null needPrOccL
                                                                matchPrf   =  ProvenAnd matchPr [] [] costZero matchEv
                                                                prf        =  ProvenAnd pr (matchPoi : map poPoi needPrOccL) [] cost evid
                                                           in   (prvgAddNd poi prf
                                                                  . prvgAddPrNd matchPr [matchPoi] matchPrf
                                                                  . prvgAddUsedAsFact matchPoi (if hasNoPre then pr else matchPr)
                                                                  $ g
                                                                ,needPrOccL ++ newPrOccL
                                                                )
                                                   )
                                                   (prvgAddPrNd pr [prPoi] (ProvenOr pr orPoiL costZero)
                                                     . prvgAddNd poiFail (ProvenArg pr costVeryMuch)
                                                     $ g
                                                   ,[])
                                                   (zip poiMatchL ms)
                                               ,depth+1
                                               )
                     in   (prfsAddPrOccL newPrOccL depth' st)
                            {prfs2Uniq = u', prfs2ProvenGraph = g'}
             []  ->  st {prfs2ProvenGraph = prvgAddPrNd pr [prPoi] ndFail g}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One proof step for lacks predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[10
prfOneStepLacks :: FIEnv -> PredOcc -> Int -> ProofState -> ProofState
prfOneStepLacks  fe prOcc@(PredOcc pr@(Pred_Lacks r l) prPoi) depth
                 st@(ProofState g@(ProvenGraph i2n p2i p2oi p2fi) u _ _ _ errL)
  =  case tyRowExtr l r of
       Just _ ->  st {prfs2ErrL = [Err_TooManyRowLabels [l] r] ++ errL}
       _      ->  let  (row,exts) = tyRowExts r
                       offset = tyExtsOffset l . tyRowCanonOrder $ exts
                  in   case row of
                         Ty_Var _ _ 
                           | null exts
                               ->  case tgamLookupAll (poiCxId prPoi) (predMatchNm pr) (fePrElimTGam fe) of  
                                     pegis@(_:_) | isJust mbMatch
                                         -> fromJust mbMatch
                                         where    (u',u1,u2)    = mkNewLevUID2 u
                                                  rules         = pegiLRuleL pegis
                                                  ruleMatches   = prfRuleMatches u1 prOcc rules
                                                  mbMatch
                                                    =  case ruleMatches of
                                                         [] ->  Nothing
                                                         ((_,(matchPr,matchPoi,matchEv),evid,cost):_)
                                                            ->  let  prf        = ProvenAnd pr [matchPoi] [] cost evid
                                                                     matchPrf   = ProvenAnd matchPr [] [] costZero matchEv
                                                                     g'         = prvgAddUsedAsFact matchPoi pr
                                                                                  . prvgAddPrNd matchPr [matchPoi] matchPrf
                                                                                  . prvgAddNd prPoi prf
                                                                                  $ g
                                                                in   Just (st {prfs2ProvenGraph = g', prfs2Uniq = u'})
                                     []  ->  st {prfs2ProvenGraph = prvgAddPrNd pr [prPoi] (ProvenArg pr costZero) g}
                           | otherwise
                               ->  let  (u',u2) = mkNewUID u
                                        poi2 = mkPrId (poiCxId prPoi) u2
                                        newPr = PredOcc (Pred_Lacks row l) poi2
                                        g2 = prvgAddPrNd pr [prPoi] (ProvenAnd pr [poi2] [] costZero (CExpr_Hole u2 `mkCExprAddInt` offset)) g
                                   in   (prfsAddPrOccL [newPr] (depth+1) st)
                                          {prfs2ProvenGraph = g2, prfs2Uniq = u'}
                         _     ->  let  g2 = prvgAddPrNd pr [prPoi] (ProvenAnd pr [] [] costZero (CExpr_Int offset)) g
                                   in   st {prfs2ProvenGraph = g2}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% One proof step for higher order predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prfOneStepPred :: FIEnv -> PredOcc -> Int -> ProofState -> ProofState
prfOneStepPred  fe prOcc@(PredOcc pr@(Pred_Pred t) prPoi) depth
                st@(ProofState g@(ProvenGraph i2n p2i p2oi p2fi) u _ _ _ _)
  =   let  (u',u1,u2,u3,u4,u5)
                            = mkNewLevUID5 u
           newCxId          = u4
           poi4             = mkPrId newCxId u4
           poi5             = mkPrId newCxId u5
           (us,vs)          = mkNewUIDTyVarL (tyArrowArity t + 1) u1
           t'               = tail vs `mkArrow` head vs
           fo               = fitsIn (predFIOpts {fioLeaveRInst = False}) emptyFE u2 t' t
           (prfCtxtTyPrL,prfTyPr)
                            = tyArrowArgsRes (foCnstr fo |=> t')
           prfCtxtPrL       = map tyPred prfCtxtTyPrL
           (prfElimTGam,prfCtxtNmL,prfCtxtUIDL)
                            = peTGamAddKnPrL newCxId u3 prfCtxtPrL (tgamPushNew (poiCxId prPoi) newCxId (fePrElimTGam fe))
           prf              = ProvenAnd (pr) (poi4 : []) (poi5 : poi4 : prfCtxtUIDL) (mkCost 1) (prfCtxtNmL `mkCExprLam` (poiId poi5 `CExpr_HoleLet` CExpr_Hole u4))
           g'               = prvgAddPrNd pr [prPoi] prf g
           g''              = g'
           st'              = (prfsAddPrOccL [PredOcc (tyPred prfTyPr) poi4] (depth+1) st)
                                {prfs2Uniq = u', prfs2ProvenGraph = g'', prfs2PrElimTGam = prfElimTGam}
      in   st'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pruning the proof graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvnPruneHighCost :: (Pred -> PrfCtxtId -> Bool) -> ProvenGraph -> (ProvenGraph,PoiSubst)
prvnPruneHighCost isPrCheap g@(ProvenGraph i2n p2i p2oi p2fi)
  =  (gPrune,PoiSubst pruneBackMp)
  where   costOf poi costMp pruneBackMp gPrune
            =  case Map.lookup poi costMp of
                 Just (Just c)  -> (poi,c,costMp,pruneBackMp,gPrune)
                 Just (Nothing) -> (poi,costVeryMuch,costMp,pruneBackMp,gPrune)
                 Nothing
                   ->  let  prvgAddPrevPrNd pr poi prf g
                              =  prvgAddPrNd pr [poi] prf g
                            costMp' = Map.insert poi Nothing costMp
                       in   case fromJust (Map.lookup poi i2n) of
                              ProvenAnd pr es les c ev
                                ->  let  (cs,cm,pbm,gp)      = costOfL es costMp' pruneBackMp gPrune
                                         c'                  = foldr costAdd c (map snd cs)
                                         cm'                 = Map.insert poi (Just c') cm
                                         pbm'                = Map.insert poi poi pbm
                                         gp'                 = prvgAddPrevPrNd pr poi (ProvenAnd pr (map fst cs) les c' ev) gp
                                    in   (poi,c',cm',pbm',gp')
                              ProvenOr pr es _
                                ->  let  (cs,cm,pbm,gp)      = costOfL es costMp' pruneBackMp gPrune
                                         alts@((_,calt):_)   = head . groupSortOn snd $ cs
                                         (poi',gp',c')
                                           =  case alts of
                                                [(poia,_)]    ->  (poia,prvgAddPrUids pr [poi] gp,calt)
                                                ((poia,_):_)  ->  (poia,prvgAddNd poi (ProvenOr pr (map fst alts) calt) gp,calt)
                                         cm'                 = Map.insert poi' (Just c') (Map.delete poi cm)
                                         pbm'                = Map.insert poi poi' pbm
                                    in   (poi',c',cm',pbm',gp')
                              ProvenShare pr e
                                ->  let  (poi',c,cm,pbm,gp)  = costOf e costMp' pruneBackMp gPrune
                                         gp'                 = prvgAddPrUids pr [poi] gp
                                         pbm'                = Map.insert poi poi' pbm
                                    in   (poi',c,Map.delete poi cm,pbm',gp')
                              ProvenArg pr c
                                ->  let  c'                  = if isPrCheap pr (poiCxId poi) then costAvailArg else c
                                         cm                  = Map.insert poi (Just c') costMp'
                                         pbm                 = Map.insert poi poi pruneBackMp
                                         gp                  = prvgAddPrevPrNd pr poi (ProvenArg pr c') gPrune
                                    in   (poi,c',cm,pbm,gp)
          costOfL poiL costMp pruneBackMp gPrune
            =  foldr  (\poi (cs,cm,pbm,g) ->  let  (poi',c,cm',pbm',g') = costOf poi cm pbm g
                                              in   ((poi',c):cs,cm',Map.insert poi poi' pbm',g'))
                      ([],costMp,pruneBackMp,gPrune) poiL
          (_,_,pruneBackMp,gPrune)
            = costOfL (prvgOrigs g) Map.empty Map.empty (ProvenGraph Map.empty Map.empty p2oi p2fi)
%%]

%%[9
prfPredsPruneProvenGraph ::  (Pred -> PrfCtxtId -> Bool) -> ProvenGraph
                                 ->  (ProvenGraph,ProvenGraph,Map.Map PredOccId PredOccId
                                     ,(ProvenGraph,ProvenGraph)
                                     )
prfPredsPruneProvenGraph isPrCheap g@(ProvenGraph i2n p2i p2oi p2fi)
  =  let  onlyReachables poiL g@(ProvenGraph i2n _ _ _)
            =  let  (reachableUIDs,_) = prvg2ReachableFrom g poiL
               in   g { prvgIdNdMp  = Map.filterWithKey (\poi _ -> poi `Set.member` reachableUIDs) i2n }
          splitOr g@(ProvenGraph i2n _ _ _)
            =  let (yesOr,noOr) = partition (\(_,n) -> case n of {ProvenOr _ _ _ -> True; _ -> False}) . Map.toList $ i2n
               in  (g {prvgIdNdMp = Map.fromList noOr}, g {prvgIdNdMp = Map.fromList yesOr}, assocLKeys yesOr)
          (gPrune,prunePois)
            = prvnPruneHighCost isPrCheap g
          pruneStartPoiL
            = prunePois `poisApp` prvgOrigs g
          (gPruneNoOr,gPruneOr,orPoiL)
            = splitOr gPrune
          gPruneReached
            = onlyReachables pruneStartPoiL gPruneNoOr
          (_,pruneRevTopSort)
            = prvg2ReachableFrom gPruneReached pruneStartPoiL
          sharePois
            = prvgShareMp gPruneReached (reverse pruneRevTopSort)
          backPois
            = sharePois `poisApp` prunePois
          origRestorePois
            = PoiSubst (Map.fromList [ (i,o) | o <- prvgOrigs g, n <- maybeToList (Map.lookup o m), o /= n, (i,n') <- Map.toList m, n' == n ])
            where m = poisMp backPois
          fullBackPois
            = origRestorePois `poisApp` backPois
          gPruneBack
            = prvgAppPoiSubst fullBackPois gPruneReached
     in   (gPruneBack, onlyReachables orPoiL gPruneOr, poisMp fullBackPois, (gPrune,gPruneReached))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
matchRule :: UID -> PredOcc -> Rule -> Maybe ([PredOcc],(Pred,PredOccId,CExpr),CExpr,ProofCost)
matchRule u prOcc r
  =  let  (_,u1,u2,u3)   = mkNewLevUID3 u
          pr             = poPr prOcc
          (us,vs)        = mkNewUIDTyVarL (tyArrowArity (rulRuleTy r)) u2
          fo             = fitsIn (predFIOpts {fioDontBind = ftv pr}) emptyFE u3 (rulRuleTy r) (vs `mkArrow` Ty_Pred pr)
     in   if foHasErrs fo
          then Nothing
          else Just  ( zipWith PredOcc (map tyPred . tyArrowArgs . foTy $ fo) (map (mkPrId . poiCxId . poPoi $ prOcc) us)
                     , (tyPred (rulRuleTy r),rulId r,CExpr_Var (rulNmEvid r))
                     , rulMkEvid r (map CExpr_Hole us)
                     , rulCost r
                     )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Retrieving evidence type for predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
fitPredToEvid :: UID -> Ty -> PrIntroGam -> FIOut
fitPredToEvid u prTy g
  =  case prTy of
       Ty_Any  ->  emptyFO
       _       ->  fPr u prTy
  where  fPr u prTy
            =  case prTy of
                 Ty_Pred p@(Pred_Class _)
                    ->  case gamLookup (predMatchNm p) g of
                           Just pigi
                             -> let (u',u1,u2) = mkNewLevUID2 u
                                    fo = fitsIn fOpts emptyFE u1 (pigiPrToEvidTy pigi) ([prTy] `mkArrow` mkTyVar u2)
                                in  fo {foTy = snd (tyArrowArgRes (foTy fo))}
                           _ -> emptyFO {foErrL = [Err_NamesNotIntrod [tyPredMatchNm prTy]]}
                 Ty_Pred (Pred_Pred t)
                    ->  let  (aL,r) = tyArrowArgsRes t
                             (_,aLr'@(r':aL')) = foldr (\t (u,ar) -> let (u',u1) = mkNewLevUID u in (u',fPr u1 t : ar)) (u,[]) (r : aL)
                        in   manyFO (aLr' ++ [emptyFO {foTy = map foTy aL' `mkArrow` foTy r'}])
         fOpts = predFIOpts {fioDontBind = ftv prTy}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrapper for elimbinds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
mkElimAltsWrap :: FIEnv -> (FIOpts -> UID -> Ty -> Ty -> (Ty,Cnstr,ErrL))
mkElimAltsWrap env
  =  \opt u t1 t2 ->  let  fo = fitsIn opt env u t1 t2
                      in   (foTy fo, foCnstr fo, foErrL fo)
%%]

