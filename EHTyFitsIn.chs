% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

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

%%[1 import(EHCommon, EHTy, EHError) export (fitsIn, FIOut(..), emptyFO)
%%]

%%[2 import(EHCnstr)
%%]

%%[4 import(EHTyInstantiate, EHGam) export(FIOpts(..), strongFIOpts, instFIOpts, instLFIOpts)
%%]

%%[5 export(weakFIOpts)
%%]

%%[6 export(fitsInL)
%%]

%%[9 import(Maybe,FiniteMap,EHPred,EHCode) export(predFIOpts,prfPreds)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- common
%%[FIOpts.4.hd
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst     ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioUniq           ::  UID
                        ,  fioCoContra       ::  CoContraVariance 
%%]

%%[FIOpts.4.tl
                        } deriving Show
%%]

%%[FIOpts.4.strongFIOpts.hd
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst     =   True
                        ,  fioBindLFirst     =   True                ,  fioUniq           =   uidStart
                        ,  fioCoContra       =   CoVariant
%%]

%%[FIOpts.4.strongFIOpts.tl
                        }
%%]

%%[FIOpts.9
                        ,  fioPredAsTy       ::  Bool
%%]

%%[FIOpts.9.strongFIOpts
                        ,  fioPredAsTy       =   False
%%]

-- versions
%%[4.FIOpts
%%@FIOpts.4.hd
%%@FIOpts.4.tl

%%@FIOpts.4.strongFIOpts.hd
%%@FIOpts.4.strongFIOpts.tl
%%]

%%[9.FIOpts -4.FIOpts
%%@FIOpts.4.hd
%%@FIOpts.9
%%@FIOpts.4.tl

%%@FIOpts.4.strongFIOpts.hd
%%@FIOpts.9.strongFIOpts
%%@FIOpts.4.strongFIOpts.tl
%%]

%%[4.FIOpts.defaults
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]

%%[5
weakFIOpts :: FIOpts
weakFIOpts = strongFIOpts {fioLeaveRInst = True, fioBindRFirst = False}
%%]

%%[9
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}
%%]

%%[4
fioSwapVariance :: FIOpts -> FIOpts
fioSwapVariance fio = fio { fioBindRFirst = fioBindLFirst fio, fioBindLFirst = fioBindRFirst fio }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Co/Contra variance Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.CoCo
type CoCoInfo = [(CoContraVariance -> CoContraVariance,FIOpts -> FIOpts)]
data CoCoGamInfo = CoCoGamInfo { ccgiCoCoL :: CoCoInfo }

type CoCoGam = Gam HsName CoCoGamInfo

mkStrong :: FIOpts -> FIOpts
mkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}

cocoGamLookup :: Ty -> CoCoGam -> CoCoInfo
cocoGamLookup ty g
  =  case ty of
       Ty_Con nm  ->  case gamLookup nm g of
                        Just ccgi                ->  ccgiCoCoL ccgi
                        Nothing | hsnIsProd nm   ->  take (hsnProdArity nm) (repeat (id,id))
                        _                        ->  unknownCoCoL
       _          ->  unknownCoCoL
  where unknownCoCoL = repeat (const CoContraVariant,id)
%%]

%%[4.cocoGam
cocoGam :: CoCoGam
cocoGam = assocLToGam [(hsnArrow, CoCoGamInfo [(cocoOpp,mkStrong),(id,id)])]
%%]

%%[7.cocoGam -4.cocoGam
cocoGam :: CoCoGam
cocoGam = assocLToGam
            [ (hsnArrow,    CoCoGamInfo [(cocoOpp,mkStrong),(id,id)])
            , (hsnRec,      CoCoGamInfo [(id,id)])
            ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIIn
data FIIn   =  FIIn     {  fiFIOpts          ::  FIOpts              ,  fiUniq            ::  UID
                        ,  fiCoContra        ::  CoContraVariance
                        }

emptyFI     =  FIIn     {  fiFIOpts          =   strongFIOpts        ,  fiUniq            =   uidStart
                        ,  fiCoContra        =   CoVariant
                        }
%%]

%%[1.FIOut
data FIOut  =  FIOut { foTy     ::  Ty      ,  foErrL   ::  ErrL    }

emptyFO     =  FIOut { foTy     =   Ty_Any  ,  foErrL   =   []      }
%%]

%%[2.FIOut -1.FIOut
data FIOut  =  FIOut  {  foTy     ::  Ty      ,  foCnstr           ::  Cnstr
                      ,  foErrL   ::  ErrL
                      }
%%]

%%[2.FIOut.empty
emptyFO     =  FIOut  {  foTy     =   Ty_Any  ,  foCnstr           =   emptyCnstr
                      ,  foErrL   =   []
                      }
%%]

%%[4.FIOut -(2.FIOut 2.FIOut.empty)
data FIOut  =  FIOut    {  foCnstr           ::  Cnstr               ,  foTy              ::  Ty
                        ,  foUniq            ::  UID                 ,  foCoContraL       ::  CoCoInfo
                        ,  foErrL            ::  ErrL
                        }

emptyFO     =  FIOut    {  foCnstr           =   emptyCnstr          ,  foTy              =   Ty_Any
                        ,  foUniq            =   uidStart            ,  foCoContraL       =   []
                        ,  foErrL            =   []
                        }
%%]

%%[9.FIOut -4.FIOut
data FIOut  =  FIOut    {  foCnstr           ::  Cnstr               ,  foTy              ::  Ty
                        ,  foUniq            ::  UID                 ,  foCoContraL       ::  CoCoInfo
                        ,  foErrL            ::  ErrL
                        ,  foPredOccL        ::  [PredOcc]
                        }

emptyFO     =  FIOut    {  foCnstr           =   emptyCnstr          ,  foTy              =   Ty_Any
                        ,  foUniq            =   uidStart            ,  foCoContraL       =   []
                        ,  foErrL            =   []
                        ,  foPredOccL        =   []
                        }
%%]

%%[1.foHasErrs
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[fitsInHead.1
fitsIn :: Ty -> Ty -> FIOut
fitsIn ty1 ty2
  =  u ty1 ty2
  where
            res t                                   = emptyFO {foTy = t}
%%]

%%[fitsInBotCon.1
            u  Ty_Any               t2              = res t2
            u  t1                   Ty_Any          = res t1
            u  t1@(Ty_Con s1)
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
                 where  ffo  = u tf1 tf2
                        afo  = u ta1 ta2
                        rt   = mkComp (foTy ffo) (foTy afo)
%%]

%%[fitsInapp.2
            comp tf1 ta1 tf2 ta2 mkComp
                 = foldr1  (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)
                           [ffo,afo,rfo]
                 where  ffo  =   u tf1 tf2
                        fs   =   foCnstr ffo
                        afo  =   u (fs |=> ta1) (fs |=> ta2)
                        as   =   foCnstr afo
                        rt   =   mkComp (as |=> foTy ffo) (foTy afo)
                        rfo  =   emptyFO {foTy = rt, foCnstr = as |=> fs}
%%]

%%[fitsInApp.1
            u  t1@(Ty_App (Ty_App (Ty_Con c1) ta1) tr1)
               t2@(Ty_App (Ty_App (Ty_Con c2) ta2) tr2)
                 | hsnIsArrow c1 && c1 == c2
                 = comp ta2 tr1 ta1 tr2 (\a r -> [a] `mkTyArrow` r)
            u  t1@(Ty_App tf1 ta1)
               t2@(Ty_App tf2 ta2)
                 = comp tf1 ta1 tf2 ta2 Ty_App
%%]

%%[fitsInRest.1
            u  t1                   t2              = err [Err_UnifyClash ty1 ty2 t1 t2]
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
            u  t1@(Ty_Var v1)       (Ty_Var v2)     ^^
                 | v1 == v2                         = res t1
            u  t1@(Ty_Var v1)       t2              = occurBind v1 t2
            u  t1                   t2@(Ty_Var v2)  = occurBind v2 t1
%%]

%%[2.fitsIn.AppRest
%%@fitsInApp.1
%%@fitsInRest.1
%%]

%%[3.fitsIn -(2.fitsIn.Base 2.fitsIn.Bind 2.fitsIn.app 2.fitsIn.BotCon 2.fitsIn.Var 2.fitsIn.AppRest)
%%@fitsInHead.1
%%@fitsInBind.2
%%@fitsInapp.2
%%@fitsInBotCon.1
            u  t1@(Ty_Var v1 f1)    (Ty_Var v2 f2)  ^^
                 | v1 == v2 && f1 == f2             = res t1
            u  t1@(Ty_Var v1 f)     t2              
                 | f == TyVarCateg_Plain            = occurBind v1 t2
            u  t1                   t2@(Ty_Var v2 f)
                 | f == TyVarCateg_Plain            = occurBind v2 t1
%%@fitsInApp.1
%%@fitsInRest.1
%%]

%%[4.fitsIn.Prelim -3.fitsIn
fitsIn :: FIOpts -> UID -> Ty -> Ty -> FIOut
fitsIn opts uniq ty1 ty2
  =  fo
  where
            res fi t                = emptyFO  { foUniq = fiUniq fi, foTy = t
                                               , foCoContraL = cocoGamLookup t cocoGam}
            err fi e                = emptyFO {foUniq = fioUniq opts, foErrL = e}
            manyFO fos              = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2) fos
            bind fi tv t            = (res fi t) {foCnstr = tv `cnstrTyUnit` t}
            occurBind fi v t
                | v `elem` ftv t    = err fi [Err_UnifyOccurs ty1 ty2 v t]
                | otherwise         = bind fi v t
%%]

%%[4.fitsIn.unquant
            unquant fi t@(Ty_Quant _ _ _) hide howToInst
                =   let  (u,uq)         = mkNewLevUID (fiUniq fi)
                         (uqt,rtvs)     = tyInst1Quants uq howToInst t
                         back           = if hide  then  \fo ->  let  s = cnstrFilter (const.not.(`elem` rtvs)) (foCnstr fo)
                                                                 in   fo {foCnstr = s, foTy = s |=> t}
                                                   else  id
                    in   (fi {fiUniq = u},uqt,back)
%%]

%%[4.fitsIn.Base
            u fi t1                     t2
                | fiCoContra fi == ContraVariant    = u  (fi  { fiCoContra = CoVariant
                                                              , fiFIOpts = fioSwapVariance (fiFIOpts fi)})
                                                         t2 t1
            u fi Ty_Any                 t2          = res fi t2
            u fi t1                     Ty_Any      = res fi t1
            u fi t1@(Ty_Con s1)         t2@(Ty_Con s2)
                | s1 == s2                          = res fi t2
            u fi t1@(Ty_Var v1 f1)      (Ty_Var v2 f2)
                | v1 == v2 && f1 == f2              = res fi t1
            u fi t1@(Ty_Var v1 f)       t2
                | fioBindLFirst (fiFIOpts fi) && f == TyVarCateg_Plain
                                                    = occurBind fi v1 t2
            u fi t1                     t2@(Ty_Var v2 f)
                | fioBindRFirst (fiFIOpts fi) && f == TyVarCateg_Plain
                                                    = occurBind fi v2 t1
%%]

%%[9
            u fi t1@(Ty_Pred (Pred_Class ct1)) t2@(Ty_Pred (Pred_Class ct2))
                | fioPredAsTy (fiFIOpts fi)         = fo {foTy = Ty_Pred (Pred_Class (foTy fo))} 
                where  fo = u fi ct1 ct2
%%]

%%[4.fitsIn.QLR
            u fi t1@(Ty_Quant q1 _ _)   t2@(Ty_Quant q2 _ _)
                | fiCoContra fi == CoContraVariant && q1 == q2
                                                    = u fi2 uqt1 uqt2
                where  (fi1,uqt1,_) = unquant fi t1 False instCoConst
                       (fi2,uqt2,_) = unquant fi1 t2 False instCoConst
%%]

%%[4.fitsIn.QR
            u fi t1                     t2@(Ty_Quant _ _ _)
                | fiCoContra fi /= CoContraVariant && fioLeaveRInst (fiFIOpts fi)
                                                    = back2 (u fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 False instCoConst
            u fi t1                     t2@(Ty_Quant _ _ _)
                | fiCoContra fi /= CoContraVariant && not (fioLeaveRInst (fiFIOpts fi))
                                                    = back2 (u fi2 t1 uqt2)
                where (fi2,uqt2,back2) = unquant fi t2 True instContra
%%]

%%[4.fitsIn.QL
            u fi t1@(Ty_Quant _ _ _)    t2
                | fiCoContra fi /= CoContraVariant  = u fi1 uqt1 t2
                where (fi1,uqt1,back1) = unquant fi t1 False instCoConst
%%]

%%[4.fitsIn.Var2
            u fi t1@(Ty_Var v1 f)       t2
                | f == TyVarCateg_Plain             = occurBind fi v1 t2
            u fi t1                     t2@(Ty_Var v2 f)
                | f == TyVarCateg_Plain             = occurBind fi v2 t1
%%]

%%[9
            u fi  t1@(Ty_App (Ty_App (Ty_Con c1) (Ty_Pred pr)) tr1)
                  t2@(Ty_App (Ty_App (Ty_Con c2) _) _)
                    | hsnIsArrow c1 && c1 == c2 && not (fioPredAsTy (fiFIOpts fi))
                = fo {foPredOccL = PredOcc pr u1 : foPredOccL fo}
                where  (u',u1) = mkNewUID (fiUniq fi)
                       fo = u (fi {fiUniq = u'}) tr1 t2
%%]

%%[4.fitsIn.App
            u fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,rfo]
                where  ffo  = u fi tf1 tf2
                       fs   = foCnstr ffo
                       ((coUpd,fiUpd):cor) = foCoContraL ffo
                       fi'  = fi  { fiCoContra  = coUpd (fiCoContra fi), fiFIOpts = fiUpd (fiFIOpts fi)
                                  , fiUniq      = foUniq ffo }
                       afo  = u fi' (fs |=> ta1) (fs |=> ta2)
                       as   = foCnstr afo
                       rt   = Ty_App (as |=> foTy ffo) (foTy afo)
                       rfo  = afo {foTy = rt, foCnstr = as |=> fs, foCoContraL = cor}
%%]

%%[9.fitsIn.App -4.fitsIn.App
            u fi t1@(Ty_App tf1 ta1)    t2@(Ty_App tf2 ta2)
                = manyFO [ffo,afo,rfo]
                where  ffo  = u fi tf1 tf2
                       fs   = foCnstr ffo
                       ((coUpd,fiUpd):cor) = foCoContraL ffo
                       fi'  = fi  { fiCoContra  = coUpd (fiCoContra fi), fiFIOpts = fiUpd (fiFIOpts fi)
                                  , fiUniq      = foUniq ffo }
                       afo  = u fi' (fs |=> ta1) (fs |=> ta2)
                       as   = foCnstr afo
                       rt   = Ty_App (as |=> foTy ffo) (foTy afo)
                       rfo  = afo {foTy = rt, foCnstr = as |=> fs, foCoContraL = cor, foPredOccL = foPredOccL afo ++ foPredOccL ffo}
%%]

%%[7
            u fi t1@(Ty_Ext tr1 l1 te1)   t2@(Ty_Ext _ _ _)
                =  case tyRowExtr l1 t2 of
                     Just (r,e)  -> let  tefo  = u fi te1 e
                                         tes   = foCnstr tefo
                                         trfo  = u fi (tes |=> tr1) (tes |=> r)
                                         trs   = foCnstr trfo
                                         rt    = Ty_Ext (foTy trfo) l1 (trs |=> foTy tefo)
                                         rfo   = trfo {foTy = rt, foCnstr = trs |=> tes}
                                    in   manyFO [tefo,trfo,rfo]
                     _           -> err fi [Err_MissingRowLabel l1 t2]
%%]

%%[4.fitsIn.Rest
            u fi t1                     t2          = err fi [Err_UnifyClash ty1 ty2 t1 t2]

            fo  = u (emptyFI {fiUniq = uniq, fiFIOpts = opts, fiCoContra = fioCoContra opts}) ty1 ty2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Subsumption for lists of types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
fitsInL :: FIOpts -> UID -> TyL -> TyL -> (TyL,FIOut)
fitsInL opts uniq tyl1 tyl2
  =  snd
     .  foldr  (\(t1,t2) (u,(ts,fos))
                  -> let  (u',ue) = mkNewLevUID u
                          fo = fitsIn opts u (foCnstr fos |=> t1) (foCnstr fos |=> t2)
                     in   (u',(foTy fo:ts, fos {foCnstr = foCnstr fo |=> foCnstr fos, foErrL = foErrL fo ++ foErrL fos}))
               )
               (uniq,([],emptyFO))
     .  zip tyl1 $ tyl2
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Proof of predicates, must be here because of mutual dep with fitsIn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
matchRule :: UID -> Pred -> Rule -> Maybe ([PredOcc],CExpr,ProofCost)
matchRule u pr r@(Rule rt mkEv _ cost)
  =  let  (_,u1,u2,u3)   = mkNewLevUID3 u
          (rTy,_)        = tyInst1Quants u1 instCoConst rt
          (us,vs)        = mkNewUIDTyVarL (tyArrowArity rTy) u2
          fo             = fitsIn predFIOpts u3 rTy (vs `mkTyArrow` Ty_Pred pr)
     in   if foHasErrs fo
          then Nothing
          else Just  ( zipWith PredOcc (map tyPred . fst . tyArrowArgsRes . foTy $ fo) us
                     , mkEv (map CExpr_Hole us)
                     , cost
                     )

prfOneStep :: PrElimGam -> PredOcc -> ProofState -> ProofState
prfOneStep eGam (PredOcc pr prUid) st@(ProofState g@(ProvenGraph i2n p2i) u toProof)
  =  let  
     in   case lookupFM p2i pr of
            Just uidL | prUid `notElem` uidL
              ->  let  uid = last uidL
                       nd = ProvenAnd pr [uid] (CExpr_Hole uid) 0
                  in   st {prfsProvenGraph = prvgAddPrNd pr (prUid : uidL) nd g}
            Nothing
              ->  case pr of
                    Pred_Class t
                      ->  let  nm = tyAppFunConNm t
                               mkNdFail uid = ProvenArg pr (CExpr_Hole uid) 111
                               ndFail = mkNdFail prUid
                          in   case gamLookupAll nm eGam of
                                 pegis@(_:_)
                                     ->  let  rs = concat . map pegiRuleL $ pegis
                                              (u',u1,u2) = mkNewLevUID2 u
                                              matches = catMaybes . zipWith (\u r -> matchRule u pr r) (mkNewUIDL (length rs) u1) $ rs
                                              mkPrf pr (prOccL,evid,cost) = ProvenAnd pr (map poId prOccL) evid cost
                                              (g',newPr)
                                                 = case matches of
                                                       [] ->  (prvgAddPrNd pr [prUid] ndFail g,[])
                                                       ms ->  let  orUids@(uidFail:uidRest) = mkNewUIDL (length ms + 1) u2
                                                              in   foldr
                                                                       (\(uid,m@(prOccL,_,_)) (g,newPr)
                                                                          -> (prvgAddNd uid (mkPrf pr m) g,prOccL ++ newPr)
                                                                       )
                                                                       (prvgAddPrNd pr [prUid] (ProvenOr pr orUids)
                                                                          (prvgAddNd uidFail (mkNdFail uidFail) g)
                                                                       ,[])
                                                                       (zip uidRest ms)
                                         in   st {prfsUniq = u', prfsProvenGraph = g', prfsPredsToProve = newPr ++ toProof}
                                 []  ->  st {prfsProvenGraph = prvgAddPrNd pr [prUid] ndFail g}
                    _ ->  st
            _ ->  st

prfPreds :: UID -> PrElimGam -> [PredOcc] -> ProvenGraph
prfPreds u eGam prL
  =  let  initState = ProofState (ProvenGraph emptyFM emptyFM) u prL
          resolve st@(ProofState _ _ (pr:prL))
            =  let  st' = prfOneStep eGam pr (st {prfsPredsToProve = prL})
               in   resolve st'
          resolve st@(ProofState _ _ []) = st
     in   prfsProvenGraph (resolve initState)
%%]
