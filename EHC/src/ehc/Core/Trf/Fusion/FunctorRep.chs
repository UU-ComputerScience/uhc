%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.FunctorRep}
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloRep})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloContext})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloFace})
import List
import Maybe(catMaybes)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.RenVars})
import Control.Monad.Error(throwError)
import Control.Monad.State(runState,get,put,State)
import Control.Monad.Trans(lift)
import Control.Monad(zipWithM)
import qualified Data.Map as M(lookup,adjust,insert)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Inline})
--import HsPretty


cargs h1 h2 = setContext (combineContexts (getContext h1) (getContext h2))

remap :: CHylo h => [[(Position,Int)]] -> h a b -> h a b 
remap idxs h = setFunctor (zipWith remapPositions idxs (getFunctor h)) h






getCata :: CHylo h => [h a Psi] -> h a Psi -> FusionState (h a OutF)
getCata hs h' =let (vsm,t0,Psi psis)=getCoalgebra h in
          if length t0 /= 1 || length vsm /= 1 then throwError NotOutF
            else if repitePatron (map (head . getPatterns) psis)|| distinct (head vsm) (head t0) 
                   then throwError NotOutF
                   else do res<-sequence$ zipWith3 analizePsii (getFunctor h) (getEta h) psis
                           let (etas,outF,fncs)=unzip3 res
                           return$ setEta etas.setFunctor fncs.setCoalgebra (vsm,t0,OutF outF)$ h
 where
 h=basicCAR hs h'
 analizePsii fnc etai psii =
   let analizePattern (Pvar v) = return (Right v,[])
       analizePattern    p     = do u<-lift$ getFreshVar "p"
                                    return (Left (u,p),vars p)
       ts = getTerms psii
    in case (head . getPatterns) psii of
        Pcons c ps -> do res<-mapM analizePattern ps
                         let (ps',vs)=unzip res
                             (change,nchange)=partition (flip elem (concat vs).t2v.getTerm) ts
                             vss = map getPosition change
                             (vt0s,pi)=unzip$ getvt0pi ps'
                             t0s=zipWith tupleterm vt0s (map Tvar vt0s)
                             inputs=map (either fst id) ps'
                             applyH p = maybe id (\i->applyHyloWithCtxCntArgs (getContext h) (hs!!i))$ getRecIndex fnc p
                             para p = mapStructure (applyH p (Tvar p)) (Tvar p)$ expanded fnc p
                         return$ (etai `rightCompose` EOsust vss (map para vss) (map (Bvar .getPosition) ts)
                                       `rightCompose` EOlet (map getTerm t0s) pi (vt0s++map getPosition nchange) 
                                                                                 (map (getTermLet change) ts),
                                      OutFc (c,inputs,t0s++nchange),
                                      makePosNR vss fnc)
        _ -> throwError NotOutF
 getvt0pi (Left p:ps) = p:getvt0pi ps
 getvt0pi (_:ps) = getvt0pi ps
 getvt0pi  [] = []
 getTermLet change tt = if elem tt change then getTerm tt else Tvar (getPosition tt)
 distinct (Bvar v) (Tvar v') = v/=v'
 distinct _ _ = True
 repitePatron (Pcons c _:ps) = any (has c) ps || repitePatron ps
 repitePatron [] = False
 repitePatron _ = True
 has c (Pcons c' _) = c==c'
 has c _ = True
 tt2v ts v = filter ((v==).t2v.getTerm) ts
 t2v (Tvar v) = v
 t2v _ = error$ coalgebra_Should_Not_Return_Terms_Diffrent_From_Vars







basicCAR :: CHylo h => [h a b] -> h a Psi -> h a Psi
basicCAR hs h = let (etas,psis',fncs)=unzip3$ zipWith3 analizePsii (getFunctor h) (getEta h) psis
                 in setEta etas.setFunctor fncs.setCoalgebra (vsm,t0s,Psi psis')$ h
 where
 (vsm,t0s,Psi psis)=getCoalgebra h
 analizePsii fnc etai psii =
   let isNonVarRec tt = case getTerm tt of
                           Tvar v -> False
                           Ttuple _ ts | all isVar ts && length ts==length t0s -> False
                           t -> isRec fnc$ getPosition tt
       isVar (Tvar _) = True
       isVar _ = False
       ts = getTerms psii
       (change,nchange)=partition isNonVarRec ts
       pos=map getPosition change++posToNR
       posToNR =  let ts1 = filter (maybe False (const True).fst) $ map (\t->(getRecIndex fnc (getPosition t),t)) (ts\\change)
                      ts2 = nubBy (\(i1,t1) (i2,t2)-> (i1/=i2) && (getTerm t1==getTerm t2)) ts1
                   in map (getPosition.snd) (ts1\\ts2)
       nuevas = nub.concat.map (filter (flip elem (vars$ (head . getPatterns) psii)).vars.getTerm)$ change
       para tt = let p=getPosition tt 
                     t = if elem tt change then getTerm tt else Tvar p
                     applyH = maybe id (\i->applyHyloWithCtxCntArgs (getContext h) (hs!!i))$ getRecIndex fnc p
                  in if elem p pos then mapStructure (applyH t) t$ expanded fnc p
                       else t
       nuevastts = zipWith tupleterm nuevas (map Tvar nuevas)
    in (etai `rightCompose` if null pos then EOid else EOgeneral  (map Bvar$ nuevas++map getPosition nchange) (map para ts),
        setTerms (nuevastts++nchange) psii,
        makePosNR pos fnc)


getAna :: CHylo h => [h Phii ca] -> h Phii ca -> FusionState (h InF ca)
getAna hs h = let phis = getAlgebra h in
               do res<-sequence$ zipWith (\phii fnc->analizePhii (getVars phii) fnc (unwrapA phii)) phis (getFunctor h)
                  let (inF,fncs)=unzip res
                  return$ setFunctor fncs.setAlgebra (zipWith (wrapA.getVars) phis inF)$ h
 where
   analizePhii :: [Boundvar] -> HFunctor -> TermWrapper Phii -> FusionState (TermWrapper InF,HFunctor)
   analizePhii inputs fnc t = do (vs,inF)<-mapTWaccM inputs (analizePhii' fnc) concat [] t
                                 return (inF,makePosNR vs fnc)
   analizePhii' fnc inputs t =
    let analizeTerm (Tvar _) = []
        analizeTerm    t     = foldl (\r (v,m)->maybe r (\i->(v,i):r) m) []$ map (\v->(v,getRecIndex fnc v))$ vars t
     in case t of
         Tcapp c ts -> let pvs=concat$ map analizeTerm ts 
                           vs = map fst pvs
                           eta=(EOgeneral inputs ts) `leftCompose` 
                               EOsust vs (map (\ (v,i)->applyHyloWithCtxCntArgs (getContext h) (hs!!i) (Tvar v)) pvs) inputs `leftCompose` idEta
                        in return (vs,(if isIdEta eta then id else flip TWeta eta) (TWsimple$ InF (c,ts)))
         Tcase t0 ps ts -> do res<-mapM (analizePhii' fnc inputs)$ ts 
                              let (vs,inFs)=unzip res
                              return$ ((concat vs++).filter (isRec fnc).vars$ t0,TWcase t0 ps inFs)
         t -> let pvs = analizeTerm t
                  vs = map fst pvs
                  eta=EOgeneral inputs [t] `leftCompose` 
                      EOsust vs (map (\ (v,i)->applyHyloWithCtxCntArgs (getContext h) (hs!!i) (Tvar v)) pvs) inputs `leftCompose` idEta
               in do u<-case t of Tvar v -> return v; _ -> lift$ getFreshVar "v"
                     return (vs,(if isIdEta eta then id else flip TWeta eta) (TWsimple$ InF ("_",[Tvar u])))

















etaPara :: (Term -> Term) -> HFunctor -> [Boundvar] -> (TupleTerm -> Term) -> [TupleTerm] -> EtaOp
etaPara h fnc input sel output = EOgeneral input (map applyh output)
   where applyh tt = let seltt=sel tt
                      in if any (isRec fnc).t2positions$ tt
                           then mapStructure seltt (h seltt) (expanded fnc (getPosition tt))
                           else mapStructure seltt seltt (expanded fnc (getPosition tt))
         t2positions tt = 
             case getTerm tt of
               Tvar vt -> getPositions output vt
               Ttuple _ ts | all isVar ts -> concat . map (getPositions output) . nub . vars $ ts
               t -> error ("FunctorRep: etaPara: unexpected tuple term: ")
         isVar (Tvar _) = True
         isVar _ = False

fst3 (a,_,_)=a
snd3 (_,a,_)=a
thrd3 (_,_,a)=a





buildParaStructure :: (Int->Term -> Term) -> HFunctor -> [(Variable,(Int,ParaFunctor))] 
                      -> [Variable] -> [TupleTerm] -> EtaOp
buildParaStructure h fnc mvs input output = EOgeneral (map buildInput input) (map buildOutput output)
   where buildInput v = maybe (Bvar v) (buildBv v .snd)$ lookup v mvs
         buildOutput tt = let pf = case expanded fnc (getPosition tt) of
                                      p@(PFcnt _) -> maybe p (maybe (PFcnt vt) id . getInnermostNonRecVar . snd)$ lookup vt mvs
                                      pf' -> pf'
                              t@(Tvar vt)=getTerm tt
                           in maybe (foldPF (const t) (const t) (Ttuple False) pf) (\(i,_)->foldPF Tvar (h i.Tvar) (Ttuple False) pf)$ lookup vt mvs
         buildBv v (PFprod []) = Bvar v
         buildBv _ pf = foldPF Bvar Bvar (Bvtuple False) pf
         getInnermostNonRecVar (PFprod pfl) | all (not . isPFprod) pfl = find isPFcnt pfl
                                            | otherwise = case catMaybes (map getInnermostNonRecVar pfl) of
                                                           p:_ -> Just p
                                                           [] -> Nothing
         getInnermostNonRecVar _ = Nothing
         isPFcnt (PFcnt _) = True
         isPFcnt _ = False
         isPFprod (PFprod _) = True
         isPFprod _ = False




paraMKNR :: (Int->Term->Term) -> HFunctor -> [Position] -> [Boundvar] -> EtaOp
paraMKNR applyHList fnc pos inputs = 
      let applyH p = maybe (Tvar p) (flip applyHList (Tvar p))$ getRecIndex fnc p
          genInputs bv@(Bvar v) | isRec fnc v = foldPF Bvar Bvar (Bvtuple False) (expanded fnc v)
                                | otherwise = bv
          genInputs (Bvtuple b bvs) = Bvtuple b (map genInputs bvs)
          susts p = foldPF (:[]) (const []) concat (expanded fnc p)
          pos' = concat$ map susts pos
       in EOsust pos' (map applyH pos') (map genInputs inputs)

applyHyloList hs h i i' | i==i' = applyHyloWithCtxCntArgs (getContext h) h
                        | otherwise = applyHyloWithCtxCntArgs (getContext h) (hs!!i')

applyHyloListCtx hs h ctx i i' | i==i' = applyHyloWithCtxCntArgs ctx h
                               | otherwise = applyHyloWithCtxCntArgs ctx (hs!!i')

applyHyloWithCtxCntArgs ctx h t = applyHyloWithCntArgs h (map Tvar (getConstantArgs ctx)) (getCntArgPos ctx) t


fusionarSimple :: (CHylo hylo,Vars a, Substitutable a, Vars ca, Substitutable ca, VarsB ca,VarsB a,TermWrappable a,HasComponents ca) => 
                   [hylo a OutF] -> Int -> [hylo InF ca] -> Int -> FusionState (Int,[hylo a ca])
fusionarSimple hs1 i1 hs2 i2 = fusionarSimpleAcc [((i1,i2),0)] [(i1,i2)]
 where 
  fusionarSimpleAcc accfi@((_,acci):_) is = 
     do res<-mapM (\(i1,i2)->do (h1,h2)<-lift$ renameVariables (hs1!!i1) (hs2!!i2) [];fusionarSimple' h1 i1 h2 i2) is
        let (ws,hs,hused) = unzip3 res
            nused = nub [ p |  ll<-hused, l<-ll, (_,p)<-l, all ((/=p).fst) accfi]
            lnused = length nused+acci
            accfi'= zip nused [acci+1..lnused]++accfi
            recmaps = map (map (map (\(v,p)->maybe (error "fusionarSimple: recmaps") (\i->(v,i))$ lookup p accfi'))) hused
            hs'=zipWith remap recmaps hs
         in if null nused then return (sum ws,hs') 
              else do (w,hss)<-fusionarSimpleAcc accfi' nused
                      return (w+sum ws,hs'++hss)
  fusionarSimpleAcc [] is = error$ "fusionarSimpleAcc: unexpected empty request list"
  fusionarSimple' :: (CHylo hylo,HasComponents ca,TermWrappable a) => hylo a OutF -> Int -> hylo InF ca -> Int 
                        -> FusionState (Int,hylo a ca,[[(Position,(Int,Int))]])
  fusionarSimple' h1 i1 h2 i2 = 
            do res<-zipWithM (fusionar lines) inF fncs2
               let (acc,phis,hused)=unzip3 res
                   (matches,fncs2')=unzip acc
               return (sum matches,cargs h1 h2.setAlgebra phis.setFunctor fncs2'$ h2,hused)
   where
    inF=getAlgebra h2
    fncs2 = getFunctor h2
    (_,_,OutF outF)=getCoalgebra h1
    lines= zip4 (getAlgebra h1) (getEta h1) (getFunctor h1) outF
    fusionar :: TermWrappable a => [(Acomponent a,Etai,HFunctor,OutFi)]->Acomponent InF->HFunctor
                  -> FusionState ((Int,HFunctor),Acomponent a,[(Position,(Int,Int))])
    fusionar lines1 inf fnc2 = 
      do let accumCase acc = let (is,ls,hused)=unzip3 acc
                              in (sum is,concat ls,concat hused)
         ((i,mvs,hused),tw)<-mapTWaccM (getVars inf) (buildAlg lines1 fnc2) accumCase (0,[],[]) (unwrapA inf)
         return ((i,expandPositions mvs$ fnc2),wrapA (getVars inf) tw,hused)
    buildAlg :: TermWrappable a => [(Acomponent a,Etai,HFunctor,OutFi)]->HFunctor->[Boundvar]->InF
                                   -> FusionState ((Int,[(ParaFunctor,Position)],[(Position,(Int,Int))]),TermWrapper a)
    buildAlg lines1 fnc2 input2 (InF ("_",[t@(Tvar v)])) 
                     | Just i2<-getRecIndex fnc2 v = return ((1,[],[(v,(i1,i2))]),TWacomp$ wrapA [Bvar v] (TWsimple (wrapTerm t)))
                     | otherwise = return ((0,[],[]),TWacomp$ wrapA [Bvar v] (TWsimple (wrapTerm (applyHyloList hs1 h1 i1 i1 t))))
    buildAlg lines1 fnc2 input2 (InF (c,inFoutput))=
         case find (fbranch c) lines1 of
          Just (phi1,eta1,fnc1,OutFc (_,input1,outFoutput))->
                   do res<-zipWithM (match fnc1 fnc2 outFoutput) input1 inFoutput
                      let (matches,mvs,izqs,hused)=unzip4 res 
                          cizqs=concat izqs
                          cmvs=concat mvs
                          cmvs'=map (\(a,_,i,(pf,_))->(a,(i,pf))) cmvs
                          toVar (Tvar v)=v
                          toVar t = error$ "buildAlg: toVar: unexpected term: "
                          mkNRh1= paraMKNR (applyHyloList hs1 h1 i1) fnc1 cizqs (map (Bvar .getPosition) outFoutput)
                          epara= buildParaStructure (applyHyloList hs2 h2 i2) (makePosNR cizqs fnc1) cmvs' input1 outFoutput
                      return ((sum matches,map (\(_,_,_,a)->a) cmvs,concat hused),
                             phi1 `composeEta` eta1 `rightCompose` mkNRh1 `rightCompose` epara)
          _ -> return ((0,[],[]),bottom)
    fbranch cn (_,_,_,OutFc (cn',_,_)) = cn'==cn
    match :: HFunctor -> HFunctor -> [TupleTerm] -> Variable->Term->
                             FusionState (Int,[(Variable,[Position],Int,(ParaFunctor,Position))],[Position],[(Position,(Int,Int))])
    match fnc1 fnc2 out v t =
     let posv=getPositions out v
      in case t of
        Tvar vt | Just i2<-getRecIndex fnc2 vt ->
                   let hused= map (\i->(vt,(i,i2))) $ catMaybes$ map (getRecIndex fnc1) posv
                       freshConstVar (PFcnt _) = getFreshVar "v" >>= return . PFcnt
                       freshConstVar pf = return pf
                    in do expds<-lift$ mapM freshConstVar$ map (expanded fnc1) posv
                          return (length (filter (isRec fnc1) posv),[(v,posv,i2,(mkpf' expds,vt))],[],hused)
        _ -> return (0,[],posv,[])
    mkpf' [bv] = bv
    mkpf' bvs = PFprod bvs


fusionarTau :: (CHylo h,WrapTau a,HasComponents cb,VarsB cb,Substitutable cb,Vars cb,VarsB a,Substitutable a,Vars a) =>
                [h a OutF] -> Int -> [h Phii cb] -> Int -> IntState (Int,[h Tau cb])
fusionarTau hs1 i1 hs2 i2 = fusionarTauAcc [((i1,i2),0)] [(i1,i2)]
 where 
  fusionarTauAcc accfi@((_,acci):_) is = 
     do res<-mapM fusionarTau' is
        let (ws,hs,hused) = unzip3 res
            nused = nub$ map snd$ concat$ map (concat . map (filter (\(_,pair)->all ((/=pair).fst) accfi))) hused
            lnused = length nused+acci
            accfi'= zip nused [acci+1..lnused]++accfi
            recmaps = map (map (map (\(v,p)->maybe (error "fusionarTau: recmaps") (\i->(v,i))$ lookup p accfi'))) hused
            hs'=zipWith remap recmaps hs
        if null nused then return (sum ws,hs') 
           else fusionarTauAcc accfi' nused >>= (\(w,hss)->return (w+sum ws,hs'++hss))
  fusionarTauAcc [] is = error$ "fusionarTauAcc: unexpected empty request list"
  fusionarTau' (i1,i2) =
   do (h1,h2')<-renameVariables (hs1!!i1) (hs2!!i2) []
      h2<-toPara hs2 h2' i2
      let psitts=getComponentTerms.(\(_,_,a)->a).getCoalgebra$ h2
          lines h1=let (_,_,OutF outF)=getCoalgebra h1 in zip4 (getAlgebra h1) (getEta h1) (getFunctor h1) outF
          linesh=map lines hs1
          phis=getAlgebra h2
          gt eta2 fnc2 phii psitts= 
            do ((matches,ders,hused),tau)<-getTau linesh i1 fnc2 phii
               let mkNRh2=paraMKNR (applyHyloList hs2 h2 i2) fnc2 ders$ map (Bvar .getPosition) psitts
               return (matches,tau,eta2 `rightCompose` mkNRh2,
                       makePosNR ders fnc2,hused)
      res<-sequence$ zipWith4 gt (getEta h2) (getFunctor h2) phis psitts
      let (matches,taus,etas,fncs,hused)=unzip5 res
      st<-get
      return (sum matches,cargs (head hs1) h2.setAlgebra (zipWith (\p t->wrapA (getVars p) (TWsimple .wrapTau$ t)) phis taus).
                                setEta etas.setFunctor fncs$ h2, hused)
  getTau :: [[(a,Etai,HFunctor,OutFi)]]->Int->HFunctor->Acomponent Phii
                              ->IntState ((Int,[Position],  [(Position,(Int,Int))]),TermWrapper (TauTerm a))
  getTau psis ih1 fnc2 phi =
           mapTWvars (getVars' phi) (buildTW psis ih1 fnc2)
                     ((\ (ms,ps,hused)->(sum ms,concat ps,concat hused)).unzip3) (0,[],[]) (unwrapA phi)
  getVars' phi = concat (map two (getVars phi))
  two (Bvtuple False [Bvar a,Bvar b]) = [(a,b)]
  two _ = []
  h1 = hs1 !! i1
  fapp i1 t =Ttuple False [t,applyHyloWithCtxCntArgs (getContext h1) h1 t]
  pi2 t = case t of Taupair _ t2 -> t2
                    Taucata ft tau -> Taucata (pi2'.ft) tau
                    _ -> error "fusionarTau: pi2: No se esperaba el termino."
  pi2' t= case t of Ttuple _ [t1,t2] -> t2;_ -> error "fusionarTau: pi2': No se esperaba el termino."
  buildTW psis ih1 fnc2 pairs t =
     case t of
      Tcase t0 ps ts -> do res<-mapM (buildTW psis ih1 fnc2 pairs) ts
                           let (acc,taus)=unzip res
                               (matches,ders,hused)=unzip3 acc
                           return ((sum matches,concat ders,concat hused),TWcase t0 ps taus)
      _ -> do (acc,tau)<-buildTauTerm psis ih1 fnc2 pairs t; return (acc,TWsimple (pi2 tau))
  buildTauTerm psis ih1 fnc2 pairs t =
     case t of
      Tcapp c ts -> 
          case find (fbranch c) (psis!!ih1) of
           Just (phii,etai,fnc1,ca@(OutFc (_,input,output))) -> 
            do res<-sequence$ zipWith (match fnc1 fnc2 output) input ts
               let (acc,taus)=unzip res
                   (matches,ders,hused)=unzip3 acc
                   inFapply = Tcapp c (map getCnt taus)
               epara<-etaParaTau fnc1 input output
               return ((sum matches,concat ders,concat hused),
                       (Taupair inFapply (Taucons c taus phii (etai `rightCompose` epara)
                       )))
           Nothing -> return ((0,[],[]),Taupair Tbottom (Tausimple Tbottom))
      Tvar v -> return$ maybe ((0,[],[]),Taucata (fapp ih1) (Tausimple t))
                              (\ih2->((1,[],[(v,(ih1,ih2))]),Taupair (Tvar$ replv v) (Tausimple t)))
                      $ getRecIndex fnc2 v 
      _ -> return ((0,filter (isRec fnc2)$ vars t,[]),Taucata (fapp ih1) (Tausimple t))
     where
       replv v = maybe (error (fuse_Tau_Operation_Label ++ ":" ++ {-show pairs++show v++ -} var_Rec_Not_Binded)) 
                       fst (find ((v==).snd) pairs)
       fbranch cn (_,_,_,OutFc (cn',_,_)) = cn'==cn
       match fnc1 fnc2 out v t =
         case find (maybe False (const True))$ map (getRecIndex fnc1) (getPositions out v) of
          Just (Just ih1') -> buildTauTerm psis ih1' fnc2 pairs t
          _ -> return ((0,filter (isRec fnc2)$ vars t,[]),Tausimple t)


getCnt (Tausimple t) = t
getCnt (Taupair t _) = t
getCnt (Taucata _ t) = getCnt t
getCnt _ = error (fuse_Tau_Operation_Label ++ ":" ++ unexpected_Constructed_Pair)

etaParaTau :: HFunctor -> [Variable] -> [TupleTerm] -> IntState EtaOp
etaParaTau fnc input output = 
             do ps<-parear fnc output input
                let selectV (Tvar x) = Tvar .maybe x id.flip lookup ps$ x
                    selectV t = error$ "selectV: unexpected term: "
                return$ etaPara selectV fnc (map (buildInput ps) input) getTerm output
 where -- find which positions must be duplicated, they are paired with fresh variables.
       parear fnc output lst = 
        let parear' v | any (isRec fnc) (getPositions output v) =  do u<-getFreshVar (varPrefix v); return [(v,u)]
                      | otherwise = return []
         in do l<-mapM parear' input
               return (concat l)
       buildInput ps v = maybe (Bvar v) (Bvtuple False .(:[Bvar v]).Bvar) .flip lookup ps$ v


mapTWvars:: [(Variable,Variable)]->([(Variable,Variable)]->a->IntState (c,TermWrapper b))->([c]->c)
             ->c->TermWrapper a->IntState (c,TermWrapper b)
mapTWvars lst f1 f2 f3 = foldTWM (\t0 ps ->(\(cs',ts')-> return (f2 cs',TWcase t0 ps ts')).unzip)
                             (\(c,tw) e-> return (c,TWeta tw e))
                             (f1 lst)
                             (\a->do (c,tw)<-mapTWvars (getPairs$ getVars a) f1 f2 f3.unwrapA$ a
                                     return (c,TWacomp (wrapA (getVars a) tw)))
                             (return (f3,TWbottom))
  where getPairs = concat.map getPair
        getPair (Bvtuple _ [Bvar v1,Bvar v2]) = [(v1,v2)]
        getPair bv = []




toPara :: (CHylo h,HasComponents ca) => [h a ca] -> h a ca -> Int -> IntState (h a ca)
toPara hs h ih = 
           do let (_,_,cas)=getCoalgebra h
              res <- sequence$ zipWith4 splitRecPos (getAlgebra h) (getEta h) (getFunctor h) (getComponentTerms cas)
              let (alg,etas,fncs)=unzip3 res
              return. setAlgebra alg . setEta etas. setFunctor fncs$ h
   where splitRecPos a eta fnc tts = 
                 do (_,TWacomp a',ps)<-handleAcomp a
                    let changeVars = map ((\(PFprod (PFcnt v:_))->v).fst) ps
                        fnc'=expandPositions ps fnc
                        e = EOsust changeVars (zipWith applyMutualHylo (map snd ps) changeVars) (inputList fnc')
                    return (a',eta `rightCompose` e,fnc')
           where inputList fnc' = map (foldPF Bvar Bvar (Bvtuple False) . expanded fnc')  (map getPosition tts)
                 inside :: TermWrapper a -> IntState (Bool,TermWrapper a,[(ParaFunctor,Position)])
                 inside = foldTWM handleCase (\(b,tw,ps) e->return (b,TWeta tw e,ps)) (\a->return (False,TWsimple a,[]))
                                  handleAcomp (return (True,TWbottom,[]))
                 handleCase t0 ps bs = let (bools,tws,pairs)=unzip3 bs
                                        in return (and bools,TWcase t0 ps tws,concat pairs)
                 handleAcomp :: Acomponent a -> IntState (Bool,TermWrapper a,[(ParaFunctor,Position)])
                 handleAcomp a = 
                    do (b,tw,ps)<-inside (unwrapA a)
                       if b then return (b,TWacomp (wrapA (getVars a) tw),ps)
                            else do res<-mapM pair (getVars a)
                                    let (bvs,pairs)=unzip res
                                    return (True,TWacomp (wrapA bvs tw),concat pairs++ps)
                 pair bv@(Bvar v) 
                        | isRec fnc v = do u<-getFreshVar (varPrefix v)
                                           return (Bvtuple False [Bvar u,bv],[(PFprod [PFcnt u,PFid v],v)])
                        | otherwise = return (Bvar v,[])
                 pair _ = error (fuse_Tau_Operation_Label ++ ":" ++ " toPara: " ++ unexpected_Non_Variables)
                 applyMutualHylo p v = maybe (Tvar v) (\i->applyHyloList hs h ih i (Tvar v))$ getRecIndex fnc p






data STfl = STfl {ivar::GenDict,matches::Int}

deriveSigmaPatterns :: Int -> Int -> [(PatternS,[TupleTerm],HFunctor)] 
                               -> [[(Acomponent InF,HFunctor)]] -> IntState (Int,[PatternS],[[((Position,Int),(Int,Int))]])
deriveSigmaPatterns ih2 ia pts inF = 
        do i<-get 
           let (pshused,STfl i' matches) = runState (mapM (derivePattern' inF)$ pts) (STfl i 0)
           let (ps,hused) = unzip pshused
           put i'
           return (matches,ps,concat hused)
  where getFreshVar pr = do k<-get
                            case M.lookup pr (ivar k) of
                              Just i -> put (k {ivar=M.adjust (+1) "v" (ivar k)}) >> return (Vgen pr i)
                              _ -> put (k {ivar=M.insert "v" 1 (ivar k)}) >> return (Vgen pr 0)
        derivePattern' inF (Ppattern v p,tts,fnc1) = derivePattern ih2 v inF p tts fnc1 Pdone
        derivePattern' inF  (p,_,_) = error$ "deriveSigmaPatterns: derivePattern': unexpected pattern: "
        derivePattern :: Int -> Variable -> [[(Acomponent InF,HFunctor)]] -> Pattern -> [TupleTerm]
                                 -> HFunctor -> PatternS -> State STfl (PatternS,[[((Position,Int),(Int,Int))]])
        derivePattern ih2 t0 inF (Pcons c ps) tts fnc1 t = 
           do let cocs = collectC c (inF!!ih2)
              args<-mapM generateVars ps
              let ps' = map removePas ps
              ts<- mapM (buildAlt (zip args ps')) cocs
              let (ts',hused)=unzip (concat ts)
              return (PcaseR ih2 t0 c args ts',concat hused)
         where 
           generateVars (Pvar v) | not (isPany v) = getFreshVar (varPrefix v)
           generateVars (Pas v _) | not (isPany v) = getFreshVar (varPrefix v)
           generateVars _ = getFreshVar "v"
           isPany (Vuserdef "_") = True
           isPany _ = False
           removePas (Pas _ p) = p
           removePas p = p
           buildAlt args (inFs,fnc2) = mapM (checkArg args fnc2) $ inFs
           checkArg args fnc2 (InF (c,ts)) = 
               do (t,izqs,hused)<-foldrM (buildCase2 fnc2) (t,[],[[]]) (zip args ts)
                  return ((t,izqs),hused)
           getr fnc2 t = case t of (Tvar vt) -> getRecIndex fnc2 vt; _ -> Nothing
           getrecindex fnc2 (Tvar vt) = getRecIndex fnc2 vt
           getrecindex _ t = error$ "getrecindex: unexpected term: "
           buildCase2 :: HFunctor -> ((Variable,Pattern),Term) ->
                            (PatternS,[Variable],[[((Position,Int),(Int,Int))]]) -> 
                                 State STfl (PatternS,[Variable],[[((Position,Int),(Int,Int))]]) 
           buildCase2 fnc2 ((u,p),t) (sigOk,nrec,hused0) =
            let vp = vars p in
             maybe (return (PcaseS u p sigOk, u:nrec, hused0))
                   (\i2-> do (t',hused1)<-derivePattern i2 u inF p tts fnc1 sigOk
                             return (t',nrec, [ l1++l2 | l1<-hused0, l2<-hused1]))
                   $ getr fnc2 t
        derivePattern ih2 t0 inF p@(Pvar vp) tts fnc1 t =
               let pos = getTupletermsWithArgIndexes tts vp
                   ipos = [ (i,ppos) | ppos@(tt,_)<-pos, Just i<-[getRecIndex fnc1 (getPosition tt)], isValidRecArg fnc1 vp tt ]
                in if null ipos then return (PcaseSana ih2 t0 p t,[[]])
                     else do k<-get
                             put (k {matches=matches k+length ipos})
                             return (PcaseS t0 p t,[[ ((getPosition tt,i1),(ia,ih2)) | (i1,(tt,ias))<-ipos, ia<-ias ]]) 
        derivePattern ih2 t0 inF p _ _ t = return (PcaseSana ih2 t0 p t,[[]])
        isValidRecArg fnc1 vp tt = all (isValidInPos vp)$ (termToList (getTerm tt))
        isValidInPos vp (Tvar _) = True
        isValidInPos vp t = notElem vp (vars t)
        termToList (Ttuple _ ts) = ts
        termToList t = [t]


collectC :: Constructor -> [(Acomponent InF,HFunctor)] -> [([InF],HFunctor)]
collectC c inFs = map collect' inFs
 where collect' :: (Acomponent InF,HFunctor) -> ([InF],HFunctor)
       collect' (inF,fnc2) = (collect'' inF,fnc2)
       collect'' :: Acomponent InF -> [InF]
       collect'' = foldTW (\_ _ is->concat is) const isC collect'' [] . unwrapA
       isC i@(InF (c',_)) | c==c' = [i]
                          | otherwise = []



countCases :: PatternS -> Int
countCases p =
  case p of
   Pdone -> 1
   PcaseS t0 p t -> countCases t
   PcaseSana i t0 p t -> countCases t
   PcaseR i t0 c vrs ps -> sum$ map (countCases . fst) ps
   _ -> error$ "FunctorRep: countCases: "



getSigma :: (CHylo h, HasComponents b, TermWrappable a ) => [h a ca] -> [[(Acomponent InF,HFunctor)]] -> 
                         h a Sigma -> Int -> Int -> [h InF b] -> h InF b -> Int -> [(Acomponent a,Etai,HFunctor)] ->
                         FusionState (Int,Coalgebra Sigma,[(Acomponent a,Etai,HFunctor)],[[((Position,Int),(Int,Int))]])
getSigma hs1 inF h1 ih1 ia hs2 h2 ih2 as = 
     do 
        let etas2 h2=zipWith3 applypara (getEta h2) (getFunctor h2).getComponentTerms.(\(_,_,ca)->ca).getCoalgebra$ h2
            applypara etai2 fnc2 tts = 
              let para2 p = mapStructure (Tvar p) (Tvar p)$ expanded fnc2 p
                  pos=map getPosition tts
               in etai2 `rightCompose` EOgeneral (map Bvar pos) (map para2 pos)
        v0s'' <-lift$ mapM regenVars v0s'
        (pss,tts',fcn1s',as',casemap)<-addInFNoConstructorCase (pss'!!ia) pss' tts (getFunctor h1) as casemap' h2
        (matches,sps,hused)<-lift$ deriveSigmaPatterns ih2 ia (zip3 (pss!!ia) tts' fcn1s') inF
        let t0t = bv2term (recbvtuple v0s'')
            caseCounts = map countCases$ sps
            joined = replicateList (replicateList casemap caseCounts) (zip as' (replicateList casemap (zip (transpose pss) tts')))
            (psb',psa') = splitAt ia pss
            (hsb',hsa') = splitAt ia hss
        return$ (matches,(v0b++recbvtuple v0s'':tail v0a,t0b++t0t:tail t0a,Sigma (zipWith (*) casemap caseCounts,tts',
                               transpose (psb'++sps:(tail psa')),
                               hsb'++ 
                                 Just (ih2,getAlgebra h2,etas2 h2,wrapSigma (getCoalgebra h2), applyHyloWithCtxCntArgs (getContext h2).(hs2!!)) 
                                 : tail hsa'
                               )),
                 map fst joined, hused)
  where (v0,t0s,Sigma (casemap',tts,pss'',hss))=getCoalgebra h1
        (v0b,v0a) = break ((head t0a==) . bv2term) v0
        (t0b,t0a) = splitAt ia t0s
        (v0s',_,_) = getCoalgebra h2
        pss' = transpose pss''
        regenVars (Bvar v) = getFreshVar (varPrefix v) >>= return . Bvar
        regenVars (Bvtuple b bvs) = mapM regenVars bvs >>= return . Bvtuple b
        recbvtuple [bv] = bv
        recbvtuple bvs = Bvtuple True bvs
        addInFNoConstructorCase ps@(Ppattern u0 _:_) pss tts fcn1s as casemap h2 | not (null inFs) =
                if all noNestedCons ps then
                  do us<-lift$ zipWithM getPpatternVar t0s pss
                     u1<-lift$ getFreshVar (varPrefix u0)
                     let acomp = wrapA [Bvar u1] (TWsimple (wrapTerm (Tvar u1)))
                         fnc = HF [(u1,ih1,map (:[]) [0..length us-1],PFid u1)]
                         eqPRow (Ppattern _ (Pcons _ _),_,prow,_,_,_) (Ppattern _ (Pcons _ _),_,prow',_,_,_) = 
                                             and$ zipWith matchPattern (take ia prow) (take ia prow')
                         eqPRow _ _ = False
                         matchPattern p0 p1 = nullPatternS p0 == nullPatternS p1
                         to5 (_,a,b,c,d,e) = (a,b,c,d,e)
                         ignorePattern (Ppattern _ _) u = Ppattern u (Pvar u)
                         ignorePattern _ u = PcaseS u (Pvar u) Pdone
                         addInFCons ls@((Ppattern _ (Pcons _ _),_,psrow,_,_,_):_) = 
                             map to5 ls ++ [((acomp,Etai ([],[]),fnc),
                                    take ia psrow++ Ppattern u0 (Pcons "_" [Pvar u1]) : drop (ia+1) (zipWith ignorePattern psrow us),
                                    [tupleterm u1 (Ttuple True$ zipWith (\i u->Tvar (if i==ia then u1 else u)) [0..] us)],
                                    fnc,1)]
                         addInFCons ls = map to5 ls
                         (as',pss',tts',fcn1s',casemap') = unzip5$ concat$ map addInFCons$ 
                                                            groupBy eqPRow (zip6 (pss!!ia) as (transpose pss) tts fcn1s casemap)
                     return (transpose pss',tts',fcn1s',as',casemap')
                else throwError NotInF
           where inFs = filter (not.null.fst)$ collectC "_" (zip (getAlgebra h2) (getFunctor h2))
                 noNestedCons (Ppattern _ (Pcons _ ps)) = all isVar ps
                 noNestedCons (Ppattern _ (Pvar _)) = True
                 noNestedCons (Ppattern _ p) = False
                 noNestedCons _ = False
                 isVar (Pvar _) = True
                 isVar _ = False
                 getPpatternVar (Tvar v) _ = return v
                 getPpatternVar _ (Ppattern v _:_) = return v
                 getPpatternVar _ (PcaseR _ v _ _ _:_) = return v
                 getPpatternVar _ (PcaseS v _ _:_) = return v
                 getPpatternVar _ (PcaseSana _ v _ _:_) = return v
                 getPpatternVar _ _ = getFreshVar "v"
        addInFNoConstructorCase ps pss tts fnc1s as casemap _ = return (pss,tts,fnc1s,as,casemap)

replicateList :: [Int] -> [a] -> [a]
replicateList is = concat . zipWith replicate is

nullPatternS :: PatternS -> PatternS
nullPatternS (Ppattern _ p) = Ppattern (Vuserdef "") (nullPVariables p)
nullPatternS (PcaseR _ _ c _ alts) = PcaseR 0 (Vuserdef "") c [] (map (\(p,_)->(nullPatternS p,[])) alts)
nullPatternS (PcaseS _ p pts) = PcaseS (Vuserdef "") (nullPVariables p) (nullPatternS pts)
nullPatternS (PcaseSana _ _ p pts) = PcaseSana 0 (Vuserdef "") (nullPVariables p) (nullPatternS pts)
nullPatternS Pdone = Pdone

nullPVariables (Pvar _) = Pvar (Vuserdef "")
nullPVariables p@(Plit _) = p
nullPVariables (Pcons c ps) = Pcons c (map nullPVariables ps)
nullPVariables (Ptuple ps) = Ptuple (map nullPVariables ps)
nullPVariables (Pas _ p) = Pas (Vuserdef "") (nullPVariables p)




etaParaSigma :: [Variable] -> [Boundvar] -> Int -> Int -> [(Position,Int)] -> (Int -> Term -> Term) -> 
                  HFunctor -> HFunctor -> [TupleTerm] -> IntState EtaOp
etaParaSigma bs v0s ia defaultIndex vis h fncOld fnc tts = 
                do vts' <- if nr>1 then mapM (expandHyloMutipleArgs fncOld fnc . getPosition) tts
                             else return$ map (Bvar . getPosition) tts
                   -- variables which are recursive with respect to the ia argument
                   let rs = (vars (v0s!!ia) \\ bs) ++ (concat$ map (recvars ia) tts)
                   return$ EOgeneral vts' (zipWith (applyh rs) vts' tts)
   where nr = length v0s
         recvars ia tt = vars$ map (\(i,_)->filter isVar [termToList (getTerm tt)!!i])$ filter (elem ia.snd)$ zip [0..]$
                               getArgIndexes fnc (getPosition tt) 
         applyh :: [Variable] -> Boundvar -> TupleTerm -> Term
         applyh recs bv tt = 
            let seltt = bv2term bv
                ih = maybe defaultIndex snd$ find (recPosFor tt) vis
             in if (not . null . intersect recs . vars . getTerm$ tt)
                  then mapStructureBv bv2term (buildHyloApp (h ih) recs (getTerm tt)) bv$ expanded fnc (getPosition tt)
                  else mapStructure seltt seltt (expanded fnc (getPosition tt))
         isVar (Tvar _) = True
         isVar _ = False
         expandHyloMutipleArgs fncOld fnc1 v 
                | isRec fncOld v = let expArgs v = do bvs<-sequence$ replicate nr (getFreshVar (varPrefix v)) 
                                                      return$ bvtuple (map Bvar bvs)
                                  in foldPFM (return.Bvar) expArgs (return.Bvtuple False)$ expanded fnc1 v
                | otherwise = return$ Bvar v
         buildHyloApp h recs (Tvar v) (Bvar bv) | elem v recs = h (Tvar bv)
         buildHyloApp h recs (Ttuple b ts) (Bvtuple _ bvs) = Ttuple b (zipWith (buildHyloApp h recs) ts bvs)
         buildHyloApp h recs _ bv = bv2term bv
         termToList (Ttuple _ ts) = ts
         termToList t = [t]
         recPosFor tt (v,_) = any (elem v) (map (getPositions tts) (vars (getTerm tt)))
         mapStructureBv :: (Boundvar->Term) -> (Boundvar->Term) -> Boundvar -> ParaFunctor -> Term
         mapStructureBv fid fconst bv (PFcnt _) = fconst bv
         mapStructureBv fid fconst bv (PFid _) = fid bv
         mapStructureBv fid fconst (Bvtuple b bvs) (PFprod pfs) = Ttuple b$ zipWith (mapStructureBv fid fconst) bvs pfs
         mapStructureBv fid fconst bv pf = error$ "etaParaSigma: internal structures do not match: "


psiToSigma :: CHylo hylo => [hylo a Psi] -> FusionState [hylo a Sigma]
psiToSigma hs = mapM (psiToSigma' hs) hs
 where psiToSigma' hs1 h1 =
         let (v0s,t0s,Psi psis)=getCoalgebra h1
             pss = map getPatterns psis
             tts = map getTerms psis
          in do pss'<-zipWithM pattern2Ppattern t0s (transpose pss)
                return$ setCoalgebra (v0s,t0s,Sigma (map (const 1) tts,tts,transpose pss',map (const Nothing) v0s)) h1
       pattern2Ppattern (Tvar t0) ps = mapM (return . Ppattern t0) ps
       pattern2Ppattern _ ps = lift (getFreshVar "v") >>= \u-> mapM (return . Ppattern u) ps

fusionarSigma :: (CHylo hylo,HasComponents ca,VarsB ca, Substitutable ca, Vars ca,Vars a,TermWrappable a, VarsB a,Substitutable a) => 
                 [hylo a Sigma] -> Int -> Int -> [hylo InF ca] -> Int -> FusionState (Int,[hylo a Sigma])
fusionarSigma hs1 i1 ia' hs2 i2 = fusionarSigmaAcc [((i1,[(ia,i2)]),0)] [(i1,[(ia,i2)])]
 where
  (v0s',t0s',_)=getCoalgebra (hs1!!i1)
  ia = max 0 (min (length v0s'-1) ia')
  fusionarSigmaAcc accfi@((_,acci):_) is = 
     do res<-mapM fusionarSigma' is
        let (ws,hs,hused) = unzip3 res
            hused' :: [[[(Position,(Int,[(Int,Int)]))]]]
            hused' = map (map joinHylos) hused 
            nused = nub$ map snd$ concat$ map (concat . map (filter (\(_,pair)->all ((/=pair).fst) accfi))) hused'
            lnused = length nused+acci
            accfi'= zip nused [acci+1..lnused]++accfi
            recmaps = map (map (map (\(v,p)->maybe (error "fusionarSigma: recmaps") (\i->(v,i))$ lookup p accfi'))) hused'
            hs'=zipWith remap recmaps hs
        if null nused then return (sum ws,hs')
           else fusionarSigmaAcc accfi' nused >>= (\(w,hss)->return (w+sum ws,hs'++hss))
  fusionarSigmaAcc [] is = error$ "fusionarSigmaAcc: unexpected empty request list"
   -- join requests for fusion of the same hylo with different arguments
  joinHylos us@(((v,ih1),_):_) = (v,(ih1,sort$  map snd us')) : joinHylos uss
        where (us',uss) = partition ((==v).fst.fst) us
  joinHylos [] = []
  fusionarSigma' (i1,is) = 
        do (s,h,hused) <- foldrM (fusionarSigma'' i1) (0,hs1!!i1,repeat []) is
           h' <- lift$ updateHylo h hused
           return (s,cargs h' (head hs2) h',hused)
    where updateHylo h hused = let (v0,_,Sigma (casemap,tts,pss,_)) = getCoalgebra h
                                   reptts = replicateList casemap tts
                                   (etas1,fncs) = unzip$ zipWith3 (paraMKNRSigma hs1 (hs1!!i1) (getContext h) i1)
                                                                  (map (nub.map (fst.fst)) hused)
                                                                  (getFunctor h) reptts
                                   updPara :: [HFunctor] -> (Int,Int) -> [[EtaOp]] -> IntState [[EtaOp]]
                                   updPara fncs (ia,ih2) etas = 
                                               do etas'<-mapM (upd ia ih2 v0)$ zip5 (getFunctor h) fncs 
                                                                                   (map (nub . map (\((a,_),(_,d))->(a,d))) hused)
                                                                                   (replicateList casemap pss)
                                                                                   reptts
                                                  return$ zipWith (:) etas' etas
                                in do etas2<-foldrM (updPara fncs) (replicate (length fncs) []) is
                                      return$ setEta (zipWith3 composeEtas (getEta h) etas1 etas2)$ setFunctor fncs h
          upd ia ih2 v0 (fnc1,fnc1',indexes,ps,tts) = 
                                  etaParaSigma (vars ps) v0 ia ih2 indexes (applyHyloList hs2 (hs2!!ih2) ih2) fnc1 fnc1' tts
          composeEtas eta e1 es2 = foldl rightCompose (eta `rightCompose` e1) es2
          paraMKNRSigma :: CHylo h => [h a ca] -> h a ca -> Context -> Int -> [Position] -> HFunctor -> [TupleTerm] -> (EtaOp,HFunctor)
          paraMKNRSigma hs1 h1 ctx ih1 recs fnc1 tts = 
              let vts = map getPosition tts
                  izqs = nub (vts\\recs)
               in (paraMKNR (applyHyloListCtx hs1 h1 ctx ih1) fnc1 izqs (map Bvar vts), makePosNR izqs fnc1)
  fusionarSigma'' i1 (ia',i2) (s,h1'',oldhused) = 
    do let h2'' = hs2!!i2
       (h1,h2')<-lift$ renameVariables h1'' (setContext emptyContext h2'') (getConstantArgs (getContext h2''))
       let h2 = setContext (getContext h2'') h2'
           ia = max 0 (min (length v0s-1) ia')
           --h1=basicCARSigma hs1 ia h1'
           (v0s,t0s,Sigma (_,_,pss,_))=getCoalgebra h1
           fncs1=getFunctor h1
           etas1=getEta h1
           alg1=getAlgebra h1
           lines1=zip3 alg1 etas1 fncs1
           iabv = v0s!!ia
       if all (\(t,i)-> i/=ia && null (intersect (vars iabv) (vars t)) 
                        || i==ia && bv2term iabv==t0s!!ia) (zip t0s [0..])
          && all (isPpattern . (!!ia)) pss then
         do (matches,caS,branches,hused)<-getSigma hs1 inF h1 i1 ia hs2 h2 i2 lines1
            let (alg1',etai1',fncs1')=unzip3 branches
            return$ (matches+s,setContext (getContext h1)$ consHylo alg1' etai1' fncs1' caS,
                               zipWith (++) hused oldhused)
        else throwError NotSigma
  inF=map (\h2->zip (getAlgebra h2) (getFunctor h2)) hs2
  isPpattern (Ppattern _ _) = True
  isPpattern _ = False

instance Vars PatternS where
  vars (PcaseS t0 pat termS) =  vars termS ++ vars pat
  vars (PcaseSana _ t0 pat termS) =  t0 : vars termS ++ vars pat
  vars (PcaseR _ t0 _ vrs ts) =  t0 : vrs ++ concat (map (vars.fst) ts)
  vars (Ppattern v p) =  vars p
  vars Pdone = []

%%]