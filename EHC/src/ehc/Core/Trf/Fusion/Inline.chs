  [(8 codegen) hs module {%{EH}Core.Trf.Fusion.Inline} export (inline, showHylo, ShowDocA, CoalgebraPrintable, applyHyloWithCntArgs)

%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.Inline} export (inline, applyHyloWithCntArgs)
import List
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloRep})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloContext})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.RenVars})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsPretty})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
import Data.Char(isAlpha,isLower,isDigit)
import Control.Monad.State(get,put,State(State),runState,evalState)
import Control.Monad.Reader(Reader(..),ask,local)
import Control.Monad.Identity(Identity(..))
import qualified Data.Map as M(lookup)






class Inlineable a where
 getInline :: [Term] -> a -> Term





class CAInlineable c where
 caGetInline :: [Term] -> Term -> Coalgebra c -> IntState Term












instance CAInlineable Psi where
 caGetInline ts arg (bv,t0,Psi psis) 
           | null ts = return$ delCases$ substitution (zipTree [bvtuple bv] [arg])$ Tcase (toTerm t0) ps (map getCaTerm psis)
           | otherwise = return$ delCases$ substitution (zipTree [bvtuple bv] [arg])$ Tcase (toTerm t0) ps ts
  where getCaTerm =Ttuple False . map getTerm.getTerms
        ps=map (toPattern . getPatterns) psis
        eq (Tvar v') v = (v'==v)
        eq _ _ = False
        toTerm [t] = t
        toTerm ts = Ttuple False ts
        toPattern [p] = p
        toPattern ps = Ptuple ps



instance CAInlineable Sigma where
 caGetInline ts arg (bv,t0s,coalgebra) =
           do t<-inlineSigma t0s coalgebra ts
              return . substitution (zipTree [bvtuple bv] [arg]) . delCases$ t

splitList :: [a] -> [Int] -> [[a]]
splitList ls (i:is) = ls' : splitList lss is
  where (ls',lss) = splitAt i ls
splitList ls [] = []

insertRecvarCases :: [Term] -> [[PatternS]] -> Term -> Term
insertRecvarCases ts pss t = foldr insertCase t (zip ts (head pss))
  where insertCase (t0t@(Tvar t0),p) t 
                        | t0/=v = Tcase t0t [Pvar v] [t]
                        | otherwise = t
           where v = pvar p
        insertCase (t0,p) t = Tcase t0 [Pvar (pvar p)] [t]

matchSigmaTerms :: [Term] -> [[PatternS]] -> [(Variable,Term)]
matchSigmaTerms ts pss = zipWith matchCase ts (head pss)
  where matchCase t p = (pvar p,t)

pvar :: PatternS -> Variable
pvar (Ppattern v _) = v
pvar (PcaseR _ v _ _ _) = v
pvar (PcaseSana _ v _ _) = v
pvar (PcaseS v _ _) = v
pvar Pdone = error$ "pvar: unexpected PatterS: Pdone"

zipTree :: [Boundvar] -> [Term] -> [(Variable, Term)]
zipTree bvs ts = maybe (error ("zipTree: No coinciden las listas." {- ++show bvs++show ts -})) id$ zipTree' bvs ts


data InlST = InlST {fterms::[(Constructor,[[Term]->IntState Term])],gi::GenDict}

inlineSigma :: [Term] -> Sigma -> [Term] -> IntState Term
inlineSigma t0s (Sigma (casemap,ts,pss,hss)) ts' = 
          do t<-inlineTHS (reorganizeSigma$ weaveTermS (splitList initialTerms casemap)$ pss)
             return$ insertRecvarCases t0s pss t
 where inlineTHS t = inlineTermS inlWCA (\ia ih -> lookupfapp ih (hss!!ia)) (matchSigmaTerms t0s pss) t
       getCaTerm = Ttuple False . map getTerm
       initialTerms | null ts' = map getCaTerm ts
                    | otherwise = ts'
       inlWCA :: Int -> Int -> [(Variable,Term)] -> Variable -> 
                 (Acomponent InF->(Term->Term,[Term])->State InlST Term)->State InlST Term
       inlWCA ia ih sust t0 f = 
                        do let Just (_,inFs,etas,wca,fapp) = hss!!ia
                           k<-get
                           let (wca',i')=runState (renamePatternVars wca) (gi k)  -- renombrar variables de patrones
                           put (k {gi=i'})
                           ts<-sequence$ zipWith f inFs (inlEtas wca' sust etas)
                           k<-get
                           let (t,i')=runState (inlCA wca' (substitution sust (Tvar t0)) ts) (gi k)
                           put (k {gi=i'})
                           return t
       inlEtas wca sust etas = zipWith reduceTrans (map (map (substitution sust . getTerm))$ getComponentTerms wca) etas
       inlCA (WCApsi c) t0 ts = caGetInline ts t0 c
       inlCA (WCAoutF c) t0 ts = caGetInline ts t0 c
       inlCA (WCAsigma c) t0 ts = caGetInline ts t0 c
       lookupfapp i (Just (_,_,_,_,fapp)) = fapp i
       lookupfapp i _ = error "lookupfapp: unexpected case"
inlineTermS inlWCA fapp sust t =
   case t of 
    TtermS t -> return t
    TcaseS t0 p t a -> do t'<-inlS t
                          a'<-inlS a
                          return$ Tcase (substitution sust (Tvar t0)) (conda a p pany) (conda a t' a')
    TcaseSana ia ih t0 p t a -> do t'<-inlS t
                                   a'<-inlS a
                                   return$ Tcase (fapp ia ih (substitution sust (Tvar t0)))
                                                 (conda a p pany) (conda a t' a')
    TcaseR ia ih t0 ps a -> do let fts'=map inlAlt' ps
                               a'<-inlS a
                               i<-get
                               let (res,k)=runState (inlWCA ia ih sust t0$ inlA (inlTermS a'))$ InlST fts' i
                               put (gi k)
                               return res
    TbottomS -> return$ Tbottom
 where conda TbottomS t1 t2 = [t1]
       conda _ t1 t2 = [t1,t2]
       inlS = inlineTermS inlWCA fapp sust
       inlAlt' (c,vrs,ts) = (c,map (inlAlt vrs) ts)
       inlAlt :: [Variable] -> (TermS,[Variable]) -> [Term] -> IntState Term
       inlAlt vrs (t,nrs) ts = do let (recs,nrecs) = partition (flip notElem nrs.fst)$ zip vrs ts
                                  t'<-inlineTermS inlWCA fapp (sust++recs) t
                                  return (foldr (\(u,t0) ti->Tcase t0 [(Pvar u)] [ti]) t' nrecs)
       inlTermS a (InF (c',_)) ts sust = 
              do k<-get 
                 case lookup c' (fterms k) of
                  Just fts -> do k<-get
                                 let (t,i')=runState (head fts ts) (gi k)
                                 put (k {fterms=inlstupd c' (tail fts) (fterms k),gi=i'})
                                 return t
                  Nothing -> return a
       inlstupd c l [] = []
       inlstupd c l (h@(c',_):as) | c==c' = (c,l):as
                                  | otherwise = h: inlstupd c l as




data TermS = TcaseS Variable Pattern TermS TermS



           | TcaseSana Int Int Variable Pattern TermS TermS






           | TcaseR Int Int Variable [(Constructor,[Variable],[(TermS,[Variable])])] TermS







           | TtermS Term



           | TbottomS



--      deriving Show











weaveTermS :: [[Term]] -> [[PatternS]] -> TermS
weaveTermS tss pss = foldr (\(ps,ts) tb -> fst$ runState (toTermS 0 ps tb) ts) TbottomS (zip pss tss)
  where toTermS :: Int -> [PatternS] -> TermS -> State [Term] TermS
        toTermS i (p:ps) tb = toTermS' i p (toTermS (i+1) ps tb) tb
        toTermS _ [] tb = do ts<-get
                             put (tail ts)
                             return$ TtermS (head ts)
        toTermS' :: Int -> PatternS -> State [Term] TermS -> TermS -> State [Term] TermS
        toTermS' ia (PcaseS t0 p t1) t tb = 
                        do t'<-toTermS' ia t1 t tb
                           return$ TcaseS t0 p t' tb
        toTermS' ia (PcaseSana ih t0 p t1) t tb = 
                        do t'<-toTermS' ia t1 t tb
                           return$ TcaseSana ia ih t0 p t' tb
        toTermS' ia (PcaseR ih v c vrs []) t tb = return tb
        toTermS' ia (PcaseR ih v c vrs ts) t tb = 
                        do ts' <- mapM (toTermSalts' ia t tb) ts
                           return$ TcaseR ia ih v [(c,vrs,ts')] tb
        toTermS' ia (Ppattern v p) t tb = t >>= \t -> return$ TcaseS v p t tb
        toTermS' ia Pdone t _ = t
        toTermSalts' ia t tb (t',recs) = toTermS' ia t' t tb >>= \r-> return (r,recs)













type NodeDown = (Variable,Either (Maybe (Constructor,Int),[(Constructor,[Variable],[(TermS,[Variable])])])
                                  (Pattern,Bool))






type NodeUp = (Variable,[(Constructor,[Variable],[(TermS,[Variable])])])

reorganizeSigma :: TermS -> TermS
reorganizeSigma = snd . rs [] []
 where rs :: [NodeDown] -> [Either (TermS->TermS) Variable] -> TermS -> ([NodeUp],TermS)
       rs ns _ t@(TtermS _) = ([],t)
       rs ns ctx (TcaseS t0 p t a) | not (isRefusable p) =
                        let (adds',t') = rs ns (Left (\t->TcaseS t0 p t TbottomS):ctx) t
                         in (adds', TcaseS t0 p t' TbottomS)
       rs ns ctx (TcaseS t0 p t a) =
          case lookup t0 ns of
            Just (Right (p',True)) 
                  | matches p' p ->
                          case matchingSubsts (Just t0) p' p of
                            Just sust -> 
                                  let mkTermS t = foldr (\(t0r,p) tr->TcaseS t0r p tr TbottomS) t sust
                                      (adds',t')=rs ((t0,Right (p,True)):ns) (Left mkTermS:ctx) t
                                   in (adds',mkTermS t')
                            Nothing -> let (adds',t')=rs ((t0,Right (p,True)):ns) (Left (\t->TcaseS t0 p t TbottomS):ctx) t
                                        in (adds',TcaseS t0 p t' TbottomS)
                  | otherwise -> rs ns ctx a
            Just (Right (p',False)) | matches p p' -> rs ns ctx a
            _ ->
                  let (adds',t')=rs ((t0,Right (p,True)):ns) (Left (\t->TcaseS t0 p t a'):ctx) t
                      (adds'',a')=rs ((t0,Right (p,False)):ns) ctx a
                   in (adds' ++ adds'',TcaseS t0 p t' a')
       rs ns ctx (TcaseSana ia ih t0 p t a) | not (isRefusable p) = 
                        let (adds',t') = rs ns (Left (\t->TcaseSana ia ih t0 p t TbottomS):ctx) t
                         in (adds', TcaseSana ia ih t0 p t' TbottomS)
       rs ns ctx (TcaseSana ia ih t0 p t a) = 
                        let (adds',t')=rs ns (Left (\t->TcaseSana ia ih t0 p t a'):ctx) t
                            (adds'',a')=rs ns ctx a
                         in (adds' ++ adds'',TcaseSana ia ih t0 p t' a')
       rs ns ctx (TcaseR ia ih t0 ps a) =
             case lookup t0 ns of
                Just (Left (Just (c,i),ps')) -> 
                   case find (\(c',_,_)->c==c') ps of
                     Just (_,vrs',ts') -> let (_,vrs,_)= maybe (error ("reorganizeSigma: i have a problem with constructor "++c))
                                                               id $ find (\(c',_,_)->c==c') ps'
                                           in rs ns (Right t0:ctx) . substituteTerms (zip vrs' vrs)  . fst . (ts'!!) $ i
                     Nothing -> rs ns ctx a
                Just (Left (Nothing,ps')) -> let d=ps `diff` ps'
                                                 (adds,d')= applyCtx t0 ctx$ recurse t0 ns ctx d
                                                 r@(addsa,da)=rs ns ctx a
                                              in if null d then r else ((t0,d'):(addsa++adds),da)
                _       -> let ctx' = Right t0:ctx 
                               (adds,ps') = recurse t0 ns ctx' ps
                               (adds',a') = rs ((t0,Left (Nothing,ps)):ns) ctx' a
                               (addnow,pass) = partition ((==t0).fst) (adds++adds')
                            in (pass,TcaseR ia ih t0 (ps'++concat (map snd addnow)) a')
       rs ns ctx TbottomS = ([],TbottomS)

       recurse v ns ctx ps = let (addss,ps')=unzip $ map (rs' v ns ctx ps) ps
                              in (concat addss,ps')

       isRefusable (Pas _ p) = isRefusable p
       isRefusable (Pvar _) = False
       isRefusable _ = True
       applyCtx v ctx (adds,d) = (adds,map (\ (c,vs,ts)-> (c,vs,map (appCtx v ctx)  ts)) d)
       appCtx v ctx (s,pos) = (foldl appf s (takeWhile (either (const True) (v/=)) ctx),pos)
       appf s (Left f) = f s
       appf s _ = s

       rs' v ns ctx ps (c,vrs,ts) = let (addss,ts')=unzip $ map (rs'' v c ns ctx ps) (zip ts [0..])
                                     in (concat addss,(c,vrs,ts'))
       rs'' v c ns ctx ps ((t,pos),i) = let (adds,t')=rs ((v,Left (Just (c,i),ps)):ns) ctx t
                                         in (adds,(t',pos))

       diff l1 l2 = let notinl2 (c,_,_) = maybe True (const False)  $ find (\(c',_,_)->c==c') l2 
                     in filter notinl2 l1



       matches (Pas _ p) p' = matches p p'
       matches p (Pas _ p') = matches p p'
       matches _ (Pvar v) = True
       matches (Pvar v) _ = False
       matches (Ptuple ps) (Ptuple ps') = length ps == length ps' && and (zipWith matches ps ps')
       matches (Pcons c ps) (Pcons c' ps') = c==c' && length ps == length ps' && and (zipWith matches ps ps')
       matches (Plit l) (Plit l') = l==l'
       matches _ _ = False





       matchingSubsts :: Maybe Variable -> Pattern -> Pattern -> Maybe [(Variable,Pattern)]
       matchingSubsts v (Pas vp p) p' = matchingSubsts (Just vp) p p'
       matchingSubsts v (Pvar vp) p = Just [(vp,p)]
       matchingSubsts (Just v) _ p1@(Pvar _) = Just [(v,p1)]
       matchingSubsts v (Ptuple ps) (Ptuple ps') 
                 | length ps == length ps' =  
                        sequence (zipWith (matchingSubsts Nothing) ps ps') >>= return . concat
       matchingSubsts v (Pcons c ps) (Pcons c' ps') 
                 | c==c' && length ps == length ps' = 
                        sequence (zipWith (matchingSubsts Nothing) ps ps') >>= return . concat
       matchingSubsts v (Plit l) (Plit l') | l==l' = Just []
       matchingSubsts _ _ _ = Nothing



       safeMatch (Pvar _) = True
       safeMatch (Pas _ p) = safeMatch p
       safeMatch _ = False

       substituteTerms :: [(Variable,Variable)]->TermS->TermS
       substituteTerms susts (TtermS t) = TtermS (substitution (map (\(v1,v2)->(v1,Tvar v2)) susts) t)
       substituteTerms susts (TcaseS t0 p t a) = 
                        case lookup t0 susts of
                              Just v -> TcaseS v p (substituteTerms susts t) (substituteTerms susts a)
                              Nothing -> TcaseS t0 p (substituteTerms susts t) (substituteTerms susts a)
       substituteTerms susts (TcaseSana ia ih t0 p t a) = 
                        case lookup t0 susts of
                              Just v -> TcaseSana ia ih v p (substituteTerms susts t) (substituteTerms susts a)
                              Nothing -> TcaseSana ia ih t0 p (substituteTerms susts t) (substituteTerms susts a)
       substituteTerms susts (TcaseR ia ih t0 ps a) = 
                    let rec (c,vrs,ts) = (c,vrs,map (\(t,pos)->(substituteTerms susts t,pos)) ts)
                     in case lookup t0 susts of
                              Just v -> TcaseR ia ih v (map rec ps) (substituteTerms susts a)
                              Nothing -> TcaseR ia ih t0 (map rec ps) (substituteTerms susts a)
       substituteTerms susts TbottomS = TbottomS




instance CAInlineable OutF where
 caGetInline ts arg (bv,t0:_,OutF psis) 
     | null ts = return$ substitution (zipTree [bvtuple bv] [arg])$ Tcase t0 ps (map getCaTerm psis)
     | otherwise = return$ substitution (zipTree [bvtuple bv] [arg])$ Tcase t0 ps ts
  where getCaTerm =Ttuple False . map getTerm.getTerms
        ps=map (toPattern . getPatterns) psis
        eq (Tvar v') v = (v'==v)
        eq _ _ = False
        toPattern [p] = p
        toPattern ps = Ptuple ps
 caGetInline ts arg (bv,[],OutF psis) = error$ "caGetInline: unexpected empty list of terms"




instance (Inlineable a) => Inlineable (Acomponent a) where
 getInline terminosEntrada (Acomp (vs, termwrapper)) = maybe nop ok$ zipTree' vs terminosEntrada
  where ok parejasSubstituciones = substitution parejasSubstituciones$ getInline terminosEntrada termwrapper
        nop = Tcase (ttuple terminosEntrada) [ptuple (map bv2pat vs)] [getInline (map bv2term vs) termwrapper]

instance (Inlineable a) => Inlineable (TauTerm a) where
 getInline terminosEntrada (Taucons cons ts phi etai) =
     let tsinlines = map (getInline terminosEntrada) ts
         (fts,newts) = reduceTrans tsinlines etai
     in fts$ getInline newts phi
 getInline _ (Tausimple t) = t
 getInline terminosEntrada (Taucata ft t) = ft (getInline terminosEntrada t)
 getInline terminosEntrada (Taupair term tauTerm) =
       let tauTermInline = getInline terminosEntrada tauTerm
        in Ttuple False [term, tauTermInline]

instance Inlineable Phii where
 getInline _ t = t

instance Inlineable Tau where
 getInline terminosEntrada (Tauphi phi) =
     getInline terminosEntrada phi
 getInline terminosEntrada (TauinF inf) =
     getInline terminosEntrada inf
 getInline terminosEntrada (Tautau tau) =
     getInline terminosEntrada tau

instance Inlineable InF where
 getInline _ (InF (cons, ts)) =
     (Tcapp cons ts)

instance (Inlineable a) => Inlineable (TermWrapper a) where

 getInline terminosEntrada (TWcase term patterns termWrappers) =
     let
      terminos = map (getInline terminosEntrada) termWrappers
     in
     Tcase term patterns terminos



 getInline terminosEntrada tw@(TWeta termWrapper etai) = 
     let (fts,resultadosEta) = reduceTrans terminosEntrada etai
      in fts$ getInline resultadosEta termWrapper



 getInline terminosEntrada (TWsimple a) =
     getInline terminosEntrada a



 getInline terminosEntrada (TWacomp acomponent) =
     getInline terminosEntrada acomponent



 getInline _ (TWbottom) =
     (Tlamb (Bvtuple False []) Tbottom) -- bottom















inline :: (CHylo h, CAInlineable ca, Inlineable a, HasComponents ca) => [h a ca] -> h a ca -> IntState Def
inline hs hylo =
    let       
      (boundvar, t0, coalgebra) = getCoalgebra hylo
      algebra = getAlgebra hylo
      eta = getEta hylo
      functor = getFunctor hylo
      tupleTerms = getComponentTerms coalgebra      
      terminosAlternativas = map (map getTerm) tupleTerms
      varsAlts = map (map getPosition) tupleTerms
      etaResults = map (flip reduceTrans)$ zipWith3 (\e tts fnc -> e `rightCompose` inlineDelta hs tts fnc) eta tupleTerms functor
      phis = zipWith (\ etaRes algebrai terminos -> 
                         let (etaf,ts) = etaRes terminos
                          in etaf (getInline ts algebrai)) 
                     etaResults algebra
    in do i<-get
          let cainl=caGetInline (zipWith ($) phis terminosAlternativas) 
                                (bv2term (bvtuple boundvar)) (getCoalgebra hylo)
              (cainlres,i') = runState cainl i
          put i'
          return$ Defvalue (getName hylo)$ removeHyloApps$ nullPatternVariables$ mergeCasePatterns$ 
                   foldr Tlamb cainlres$ concat$ (map flattenBv)$ boundvar
 where flattenBv (Bvtuple True bvs) = concat$ map flattenBv bvs
       flattenBv bv = [bv]



inlineDelta :: CHylo h => [h a ca] -> [TupleTerm] -> HFunctor -> EtaOp
inlineDelta hs tts fnc =
         let ps=map (\tt -> let p=getPosition tt in (p,getRecIndex fnc p)) tts 
          in EOsust (map fst ps) 
                    (zipWith mksust ps (map (expanded fnc.fst) ps))
                    (map (Bvar . fst) ps)
 where mksust (rv,ri) pf = let tp = Tvar rv
                            in mapStructure (applyHylo (hs!!maybe 0 id ri) tp) tp pf
       applyHylo h t = applyHyloWithCntArgs h [] Nothing t

applyHyloWithCntArgs :: CHylo h => h a ca -> [Term] -> Maybe [Int] -> Term -> Term
applyHyloWithCntArgs h cntargs cntpos t = Thyloapp (getName h) (sum$ map countArgs bvs) cntargs cntpos t
  where (bvs,_,_)=getCoalgebra h
        countArgs (Bvtuple True bvs) = sum (map countArgs bvs)
        countArgs bv = 1

%%]

instance Show EtaOp where
 show = render.showDoc

instance ShowDoc EtaOp where
 showDoc (EOgeneral bvs tts) = text "(\\"<>showTuple bvs<>text "->"<>showTuple tts<>char ')'
 showDoc EOid = text "id"
 showDoc (EOsust vs ts vss) = text "(\\"<>showTuple vss<>text "->"<>showTuple (map (sust$ zip vs ts) vss)<>char ')'
   where sust vts (Bvar v) = maybe (Tvar v) id$ lookup v vts
         sust vts (Bvtuple b vs) = Ttuple b$ map (sust vts) vs
 showDoc (EOlet t0s ps vs ts) = text "(\\"<>showTuple vs
                                   <>text "->case"<+>showTuple t0s<+>text "of"$$
                                             (showTuple ps<+>text "->"<>showTuple ts<>char ')')

%%[(8 codegen)

removeHyloApps :: Term -> Term
removeHyloApps = transformTerm remHApp
 where remHApp _ (Thyloapp v i ts pos t) = Tfapp v (thyloArgs i ts pos t)
       remHApp _ (Tlamb (Bvtuple True bv) t) = foldr Tlamb t$ concat$ map flattenbv bv
       remHApp _ t = t
       flattenbv (Bvtuple True bvs) = concat$ map flattenbv bvs
       flattenbv bv = [bv]






reduceTrans :: [Term] -> Etai -> (Term->Term,[Term])
reduceTrans ts (Etai (etasIzq, etasDer)) = (ftr.fti,tsall)
 where (fti,tsall)=reduceFromRight etasIzq tsr
       (ftr,tsr)=reduceFromRight (reverse etasDer) ts
       reduceFromRight etas tr = foldr atomicOperation' (id,tr) etas
       atomicOperation' etaop (ft,ts) = let (ft',ts')=atomicOperation etaop ts
                                         in (ft.ft',ts')

atomicOperation :: EtaOp -> [Term] -> (Term->Term,[Term])
atomicOperation eta terminosContexto =
       case eta of
        EOid -> (id,terminosContexto)
        EOgeneral vs ts -> case zipTree' vs terminosContexto of
                    Just sust -> (id,map (substitution sust) ts)
                    _ -> (Tcase (ttuple terminosContexto) [Ptuple (map bv2pat vs)].(:[]),ts)
        EOsust vks ts vs ->
                 let termino = map (substitution ((zip vks ts))) (map bv2term vs)
                  in case zipTree' vs terminosContexto of
                      Just zips -> (id,map (substitution zips) termino)
                      _ -> (Tcase (ttuple terminosContexto) [Ptuple (map bv2pat vs)].(:[]),termino)
        EOlet t0s ps vs ts -> let sust = zip vs terminosContexto in
                         (Tcase (ttuple (map (substitution sust) t0s)) [ptuple ps].(:[]), map (substitution sust) ts)

data InlAEnv = InlAEnv {terms::[Term], susts::[(Variable,Term)]}



inlA :: Monad m => (a->[Term]->[(Variable,Term)]->m Term) -> Acomponent a -> (Term->Term,[Term]) -> m Term
inlA f inF (fts,ts) = maybe bad ok (zipTree' (getVars inF) ts)
 where ok sts = runReader (inlTW f$ unwrapA inF) (InlAEnv ts sts) >>= return . fts
       bad = do t<-runReader (inlTW f$ unwrapA inF)$ InlAEnv ts []
                return$ fts$ Tcase (ttuple ts) [ptuple (map bv2pat (getVars inF))] [t]



inlTW :: Monad m => (a->[Term]->[(Variable,Term)]->m Term) -> TermWrapper a -> Reader InlAEnv (m Term)
inlTW = inlTW' False
inlTW' :: Monad m => Bool -> (a->[Term]->[(Variable,Term)]->m Term) -> TermWrapper a -> Reader InlAEnv (m Term)
inlTW' b f tw = foldTW hcase (if b then heta' else heta) hsimple hacomp (return (return Tbottom)) tw
 where hcase t0 ps ts = do env<-ask
                           ts'<-sequence ts
                           return (do ts''<-sequence ts';return (Tcase (substitution (susts env) t0) ps ts''))
       --heta t eta = local (\k->k {terms=reduceTrans (terms k) eta}) t
       heta = heta'
       heta' mmt eta = do env<-ask
                          let (ft,ts)=reduceTrans (terms env) eta
                          local (\k->k {terms=ts}) (do mt<-mmt;return (do t<-mt;return (ft t)))
       hsimple inf = do env<-ask; return$ f inf (terms env) (susts env)
       hacomp inF = do env<-ask; maybe (bad env) (ok env)$ zipTree' (getVars inF) (terms env)
         where ok env sts = local (\k->k {susts=sts})$ inlTW f (unwrapA inF)
               bad env = local (\k->k {susts=filter (flip notElem (vars (getVars inF)).fst) (susts k)}) $ 
                           do m<-inlTW f (unwrapA inF)
                              return$ do t<-m
                                         return$ Tcase (ttuple (terms env)) [ptuple (map bv2pat (getVars inF))] [t]

%%]

termS2Term :: (Int->Int->[Term]->Term)->TermS->Term
termS2Term _ TbottomS = Tbottom
termS2Term f (TcaseS t0 p t v) = 
     case v of 
       TbottomS -> Tcase (Tvar t0) [p] [termS2Term f t]
       _ -> Tcase (Tvar t0) [p,pany] [termS2Term f t,termS2Term f v]
termS2Term f (TcaseSana ia ih t0 p t v) = 
     case v of 
       TbottomS -> Tcase (applyana (Tvar t0)) [p] [termS2Term f t]
       _ -> Tcase (Tvar t0) [p,pany] [termS2Term f t,termS2Term f v]
 where applyana = Tapp (Tlit$Lint$ "[("++(show$ f ia ih [])++")]")
termS2Term f (TcaseR ia ih t0 ps v) = 
     case v of 
       TbottomS -> Tcase (f ia ih [Tvar t0]) (concat (map mkPattern ps)) (concat (map mkTerm ps))    
       _ ->  Tcase (f ia ih [Tvar t0]) (concat (map mkPattern ps)++[pany]) (concat (map mkTerm ps)++[termS2Term f v])
 where pf (c,vrs) i = Ptuple [Pvar (Vuserdef (c++show i)),Ptuple (map Pvar vrs)]
       alt2Term vrs (t,_) = termS2Term f t
       mkPattern (c,vrs,ts) = zipWith pf (replicate (length ts) (c,vrs)) [1..]
       mkTerm (c,vrs,ts) = map (alt2Term vrs) ts
termS2Term _ (TtermS t) = t

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloRep})

nullPatternVariables :: Term -> Term
nullPatternVariables = transformTerm f
  where f _ (Tcase t0 ps ts) = Tcase t0 (zipWith subst ps ts) ts
        f _ t = t
        subst p t = substitute [] (zip (filter (isLegalStart . head . show) (vars p \\ vars t)) (repeat (Vuserdef "_"))) p
        isLegalStart c = isAlpha c && isLower c || isDigit c



%%]
showHylo :: (CHylo hylo,ShowDocA a,HasComponents ca,CoalgebraPrintable ca) => Bool -> GenDict -> hylo a ca -> String
showHylo isTau i h =
      let s = text "--------------"
      in   render $
           text ("Hylo "++show (getContext h)) $$
           nest 2 (s $$
                  (showDocA i (getAlgebra h)) $$ s $$
                  (vcat (map showDoc (getEta h))) $$ s $$
                  (if deltaToPrint tts (getFunctor h) 
                     then  printDeltas tts (getFunctor h) 
                     else empty
                  ) $$
                  printCA 0 0 vsm t0 coalg $$ s $$
                  (showFunctor coalg (getFunctor h)))
  where
       (vsm,t0,coalg) = getCoalgebra h
       tts=getComponentTerms coalg
       showFunctor ca fncs=
         let fcts=zipWith printProds tts fncs
          in if not (null tts) then foldl ((<+>).(<+>char '+')) (head fcts) (tail fcts)  else empty
       printProds [] fnc = text "1"
       printProds tts fnc = let prods=map (printFnc fnc.getPosition) tts
                             in if not (null prods) then foldl ((<>).(<>char 'x')) (head prods) (tail prods)  else empty
       printFnc fnc p = maybe (char 'C') (\i->showDocPF i (expanded fnc p))$ getRecIndex fnc p 
       printDeltas ts@(tts:ttss) fs@(f:fncs) =
                                          text "delta" $$ 
                                          nest 4 (vcat (
                                               (if isDeltaId tts f then text "id" else printDelta tts f)
                                               : zipWith printDelta ttss fncs)
                                          )
                                         $$ text "end delta" 
       printDeltas _ _ = empty 
       deltaToPrint tts fncs = any (not . uncurry isDeltaId)$ zip tts fncs
       isDeltaId tts fnc = all (foldPF (const True) (const True) (const False) . expanded fnc)$ map getPosition tts
       printDelta tts fnc = let vs = map getPosition tts
                             in showDoc$ Tlamb (Bvtuple False (map Bvar vs)) 
                                               (Ttuple False (map (\v->mapStructure (Tvar v) (Tvar v)$ expanded fnc v) vs))



%%]
class CoalgebraPrintable a where
  printCA :: Int -> Int -> [Boundvar] -> [Term] -> a -> Doc

instance CoalgebraPrintable WrappedCA where
 printCA i ia v t wac =
   case wac of
    WCApsi (_,_,wa) -> f wa
    WCAoutF (_,_,wa) -> f wa
    WCAsigma (_,_,wa) -> f wa
  where f wa=printCA i ia v t wa

showDocWCA i ia wac =
   case wac of
    WCApsi wa -> f wa
    WCAoutF wa -> f wa
    WCAsigma wa -> f wa
  where f (v,t0,a) = printCA i ia v t0 a



instance CoalgebraPrintable Sigma where
 printCA i ia v t0s coalg = showDocSigma i ia v t0s coalg

instance CoalgebraPrintable Psi where
 printCA _ _ vsm t0 coalg = char '\\'<>text (show (bvtuple vsm))<>text "->case"
                           <+>showDoc (Ttuple False t0)<+>text "of"$$ nest 4 (showDoc coalg)

instance CoalgebraPrintable OutF where
 printCA _ _ vsm t0 coalg = char '\\'<>text (show (bvtuple vsm))<>text "->case"
                           <+>showDoc (Ttuple False t0)<+>text "of"$$ nest 4 (showDoc coalg)


showDocPF i (PFprod (p:ps)) = char '('<>hcat (showDocPF i p:map ((char 'x'<>).showDocPF i) ps)<>char ')'
showDocPF i (PFprod _) = text "()"
showDocPF i (PFid _) = text ("PI_"++show i)
showDocPF i (PFcnt _) = char 'C'

instance ShowDoc Psi where
 showDoc (Psi alts) =
   vcat$ zipWith (\i a->showDoc (toPattern . getPatterns$ a)<+>text "->"<+>
                   (showDoc (Ttuple False [Tlit (Lint (show i)), Ttuple False .map getTerm$ getTerms a])))  [1..] alts
  where toPattern [p] = p
        toPattern ps = Ptuple ps

instance ShowDoc OutF where
 showDoc (OutF alts) =
  vcat$ zipWith (\i (OutFc (c,vs,tts))->showDoc (Pcons c (map Pvar vs)) <+> text "->" <+>
                  (showDoc (Ttuple False [Tlit (Lint (show i)),(Ttuple False .map getTerm$ tts)])))  [1..] alts


showDocSigma i ia bvs t0s (Sigma (casemap,tts,pss,hss)) =
     (prefix i ia <+>text ("Sigma_"++show i) <> 
          cat (text "(" : map (nest 2) (mapSeparator (text ","<+>) sigmaargs)++[text ")"]) 
      $$) . nest 2 . (text "where" <+>) $ 
         (((text ("Sigma_"++show i++" =") <+>) . showDoc . 
               Tlamb (Bvtuple False [ Bvar (beta ia) |  (ia,Just _)<-zip [0..] hss]) . 
                      Tlamb (bvtuple bvs) . nullPatternVariables . delCases . insertRecvarCases t0s pss .
                      termS2Term (\ia ih->Tfapp (beta ia)) . 
                      reorganizeSigma . weaveTermS (zipWith (zipWith mksum) 
                                                            (splitList [1..] casemap) 
                                                            (zipWith replicate casemap tts))$ pss )
          $$ vcat [ showCA ia h | (ia,Just h)<-zip [0..] hss])
   where sigmaargs = [ text (eta' ia++"."++eta'' ia++"."++psi ia) |  (ia,Just _)<-zip [0..] hss]
         prefix 0 _ = empty
         prefix _ ia = text (psi ia++" =")
         showCA ia (_,acomps,etas,ca,_) = 
                    text (eta' ia) <+> char '=' <+> vcat (map showDoc (evalState (mapM buildeta' acomps) []))
                    $$ text (eta'' ia) <+> char '=' <+> vcat (map showDoc etas)
                    $$ innerPrefix ca ia <+> showDocWCA (i+1) ia ca
         psi ia = "psi_"++show ia++"'"
         eta' ia = "eta_"++show ia++"'"
         eta'' ia = "eta_"++show ia++"''"
         innerPrefix (WCAsigma _) _ = empty
         innerPrefix _ ia = text (psi ia++" =") 
         mksum i tts = Ttuple False [Tlit (Lint (show i)),Ttuple False$ map getTerm tts]
         beta ia = Vuserdef ("@beta_"++show ia)
         buildeta' acomp = do let bvs=getVars acomp
                              t<-inlA inF2Term acomp (id,map bv2term bvs)
                              return$ Tlamb (Bvtuple False bvs) t
         inF2Term:: InF -> [Term] -> [(Variable,Term)] -> State [(Constructor,Int)] Term
         inF2Term (InF (c,_)) ts _ = do i<-geti c; return$ Ttuple False [Tlit $ Lint $ c++show i,Ttuple False ts]
         geti::Constructor->State [(Constructor,Int)] Int
         geti c = State (\st->maybe (1,(c,1):st) (\i->(i+1,upd c st)) $ lookup c st)
         upd c ((c',i):cs) = (c,i+1):cs
         upd c [] = [(c,1)]

class ShowDocA a where
 showDocA :: GenDict -> Algebra a -> Doc
instance ShowDocA InF where
 showDocA _ ls = vcat (map (showDoc.acomp2term) ls)
instance ShowDocA Phii where
 showDocA _ ls = vcat (map (showDoc.acomp2term) ls)

class Acomp2Term a where
 acomp2term :: Acomponent a -> Term
instance Acomp2Term InF where
 acomp2term = a2term tinF
  where tinF (InF (c,_)) ts sust = Tcapp c ts
instance Acomp2Term Phii where
 acomp2term = a2term tphii
  where tphii t ts sust = substitution sust t
instance Acomp2Term Tau where
 acomp2term = a2term ttau
  where ttau tau ts sust = let f tw=tw2term ts sust tterm tw
                            in foldTau f f f tau
        tterm t ts sust= tterm' t ts sust
        tterm' t ts sust = let tt t=tterm' t ts sust
                            in case t of
                                Taucons c taus a eta -> 
                                     let (fts,ets) = reduceTrans (map tt taus) eta
                                      in fts$ Tapp (acomp2term a) (Ttuple False ets)
                                Taupair t tau -> Ttuple False [t,tt tau]
                                Taucata ft tau -> ft (tt tau)
                                Tausimple t -> substitution sust t

instance ShowDocA Tau where
 showDocA di ls = vcat (map (showDoc.a2term (ttau csalg)) ls)
                 $$ text "--"
                 $$ vcat (map showcsalg csalg)
  where i = maybe 0 id$ M.lookup "a" di
        csalg::[(Constructor,(Int,(Term,Etai)))]
        csalg = zip cons$ zip [i..] as
        (cons,as)=unzip$ evalState (do mapM_ (collect.unwrapA) ls;get) []
        showcsalg (_,(i,(a,eta))) = showDoc (Vgen "a" i) <+> text "= (" <> showDoc a <> text ") ." <+> showDoc eta
        collect = foldTWM hcase (const.const (return ())) colTau (collect.unwrapA) (return ())
        colTau = foldTau collect' collect' collect'
        collect' tw= foldTWM hcase (const.const (return ())) tc (collect'.unwrapA) (return ()) tw
        hcase t0 ps tws = return ()
        tc (Taucons c taus a eta) =
          do ls<-get
             case lookup c ls of
              Nothing -> put ((c,(acomp2term a,eta)):ls)
              Just _ -> return ()
        tc (Taupair t tau) = tc tau
        tc _ = return ()
        ttau csalg tau ts sust = let f tw=tw2term ts sust (tau2term csalg) tw
                                  in foldTau f f f tau

a2term :: (a ->[Term] -> [(Variable,Term)] -> Term) -> Acomponent a -> Term
a2term f a =Tlamb (bvs (getVars a)) (tw2term (map bv2term (getVars a)) [] f$ unwrapA a)
    where bvs [bv]=bv
          bvs ls = Bvtuple False ls

tw2term :: [Term] -> [(Variable,Term)] -> (a -> [Term] -> [(Variable,Term)] -> Term) -> TermWrapper a -> Term
tw2term ts sust f tw = runIdentity$ runReader (inlTW' True (\a b->Identity . f a b) tw)$ InlAEnv ts sust

tau2term :: [(Constructor,(Int,c))] -> TauTerm a -> [Term] -> [(Variable,Term)] ->Term
tau2term ls t ts sust =
   let tt t=tau2term ls t ts sust
    in case t of
        Taucons c taus a eta -> Tfapp (maybe (error "tau2term: No se encuentra el constructor") (Vgen "a" . fst)$ lookup c ls)
                                      [Ttuple False (map tt taus)]
        Taupair t tau -> Ttuple False [t,tt tau]
        Taucata ft tau ->
           let cs=map (Vgen "a".fst.snd) ls
               showcomma (a:as) = concat$ show a : map ((',':).show) as
               showcomma _ = []
               applycata=Tapp .Tlit .Lint .("(|"++).(++"|)").showcomma$ cs
               t=tt tau
            in case ft t of
                Ttuple b _ -> Ttuple b [t,applycata t]
                _ -> applycata t
        Tausimple t -> substitution sust t

instance ShowDoc TupleTerm where
 showDoc (Tterm t p) = char '('<>showDoc t<+>char ','<+>(text$ show p)<>char ')'


instance Show Etai where
 show = render.showDoc

instance ShowDoc Etai where
 showDoc e@(Etai (l1,l2))
   | isIdEta e = text "id"
   | otherwise = sep (mapDocSeparator (char '.'<>) $ l1++reverse l2)

%%]
