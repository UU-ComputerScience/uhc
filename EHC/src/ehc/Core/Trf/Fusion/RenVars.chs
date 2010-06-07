%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.RenVars} export (renameVariables,Substitutable(..),VarsB(..))
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloContext})
import List((\\),intersect,nub)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloFace})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})

renameVariables :: (VarsB a', VarsB ca', Substitutable ca', Substitutable a', VarsB a, 
                    Vars a', Vars ca', Substitutable a, Vars a,Substitutable ca, Vars ca, VarsB ca, CHylo h) => 
                 (h a ca) -> (h a' ca') -> [Variable] -> IntState (h a ca, h a' ca')
renameVariables left right fvright = 
         do right'<-if null i0 then return right
                       else do us <- mapM (getFreshVar . varPrefix) i0
                               return (substitute' (zip i0 us) right)
            let i1 = nub$ intersect (vars' right'++fvright) (varsB' left)
            left'<-if null i1 then return left
                     else do us <- mapM (getFreshVar . varPrefix) i1
                             return (substitute' (zip i1 us) left)
            return (left',right')
  where i0 =  nub$ intersect (vars' left) (varsB' right)
        vars' h = getName h  : ( (\(bv,t0,c)->varsB c++vars c++vars t0++vars bv) $ getCoalgebra h) ++ vars (getAlgebra h)
                  ++ vars (getEta h) ++ vars (getContext h) ++ (\(bvs,_,_)->vars bvs) (getCoalgebra h)
        varsB' h = ( (\(bv,t0,c)-> varsB c++varsB t0 ++ vars bv) $ getCoalgebra h) 
                     ++ varsB (getAlgebra h) ++ vars (getContext h)
        substitute' ss h = 
                let constantArgs_h = getConstantArgs (getContext h)
                    (bv,t0,c) = getCoalgebra h
                    sc' = constantArgs_h ++ vars bv
                in setName (getName h) $ setContext (substitute constantArgs_h ss$ getContext h) $
                                    consHylo (substitute constantArgs_h ss $ getAlgebra h) 
                                    (substitute constantArgs_h ss $ getEta h) (getFunctor h)
                                    ((\(bv,t0,c)->(substitute constantArgs_h ss bv,
                                                   substitute sc' ss t0,
                                                   substitute sc' ss c)) $ getCoalgebra h)





instance (VarsB a) => VarsB (Acomponent a) where
  varsB (Acomp (bvs, termwrapper)) = varsB termwrapper ++ vars bvs

instance VarsB PatternS where
  varsB (PcaseS t0 pat termS) =  varsB termS ++ vars pat
  varsB (PcaseSana _ t0 pat termS) =  varsB termS ++ vars pat
  varsB (PcaseR _ t0 _ _ ts) =  concat (map (varsB.fst) ts)
  varsB (Ppattern v p) =  vars p
  varsB Pdone = []


instance VarsB Sigma where
  varsB (Sigma (_,listatps,pss,hss)) = concat (map varsB pss)

instance VarsB WrappedCA where
  varsB (WCApsi (bv, t0, psi)) = varsB t0 ++ varsB psi ++ vars bv
  varsB (WCAoutF (bv, t0, outf)) = varsB t0 ++ varsB outf ++ vars bv
  varsB (WCAsigma (bv, t0, sigma)) = varsB t0 ++ varsB sigma ++ vars bv

instance VarsB InF where
  varsB (InF (cons,ts)) = varsB ts

instance VarsB Tau where
  varsB (Tauphi tauphii) = varsB tauphii
  varsB (TauinF tauinf) = varsB tauinf
  varsB (Tautau tautau) = varsB tautau

instance VarsB a => VarsB (TermWrapper a) where
  varsB = foldTW (\t0 pts vs -> varsB t0 ++ vars pts ++ concat vs) const varsB varsB [] 

instance (VarsB a) => VarsB (TauTerm a) where
  varsB t = []

instance VarsB OutF where
  varsB (OutF outfis) = varsB outfis

instance VarsB OutFi where
  varsB (OutFc (cons,vs,tps)) = vs

instance VarsB Psi where
  varsB (Psi psis) = varsB psis

instance VarsB Psii where
  varsB (Psii (pat, tps)) = vars pat

instance VarsB TupleTerm where
  varsB tt = []






instance (Vars a) => Vars (Acomponent a) where
  vars (Acomp (bvs, termwrapper)) = vars termwrapper \\ vars bvs

instance Vars Sigma where
  vars (Sigma (_,listatps,pss,hss)) = vars listatps ++ concat (map varshs hss)
    where varshs (Just (_,apcomsInf,etais,wca,functerms)) = vars apcomsInf ++ vars wca
          varshs _ = []

instance Vars WrappedCA where
  vars (WCApsi (bv, t0, psi)) = (vars t0 ++ vars psi) \\ vars bv
  vars (WCAoutF (bv, t0, outf)) = (vars t0 ++ vars outf) \\ vars bv
  vars (WCAsigma (bv, t0, sigma)) = (vars t0 ++ vars sigma) \\ vars bv

instance Vars InF where
  vars (InF (cons,ts)) = vars ts

instance Vars Tau where
  vars (Tauphi tauphii) = vars tauphii
  vars (TauinF tauinf) = vars tauinf
  vars (Tautau tautau) = vars tautau

instance Vars a => Vars (TermWrapper a) where
  vars = foldTW (\t0 pts vs -> vars t0 ++ (concat vs \\ vars pts)) (\vs eta->vs++vars eta) vars vars [] 

instance (Vars a) => Vars (TauTerm a) where
  vars (Taucons cons tauterms a etai) = vars tauterms ++ vars a ++ vars etai
  vars (Tausimple term) = vars term
  vars (Taupair term tauterm) = vars term ++ vars tauterm
  vars (Taucata func tauterm) = vars tauterm

instance Vars OutF where
  vars (OutF outfis) = vars outfis

instance Vars OutFi where
  vars (OutFc (cons,vs,tps)) = vars tps \\ vs

instance Vars Psi where
  vars (Psi psis) = vars psis

instance Vars Psii where
  vars (Psii (pat, tps)) = vars tps \\ vars pat

instance Vars TupleTerm where
  vars tt = vars (getTerm tt)

instance Vars EtaOp where
  vars EOid = []
  vars (EOgeneral bvs ts) = vars ts \\ vars bvs
  vars (EOsust vs ts bvs) = vars ts \\ vars bvs
  vars (EOlet ts ps vs ts1) = (vars ts ++ (vars ts1 \\ vars ps)) \\ vs

instance Vars Etai where
  vars  (Etai (etaOp1,etaOp2)) = vars etaOp1 ++ vars etaOp2



instance (Substitutable a) => Substitutable (Acomponent a) where
  substitute sc lvars  (Acomp (vs, termwrapper)) = Acomp (substitute sc lvars vs,substitute (sc++vars vs) lvars termwrapper)

instance Substitutable EtaOp where
  substitute sc lvars EOid = EOid
  substitute sc lvars (EOgeneral bvs ts) = EOgeneral (substitute sc lvars bvs) (substitute (sc++vars bvs) lvars ts)
  substitute sc lvars (EOsust vs ts bvs) = EOsust (substitute sc' lvars vs) 
                                                  (substitute sc' lvars ts) 
                                                  (substitute sc lvars bvs)
    where sc'=sc++vars bvs
  substitute sc lvars (EOlet ts ps vs ts1) = EOlet (substitute sc' lvars ts) 
                                                   (substitute sc' lvars ps)
                                                   (substitute sc lvars vs) 
                                                   (substitute (sc'++vars ps) lvars ts1)
    where sc'=sc++vs

instance Substitutable Etai where
  substitute sc lvars  (Etai (etaOp1,etaOp2)) = Etai (substitute sc lvars etaOp1, substitute sc lvars etaOp2)

instance (Substitutable a) => Substitutable (TermWrapper a) where
  substitute sc lvars tw = foldTW (\t0 ps ts sc -> TWcase (substitute sc lvars t0) ps (zipWith ($) ts (map ((sc++).vars) ps))) 
                                  (\t e sc ->TWeta (t sc) (substitute sc lvars e)) (\t sc -> TWsimple (substitute sc lvars t) )
                                  (\t sc -> TWacomp (substitute sc lvars t)) (const TWbottom) tw sc






instance Substitutable Psi where
  substitute sc lvars (Psi psis) = Psi (substitute sc lvars psis)

instance Substitutable Psii where
  substitute sc lvars (Psii (pat, tuplets)) = Psii (substitute sc lvars pat,substitute (sc++vars pat) lvars tuplets)

instance Substitutable TupleTerm where
  substitute sc lvars (Tterm term position) = Tterm (substitute sc lvars term) position

instance Substitutable InF where
  substitute sc lvars (InF (cons, ts)) = InF (cons,substitute sc lvars ts)

instance Substitutable Sigma where
  substitute sc lvars (Sigma (casemap,listatupleterms, pss, hss)) = 
                       Sigma (casemap,substitute (sc++varsB pss) lvars listatupleterms, 
                              map (substitute sc lvars) pss, map ss hss)
    where ss (Just (i,compsInf, etais, wrappedCa, funcTermTerm)) = 
                Just (i,substitute sc lvars compsInf, substitute sc lvars etais, substitute sc lvars wrappedCa, funcTermTerm)
          ss _ = Nothing

instance Substitutable WrappedCA where
  substitute sc lvars (WCApsi (bound,term,psi)) = WCApsi (substitute sc lvars bound,substitute sc' lvars term,substitute sc' lvars psi)
    where sc'=sc++vars bound
  substitute sc lvars t@(WCAoutF (bound,term,outf)) = WCAoutF (substitute sc lvars bound,substitute sc' lvars term,substitute sc' lvars outf)
    where sc'=sc++vars bound
  substitute sc lvars (WCAsigma (bound,term,sigma)) = WCAsigma (substitute sc lvars bound,substitute sc' lvars term,substitute sc' lvars sigma)
    where sc'=sc++vars bound

instance Substitutable PatternS where
  substitute sc susts (PcaseS t0 p t) = PcaseS (substitute sc susts t0)
                                               (substitute sc susts p)
                                               (substitute sc' susts t) 
    where sc'=sc++vars p
  substitute sc susts (PcaseSana i t0 p t) = PcaseSana i (substitute sc susts t0)
                                                       (substitute sc susts p)
                                                       (substitute sc' susts t)
    where sc'=sc++vars p
  substitute sc susts (PcaseR i t0 c vrs ts) = PcaseR i (substitute sc susts t0) c (substitute vrs susts vrs)
                                                 (map (\ (t,pos)-> (substitute (vrs++sc) susts t,pos)) ts)
  substitute sc susts (Ppattern v p) = Ppattern v (substitute sc susts p)
  substitute sc susts t@Pdone = t

instance Substitutable OutFi where
  substitute sc lvars (OutFc (cons,vars,tupleterms)) = OutFc (cons,substitute sc lvars vars,substitute (sc++vars) lvars tupleterms)

instance Substitutable OutF where
  substitute sc lvars (OutF outfis) = OutF (substitute sc lvars outfis)

instance Substitutable Tau where
  substitute sc lvars (Tauphi tau) = Tauphi (substitute sc lvars tau)
  substitute sc lvars (TauinF tau) = TauinF (substitute sc lvars tau)
  substitute sc lvars (Tautau tau) = Tautau (substitute sc lvars tau)

instance (Substitutable a) => Substitutable (TauTerm a) where
  substitute sc lvars (Taucons cons tauterms a etai) = Taucons cons (substitute sc lvars tauterms) 
                                                            (substitute sc lvars a) (substitute sc lvars etai)
  substitute sc lvars (Tausimple term) = Tausimple (substitute sc lvars term)
  substitute sc lvars (Taupair term tauterm) = Taupair (substitute sc lvars term) (substitute sc lvars tauterm)
  substitute sc lvars (Taucata func tauterm) = Taucata (substitute sc lvars.func) (substitute sc lvars tauterm)
%%]



