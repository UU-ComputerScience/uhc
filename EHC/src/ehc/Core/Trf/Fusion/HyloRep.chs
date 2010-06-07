%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HyloRep} export (module {%{EH}Core.Trf.Fusion.HyloFace},module {%{EH}Core.Trf.Fusion.HsSyn},Hylo(..),composeEta)
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
import List
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloFace})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.FsDeriv})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloContext})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})


data Hylo a  ca = Hylo { hylo_algebra :: Algebra a,
                         hylo_nattrans :: [Etai],
                         hylo_functor :: HyloFunctor,
                         hylo_coalgebra :: Coalgebra ca,
                         hylo_context :: Context,
                         hylo_name :: Variable 
                       }












instance CHylo Hylo where

 buildHylo names ts = aA names ts >>= (return . zipWith buildHylo' names)
  where buildHylo' :: Variable -> ([Boundvar],[Term],[([Pattern],([(Variable,Variable)], [(Variable, Int,[[Int]], [Term])], Term))])
                             -> Hylo Phii Psi
        buildHylo' name (vsm,t0,lns) = Hylo { hylo_algebra = a,
                                              hylo_nattrans = etas,
                                              hylo_functor = fncs,
                                              hylo_coalgebra = (vsm,t0,Psi ca),
                                              hylo_context = emptyContext,
                                              hylo_name = name }
         where
          (a,etas,fncs,ca)=unzip4$ map buildLine lns
          buildLine :: ([Pattern],([(Variable,Variable)], [(Variable, Int,[[Int]], [Term])], Term)) 
                          -> (Acomponent Phii,Etai,HFunctor,Psii)
          buildLine (pattern,(vs,vts,t)) =
             (wrapA (map Bvar$ phiNoRecVars++phiRecVars) (TWsimple t),idEta,HF .zip4 phiRecVars idxs ias.map PFid$ phiRecVars,
              Psii (pattern,zipWith (flip tupleterm.Tvar) psiNoRecVars phiNoRecVars
                            ++zipWith (\v ts -> tupleterm v (recArgsToTerm ts)) phiRecVars psiRecTerms))
           where (phiRecVars,idxs,ias,psiRecTerms) = unzip4 vts
                 (psiNoRecVars,phiNoRecVars) = unzip vs
                 recArgsToTerm [t] = t
                 recArgsToTerm ts = Ttuple True ts

 getAlgebra = hylo_algebra
 setAlgebra a h = h {hylo_algebra = a}

 getEta = hylo_nattrans
 setEta etas h = h {hylo_nattrans = etas}

 getCoalgebra = hylo_coalgebra
 setCoalgebra ca h = h {hylo_coalgebra = ca}

 getFunctor = hylo_functor
 setFunctor fncs h = h {hylo_functor = fncs}

 getContext = hylo_context
 setContext args h = h {hylo_context = args}

 getName = hylo_name
 setName name h = h {hylo_name = name}


 consHylo a etas fncs ca = Hylo { hylo_algebra = a,
                                  hylo_nattrans = etas,
                                  hylo_functor = fncs,
                                  hylo_coalgebra = ca,
                                  hylo_context = emptyContext,
                                  hylo_name = Vuserdef "default"}


instance CoalgebraTerm Psii where
  getPatterns (Psii (p,_)) = p
  getTerms (Psii (_,tts)) = tts
  setTerms tts (Psii (vs,_)) = Psii (vs,tts)

instance CoalgebraTerm OutFi where
  getPatterns (OutFc (c,args,_)) = [Pcons c (map Pvar args)]
  getTerms (OutFc (_,_,tts)) = tts
  setTerms tts (OutFc (c,vs,_)) = OutFc (c,vs,tts)

instance CEta Etai where
   leftCompose op e@(Etai (l,r)) = if isId op then e else Etai (op:l,r)
   rightCompose e@(Etai (l,r)) op = if isId op then e else Etai (l,op:r)
   compose e1 (Etai ([],[])) = e1
   compose (Etai ([],[])) e2 = e2
   compose (Etai (l1,r1)) (Etai (l2,r2)) = Etai (l1++reverse r1,r2++reverse l2)
   idEta = Etai ([],[])
   isIdEta (Etai ([],[])) = True
   isIdEta _ = False

isId :: EtaOp -> Bool
isId (EOsust [] _ _) = True
isId (EOgeneral bv ts) = and (zipWith eq bv ts) && (length bv == length ts)
        where eq (Bvar bv) (Tvar v) = bv==v
              eq (Bvtuple _ bvs) (Ttuple _ ts) = and (zipWith eq bvs ts)
              eq (Bvar bv) (Ttuple _ [Tvar v]) = bv==v
              eq (Bvtuple _ [Bvar bv]) (Tvar v) = bv==v
              eq _ _ = False
isId (EOlet [] _ _ _) = True
isId EOid = True
isId _ = False

composeEta :: Acomponent a -> Etai -> TermWrapper a
composeEta a eta | isIdEta eta = TWacomp a
                 | otherwise = TWeta (TWacomp a) eta
infixr 2  `composeEta`



instance HasComponents Psi where
 getComponentTerms (Psi psis) = map getTerms psis
 renamePatternVars (Psi psis) = do alts<-mapM rename psis
                                   return$ Psi alts
       where rename (Psii (p,tts)) = do (p',susts)<-regenPatternVars p
                                        let ts' = map (substitution (map snd2Term susts) . getTerm) tts
                                        return$ Psii (p',zipWith tupleterm (map getPosition tts) ts')
 wrapSigma a = WCApsi a

snd2Term (a,b) = (a,Tvar b)

instance HasComponents OutF where
 getComponentTerms (OutF outfs) = map (\(OutFc (_,_,outs))->outs) outfs
 renamePatternVars (OutF outfs) = do alts<-mapM rename outfs
                                     return$ OutF alts
       where rename (OutFc (c,vs,tts)) = do vs'<-mapM (getFreshVar . varPrefix) vs
                                            let ts' = map (substitution (zip vs$ map Tvar vs') . getTerm) tts
                                            return$ OutFc (c,vs',zipWith tupleterm (map getPosition tts) ts')  
 wrapSigma a = WCAoutF a


regenSigmaPatternVars :: [(Variable,Variable)] -> PatternS -> IntState (PatternS,[(Variable,Variable)])
regenSigmaPatternVars acc (PcaseS t0 p t1) = do (p',sustsp)<-regenPatternVars' acc p
                                                (t1',susts1)<-regenSigmaPatternVars sustsp t1
                                                return (PcaseS t0 p' t1',susts1)
regenSigmaPatternVars acc (PcaseSana i t0 p t1) = do (p',sustsp)<-regenPatternVars' acc p
                                                     (t1',susts1)<-regenSigmaPatternVars sustsp t1
                                                     return (PcaseSana i t0 p' t1',susts1)
regenSigmaPatternVars acc (PcaseR i t0 c vs ts) = do (ts',susts)<-regen ts 
                                                     return (PcaseR i t0 c vs ts',susts)
     where regen alts = do (ts',susts)<-chainAccM acc (:) []$ map (flip regenSigmaPatternVars .fst) alts
                           let alts'= zipWith (\t (_,ps)->(t,ps)) ts' alts
                           return (alts',susts)
regenSigmaPatternVars acc (Ppattern v p) = do (p',sustsp)<-regenPatternVars' acc p
                                              return (Ppattern v p',sustsp)
regenSigmaPatternVars acc t@Pdone = return (t,acc)

instance HasComponents Sigma where
 getComponentTerms (Sigma (casemap,tts,_,_)) = concat$ zipWith replicate casemap tts
 renamePatternVars (Sigma (casemap,tts,pss,hss)) = 
                do (tts',ts')<-rename tts pss
                   return$ Sigma (casemap,tts',ts',hss)
       where rename :: [[TupleTerm]] -> [[PatternS]] 
                         -> IntState ([[TupleTerm]],[[PatternS]])
             rename tts ps = do tsusts <-mapM (mapM (regenSigmaPatternVars [])) ps
                                let (ts',susts) = unzip . map unzip $ tsusts
                                return (zipWith termSust tts (map (map snd2Term.concat) susts),ts')
             termSust :: [TupleTerm] -> [(Variable,Term)] -> [TupleTerm]
             termSust ts susts = map (\t -> tupleterm (getPosition t) (substitution susts (getTerm t))) ts
 wrapSigma a = WCAsigma a


instance HasComponents WrappedCA where
 getComponentTerms wac =
   case wac of
    WCApsi (_,_,wa) -> f wa
    WCAoutF (_,_,wa) -> f wa
    WCAsigma (_,_,wa) -> f wa
  where f wa=getComponentTerms wa
 renamePatternVars wac =
   case wac of
    WCApsi (v,t,wa) -> do wa'<-renamePatternVars wa;return$ WCApsi (v,t,wa')
    WCAoutF (v,t,wa) -> do wa'<-renamePatternVars wa;return$ WCAoutF (v,t,wa')
    WCAsigma (v,t,wa) -> do wa'<-renamePatternVars wa;return$ WCAsigma (v,t,wa')
 wrapSigma _ = error ("WrappedCA::wrapSigma: " ++ no_Sound_Operation_Aplication)


regenPatternVars :: [Pattern] -> IntState ([Pattern],[(Variable,Variable)])
regenPatternVars ps = do (pss,stss) <- chainAccM [] (:) []$ map (flip regenPatternVars') ps
                         return (pss,stss)

regenPatternVars' :: [(Variable,Variable)] -> Pattern -> IntState (Pattern,[(Variable,Variable)])
regenPatternVars' ss p@(Pvar v) | p/=pany =
      case lookup v ss of
        Nothing -> do u<-getFreshVar (varPrefix v)
                      return (Pvar u,(v,u):ss)
        Just u -> return (Pvar u,ss)
regenPatternVars' ss (Ptuple ps) = 
           do (ps',susts)<-chainAccM ss (:) []$ map (flip regenPatternVars') ps
              return (Ptuple ps',susts)
regenPatternVars' ss (Pcons c ps) = 
           do (ps',susts)<-chainAccM ss (:) []$ map (flip regenPatternVars') ps
              return (Pcons c ps',susts)
regenPatternVars' ss p = return (p,ss)

chainAccM :: Monad m => a -> (b->c->c)-> c -> [a->m (b,a)] -> m (c,a)
chainAccM ss f e = foldrM (\op (pss,ss)-> op ss >>= \(p,ss')->return (f p pss,ss')) (e,ss)

%%]
