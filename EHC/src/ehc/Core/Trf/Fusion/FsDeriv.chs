%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.FsDeriv} export (aA)
import List hiding (intersperse)
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.RenVars})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloFace})

import Control.Monad.Error(throwError)
import Control.Monad.Trans(lift)
import Maybe(catMaybes)


type DReturnType = ([(Variable,Variable)], [(Variable, Int,[[Int]], [Term])], Term)

aD :: [(Int,[Variable])] -> [Variable] -> [(Variable,Int)] -> Term -> FusionState DReturnType

aD pvss c1 fs t =
 let d = aD pvss c1 fs
     except ex t =
            -- I want to eliminate variables of vsis when examining
            -- expressions in a let or lambda scope.
            -- If the intersections of variables to eliminate and
            -- vsis is non-empty, then if recursive calls appear in the
            -- the term they are not saturated and so I must throw NotSaturated.
            let vs = vars ex in
                aD pvss (c1 \\ vs) fs t
 in
 case t of
   Tvar v -> if elem v c1 then do u<-lift$ getFreshVar (varPrefix v); return ([(v,u)],[],Tvar u)
                          else return ([],[],t)
   Tlit _ -> return ([],[],t)
   Ttuple b ts -> do res<- mapM d ts
                     let (vs,ps,ts') = unzip3 res
                     return (foldr (++) [] vs, concat ps, Ttuple b ts')
   Tlamb bv t -> do (c,p,t') <- except bv t
                    return (c,p,Tlamb bv t')
   Tlet v t1 t0 -> do (s0,c0,t0') <- except v t0
                      (s1,c1,t1') <- except v t1
                      return (s0++s1, c0++c1, Tlet v t1' t0')
   Tcase t0 ps ts -> let aux pi t = do (vs2,ps2,t') <- except pi t
                                       return (vs2,ps2,t')
                   in
                   do res <- sequence$ zipWith aux ps ts
                      let (vs,pos,ts') = unzip3 res
                      (v0,p0,t0') <- d t0
                      return ((v0++).foldr (++) []$ vs,
                              p0++concat pos, Tcase t0' ps ts')
   Tfapp v ts -> 
          do res <- mapM d ts
             let (vs,ps,ts') = unzip3 res
             (v0,p0,_) <- d (Tvar v)
             let retres = ((v0++).foldr (++) []$ vs, p0++concat ps, Tfapp v ts')
             if any (not.null) ps then return retres
               else case findIndex ((v==).fst) fs of
                     Just idx | Just nargs<-lookup v fs ->
                          -- All application parameters must be equal to the
                          -- variables in vsis, except the last arguments that
                          -- are the recursive ones.
                          if length ts==nargs then
                             do let tvs=vars ts
                                u<-lift$ getFreshVar (if null tvs then "v" else varPrefix (head tvs))
                                return ([],[(u,idx,map (findArgumentIndexes pvss) ts,ts)],Tvar u)
                          else throwError (NotSaturated t)
                     _ -> return retres
 
   Tcapp cons ts -> do res <- mapM d ts
                       let (vs,ps,ts') = unzip3 res
                       return (foldr (++) [] vs,
                               concat ps, Tcapp cons ts')
 
   Tapp t1 t2 -> do (vs1,ps1,t1') <- d t1
                    (vs2,ps2,t2') <- d t2
                    return (vs1++vs2, ps1++ps2, Tapp t1' t2')
   _ -> throwError (NotExpected t)
 where findArgumentIndexes pvss t = map fst . filter (not . null . intersect (vars t) . snd)$ pvss
















type InputLine = ([Pattern],DReturnType)
aA :: [Variable] -> [Term] -> FusionState [([Boundvar],[Term],[InputLine])]
















aA fs ts = sequence . zipWith (aA' id (zip fs (map countArgs ts))) fs$ ts
  where countArgs (Tlamb _ t) = 1 + countArgs t
        countArgs t = 0



aA' :: ([Boundvar]->[Boundvar]) -> [(Variable,Int)] -> Variable -> Term -> FusionState ([Boundvar],[Term],[InputLine])
aA' fvars fs f (Tlamb bv t) =
                   do res<-aA' (fvars.(bv:)) fs f t
                      return res
 
aA' vsis fs f (Tcase t0 ps ts) =
                     aA'' fs f (vsis []) t0 ps ts







aA' vsis fs f t =
                 do res<-aA'' fs f (vsis []) (Ttuple False []) [pany] [t]
                    return res





aA'' :: [(Variable,Int)] -> Variable -> [Boundvar] -> Term ->
        [Pattern] -> [Term] -> FusionState ([Boundvar],[Term],[InputLine])
aA'' fs f recs'' t' ps' ts' =
  do (recs,t,ps,ts) <- lift$ removePas recs'' t' ps' ts'
     let recs' = splitVars t0s (\bv t -> not$ null$ intersect (vars t)$ vars$ bv) recs t0s -- variables to insert in the case
         t0s = termToList t
         t0s' = adaptTerms recs' t0s
         ps' = map (adaptPattern recs' t0s) ps
     if not (matchOrder recs t0s') 
       then throwError$
              Msg$ show f++": Order or ammount of recursive arguments "
                         ++"does not match with the main case." {- ++show recs++" "++show t0s' -}
       else do lines <- sequence (zipWith (appD recs) 
                                          ps'
                                          ts)
               return (recs,t0s',lines)
  where matchOrder (Bvar bv:bvs) (Tvar v:ts) = bv==v && matchOrder bvs ts
        matchOrder (_:bvs) (_:ts) = matchOrder bvs ts
        matchOrder [] [] = True
        matchOrder _ _ = False
        -- calls aD. pis are the argument indexes for
        -- each t0, recs are the recursive arguments,
        -- pi is the pattern in a case alternative and ti the corresponding term.
        appD recs pi ti =
             let ps = patternToList pi
              in do r<-aD [(i,vars r++vars p) | (i,Bvar r,p)<-zip3 [0..] recs ps]
                          (union (vars recs) (vars ps)) fs ti
                    return (ps,r)
        adaptPattern recs [] p = ptuple$ map (toPat (vars p))$ concat$ recs
        adaptPattern recs t0s p = 
                  ptuple$ intersperse (map (map (toPat (vars p))) recs) (patternToList p)
        toPat vps (Bvtuple _ bvs) = Ptuple (map (toPat vps) bvs)
        toPat vps (Bvar v) | elem v vps = pany
                           | otherwise = Pvar v
        adaptTerms recs t0s = intersperse (map (map bv2term) recs) t0s
        eqTermBv (Tvar v) (Bvar v') = v==v'
        eqTermBv (Ttuple _ bvs) (Bvtuple _ ts) = and . zipWith eqTermBv bvs$ ts
        eqTermBv _ _ = False
        splitVars :: [b] -> (a->b->Bool) -> [a] -> [b] -> [[a]]
        splitVars bst p [] bs = []
        splitVars bst p as [] = [as]
        splitVars bst p as bs = let (as',ass) = break (flip any bst . p) as
                                    (bs',bss) = break (p (head ass)) bs
                                    tail' [] = []
                                    tail' ls = tail ls
                                 in if null ass then [as']
                                      else as' : map (const []) bs' ++ splitVars bst p (tail' ass) (tail' bss)

intersperse :: [[a]] -> [a] -> [a]
intersperse (as:ass) (b:bs) = as ++ b : intersperse ass bs
intersperse ass [] = concat ass
intersperse [] bs = bs











removePas :: [Boundvar] -> Term -> [Pattern] -> [Term] -> IntState ([Boundvar],Term,[Pattern],[Term])
removePas bvs t0 ps ts =
      do removeus <- sequence (zipWith removable t0s pss)
         let catremoveus = catMaybes removeus
         if null catremoveus then return (bvs,t0,ps,ts)
           else let (pss',sustss)=unzip$ zipWith removePas' removeus pss
                 in return (map (substitute [] catremoveus) bvs
                           ,substitution (map (\(v,u)->(v,Tvar u)) catremoveus) t0
                           ,map ptuple$ transpose pss'
                           ,zipWith substitution (map catMaybes$ transpose sustss) ts)
  where t0s = termToList t0
        pss = transpose$ map patternToList ps
        removePas' (Just (_,u)) ps = (map remPas ps,map (getPas (Tvar u)) ps)
        removePas' _ ps = (ps,[])
        removable :: Term -> [Pattern] -> IntState (Maybe (Variable,Variable))
        removable (Tvar v) ps = if any isPas ps then getFreshVar "t" >>= \u->return (Just (v,u)) else return Nothing
        removable _ _ = return Nothing
        remPas (Pas _ p) = p
        remPas p = p
        getPas u (Pas v _) = Just (v,u)
        getPas u _ = Nothing
        isPas (Pas _ _) = True
        isPas _ = False

termToList (Ttuple _ t0s) = t0s
termToList t = [t]
patternToList (Ptuple ps) = ps
patternToList p = [p]

%%]
