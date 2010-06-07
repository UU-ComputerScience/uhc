%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HyloContext}
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})
%%]
import HsPretty
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
%%[(8 codegen)
import List(intersect,find,(\\),delete)



data Context = Ctx [Variable] (Maybe [Int])
 deriving Show

emptyContext :: Context
emptyContext = Ctx [] Nothing

getConstantArgs :: Context -> [Variable]
getConstantArgs (Ctx vrs _) = vrs

getCntArgPos :: Context -> Maybe [Int]
getCntArgPos (Ctx _ pos) = pos

combineContexts :: Context -> Context -> Context
combineContexts (Ctx vs _) (Ctx vs' _) = Ctx (vs++vs') Nothing

instance Vars Context where
 vars (Ctx vs _) = vs

instance Substitutable Context where
  substitute sc ss (Ctx vrs pos) = Ctx (map (substitute sc ss) vrs) pos

extractContext :: [Def] -> [(Context,Def)]
extractContext dfs@(firstd:taild) = (ctx,removeArgsDef firstd) 
                                  : map (\d -> (ctx,sustDef (zip vrs (getVars d))$ removeArgsDef d)) taild
  where idxs = findConstantArguments dfs
        vrs = getVars firstd
        ctx = Ctx vrs (Just idxs)
        getVars :: Def -> [Variable]
        getVars (Defvalue _ t) = [ v | (i,Bvar v)<-zip [0..] (fst$ extractVars t), elem i idxs ]
        getVars _ = error "extractContext.getVars: unexpected Def value."
        removeArgsDef (Defvalue v t) = Defvalue v (removeArgs (map getDefName dfs) idxs t)
        removeArgsDef _ = error "extractContext.removeArgsDef: unexpected Def value."
        sustDef ss (Defvalue v t) = Defvalue v (substitution ss' t)
           where ss' = map (\(v,v')->(v,Tvar v'))$ filter (\(v,v')->v/=v')$ ss
        sustDef _ _ = error "extractContext.removeArgsDef: unexpected Def value."
extractContext []  = [] 

mergeContext :: [(Context,Def)] -> [Def]
mergeContext dfs = map (merge (map (getDefName.snd) dfs)) dfs 
  where merge fs (Ctx vs idxs,Defvalue v t) = Defvalue v (addArgs fs (zip vs$ maybe [0..] id idxs) t)
        merge _ _ = error "mergeContext: unexpected Def value."





removeArgs :: [Variable] -> [Int] -> Term -> Term
removeArgs fs idxs t = foldr Tlamb (rmFromCalls (fs\\vars vsis) t') (removeFromList vsis idxs)
  where (vsis,t') = extractVars t
        rmFromCalls = transformTerm . removeFromCalls
        removeFromCalls fs t@(Tfapp v ts) tr | elem v fs = Tfapp v (removeFromList ts idxs)
                                             | otherwise = tr
        removeFromCalls fs (Tcase t0 ps ts) _ = Tcase (rmFromCalls fs t0) ps 
                                                      (zipWith (rmFromCalls . (fs\\) . vars) ps ts)
        removeFromCalls fs (Tlet v t0 t1) _ = Tlet v (rmFromCalls (delete v fs) t0) (rmFromCalls (delete v fs) t1)
        removeFromCalls fs (Tlamb bv t) _ = Tlamb bv (rmFromCalls (fs\\vars bv) t)
        removeFromCalls fs t tr = tr
        removeFromList xs idxs = [ x | (i,x)<-zip [0..] xs,notElem i idxs]






addArgs :: [Variable] -> [(Variable,Int)] -> Term -> Term
addArgs fs vs t = foldr Tlamb (addArgCalls fs t') (insertElems (map (applyFst Bvar) vs) vsis)
  where (vsis,t') = extractVars t
        tvs = map (applyFst Tvar) vs
        addArgCalls = transformTerm . addToCalls
        addToCalls fs t@(Tfapp v ts) tr | elem v fs = Tfapp v (insertElems tvs ts)
                                        | otherwise = tr
        addToCalls fs (Tcase t0 ps ts) _ = Tcase (addArgCalls fs t0) ps 
                                                  (zipWith (addArgCalls . (fs\\) . vars) ps ts)
        addToCalls fs (Tlet v t0 t1) _ = Tlet v (addArgCalls (delete v fs) t0) (addArgCalls (delete v fs) t1)
        addToCalls fs (Tlamb bv t) _ = Tlamb bv (addArgCalls (fs\\vars bv) t)
        addToCalls fs t tr = tr




findConstantArguments :: [Def] -> [Int]
findConstantArguments dfs = foldr1 intersect  . map (findConstantArgs' (map getDefName dfs))$ dfs
  where findConstantArgs' fs (Defvalue v t) = getIndexes$ foldr (zipWith matchArg) (map maybeBvar vsis) (collectCalls (fs\\vars vsis) t')
          where (vsis,t') = extractVars t
        findConstantArgs' _ _ = error "findConstantArgs': unexpected Def value."
        collectCalls :: [Variable] -> Term -> [[Maybe Variable]]
        collectCalls fn t =
           case t of
             Tvar _ -> []
             Tlit _ -> []
             Ttuple _ ts -> concatMap (fcaa fn) ts
             Tcase t0 ps ts -> fcaa fn t0 ++ concat (zipWith (\p->except p . fcaa (fn\\vars p)) ps ts)
             Tcapp _ ts -> concatMap (fcaa fn) ts
             Tapp t0 t1 -> fcaa fn t0 ++ fcaa fn t1
             Tlet v t0 t1 -> except v (fcaa (delete v fn) t0 ++ fcaa (delete v fn) t1)
             Tlamb bv t -> except bv$ fcaa (fn\\vars bv) t
             Tfapp v ts -> if elem v fn && null (intersect fn (vars ts))
                             then [map maybeTvar ts]
                             else concatMap (fcaa fn) ts
--             _ -> error ("findConstantArgs: non-expected term: "++show t)
          where fcaa fn = collectCalls fn 
        except :: Vars a => a -> [[Maybe Variable]] -> [[Maybe Variable]]
        except t = map (map except')
          where tvs = vars t
                except' (Just v) | elem v tvs = Nothing
                except' mv = mv
        maybeTvar (Tvar v) = Just v
        maybeTvar _ = Nothing
        maybeBvar (Bvar v) = Just v
        maybeBvar _ = Nothing
        matchArg (Just v) (Just v') | v==v' = Just v
        matchArg _ _ = Nothing
        getIndexes ls = [ i | (i,Just _)<-zip [0..] ls ]

%%]

