
%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.HyloParser} 
--export (parseAST,parseHsModule)
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Exception},{%{EH}Core.Trf.Fusion.HsSyn},{%{EH}Core.Trf.Fusion.HyloFace},{%{EH}Core.Trf.Fusion.HyloContext},{%{EH}Core.Trf.Fusion.FuseFace})
%%]

%%[(8 codegen)


--import Language.Haskell.Parser(parseModule,ParseResult(..))
--import Language.Haskell.Syntax(SrcLoc,HsModule)
--import Translator




--import Utils
--import RenVars
--import FuseFace
--import FsDeriv
--import HyloFace
--import HyloContext
import Control.Monad(zipWithM)
import Control.Monad.Error(throwError,runErrorT)
import Control.Monad.Trans(lift)
import Control.Monad.State(StateT(..),State,MonadState(..))
import List(partition,intersect,union,find,findIndex,nubBy,(\\),deleteFirstsBy,delete,sort)


%%]

parse :: String -> FusionState [HyloT]
parse inp = parseAST (parseModule inp)
%%[(998 codegen)

parseAST ast = {*  *}catchParseState (\m-> convertHsModule2Syn m >>= 
                                  lift . removeInputVar >>= 
                                  lift . handleRegularFunctions . getCycles . filterDataTypeDefs >>=
                                  mapM deriveHylo) 
                            (\loc -> throwError . errorParserError loc)$ ast

parseHsModule :: HsModule -> IntState ([FusionError],[HyloT])
parseHsModule m = 
  convertHsModule2Syn_ m >>= 
  removeInputVar . snd >>= 
  handleRegularFunctions . getCycles . filterDataTypeDefs >>=
  (\dss -> do he <- mapM (runErrorT . deriveHylo) dss
              return (concat (map (either (:[]) (const [])) he),concat (map (either (const []) (:[])) he))
  )


filterDataTypeDefs :: [Def] -> [Def]
filterDataTypeDefs list = filter fdefs list
 where fdefs (Defvalue _ _) = True
       fdefs _ = False




%%]

catchParseState :: (a->b) -> (SrcLoc->String->b) -> ParseResult a -> b
catchParseState f h (ParseOk p) = f p
catchParseState f h (ParseFailed loc err) = h loc err

%%[(998 codegen)


getCycles :: [Def] -> [[Def]]
getCycles defs = let idxs=findCycles (getDependencyGraph (filter funs defs))
                  in map (map (defs!!). sort) idxs
  where funs (Defvalue _ _) = True
        funs _ = False

collect :: [Maybe a] -> [a]
collect = foldr (\a r->maybe r (:r) a) []



getDependencyGraph :: [Def] -> [[Int]]
getDependencyGraph ds = map (dps (zip (map getV ds) [0..])) ds
 where dps ps (Defvalue v t) = collect .map (flip lookup ps).vars$ t
       dps _ _ = error$ "getDependencyGraph: dps: unexpected Defdata value"
       getV (Defvalue v t) = v
       getV _ = error$ "getDependencyGraph: getV: unexpected Defdata value"

findCycles :: [[Int]] -> [[Int]]
findCycles g = joinCycles [] $ concat $ map (follow [] g) [0..length g-1]
  where follow :: [Int] -> [[Int]] -> Int -> [[Int]]
        follow vs g i | elem i vs = [i:takeWhile (/=i) vs]
                      | otherwise = concat$ map (follow (i:vs) g) (g!!i)
        joinCycles :: [[Int]] -> [[Int]] -> [[Int]]
        joinCycles ant (is:iss) = let (cs1,cs2) = partition (null.intersect is) ant
                                   in joinCycles (foldr union is cs2:cs1) iss
        joinCycles ant [] = ant











removeInputVar :: [Def] -> IntState [Def]
removeInputVar defs = mapM removeInputVar' defs
removeInputVar' (Defvalue v t) = inTlamb t >>= return . Defvalue v
 where inTlamb :: Term -> IntState Term
       inTlamb (Tlamb bv t) = inTlamb t >>= return . Tlamb bv
       inTlamb (Tcase t0@(Tvar v0) ps ts) = sequence (zipWith (selectP v0) ps ts) >>= return . uncurry (Tcase t0) . unzip
       inTlamb t = return t
       selectP v0 p t | not (elem v0 (vars p)) && elem v0 (vars t) = 
                                  renamePany p >>= \p'-> return (p',substitution [(v0,pat2term p')] t)
                      | otherwise = return (p,t)
       renamePany (Pvar (Vuserdef "_")) = getFreshVar "v" >>= return . Pvar
       renamePany p@(Pvar _) = return p
       renamePany (Ptuple ps) = mapM renamePany ps >>= return . Ptuple
       renamePany (Pcons c ps) = mapM renamePany ps >>= return . Pcons c
       renamePany (Pas v p) = renamePany p >>= return . Pas v
       renamePany p@(Plit _) = return p
       pat2term (Pvar v) = Tvar v
       pat2term (Ptuple ps) = Ttuple False$ map pat2term ps
       pat2term (Pcons c ps) = Tcapp c$ map pat2term ps
       pat2term (Plit l) = Tlit l
       pat2term (Pas v _) = Tvar v
removeInputVar' d = return d


type CallDescription = (Variable,Def,Int,[Variable],Term)




handleRegularFunctions :: [[Def]] -> IntState [[Def]]
handleRegularFunctions dss = handleRegularFunctions' dss [] (map (const []) dss) dss
handleRegularFunctions' :: [[Def]] -> [CallDescription] -> [[Variable]] -> [[Def]] -> IntState [[Def]]
handleRegularFunctions' p calls dns [] = return p
handleRegularFunctions' p calls dns ds = 
  do cs<-zipWithM (getCallDefs p calls) dns ds
     let (dfs,nfs)=unzip cs
     if all null nfs then return dfs
       else mapM (mapM buildDef . nubBy eq) nfs 
            >>= handleRegularFunctions' (zipWith (++) dfs (zipWith (deleteFirstsBy eqDefs) p dfs)) 
                                        (calls++concat nfs) (zipWith (++) dns (map (map getDefName) ds))
            >>= return . zipWith (++) dfs
 where eq (v1,d1,i1,_,t1) (v2,d2,i2,_,t2) = i1==i2 && (getDefName d1)==(getDefName d2) && t1==t2
       getCallDefs :: [[Def]] -> [CallDescription] -> [Variable] -> [Def] -> IntState ([Def],[CallDescription])
       getCallDefs p calls dns ds = mapM (getCalls p calls (dns++map getDefName ds)) ds 
                                    >>= (\ (dfs,m)-> return (dfs,concat m)) . unzip
       eqDefs d d' = getDefName d == getDefName d'

buildDef :: CallDescription -> IntState Def
buildDef (u,d,i,vs,t) = buildDef' u i vs t d
buildDef' u i vs t (Defvalue nd t0) = 
   do (us,t')<-regenVars t
      t0'<-regenConstantArgs t t0
      let (bvs,t0'') = getInputVars t0'
          (ant,pos)=splitAt i bvs
          bs=vars ant++vars (tail pos)
      return$ Defvalue u (foldr Tlamb (adapt bs us t' (head pos) t0'') (map Bvar us++ant++tail pos))
 where adapt bs us t (Bvar l) t0 = substitution [(l,t)]$ adaptr bs us t0
       adapt bs us t bv t0 = Tcase t [bv2pat bv] [adaptr bs us t0]
       getInputVars (Tlamb bv t) = let (bs,t')=getInputVars t in (bv:bs,t')
       getInputVars t = ([],t)
       bv2pat (Bvar v) = Pvar v
       bv2pat (Bvtuple _ bvs) = Ptuple (map bv2pat bvs)
       regenConstantArgs t t0 = do let freeVars = vars t \\ vs
                                   us<-mapM (getFreshVar . varPrefix) freeVars
                                   return$ substitute [] (zip freeVars us) t0
       regenVars t = do us<-mapM (getFreshVar . varPrefix) vs
                        return (us,substitution (zip vs (map Tvar us)) t)
       adaptr bs us (Ttuple b ts) = Ttuple b (map (adaptr bs us) ts)
       adaptr bs us (Tcapp c ts) = Tcapp c (map (adaptr bs us) ts)
       adaptr bs us (Tcase t0 ps ts) = Tcase (adaptr bs us t0) ps 
                                               (zipWith (\p->adaptr (vars p++bs) us) ps ts)
       adaptr bs us (Tlet v t0 t1) = Tlet v (adaptr (v:bs) us t0) (adaptr (v:bs) us t1)
       adaptr bs us (Tlamb v t) = Tlamb v (adaptr (vars v++bs) us t)
       adaptr bs us (Tapp t0 t1) = tapp (adaptr bs us t0) (adaptr bs us t1)
       adaptr bs us (Tfapp fv ts) | elem fv bs = Tfapp fv (map (adaptr bs us) ts)
                                  | fv == nd = let (ant,pos)=splitAt i (map (adaptr bs us) ts)
                                                   in Tfapp u (map Tvar us++ant++tail pos)
                                  | otherwise = Tfapp fv (map (adaptr bs us) ts)
       adaptr bs us t = t
buildDef' u i rvs t (Defdata _) = error "buildDef': unexpected Defdata"














getCalls :: [[Def]] -> [CallDescription] -> [Variable] -> Def -> IntState (Def,[CallDescription])
getCalls ps calls ds d@(Defvalue v t) = runStateT (do (t',ds')<-getCalls' [] t; return$ (Defvalue v t',ds')) calls >>= return . fst
 where getCalls' :: [Variable] -> Term -> StateT [CallDescription] (State GenDict) (Term,[CallDescription])
       getCalls' bs (Ttuple b ts) = do (ts',ns)<-mapGetCalls' bs ts
                                       return (Ttuple b ts',ns)
       getCalls' bs (Tlamb bv t) = do (t',ns)<-getCalls' (bs++vars bv) t; return (Tlamb bv t',ns)
       getCalls' bs (Tcase t0 ps ts) = do (t0',n0)<-getCalls' bs t0
                                          res<-sequence$ zipWith (getCalls'.(bs++).vars) ps ts
                                          let (ts',ns)=unzip res
                                          return (Tcase t0' ps ts',n0++concat ns)
       getCalls' bs (Tapp t0 t1) = do (t0',n0)<-getCalls' bs t0
                                      (t1',n1)<-getCalls' bs t1
                                      return (tapp t0' t1',n0++n1)
       getCalls' bs (Tlet v t0 t1) = do (t0',n0)<-getCalls' (v:bs) t0
                                        (t1',n1)<-getCalls' (v:bs) t1
                                        return (Tlet v t0' t1',n0++n1)
       getCalls' bs (Tcapp c ts) = do (ts',ns)<-mapGetCalls' bs ts
                                      return (Tcapp c ts',ns)
       getCalls' bs (Tfapp v ts) =
           do (ts',ns)<-mapGetCalls' bs ts
              let rr = return (Tfapp v ts',ns)
                  mi = [ p | p@(_,t)<-zip [0..] ts', any (flip elem (vars t)) ds ]
                  checkNoPattern (idxs,d@(Defvalue v t)) = 
                         if not (null mi) -- there is a recursive call
                              && all (flip elem idxs.fst) mi -- all recursive calls appear in constant positions
                              && all callIsOkToSpecialize mi 
                            then mr (fst (head mi)) d
                            else rr
                     where (vargs,t') = extractVars t
                           callIsOkToSpecialize (i,Tfapp v' ts) = 
                                         elem v' ds && all isVar ts 
                                         && (length ts < lengthvargs'
                                             || length ts==lengthvargs'
                                                -- variable is used at most once
                                                && countLinear (getVar (vargs!!i)) t'<2)
                               where lengthvargs' = maybe (error "lengthvars'") 
                                                    (length.fst.extractVars.getDefTerm) $ find ((v'==).getDefName)$ concat ps
                           callIsOkToSpecialize (i,Tvar _) = True
                           callIsOkToSpecialize _ = False
                           isVar (Tvar _) = True
                           isVar _ = False
                           getVar (Bvar v) = v
                           getVar _ = error "getCalls': getVar"
                  checkNoPattern _ = error "checkNoPattern: unexpected value"
                  mr i d = do let (ant,pos)=splitAt i ts'
                                  vs = filter (\x-> elem x bs) (vars (head pos))
                              calls<-get
                              case find (\(_,d',i',vs',t')-> i'==i 
                                                          && getDefName d'==getDefName d
                                                          && vs==vs'
                                                          && t'==head pos) 
                                        calls of
                               Nothing -> do u<-lift$ getFreshVar (varPrefix v)
                                             let c = (u,d,i,vs,head pos)
                                             put (c:calls)
                                             return (Tfapp u (map Tvar vs++ant++tail pos),c:ns)
                               Just c@(u,_,_,_,_) -> return (Tfapp u (map Tvar vs++ant++tail pos),ns)
              if elem v bs then rr
               else maybe rr checkNoPattern (lookupDef v (map constantArgs ps) ps)
       getCalls' _ t = return (t,[])
       constantArgs :: [Def] -> [Int]
       constantArgs dfs = findConstantArguments dfs 
       mapGetCalls' bs ts = do res<-mapM (getCalls' bs) ts
                               let (ts',ns)=unzip res
                               return (ts',concat ns)
       lookupDef :: Variable -> [[Int]] -> [[Def]] -> Maybe ([Int],Def)
       lookupDef v argidxs ps = find ((v==).getDefName.snd)$ [ (idxs,df) | (idxs,dfs)<-zip argidxs ps, df<-dfs]
getCalls _ _ _ (Defdata _) = error "getCalls: unexpected Defdata"

%%]
