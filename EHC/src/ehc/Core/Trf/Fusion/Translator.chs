%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.Translator} export (convertHsModule2Syn,convertHsModule2Syn_)
%%]

%%[(8 codegen)

--import Utils

import Char(isUpper,isLower)
import Maybe(catMaybes)
import List(transpose,nub)
import Control.Monad.Error(throwError,runErrorT)
import Control.Monad.Trans(lift)
import Control.Monad.State(get,put)
--import Language.Haskell.Syntax
import qualified Data.Map as M(insertWith)
--import RenVars
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn},{%{EH}Core.Trf.Fusion.HyloFace})



convertHsModule2Syn :: HsModule -> FusionState [Def]
convertHsModule2Syn m = do p <- lift (convertHsModule2Syn_ m)
                           case p of
                            ([],dfs) -> return dfs
                            (e:_,_) -> throwError e


convertHsModule2Syn_ :: HsModule -> IntState ([FusionError],[Def])
convertHsModule2Syn_ (HsModule _ _ _ _ decls) = 
      do m<-mapM (runErrorT . convertDecl2Def) ((filter selectd) decls)
         return (concat (map (either (:[]) (const [])) m),concat . map (either (const []) (:[])) $ m)
 where selectd (HsFunBind _) = True
       selectd (HsPatBind _ (HsPVar hsName) _ _) = True
       selectd _ = False

convertDecl2Def :: HsDecl -> FusionState Def
convertDecl2Def hsDecl =
             case hsDecl of
               HsFunBind hsMatches -> f hsMatches 
               HsPatBind loc (HsPVar hsName) hsRhs hsDecls -> f [HsMatch loc hsName [] hsRhs hsDecls]
               HsForeignExport _ _ _ _ _ -> fail "foreign exports are not supported"
               HsForeignImport _ _ _ _ _ _ -> fail "foreign imports are not supported"
               HsTypeSig _ _ _ -> fail "type signatures are not supported"
               HsDefaultDecl _ _ -> fail "Default declarations are not supported"
               HsInstDecl _ _ _ _ _ -> fail "Instance declarations are not supported"
               HsClassDecl _ _ _ _ _ -> fail "Class declarations are not supported"
               HsNewTypeDecl _ _ _ _ _ _ -> fail "New type declarations are not supported"
               HsInfixDecl _ _ _ _ -> fail "Infix declarations are not supported"
               HsTypeDecl _ _ _ _ -> fail "Type declarations are not supported"
               HsDataDecl _ _ _ _ _ _ -> fail "Data declarations are not supported"
               HsPatBind _ _ _ _ -> fail "non variable pattern bindings are not supported"
  where f hsMatches =
           do r <- mapM convertHsMatch hsMatches
              let (ns,args,ts)= unzip3 r
              if null ns then fail "convertDecl2Term: we got an empty declaration"
                else lift (updateVariableGeneratorState (nub (vars (head ns)++vars args++varsB ts))
                           >> joinEquations args ts >>= return . Defvalue (head ns))
        updateVariableGeneratorState vs = do gi<-get; put (foldr updateSt gi vs)
        updateSt (Vgen p i) gi = M.insertWith max p (i+1) gi
        updateSt (Vuserdef s) gi = case str2var s of
                                    v@(Vgen _ _) -> updateSt v gi
                                    _ -> gi



joinEquations :: [[Either Pattern Variable]] -> [Term] -> IntState Term
joinEquations args ts = do (vs,t) <- renameVars (unifyPatterns args) ts
                           return (foldr (Tlamb . Bvar) t  vs)
  where renameVars args ts = do let args' = transpose args
                                vs' <- mapM genVar args'
                                (vs'',ts')<- susts vs' args' args ts
                                let vps=leftPos vs''
                                    t | null vps = head ts'
                                      | null (tail vps) = Tcase (Tvar (head vps)) (map head$ map leftPos args) ts'
                                      | otherwise = Tcase (Ttuple False$ map Tvar vps) (map Ptuple$ map leftPos args) ts'
                                return (map (either id id) vs'',t)
        leftPos = map (either id (error "joinEquations")) . filter (either (const True) (const False))
        genVar ls@(Left _:_) = getFreshVar "v" >>= return . Left
        genVar ls@(Right v:_) = return (Right v)
        genVar _ = error "joinEquations: This should never had hapenned."
        susts :: [Either Variable Variable] -> [[Either Pattern Variable]] -> 
                                               [[Either Pattern Variable]] -> [Term] -> IntState ([Either Variable Variable],[Term])
        susts vs' args' args ts = let inds = catMaybes$ zipWith (\i b->if b then (Just i) else Nothing) [0..]$ 
                                                        zipWith checkEq vs' args'
                                   in do us<-mapM (const (getFreshVar "v")) inds
                                         let inds' = zip inds us
                                         return (map (toVar inds') (zip [0..] vs'), zipWith (susts' vs' inds') args ts)
        susts' :: [Either Variable Variable] -> [(Int,Variable)] -> [Either Pattern Variable]->Term->Term
        susts' vs' inds' arg t = substitution (map (toPair arg) inds') t
        checkEq (Right a) ls = any (either (const False) (a/=)) ls
        checkEq _ _ = False
        toPair l (i,u) = either (error "joinEquations") (\v->(v,Tvar u)) (l!!i)
        toVar inds' (i,e) = either Left (\v->Right$ maybe v id$ lookup i inds') e


unifyPatterns :: [[Either Pattern Variable]] -> [[Either Pattern Variable]]
unifyPatterns args = map (map fitPS . zip [0..]) args
 where fitPS (i,e) =  either Left (if pindexElem i args then Left .Pvar else Right) e
       pindexElem :: Int -> [[Either Pattern Variable]] -> Bool
       pindexElem i l = let isPat  = either (const True) (const False) in any (isPat .(!!i)) l


convertHsMatch :: HsMatch -> FusionState (Variable,[Either Pattern Variable],Term)
convertHsMatch (HsMatch loc hname hpat rhs []) = do t<-convertRhs2Term loc rhs
                                                    ps<-mapM (convertPat2MyPat loc) hpat
                                                    return (str2var (convertHsName2String hname),map analisePat ps,t)
convertHsMatch (HsMatch loc _ _ _ _) = throwError (ParserError loc  "\"where\" clauses are not supported.")


analisePat :: Pattern -> Either Pattern Variable
analisePat (Pvar v) = Right v
analisePat p = Left p

convertRhs2Term :: SrcLoc -> HsRhs -> FusionState Term
convertRhs2Term loc hsRhs =
    case hsRhs of
       HsUnGuardedRhs hsExp -> convertHsExp2Term loc hsExp [] >>= (return .fixInfixAssoc)
       HsGuardedRhss hsGuardedRhss -> throwError (ParserError loc  "Guarded definitions are not supported.")


convertHsExp2Term :: SrcLoc -> HsExp -> [Term] -> FusionState Term
convertHsExp2Term loc exp args =
    let wildTerm = Tvar $ Vuserdef "_"
        appArgs t args = foldl Tapp t args
    in case exp of
        HsVar hsQName -> return $ convertHsQName2Term hsQName args
        HsCon hsQName -> return $ convertHsQName2Term hsQName args
        HsLit hsLiteral -> return $ Tlit (convertLit2Lit hsLiteral)
        HsInfixApp hsExp hsQOp hsExp1 -> do t0<-convertHsExp2Term loc hsExp []
                                            t1<-convertHsExp2Term loc hsExp1 []
                                            return$ convertHsQName2Term (convertHsQOP2Variable hsQOp) (t0:t1:args)
        HsApp hsExp hsExp1 -> convertHsExp2Term loc hsExp1 [] >>= convertHsExp2Term loc hsExp . (:args)
        HsNegApp hsExp -> convertHsExp2Term loc hsExp [] >>= \t-> return (appArgs(Tfapp (Vuserdef "-") [t]) args)
        HsLambda loc hsPats hsExp -> do ps<-mapM (convertPat2MyPat loc) hsPats
                                        ps'<-mapM ps2bv ps
                                        t<-convertHsExp2Term loc hsExp args
                                        return$ foldr Tlamb t ps'
                 where ps2bv (Pvar v) = return $ Bvar v
                       ps2bv (Ptuple ps) = mapM ps2bv ps >>= return . Bvtuple False
                       ps2bv _ = throwError (ParserError loc "Constructors are not allowed in patterns in lambda abstractions.")
        HsLet hsDecls hsExp -> do t<- convertHsExp2Term loc hsExp []
                                  listaVarsTerms <- mapM (convertHsLetsDect2PatyTerm loc) hsDecls
                                  return (appArgs (foldr (\(p,t0) -> Tcase t0 [p] . (:[])) t listaVarsTerms) args)
        HsIf hsExp hsExp1 hsExp2 -> do t0<-convertHsExp2Term loc hsExp []
                                       let trueCase = Pcons "True" []
                                           falseCase = Pcons "False" []
                                       termTrue<- convertHsExp2Term loc hsExp1 []
                                       termFalse<- convertHsExp2Term loc hsExp2 []
                                       return (appArgs (Tcase t0 [trueCase,falseCase] [termTrue,termFalse]) args)
        HsCase hsExp hsAlts -> do t0 <- convertHsExp2Term loc hsExp []
                                  alternativas <- mapM converthsAlt2PatyTerm hsAlts
                                  let pats = map fst alternativas
                                      terms = map snd alternativas
                                  return (appArgs (Tcase t0 pats terms) args)
        HsTuple hsExps -> do ts <- mapM (flip (convertHsExp2Term loc) []) hsExps
                             return (appArgs (Ttuple False ts) args)
        HsList hsExps -> do ts <- mapM (flip (convertHsExp2Term loc) []) hsExps
                            return (appArgs (foldr (\t1 t2->Tcapp ":" [t1,t2]) (Tcapp "[]" []) ts) args)
        HsParen hsExp -> convertHsExp2Term loc hsExp args >>= (return . Tpar)
        HsLeftSection hsExp hsQOp -> do t<-convertHsExp2Term loc hsExp []
                                        return $ convertHsQName2Term (convertHsQOP2Variable hsQOp) (t:args)
        HsRightSection hsQOp hsExp -> do t1<-convertHsExp2Term loc hsExp []
                                         if null args then throwError (ParserError loc "Non applied right sections are not supported.")
                                           else return $ convertHsQName2Term (convertHsQOP2Variable hsQOp) (head args:t1:tail args)
        _ -> throwError (ParserError loc "This kind of term is not supported.")


converthsAlt2PatyTerm :: HsAlt -> FusionState (Pattern, Term)
converthsAlt2PatyTerm (HsAlt loc hsPat hsGuardedAlts hsDecls) =
  do pat <- convertPat2MyPat loc hsPat
     term <- convertHsGuardedAlts2Term loc hsGuardedAlts
     return (pat, term)

convertHsGuardedAlts2Term :: SrcLoc -> HsGuardedAlts -> FusionState Term
convertHsGuardedAlts2Term loc x =
      case x of
        HsUnGuardedAlt hsExp -> convertHsExp2Term loc hsExp []
        HsGuardedAlts hsGuardedAlt -> throwError (ParserError loc "Guarded alternatives are not supported.")


convertPat2MyPat :: SrcLoc -> HsPat -> FusionState Pattern
convertPat2MyPat loc = convertPat2MyPat' loc . changeConsAssoc 
convertPat2MyPat' loc hsPat =
    case hsPat of
         HsPVar hsName -> return $ Pvar (str2var$ convertHsName2String hsName)
         HsPLit hsLiteral -> return $ Plit (convertLit2Lit hsLiteral)
         HsPInfixApp hsPat1 hsQName hsPat2 ->
                         do hsRes1 <- convertPat2MyPat' loc hsPat1
                            hsRes2 <- convertPat2MyPat' loc hsPat2
                            return$ convertHsQName2Pattern hsQName [hsRes1,hsRes2]
         HsPApp hsQName hsPats -> do ps<-mapM (convertPat2MyPat' loc) hsPats
                                     return$ convertHsQName2Pattern hsQName ps
         HsPTuple hsPats -> do ps<-mapM (convertPat2MyPat' loc) hsPats
                               return $ Ptuple ps
         HsPList hsPats -> do ps<-mapM (convertPat2MyPat' loc) hsPats
                              return (foldr (\t1 t2->Pcons ":" [t1,t2]) (Pcons "[]" []) ps)
         HsPParen hsPat -> (convertPat2MyPat' loc) hsPat
         HsPWildCard -> return pany
         HsPAsPat hsName hsPat -> do p<-convertPat2MyPat loc hsPat
                                     return$ Pas (str2var$ convertHsName2String hsName) p
         _ -> throwError (ParserError loc "This kind of pattern is not supported.")

changeConsAssoc :: HsPat -> HsPat
changeConsAssoc (HsPInfixApp pat (Special HsCons) a3) =
            case changeConsAssoc pat of 
              HsPInfixApp a1 (Special HsCons) a2 ->
                              HsPInfixApp a1 (Special HsCons) (HsPInfixApp a2 (Special HsCons) a3)
              p -> HsPInfixApp p (Special HsCons) (changeConsAssoc a3)
changeConsAssoc (HsPTuple hsPats) = HsPTuple (map changeConsAssoc hsPats)
changeConsAssoc (HsPList hsPats) = HsPList (map changeConsAssoc hsPats)
changeConsAssoc (HsPParen hsPat) = HsPParen (changeConsAssoc hsPat)
changeConsAssoc (HsPApp hsQName hsPats) = HsPApp hsQName (map changeConsAssoc hsPats)
changeConsAssoc p = p

convertHsLetsDect2PatyTerm :: SrcLoc -> HsDecl -> FusionState (Pattern,Term)
convertHsLetsDect2PatyTerm loc declaracion =
     case declaracion of
      HsPatBind loc hsPat hsRhs hsDecls -> do p <- convertPat2MyPat loc hsPat
                                              t <- convertRhs2Term loc hsRhs
                                              return (p,t)
      _ -> do Defvalue v t<-convertDecl2Def declaracion
              return (Pvar v,t)



convertHsName2String :: HsName -> String
convertHsName2String name =
      case name of
       HsIdent str -> str
       HsSymbol str -> str

convertHsQName2Pattern :: HsQName -> [Pattern] -> Pattern
convertHsQName2Pattern hsQName args =
 case hsQName of
   Qual (Module modulo) hsName -> Pcons (modulo ++ "." ++ convertHsName2String hsName) args
   UnQual hsName | isLower (head n) -> Pvar (str2var n)
                 | otherwise -> Pcons n args
        where n = convertHsName2String hsName
   Special HsUnitCon -> Pcons "()" args
   Special HsListCon -> foldr (\p -> Pcons ":" . (p:) . (:[])) (Pcons "[]" []) args
   Special HsFunCon -> Pcons "->" args
   Special (HsTupleCon i) -> Ptuple args
   Special HsCons -> Pcons ":" args

convertHsQName2Term :: HsQName -> [Term] -> Term
convertHsQName2Term hsQName args =
   case hsQName of
     Qual (Module modulo) hsName -> cons (modulo ++ "." ++ convertHsName2String hsName ) args
     UnQual hsName -> cons (convertHsName2String hsName) args
     Special HsUnitCon -> Tcapp "()" args
     Special HsListCon -> foldr (\p->Tcapp ":" .(p:) . (:[])) (Tcapp "[]" []) args
     Special HsFunCon -> Tcapp "->" args
     Special (HsTupleCon i) -> Ttuple False args
     Special HsCons -> Tcapp ":" args
 where cons s | isUpper (head s) = Tcapp s
              | not (null args) = Tfapp (str2var s)
              | otherwise = const (Tvar (str2var s))

convertLit2Lit :: HsLiteral -> Literal
convertLit2Lit lit =
    case lit of
       HsInt integer -> Lint (show integer)
       HsChar char -> Lchar char
       HsString string -> Lstring string
       HsFrac rational -> Lrat (show rational)
       HsCharPrim char -> Lchar char
       HsStringPrim string -> Lstring string
       HsIntPrim integer -> Lint (show integer)
       HsFloatPrim rational -> Lrat (show rational)
       HsDoublePrim rational -> Lrat (show rational)

convertHsQOP2Variable :: HsQOp -> HsQName
convertHsQOP2Variable op =
     case op of
       HsQVarOp hsQName -> hsQName
       HsQConOp hsQName -> hsQName




fixInfixAssoc :: Term -> Term
fixInfixAssoc (Ttuple False ps) = Ttuple False $ map fixInfixAssoc ps
fixInfixAssoc (Tapp t1 t2) = Tapp (fixInfixAssoc t1) (fixInfixAssoc t2)
fixInfixAssoc (Tlamb bv t) = Tlamb bv (fixInfixAssoc t)
fixInfixAssoc (Tlet v t0 t1) = Tlet  v (fixInfixAssoc t0) (fixInfixAssoc t1)
fixInfixAssoc (Tcase t ps ts) = Tcase (fixInfixAssoc t) ps (map fixInfixAssoc ts)
fixInfixAssoc (Tpar t) = fixInfixAssoc t
fixInfixAssoc (Tcapp n1 ts1@(t1@(Tcapp _ (_:_)):tss1)) =
       case fixInfixAssoc t1 of
        Tcapp n2 ts2 | infx n1 && infx n2 -> Tcapp n2 (init ts2 ++ [Tcapp n1 (last ts2:map fixInfixAssoc tss1)])
                     | otherwise -> Tcapp n1 $ map fixInfixAssoc ts1
        _ -> error "fixInfixAssoc Term: unexpected output."
  where infx n = n==":"
fixInfixAssoc (Tcapp n ts) = Tcapp n $ map fixInfixAssoc ts
fixInfixAssoc (Tfapp v ts) = Tfapp v $ map fixInfixAssoc ts
fixInfixAssoc t@(Tvar _) = t
fixInfixAssoc t@(Tlit _) = t
fixInfixAssoc t = error "fixInfixAssoc Term: not defined."
%%]
