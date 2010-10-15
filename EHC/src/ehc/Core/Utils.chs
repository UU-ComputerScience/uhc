%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.Utils}
%%]

%%[(8 codegen) hs import({%{EH}Base.Builtin},{%{EH}Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core},{%{EH}Gam.Full})
%%]

%%[(8 codegen) hs import({%{EH}AbstractCore})
%%]
%%[(8 codegen) hs import({%{EH}AbstractCore.Utils} hiding (rceMatch)) export(module {%{EH}AbstractCore.Utils})
%%]

%%[(8 codegen) import({%{EH}Core.Subst})
%%]
%%[(8 codegen) import({%{EH}VarMp},{%{EH}Substitutable})
%%]
%%[(8 codegen) import(Data.List,Data.Maybe,qualified Data.Set as Set,Data.List,qualified Data.Map as Map,EH.Util.Utils)
%%]

%%[(20 codegen) import(Control.Monad.State, Data.Array)
%%]
%%[(20 codegen) import(qualified EH.Util.FastSeq as Seq)
%%]
%%[(20 codegen) import({%{EH}Core.FvS}, {%{EH}Core.ModAsMap})
%%]

-- debug
%%[(8 codegen) import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(RCEEnv)
type RCEEnv = RCEEnv' CExpr CMetaVal CBind CBindAspect Ty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Remainder of pattern for extensible records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) export(MbCPatRest)
type MbCPatRest = MbPatRest' CPatRest
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FieldUpdateL,fuL2ExprL,fuMap)
type FieldUpdateL e = AssocL HsName (e,Maybe Int)

fuMap :: (HsName -> e -> (e',Int)) -> FieldUpdateL e -> FieldUpdateL e'
fuMap f = map (\(l,(e,_)) -> let (e',o) = f l e in (l,(e',Just o)))

fuL2ExprL' :: (e -> CExpr) -> FieldUpdateL e -> [CExpr]
fuL2ExprL' f l = [ f e | (_,(e,_)) <- l ]

fuL2ExprL :: FieldUpdateL CExpr -> [CExpr]
fuL2ExprL = fuL2ExprL' cexprTupFld

fuReorder :: EHCOpts -> [HsName] -> FieldUpdateL CExpr -> (CBindL,FieldUpdateL (CExpr -> CExpr))
fuReorder opts nL fuL
  =  let  (fuL',offL,_,_)
            =  foldl
                 (\(fuL,offL,exts,dels) (n,(_,(f,_)))
                     ->  let  mkOff n lbl o
                                =  let smaller l = rowLabCmp l lbl == LT
                                       off = length (filter smaller dels) - length (filter smaller exts)
                                   in  acoreBind1Cat CBindings_Plain n (acoreBuiltinAddInt opts o off)
                              no = acoreVar n
                         in   case f of
                                 CExpr_TupIns _ t l o e -> ((l,(\r -> CExpr_TupIns r t l no e,Nothing)) : fuL,(mkOff n l o):offL,l:exts,dels  )
                                 CExpr_TupUpd _ t l o e -> ((l,(\r -> CExpr_TupUpd r t l no e,Nothing)) : fuL,(mkOff n l o):offL,exts  ,dels  )
                                 CExpr_TupDel _ t l o   -> ((l,(\r -> CExpr_TupDel r t l no  ,Nothing)) : fuL,(mkOff n l o):offL,exts  ,l:dels)
                 )
                 ([],[],[],[])
            .  zip nL
            $  fuL
          cmpFU (n1,_ ) (n2,_) = rowLabCmp n1 n2
     in   (offL, sortBy cmpFU fuL')
%%]

%%[(10 codegen) export(fuMkCExpr)
fuMkCExpr :: EHCOpts -> UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = acoreBind1Cat CBindings_Plain n r : oL
     in   acoreLet CBindings_Strict bL $ foldl (\r (_,(f,_)) -> f r) (acoreVar n) $ fuL'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Free var closure, and other utils used by Trf/...GlobalAsArg transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(fvsClosure,fvsTransClosure)
fvsClosure :: FvS -> FvS -> FvS -> FvSMp -> FvSMp -> (FvSMp,FvSMp)
fvsClosure newS lamOuterS varOuterS fvmOuter fvmNew
  =  let  fvmNew2  =  Map.filterWithKey (\n _ -> n `Set.member` newS) fvmNew
          fvlam  s =  lamOuterS `Set.intersection` s
          fvvar  s =  varOuterS `Set.intersection` s
          fv     s =  fvvar s `Set.union` s'
                   where s' = Set.unions $ map (\n -> Map.findWithDefault Set.empty n fvmOuter) $ Set.toList $ fvlam $ s
     in   (Map.map fv fvmNew2,Map.map (`Set.intersection` newS) fvmNew2)

fvsTransClosure :: FvSMp -> FvSMp -> FvSMp
fvsTransClosure lamFvSMp varFvSMp
  =  let  varFvSMp2 = Map.mapWithKey
                       (\n s -> s `Set.union` (Set.unions
                                               $ map (\n -> panicJust "fvsTransClosure.1" $ Map.lookup n $ varFvSMp)
                                               $ Set.toList
                                               $ panicJust "fvsTransClosure.2"
                                               $ Map.lookup n lamFvSMp
                       )                      )
                       varFvSMp
          sz = sum . map Set.size . Map.elems
     in   if sz varFvSMp2 > sz varFvSMp
          then fvsTransClosure lamFvSMp varFvSMp2
          else varFvSMp
%%]

%%[(8 codegen) export(fvLAsArg,mkFvNm,fvLArgRepl,fvVarRepl)
fvLAsArg :: CVarIntroMp -> FvS -> CVarIntroL
fvLAsArg cvarIntroMp fvS
  =  sortOn (cviLev . snd)
     $ filter (\(_,cvi) -> cviLev cvi > cLevModule)
     $ map (\n -> (n,cviLookup n cvarIntroMp))
     $ Set.toList fvS

mkFvNm :: Int -> HsName -> HsName
mkFvNm i n = hsnUniqifyInt HsNameUniqifier_New i n -- hsnSuffix n ("~" ++ show i)

fvLArgRepl :: Int -> CVarIntroL -> (CVarIntroL,CVarIntroL,CVarReplNmMp)
fvLArgRepl uniq argLevL
  =  let  argNL = zipWith (\u (n,i) -> (mkFvNm u n,i)) [uniq..] argLevL
     in   ( argLevL
          , argNL
          , Map.fromList $ zipWith (\(o,_) (n,cvi) -> (o,(cvrFromCvi cvi) {cvrRepl = n})) argLevL argNL
          )

fvVarRepl :: CVarReplNmMp -> HsName -> CExpr
fvVarRepl nMp n = maybe (acoreVar n) (acoreVar . cvrRepl) $ Map.lookup n nMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FldOffset(..),foffMkOff,foffLabel)
data FldOffset
  = FldKnownOffset      { foffLabel'     :: !HsName, foffOffset   :: !Int      }
  | FldComputeOffset    { foffLabel'     :: !HsName, foffCExpr    :: !CExpr    }
  | FldImplicitOffset

instance Eq FldOffset where
  (FldKnownOffset _ o1) == (FldKnownOffset _ o2) = o1 == o2
  foff1                 == foff2                 = foffLabel foff1 == foffLabel foff2

instance Ord FldOffset where
  (FldKnownOffset _ o1) `compare` (FldKnownOffset _ o2) = o1 `compare` o2
  foff1                 `compare` foff2                 = foffLabel foff1 `rowLabCmp` foffLabel foff2

foffMkOff :: FldOffset -> Int -> (Int,CExpr)
foffMkOff FldImplicitOffset      o = (o,acoreInt o)
foffMkOff (FldKnownOffset   _ o) _ = (o,acoreInt o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff              = foffLabel' foff
%%]

%%[(8 codegen) export(FieldSplitL,fsL2PatL)
type FieldSplitL = AssocL FldOffset RPat

fsL2PatL :: FieldSplitL -> [RPat]
fsL2PatL = assocLElts
%%]
type FieldSplitL = AssocL FldOffset CPatL

fsL2PatL :: FieldSplitL -> CPatL
fsL2PatL = concat . assocLElts

-- Reordering compensates for the offset shift caused by predicate computation, which is predicate by predicate
-- whereas these sets of patterns are dealt with in one go.
%%[(8 codegen) export(fsLReorder)
fsLReorder :: EHCOpts -> FieldSplitL -> FieldSplitL
fsLReorder opts fsL
  =  let  (fsL',_)
            =  foldr
                 (\(FldComputeOffset l o,p) (fsL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  acoreBuiltinAddInt opts o nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   tyRowCanonOrderBy compare fsL'
%%]

%%[(8888 codegen) export(rpbReorder,patBindLOffset)
rpbReorder :: EHCOpts -> [RPatFld] -> [RPatFld]
rpbReorder opts pbL
  =  let  (pbL',_)
            =  foldr
                 (\(RPatFld_Fld l o n p) (pbL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  acoreBuiltinAddInt opts o nrSmaller
                         in   ((RPatFld_Fld l (mkOff l exts o) n p):pbL,l:exts)
                 )
                 ([],[])
            $  pbL
          cmpPB (RPatFld_Fld l1 _ _ _)  (RPatFld_Fld l2 _ _ _) = rowLabCmp l1 l2
     in   sortBy cmpPB pbL'

patBindLOffset :: [RPatFld] -> ([RPatFld],[CBindL])
patBindLOffset
  =  unzip
  .  map
       (\b@(RPatFld_Fld l o n p@(RPat_Var pn _))
           ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset $ rpatNmNm pn
               in   case o of
                      CExpr_Int _  -> (b,[])
                      _            -> (RPatFld_Fld l (acoreVar offNm) n p,[acoreBind1Cat CBindings_Plain offNm o])
       )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module merge by pulling in only that which is required
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cModMergeByPullingIn is independent of AST, so should be placed in shared module when used for (say) GRIN

%%[(20 codegen) hs export(cModMergeByPullingIn)
data PullState cat bind
  = PullState
      { pullstBinds     :: Seq.Seq (CDbBindLetInfo'2 cat bind)  -- pulled in bindings
      , pullstPulledNmS :: !HsNameS                             -- pulled in names  
      , pullstToDo      :: ![HsName]                            -- todo
      }

emptyPullState :: PullState cat bind
emptyPullState = PullState Seq.empty Set.empty []

-- | merge by pulling in that which is required only
cModMergeByPullingIn
  ::                                -- function giving bindings for name
     (HsName                        -- name
      -> Maybe
           ( cat                    -- category
           , [bind]                 -- and bindings
           , HsNameS                -- pulled in names (might be > 1 for mutual recursiveness)
     )     )
     -> (expr -> HsNameS)           -- extract free vars
     -> (bind -> [expr])            -- extract relevant exprs for binding
     -> ([(cat,[bind])] -> mod -> mod)
                                    -- update module with pulled bindings
     -> expr                        -- start of pulling in, usually top level name "main"
     -> ( (mod -> mod)              -- conversion of resulting module
        , HsNameS                   -- modules from which something was taken
        )
cModMergeByPullingIn
     pullIn getExprFvS getBindExprs updMod
     rootExpr
  = ( updMod (Seq.toList $ pullstBinds st)
    , Set.map (panicJust "cModMergeByPullingIn" . hsnQualifier) $ pullstPulledNmS st
    )
  where (_,st) = runState (pull) (emptyPullState {pullstToDo = Set.toList $ getExprFvS rootExpr})
        pull = do
          s <- get
          case pullstToDo s of
            (nm:nmRest)
              | nm `Set.notMember` pullstPulledNmS s && isJust mbPull
                -> do let pulledNms = pullstPulledNmS s `Set.union` pulled
                          newNms
                            = -- (\x -> tr "cModMergeByPullingIn.pulledNms" (nm >#< show x) x) $
                              (Set.unions $ map (Set.unions . map getExprFvS . getBindExprs) binds)
                              `Set.difference` pulledNms
                      put $
                        s { pullstToDo = Set.toList newNms ++ nmRest
                          , pullstBinds = Seq.singleton (cat,binds) `Seq.union` pullstBinds s
                          , pullstPulledNmS = pulledNms
                          }
                      pull
              | otherwise
                -> do put $
                        s { pullstToDo = nmRest
                          }
                      pull
              where mbPull@(~(Just (cat,binds,pulled))) = pullIn nm
            _ -> return ()
%%]

%%[(20 codegen) hs export(cModMerge2)
-- | merge by pulling
cModMerge2 :: ([CModule],CModule) -> CModule
cModMerge2 (mimpL,mmain)
  = mkM mmain
  where (mkM,_)   = cModMergeByPullingIn lkupPull cexprFvS cbindExprs
                                         (\bs (CModule_Mod modNm _ _) -> CModule_Mod modNm (acoreLetN bs $ rootExpr) allTags)
                                         rootExpr
        rootExpr  = cmoddbMainExpr modDbMain
        allTags   = concatMap cmoddbTagsMp $ modDbMain : modDbImp
        modDbMain =     cexprModAsDatabase mmain
        modDbImp  = map cexprModAsDatabase mimpL
        modDbMp = Map.unions [ Map.singleton (cmoddbModNm db) db | db <- modDbMain : modDbImp ]
        lkupMod  n = -- (\x -> tr "cModMerge2.lkupMod" (n >#< fmap cmoddbModNm x) x) $
                     maybe (Just modDbMain) (\m -> Map.lookup m modDbMp) $ hsnQualifier n
        lkupPull n = do
           db <- lkupMod n
           (bi,_) <- cmoddbLookup n db
           let (cat,bsarr) = cmoddbBindArr db ! bi
               bs = elems bsarr
           return ( cat, bs
                  , -- (\x -> tr "cModMerge2.lkupPull" (n >#< show x) x) $
                    Set.fromList $ map cbindNm bs
                  )
%%]
        lkupMod  n = do
           m <- (\x -> tr "cModMerge2.lkupMod" (n >#< x) x) $
                hsnQualifier n
           Map.lookup m modDbMp


