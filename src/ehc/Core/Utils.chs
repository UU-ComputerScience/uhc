%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Core.Utils} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core},{%{EH}Gam}) 
%%]

%%[8 import(Data.List,qualified Data.Set as Set,Data.List,qualified Data.Map as Map,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(RCEEnv(..),emptyRCEEnv)
data RCEEnv
  = RCEEnv
      { rceValGam           :: !ValGam
      , rceDataGam          :: !DataGam
      , rceCaseFailSubst    :: !CaseFailSubst
      , rceCaseIds          :: !(Set.Set UID)
      , rceCaseCont         :: !CExpr
      , rceEHCOpts          :: !EHCOpts
      }

emptyRCEEnv :: EHCOpts -> RCEEnv
emptyRCEEnv opts = RCEEnv emptyGam emptyGam Map.empty (Set.singleton uidStart) (cundefined opts) opts
%%]

%%[8
rceEnvDataAlts :: RCEEnv -> CTag -> [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> maybe [] id $ dataGamTagsOfTy ty (rceDataGam env)
              _  -> [t]
      _  -> [t]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkCPatCon)
mkCPatCon :: CTag -> Int -> Maybe [HsName] -> CPat
mkCPatCon ctag arity mbNmL
  = CPat_Con hsnWild ctag CPatRest_Empty (zipWith mkB nmL [0..arity-1])
  where mkB n o = CPatBind_Bind hsnUnknown (CExpr_Int o) n (CPat_Var n)
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caltLSaturate :: RCEEnv -> CAltL -> CAltL
caltLSaturate env alts
  = case alts of
      (alt1:_) -> listSaturateWith 0 (length allAlts - 1) (ctagTag . caltTag) allAlts alts
            where allAlts
                    = [ (ctagTag t,mkA env t (ctagArity t)) | t <- rceEnvDataAlts env (caltTag alt1) ]
                    where mkA env ct a = CAlt_Alt (mkCPatCon ct a Nothing) (rceCaseCont env)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpatBindOffsetL :: [CPatBind] -> ([CPatBind],CBindL)
cpatBindOffsetL pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b@(CPatBind_Bind l o n p@(CPat_Var pn))
                        ->  let  offNm = hsnPrefix "off_" pn
                            in   case o of
                                   CExpr_Int _  -> (b,[])
                                   _            -> (CPatBind_Bind l (CExpr_Var offNm) n p,[CBind_Bind offNm o])
                    )
               $  pbL
     in   (pbL',concat obL)

caltOffsetL :: CAlt -> (CAlt,CBindL)
caltOffsetL alt
  =  case alt of
       CAlt_Alt (CPat_Con n t r b) e
         ->  (CAlt_Alt (CPat_Con n t r b') e,offBL)
             where (b',offBL) = cpatBindOffsetL b
       _ ->  (alt,[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(MbCPatRest)
type MbCPatRest = Maybe (CPatRest,Int) -- (pat rest, arity)
%%]

%%[8 export(mkCExprStrictSatCase)
mkCExprStrictSatCase :: RCEEnv -> Maybe HsName -> CExpr -> CAltL -> CExpr
mkCExprStrictSatCase env eNm e [CAlt_Alt (CPat_Con _ (CTag tyNm _ _ _ _) CPatRest_Empty [CPatBind_Bind _ _ _ (CPat_Var pnm)]) ae]
  | dgiIsNewtype dgi
  = mkCExprLet CBindPlain [CBind_Bind pnm e] ae
  where dgi = panicJust "mkCExprStrictSatCase" $ dataGamLookup tyNm (rceDataGam env)
mkCExprStrictSatCase env eNm e alts
  = case eNm of
      Just n  -> mkCExprStrictIn n e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = mkCExprLet CBindStrict altOffBL (CExpr_Case n (caltLSaturate env (alt':alts)) (rceCaseCont env))
          where (alt',altOffBL) = caltOffsetL alt
        mk [] n
          = CExpr_Case n [] (rceCaseCont env) -- dummy case
%%]

%%[8 export(mkCExprSelCase,mkCExprSatSelCase)
mkCExprSelCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> HsName -> HsName -> CExpr -> MbCPatRest -> CExpr
mkCExprSelCase env ne e ct n lbl off mbRest
  = mkCExprSelsCase' env ne e ct [(n,lbl,off)] mbRest (CExpr_Var n)

mkCExprSatSelCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> HsName -> HsName -> Int -> MbCPatRest -> CExpr
mkCExprSatSelCase env ne e ct n lbl off mbRest
  = mkCExprSatSelsCase env ne e ct [(n,lbl,off)] mbRest (CExpr_Var n)
%%]

%%[8
mkCExprSelsCases' :: RCEEnv -> Maybe HsName -> CExpr -> [(CTag,[(HsName,HsName,CExpr)],MbCPatRest,CExpr)] -> CExpr
mkCExprSelsCases' env ne e tgSels
  = mkCExprStrictSatCase env ne e alts
  where  alts = [ CAlt_Alt
                    (CPat_Con (maybe (cexprVar e) id ne) ct
                       (mkRest mbRest ct)
                       [CPatBind_Bind lbl off n (CPat_Var n) | (n,lbl,off) <- nmLblOffL]
                    )
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (CPatRest_Var hsnWild) (\_ _ _ _ _ -> CPatRest_Empty) ct
%%]

%%[8
mkCExprSelsCase' :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,CExpr)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSelsCase' env ne e ct nmLblOffL mbRest sel
  = mkCExprSelsCases' env ne e [(ct,nmLblOffL,mbRest,sel)]
%%]

%%[8 export(mkCExprSatSelsCases)
mkCExprSatSelsCases :: RCEEnv -> Maybe HsName -> CExpr -> [(CTag,[(HsName,HsName,Int)],MbCPatRest,CExpr)] -> CExpr
mkCExprSatSelsCases env ne e tgSels
  =  mkCExprSelsCases' env ne e alts
  where mkOffL ct mbr nol
          = case (ct,mbr) of
              (CTagRec       ,Nothing   ) -> map mklo nol
              (CTagRec       ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a _,_         ) -> mkloL a
          where mklo (n,l,o) = (n,l,CExpr_Int o)
                mkloL a = map mklo $ listSaturateWith 0 (a-1) (\(_,_,o) -> o) [(o,(l,l,o)) | (o,l) <- zip [0..a-1] hsnLclSupply] $ nol
        alts = [ (ct,mkOffL ct mbRest nmLblOffL,mbRest,sel) | (ct,nmLblOffL,mbRest,sel) <- tgSels ]
%%]

%%[8 export(mkCExprSatSelsCase)
mkCExprSatSelsCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,Int)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSatSelsCase env ne e ct nmLblOffL mbRest sel
  = mkCExprSatSelsCases env ne e [(ct,nmLblOffL,mbRest,sel)]
%%]

%%[8 export(mkCExprSatSelsCaseUpd)
mkCExprSatSelsCaseUpd :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> Int -> [(Int,CExpr)] -> MbCPatRest -> CExpr
mkCExprSatSelsCaseUpd env ne e ct arity offValL mbRest
  = mkCExprSatSelsCase env ne e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupply
        nmLblOffL = zip3 ns ns [0..]
        sel = mkCExprApp (CExpr_Tup ct)
                         (map snd $ listSaturateWith 0 (arity-1) fst [(o,(o,CExpr_Var n)) | (n,_,o) <- nmLblOffL] offValL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FieldUpdateL,fuL2ExprL,fuMap)
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
                                   in  CBind_Bind n (caddint opts o off)
                              no = CExpr_Var n
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

%%[10 export(fuMkCExpr)
fuMkCExpr :: EHCOpts -> UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = CBind_Bind n r : oL
     in   mkCExprLet CBindStrict bL $ foldl (\r (_,(f,_)) -> f r) (CExpr_Var n) $ fuL'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Free var closure, and other utils used by Trf/...GlobalAsArg transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(fvsClosure,fvsTransClosure)
fvsClosure :: FvS -> FvS -> FvS -> FvSMp -> FvSMp -> (FvSMp,FvSMp)
fvsClosure newS lamOuterS varOuterS fvmOuter fvmNew
  =  let  fvmNew2  =  Map.filterWithKey (\n _ -> n `Set.member` newS) fvmNew
          fvlam  s =  lamOuterS `Set.intersection` s
          fvvar  s =  varOuterS `Set.intersection` s
          fv     s =  fvvar s `Set.union` s'
                   where s' = Set.unions $ map (\n -> Map.findWithDefault Set.empty n fvmOuter) $ Set.toList $ fvlam $ s
     in   (Map.map fv fvmNew2,Map.map (`Set.intersection` newS) fvmNew2)

fvsTransClosure :: FvSMp -> FvSMp -> FvSMp
fvsTransClosure frLamMp frVarMp
  =  let  frVarMp2 = Map.mapWithKey
                       (\n s -> s `Set.union` (Set.unions
                                               $ map (\n -> panicJust "fvsTransClosure.1" $ Map.lookup n $ frVarMp)
                                               $ Set.toList
                                               $ panicJust "fvsTransClosure.2"
                                               $ Map.lookup n frLamMp
                       )                      )
                       frVarMp
          sz = sum . map Set.size . Map.elems
     in   if sz frVarMp2 > sz frVarMp
          then fvsTransClosure frLamMp frVarMp2
          else frVarMp
%%]

%%[8 export(fvLAsArg,mkFvNm,fvLArgRepl,fvVarRepl)
fvLAsArg :: LevMp -> FvS -> AssocL HsName Int
fvLAsArg levMp fvS
  =  sortOn snd
     $ filter (\(_,l) -> l > cLevModule)
     $ map (\n -> (n,fvLev levMp n))
     $ Set.toList fvS

mkFvNm :: Int -> HsName -> HsName
mkFvNm i n = hsnSuffix n ("~" ++ show i)

fvLArgRepl :: Int -> AssocL HsName Int -> ([HsName],[HsName],Map.Map HsName HsName)
fvLArgRepl uniq argLevL
  =  let  argOL = assocLKeys argLevL
          argNL = zipWith (\u n -> mkFvNm u n) [uniq..] argOL
     in   (argOL,argNL,Map.fromList (zip argOL argNL))

fvVarRepl :: Map.Map HsName HsName -> HsName -> CExpr
fvVarRepl nMp n = maybe (CExpr_Var n) CExpr_Var $ Map.lookup n nMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FldOffset(..),foffMkOff,foffLabel)
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
foffMkOff FldImplicitOffset      o = (o,CExpr_Int o)
foffMkOff (FldKnownOffset   _ o) _ = (o,CExpr_Int o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff				= foffLabel' foff
%%]

%%[8 export(FieldSplitL,fsL2PatL)
type FieldSplitL = AssocL FldOffset RPat

fsL2PatL :: FieldSplitL -> [RPat]
fsL2PatL = assocLElts
%%]
type FieldSplitL = AssocL FldOffset CPatL

fsL2PatL :: FieldSplitL -> CPatL
fsL2PatL = concat . assocLElts

-- Reordering compensates for the offset shift caused by predicate computation, which is predicate by predicate
-- whereas these sets of patterns are dealt with in one go.
%%[8 export(fsLReorder)
fsLReorder :: EHCOpts -> FieldSplitL -> FieldSplitL
fsLReorder opts fsL
  =  let  (fsL',_)
            =  foldr
                 (\(FldComputeOffset l o,p) (fsL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  caddint opts o nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   tyRowCanonOrderBy compare fsL'
%%]

%%[8 export(rpbReorder,patBindLOffset)
rpbReorder :: EHCOpts -> [RPatBind] -> [RPatBind]
rpbReorder opts pbL
  =  let  (pbL',_)
            =  foldr
                 (\(RPatBind_Bind l o n p) (pbL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  caddint opts o nrSmaller
                         in   ((RPatBind_Bind l (mkOff l exts o) n p):pbL,l:exts)
                 )
                 ([],[])
            $  pbL
          cmpPB (RPatBind_Bind l1 _ _ _)  (RPatBind_Bind l2 _ _ _) = rowLabCmp l1 l2
     in   sortBy cmpPB pbL'

patBindLOffset :: [RPatBind] -> ([RPatBind],[CBindL])
patBindLOffset
  =  unzip
  .  map
       (\b@(RPatBind_Bind l o n p@(RPat_Var pn))
           ->  let  offNm = hsnPrefix "off_" . rpatNmNm $ pn
               in   case o of
                      CExpr_Int _  -> (b,[])
                      _            -> (RPatBind_Bind l (CExpr_Var offNm) n p,[CBind_Bind offNm o])
       )
%%]

