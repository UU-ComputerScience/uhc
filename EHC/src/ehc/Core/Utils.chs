%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Core.Utils} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core},{%{EH}Gam}) 
%%]

%%[8 import(Data.List,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(RCEEnv(..),emptyRCEEnv)
data RCEEnv
  = RCEEnv
      { rceValGam           :: ValGam
      , rceDataGam          :: DataGam
      , rceCaseFailSubst    :: CaseFailSubst
      , rceCaseId           :: UID
      , rceCaseCont         :: CExpr
      }

emptyRCEEnv :: EHCOpts -> RCEEnv
emptyRCEEnv opts = RCEEnv emptyGam emptyGam Map.empty uidStart (cundefined opts)
%%]

%%[8
rceEnvDataAlts :: RCEEnv -> CTag -> [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> maybe [] id $ dataGamTagsOfTy ty (rceDataGam env)
              _  -> [t]
      _  -> [t]
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
                    where mkA env ct a = CAlt_Alt [mkP ct a] (rceCaseCont env)
                          mkP     ct a = CPat_Con cpatNmNone ct (CPatConBind_One CPatRest_Empty [mkB o | o <- [0..a-1]])
                          mkB o        = CPatBind_Bind hsnUnknown (CExpr_Int o) (cpatNmNm cpatNmNone) (CPat_Var cpatNmNone)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Flatten CPatConBind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpatConBindFlatten,cpatConBindUnFlatten)
cpatConBindFlatten :: CPatConBind -> [CPatConBind]
cpatConBindFlatten b@(CPatConBind_One _ _) = [b]
cpatConBindFlatten   (CPatConBind_Many bs) = concatMap cpatConBindFlatten bs

cpatConBindUnFlatten :: [CPatConBind] -> CPatConBind
cpatConBindUnFlatten [b] = b
cpatConBindUnFlatten bs  = CPatConBind_Many bs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpatConBindOffsetL :: CPatConBind -> (CPatConBind,CBindL)
cpatConBindOffsetL (CPatConBind_Many _)
  =  panic "cpatConBindOffsetL"
cpatConBindOffsetL (CPatConBind_One r pbL)
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b@(CPatBind_Bind l o n p@(CPat_Var pn))
                        ->  let  offNm = hsnPrefix "off_" . cpatNmNm $ pn
                            in   case o of
                                   CExpr_Int _  -> (b,[])
                                   _            -> (CPatBind_Bind l (CExpr_Var offNm) n p,[CBind_Bind offNm o])
                    )
               $  pbL
     in   (CPatConBind_One r pbL',concat obL)

caltOffsetL :: CAlt -> (CAlt,CBindL)
caltOffsetL alt
  =  case alt of
       CAlt_Alt (CPat_Con n t b : ps) e
         ->  (CAlt_Alt (CPat_Con n t b' : ps) e,offBL)
             where (b',offBL) = cpatConBindOffsetL b
       _ ->  (alt,[])
  where 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(MbCPatRest)
type MbCPatRest = Maybe (CPatRest,Int) -- (pat rest, arity)
%%]

%%[8 export(mkCExprStrictSatCase)
mkCExprStrictSatCase :: RCEEnv -> Maybe HsName -> CExpr -> CAltL -> CExpr
mkCExprStrictSatCase env eNm e [CAlt_Alt [CPat_Con _ (CTag tyNm _ _ _) (CPatConBind_One CPatRest_Empty [CPatBind_Bind _ _ _ (CPat_Var pnm)])] ae]
  | dgiIsNewtype dgi
  = mkCExprLet CBindPlain [CBind_Bind (cpatNmNm pnm) e] ae
  where dgi = panicJust "mkCExprStrictSatCase" $ dataGamLookup tyNm (rceDataGam env)
mkCExprStrictSatCase env eNm e (alt:alts)
  = case eNm of
      Just n  -> mkCExprStrictIn n e mk
      Nothing -> mk e
  where (alt',altOffBL) = caltOffsetL alt
        mk n = mkCExprLet CBindStrict altOffBL (CExpr_Case n (caltLSaturate env (alt':alts)) (rceCaseCont env))
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
                    [CPat_Con (CPatNmOrig $ maybe (cexprVar e) id ne) ct
                       (CPatConBind_One (mkRest mbRest ct)
                         [CPatBind_Bind lbl off n (CPat_Var (CPatNmOrig n)) | (n,lbl,off) <- nmLblOffL])
                    ]
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (CPatRest_Var hsnWild) (\_ _ _ _ -> CPatRest_Empty) ct
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
              (CTagRec     ,Nothing   ) -> map mklo nol
              (CTagRec     ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a,_         ) -> mkloL a
          where mklo (n,l,o) = (n,l,CExpr_Int o)
                mkloL a = map mklo $ listSaturateWith 0 (a-1) (\(_,_,o) -> o) [(o,(l,l,o)) | (o,l) <- zip [0..a-1] hsnLclSupplyL] $ nol
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
  where ns = take arity hsnLclSupplyL
        nmLblOffL = zip3 ns ns [0..]
        sel = mkCExprApp (CExpr_Tup ct)
                         (map snd $ listSaturateWith 0 (arity-1) fst [(o,(o,CExpr_Var n)) | (n,_,o) <- nmLblOffL] offValL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FieldUpdateL,fuL2ExprL,fuMkCExpr,fuMap)
type FieldUpdateL e = AssocL HsName (e,Maybe Int)

fuMap :: (HsName -> e -> (e',Int)) -> FieldUpdateL e -> FieldUpdateL e'
fuMap f = map (\(l,(e,_)) -> let (e',o) = f l e in (l,(e',Just o)))

fuL2ExprL' :: (e -> CExpr) -> FieldUpdateL e -> [CExpr]
fuL2ExprL' f l = [ f e | (_,(e,_)) <- l ]

fuL2ExprL :: FieldUpdateL CExpr -> [CExpr]
fuL2ExprL = fuL2ExprL' cexprTupFld

fuReorder :: [HsName] -> FieldUpdateL CExpr -> (CBindL,FieldUpdateL (CExpr -> CExpr))
fuReorder nL fuL
  =  let  (fuL',offL,_,_)
            =  foldl
                 (\(fuL,offL,exts,dels) (n,(_,(f,_)))
                     ->  let  mkOff n lbl o
                                =  let smaller l = rowLabCmp l lbl == LT
                                       off = length (filter smaller dels) - length (filter smaller exts)
                                   in  CBind_Bind n (o `mkCExprAddInt` off)
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

fuMkCExpr :: UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder nL fuL
          bL = CBind_Bind n r : oL
     in   mkCExprLet CBindStrict bL $ foldl (\r (_,(f,_)) -> f r) (CExpr_Var n) $ fuL'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FldOffset(..),foffMkOff,foffLabel)
data FldOffset
  = FldKnownOffset      { foffLabel'     :: HsName, foffOffset   :: Int      }
  | FldComputeOffset    { foffLabel'     :: HsName, foffCExpr    :: CExpr    }
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
type FieldSplitL = AssocL FldOffset CPat

fsL2PatL :: FieldSplitL -> [CPat]
fsL2PatL = assocLElts
%%]
type FieldSplitL = AssocL FldOffset CPatL

fsL2PatL :: FieldSplitL -> CPatL
fsL2PatL = concat . assocLElts

-- Reordering compensates for the offset shift caused by predicate computation, which is predicate by predicate
-- whereas these sets of patterns are dealt with in one go.
%%[8 export(fsLReorder)
fsLReorder :: FieldSplitL -> FieldSplitL
fsLReorder fsL
  =  let  (fsL',_)
            =  foldr
                 (\(FldComputeOffset l o,p) (fsL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  o `mkCExprAddInt` nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   tyRowCanonOrderBy compare fsL'
%%]

%%[8 export(rpbReorder,patBindLOffset)
rpbReorder :: CPatBindL -> CPatBindL
rpbReorder pbL
  =  let  (pbL',_)
            =  foldr
                 (\(CPatBind_Bind l o n p) (pbL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  o `mkCExprAddInt` nrSmaller
                         in   ((CPatBind_Bind l (mkOff l exts o) n p):pbL,l:exts)
                 )
                 ([],[])
            $  pbL
          cmpPB (CPatBind_Bind l1 _ _ _)  (CPatBind_Bind l2 _ _ _) = rowLabCmp l1 l2
     in   sortBy cmpPB pbL'

patBindLOffset :: CPatBindL -> (CPatBindL,[CBindL])
patBindLOffset
  =  unzip
  .  map
       (\b@(CPatBind_Bind l o n p@(CPat_Var pn))
           ->  let  offNm = hsnPrefix "off_" . cpatNmNm $ pn
               in   case o of
                      CExpr_Int _  -> (b,[])
                      _            -> (CPatBind_Bind l (CExpr_Var offNm) n p,[CBind_Bind offNm o])
       )
%%]

