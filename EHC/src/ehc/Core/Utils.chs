%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.Utils} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core},{%{EH}Gam.Full}) 
%%]

%%[(8 codegen) import({%{EH}Core.SubstCaseAltFail})
%%]
%%[(8 codegen) import(Data.List,qualified Data.Set as Set,Data.List,qualified Data.Map as Map,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(RCEEnv(..),emptyRCEEnv)
data RCEEnv
  = RCEEnv
      { rceValGam           :: !ValGam
      , rceDataGam          :: !DataGam
      , rceCaseFailSubst    :: !CaseFailSubst
      , rceCaseIds          :: !UIDS
      , rceCaseCont         :: !CExpr
      , rceEHCOpts          :: !EHCOpts
      -- , rceIsStrict			:: !Bool			-- scrutinee must be evaluated
      }

emptyRCEEnv :: EHCOpts -> RCEEnv
emptyRCEEnv opts = RCEEnv emptyGam emptyGam Map.empty (Set.singleton uidStart) (cundefined opts) opts -- True
%%]

%%[(8 codegen)
rceEnvDataAlts :: RCEEnv -> CTag -> Maybe [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> dataGamTagsOfTy ty (rceDataGam env)
              _  -> Nothing
      _  -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(mkCPatCon)
mkCPatCon :: CTag -> Int -> Maybe [HsName] -> CPat
mkCPatCon ctag arity mbNmL
  = CPat_Con hsnWild ctag CPatRest_Empty (zipWith mkB nmL [0..arity-1])
  where mkB n o = CPatBind_Bind hsnUnknown (CExpr_Int o) n (CPat_Var n)
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
caltLSaturate :: RCEEnv -> CAltL -> CAltL
caltLSaturate env alts
  = case alts of
      (alt1:_) -> listSaturateWith 0 (length allAlts - 1) caltIntTag allAlts alts
            where allAlts
                    = case rceEnvDataAlts env (caltConTag alt1) of
                        Just ts -> [ (ctagTag t,mkA env t (ctagArity t)) | t <- ts ]
                        _       -> [ (caltIntTag a, a) | a <- alts ]
                    where mkA env ct a = CAlt_Alt (mkCPatCon ct a Nothing) (rceCaseCont env)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
cpatBindOffsetL :: [CPatBind] -> ([CPatBind],CBindL)
cpatBindOffsetL pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b@(CPatBind_Bind l o n p@(CPat_Var pn))
                        ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset pn -- hsnPrefix "off_" pn
                            in   case o of
                                   CExpr_Int _  -> (b,[])
                                   _            -> (CPatBind_Bind l (CExpr_Var offNm) n p,[mkCBind1 offNm o])
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

%%[(8 codegen) export(MbCPatRest)
type MbCPatRest = Maybe (CPatRest,Int) -- (pat rest, arity)
%%]

%%[(8 codegen) export(mkCExprStrictSatCaseMeta,mkCExprStrictSatCase)
mkCExprStrictSatCaseMeta :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> CAltL -> CExpr
mkCExprStrictSatCaseMeta env mbNm meta e [CAlt_Alt (CPat_Con _ (CTag tyNm _ _ _ _) CPatRest_Empty [CPatBind_Bind _ _ _ (CPat_Var pnm)]) ae]
  | dgiIsNewtype dgi
  = mkCExprLet CBindings_Plain
      (  [ mkCBind1Meta {- (panicJust "mkCExprStrictSatCaseMeta.mbNm" mbNm) -} {- -} pnm meta e ]
      ++ maybe [] (\n -> [ mkCBind1Meta n meta e ]) mbNm
      ) ae
  where dgi = panicJust "mkCExprStrictSatCaseMeta.dgi" $ dataGamLookup tyNm (rceDataGam env)
mkCExprStrictSatCaseMeta env mbNm meta e alts
  = case mbNm of
      Just n  -> mkCExprStrictInMeta n meta e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = mkCExprLet CBindings_Strict altOffBL (CExpr_Case n (caltLSaturate env (alt':alts)) (rceCaseCont env))
          where (alt',altOffBL) = caltOffsetL alt
        mk [] n
          = CExpr_Case n [] (rceCaseCont env) -- dummy case

mkCExprStrictSatCase :: RCEEnv -> Maybe HsName -> CExpr -> CAltL -> CExpr
mkCExprStrictSatCase env eNm e alts = mkCExprStrictSatCaseMeta env eNm CMetaVal_Val e alts
%%]

%%[(8 codegen)
mkCExprSelsCasesMeta' :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> [(CTag,[(HsName,HsName,CExpr)],MbCPatRest,CExpr)] -> CExpr
mkCExprSelsCasesMeta' env mbNm meta e tgSels
  = mkCExprStrictSatCaseMeta env mbNm meta e alts
  where  alts = [ CAlt_Alt
                    (CPat_Con (maybe (cexprVar e) id mbNm) ct
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

mkCExprSelsCases' :: RCEEnv -> Maybe HsName -> CExpr -> [(CTag,[(HsName,HsName,CExpr)],MbCPatRest,CExpr)] -> CExpr
mkCExprSelsCases' env ne e tgSels = mkCExprSelsCasesMeta' env ne CMetaVal_Val e tgSels
%%]

%%[(8 codegen)
mkCExprSelsCaseMeta' :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> CTag -> [(HsName,HsName,CExpr)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSelsCaseMeta' env ne meta e ct nmLblOffL mbRest sel = mkCExprSelsCasesMeta' env ne meta e [(ct,nmLblOffL,mbRest,sel)]

mkCExprSelsCase' :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,CExpr)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSelsCase' env ne e ct nmLblOffL mbRest sel = mkCExprSelsCaseMeta' env ne CMetaVal_Val e ct nmLblOffL mbRest sel
%%]

%%[(8 codegen) export(mkCExprSelCase)
mkCExprSelCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> HsName -> HsName -> CExpr -> MbCPatRest -> CExpr
mkCExprSelCase env ne e ct n lbl off mbRest
  = mkCExprSelsCase' env ne e ct [(n,lbl,off)] mbRest (CExpr_Var n)
%%]

%%[(8 codegen) export(mkCExprSatSelsCases)
mkCExprSatSelsCasesMeta :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> [(CTag,[(HsName,HsName,Int)],MbCPatRest,CExpr)] -> CExpr
mkCExprSatSelsCasesMeta env ne meta e tgSels
  =  mkCExprSelsCasesMeta' env ne meta e alts
  where mkOffL ct mbr nol
          = case (ct,mbr) of
              (CTagRec       ,Nothing   ) -> map mklo nol
              (CTagRec       ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a _,_         ) -> mkloL a
          where mklo (n,l,o) = (n,l,CExpr_Int o)
                mkloL a = map mklo $ listSaturateWith 0 (a-1) (\(_,_,o) -> o) [(o,(l,l,o)) | (o,l) <- zip [0..a-1] hsnLclSupply] $ nol
        alts = [ (ct,mkOffL ct mbRest nmLblOffL,mbRest,sel) | (ct,nmLblOffL,mbRest,sel) <- tgSels ]

mkCExprSatSelsCases :: RCEEnv -> Maybe HsName -> CExpr -> [(CTag,[(HsName,HsName,Int)],MbCPatRest,CExpr)] -> CExpr
mkCExprSatSelsCases env ne e tgSels = mkCExprSatSelsCasesMeta env ne CMetaVal_Val e tgSels
%%]

%%[(8 codegen) export(mkCExprSatSelsCaseMeta,mkCExprSatSelsCase)
mkCExprSatSelsCaseMeta :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> CTag -> [(HsName,HsName,Int)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSatSelsCaseMeta env ne meta e ct nmLblOffL mbRest sel = mkCExprSatSelsCasesMeta env ne meta e [(ct,nmLblOffL,mbRest,sel)]

mkCExprSatSelsCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,Int)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSatSelsCase env ne e ct nmLblOffL mbRest sel = mkCExprSatSelsCaseMeta env ne CMetaVal_Val e ct nmLblOffL mbRest sel
%%]

%%[(8 codegen) hs export(mkCExprSatSelCase)
mkCExprSatSelCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> HsName -> HsName -> Int -> MbCPatRest -> CExpr
mkCExprSatSelCase env ne e ct n lbl off mbRest
  = mkCExprSatSelsCase env ne e ct [(n,lbl,off)] mbRest (CExpr_Var n)
%%]

%%[(8 codegen) export(mkCExprSatSelsCaseUpdMeta)
mkCExprSatSelsCaseUpdMeta :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> CTag -> Int -> [(Int,(CExpr,CMetaVal))] -> MbCPatRest -> CExpr
mkCExprSatSelsCaseUpdMeta env mbNm meta e ct arity offValL mbRest
  = mkCExprSatSelsCaseMeta env mbNm meta e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupply
        nmLblOffL = zip3 ns ns [0..]
        sel = mkCExprAppMeta
                (CExpr_Tup ct)
                (map snd $ listSaturateWith 0 (arity-1) fst [(o,(o,(CExpr_Var n,CMetaVal_Val))) | (n,_,o) <- nmLblOffL] offValL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List comprehension utilities for deriving, see also HS/ToEH
%%% These functions redo on the Core level the desugaring done in ToEH. Regretfully so ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) hs export(mkCListComprehenseGenerator)
mkCListComprehenseGenerator :: RCEEnv -> CPat -> (CExpr -> CExpr) -> CExpr -> CExpr -> CExpr
mkCListComprehenseGenerator env patOk mkOk fail e
  = mkCExprLam1 x
      (mkCExprStrictSatCase (env {rceCaseCont = fail}) (Just xStrict) (CExpr_Var x)
        [CAlt_Alt patOk (mkOk e)]
      )
  where x = mkHNmHidden "x"
        xStrict = hsnUniqifyEval x -- hsnSuffix x "!"
%%]

%%[(99 codegen) hs export(mkCListComprehenseTrue)
mkCListComprehenseTrue :: RCEEnv -> CExpr -> CExpr
mkCListComprehenseTrue env e = mkCListSingleton (rceEHCOpts env) e
%%]

%%[(99 codegen) hs export(mkCMatchString)
mkCMatchString :: RCEEnv -> String -> CExpr -> CExpr -> CExpr -> CExpr
mkCMatchString env str ok fail e
  = mkCExprLetPlain x e
    $ foldr (\(c,ns@(_,xh,_)) ok
               -> matchCons ns
                  $ mkCMatchChar opts (Just $ hsnUniqifyEval xh)  c (CExpr_Var xh) ok fail
            )
            (matchNil xt ok)
    $ zip str nms
  where env' = env {rceCaseCont = fail}
        matchCons (x,xh,xt) e = mkCExprSatSelsCase env' (Just $ hsnUniqifyEval x) (CExpr_Var x) constag [(xh,xh,0),(xt,xt,1)] (Just (CPatRest_Empty,2)) e
        matchNil   x        e = mkCExprSatSelsCase env' (Just $ hsnUniqifyEval x) (CExpr_Var x) niltag  []                    (Just (CPatRest_Empty,0)) e
        constag = ctagCons opts
        niltag  = ctagNil  opts
        opts = rceEHCOpts env
        (nms@((x,_,_):_),(xt,_,_))
          = fromJust $ initlast $ snd
            $ foldr (\n (nt,l) -> (n,(n,hsnUniqifyStr HsNameUniqifier_Field "h" n,nt):l)) (hsnUnknown,[])
            $ take (length str + 1) $ hsnLclSupplyWith (mkHNmHidden "l")
%%]

%%[(99 codegen) hs export(mkCMatchTuple)
mkCMatchTuple :: RCEEnv -> [HsName] -> CExpr -> CExpr -> CExpr
mkCMatchTuple env fldNmL ok e
  = mkCExprLetPlain x e
    $ mkCExprSatSelsCase env (Just $ hsnUniqifyEval x) (CExpr_Var x) CTagRec (zip3 fldNmL fldNmL [0..]) (Just (CPatRest_Empty,length fldNmL)) ok
  where x = mkHNmHidden "x"
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
                                   in  mkCBind1 n (caddint opts o off)
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

%%[(10 codegen) export(fuMkCExpr)
fuMkCExpr :: EHCOpts -> UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = mkCBind1 n r : oL
     in   mkCExprLet CBindings_Strict bL $ foldl (\r (_,(f,_)) -> f r) (CExpr_Var n) $ fuL'
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
fvVarRepl nMp n = maybe (CExpr_Var n) (CExpr_Var . cvrRepl) $ Map.lookup n nMp
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
foffMkOff FldImplicitOffset      o = (o,CExpr_Int o)
foffMkOff (FldKnownOffset   _ o) _ = (o,CExpr_Int o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff				= foffLabel' foff
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
                                   in  caddint opts o nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   tyRowCanonOrderBy compare fsL'
%%]

%%[(8 codegen) export(rpbReorder,patBindLOffset)
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
           ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset $ rpatNmNm pn
               in   case o of
                      CExpr_Int _  -> (b,[])
                      _            -> (RPatBind_Bind l (CExpr_Var offNm) n p,[mkCBind1 offNm o])
       )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
type RCEAltL = [RAlt]
%%]

%%[(8 codegen) hs
data RCESplitCateg
  = RCESplitVar UIDS | RCESplitCon | RCESplitConMany | RCESplitConst | RCESplitIrrefutable
%%[[97
  | RCESplitBoolExpr
%%]]
  deriving Eq

rceSplitMustBeOnItsOwn :: RCESplitCateg -> Bool
rceSplitMustBeOnItsOwn RCESplitConMany     = True
rceSplitMustBeOnItsOwn RCESplitIrrefutable = True
rceSplitMustBeOnItsOwn _                   = False
%%]

%%[(8 codegen) hs
rceSplit :: (RAlt -> RCESplitCateg) -> RCEAltL -> [RCEAltL]
rceSplit f []   = []
rceSplit f [x]  = [[x]]
rceSplit f (x:xs@(x':_))
  | xcateg == f x'
    && not (rceSplitMustBeOnItsOwn xcateg)
      = let (z:zs) = rceSplit f xs
        in  (x:z) : zs
  | otherwise
      = [x] : rceSplit f xs
  where xcateg = f x
%%]

%%[(8 codegen) hs
rceRebinds :: Bool -> HsName -> RCEAltL -> CBindL
rceRebinds origOnly nm alts
  = [ mkCBind1 n (CExpr_Var nm) | pn <- raltLPatNms alts, alsoUniq || rpatNmIsOrig pn, let n = rpatNmNm pn, n /= nm ]
  where alsoUniq = not origOnly
%%]
rceRebinds :: HsName -> RCEAltL -> CBindL
rceRebinds nm alts = [ mkCBind1 n (CExpr_Var nm) | (RPatNmOrig n) <- raltLPatNms alts, n /= nm ]

%%[(8 codegen) hs
rceMatchVar :: RCEEnv ->  [HsName] -> RCEAltL -> CExpr
rceMatchVar env (arg:args') alts
  = mkCExprLet CBindings_Plain (rceRebinds True arg alts) remMatch
  where remMatch  = rceMatch env args' [RAlt_Alt remPats e f | (RAlt_Alt (RPat_Var _ : remPats) e f) <- alts]

rceMatchIrrefutable :: RCEEnv ->  [HsName] -> RCEAltL -> CExpr
rceMatchIrrefutable env (arg:args') alts@[RAlt_Alt (RPat_Irrefutable n b : remPats) e f]
  = mkCExprLet CBindings_Plain (rceRebinds False arg alts) $ mkCExprLet CBindings_Plain b remMatch
  where remMatch  = rceMatch env args' [RAlt_Alt remPats e f]

rceMkAltAndSubAlts :: RCEEnv -> [HsName] -> RCEAltL -> CAlt
rceMkAltAndSubAlts env (arg:args) alts@(alt:_)
  = CAlt_Alt altPat (mkCExprLet CBindings_Plain (rceRebinds True arg alts) subMatch)
  where (subAlts,subAltSubNms)
          =  unzip
               [ (RAlt_Alt (pats ++ ps) e f, map (rpatNmNm . rcpPNm) pats)
               | (RAlt_Alt (RPat_Con _ _ (RPatConBind_One _ pbinds) : ps) e f) <- alts
               , let pats = [ p | (RPatBind_Bind _ _ _ p) <- pbinds ]
               ]
        subMatch
          =  rceMatch env (head subAltSubNms ++ args) subAlts
        altPat
          =  case alt of
               RAlt_Alt (RPat_Con n t (RPatConBind_One r pbL) : _) _ _
                 ->  CPat_Con (rpatNmNm n) t r pbL'
                     where  pbL' = [ CPatBind_Bind l o n (CPat_Var (rpatNmNm $ rcpPNm p)) | (RPatBind_Bind l o n p) <- pbL ]

rceMatchCon :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatchCon env (arg:args) alts
  = mkCExprStrictSatCase env (Just arg') (CExpr_Var arg) alts'
  where arg'   =  hsnUniqifyEval arg
        alts'  =  map (rceMkAltAndSubAlts env (arg':args))
                  $ groupSortOn (ctagTag . rcaTag)
                  $ filter (not . null . rcaPats)
                  $ alts

rceMatchConMany :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatchConMany env (arg:args) [RAlt_Alt (RPat_Con n t (RPatConBind_Many bs) : ps) e f]
  = mkCExprStrictIn arg' (CExpr_Var arg)
                    (\_ -> foldr (\mka e -> rceMatch env [arg'] (mka e)) (rceMatch env (arg':args) altslast) altsinit)
  where arg'     = hsnUniqifyEval arg
        altsinit = [ \e -> [RAlt_Alt (RPat_Con n t b     : []) e f] | b <- bsinit ]
        altslast =         [RAlt_Alt (RPat_Con n t blast : ps) e f]
        (bsinit,blast) = panicJust "rceMatchConMany" $ initlast bs

rceMatchConst :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatchConst env (arg:args) alts
  = mkCExprStrictIn arg' (CExpr_Var arg) (\n -> mkCExprLet CBindings_Plain (rceRebinds True arg alts) (CExpr_Case n alts' (rceCaseCont env)))
  where arg' = hsnUniqifyEval arg
        alts' = [ CAlt_Alt (rpat2CPat p) (cSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e) | (RAlt_Alt (p:_) e _) <- alts ]
%%]

%%[(97 codegen) hs
rceMatchBoolExpr :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatchBoolExpr env (arg:args) alts
  = foldr (\(n,c,t) f -> mkCIf (rceEHCOpts env) (Just n) c t f) (rceCaseCont env) m
  where m = [ ( hsnUniqifyInt HsNameUniqifier_Evaluated u arg -- hsnSuffix arg $ "!" ++ show u
              , mkCExprApp b [CExpr_Var arg]
              , rceMatch env args [RAlt_Alt remPats e f]
              )
            | (u,RAlt_Alt (RPat_BoolExpr _ b : remPats) e f) <- zip [0..] alts
            ]
%%]

%%[(8 codegen) hs
rceMatchSplits :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatchSplits env args alts@(alt:_)
  |  raltIsVar          alt  = rceMatchVar          env args alts
  |  raltIsConst        alt  = rceMatchConst        env args alts
  |  raltIsIrrefutable  alt  = rceMatchIrrefutable  env args alts
%%[[97
  |  raltIsBoolExpr     alt  = rceMatchBoolExpr     env args alts
%%]]
  |  raltIsConMany      alt  = rceMatchConMany      env args alts
  |  otherwise               = rceMatchCon          env args alts

%%]

%%[(8 codegen) hs export(rceMatch)
rceMatch :: RCEEnv -> [HsName] -> RCEAltL -> CExpr
rceMatch env [] []    =  rceCaseCont env
rceMatch env [] alts  
  =  case [ e | (RAlt_Alt [] e _) <- alts ] of
       (e:_)  -> cSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e
       _      -> rceCaseCont env
rceMatch env args alts
  =  foldr
        (\alts e
           ->  case e of
                  CExpr_Var _
                     ->  rceMatchSplits (rceUpdEnv e env) args alts
                  _  ->  mkCExprLet CBindings_Plain [mkCBind1 nc e]
                         $ rceMatchSplits (rceUpdEnv (CExpr_Var nc) env) args alts
                     where nc  = hsnUniqify HsNameUniqifier_CaseContinuation (rpatNmNm $ rcpPNm $ rcaPat $ head alts)
        )
        (rceCaseCont env)
     $ (rceSplit (\a -> if      raltIsVar           a  then RCESplitVar (raaFailS a)
                        else if raltIsConst         a  then RCESplitConst
                        else if raltIsIrrefutable   a  then RCESplitIrrefutable
%%[[97
                        else if raltIsBoolExpr      a  then RCESplitBoolExpr
%%]]
                        else if raltIsConMany       a  then RCESplitConMany
                                                       else RCESplitCon
                 ) alts)
%%]

%%[(8 codegen) hs export(rceUpdEnv)
rceUpdEnv :: CExpr -> RCEEnv -> RCEEnv
rceUpdEnv e env
  = env { rceCaseFailSubst = Map.union (Map.fromList [ (i,e) | i <- Set.toList (rceCaseIds env) ])
                             $ rceCaseFailSubst env
        , rceCaseCont      = e
        }
%%]

