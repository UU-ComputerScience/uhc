%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}TyCore.Utils2} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Base.Opts},{%{EH}Base.Common}) 
%%]
%%[(8 codegen) import({%{EH}TyCore.Base})
%%]

%%[(8 codegen) import({%{EH}Gam},{%{EH}VarMp},{%{EH}Substitutable},{%{EH}Gam.ValGam},{%{EH}Gam.DataGam})
%%]

%%[(8 codegen) import({%{EH}TyCore.SubstCaseAltFail})
%%]
%%[(8 codegen) import(Data.List,qualified Data.Set as Set,Data.List,qualified Data.Map as Map,EH.Util.Utils)
%%]

-- debug
%%[(8 codegen) import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(RCEEnv(..),emptyRCEEnv)
data RCEEnv
  = RCEEnv
      { rceValGam           :: !ValGam				-- type of value (amongst other)
      , rceTyVarMp          :: !VarMp				-- tvar bindings for ValGam
      , rceDataGam          :: !DataGam				-- data type + constructor info
      , rceCaseFailSubst    :: !CaseFailSubst		-- fail continuation map
      , rceCaseIds          :: !UIDS				-- fail ids
      , rceCaseCont         :: !Expr				-- continuation
      , rceEHCOpts          :: !EHCOpts				-- options
      -- , rceIsStrict			:: !Bool			-- scrutinee must be evaluated
      }

emptyRCEEnv :: EHCOpts -> RCEEnv
emptyRCEEnv opts = RCEEnv emptyGam emptyVarMp emptyGam Map.empty (Set.singleton uidStart) (tcUndefined opts) opts -- True
%%]

%%[(8 codegen)
-- All tags of the type of the constructor for a tag t
rceEnvDataAlts :: RCEEnv -> CTag -> Maybe [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> dataGamTagsOfTy (rceTyVarMp env |=> ty) (rceDataGam env)
              _  -> Nothing
      _  -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(mkPatCon)
mkPatCon :: RCEEnv -> CTag -> Int -> Maybe [HsName] -> Pat
mkPatCon env ctag arity mbNmL
  = Pat_Con ctag PatRest_Empty (zipWith mkB nmL [0 .. arity - 1])
  where mkB n o = FldBind_Fld n (tyErr "mkPatCon") (tcInt o)
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
caltLSaturate :: RCEEnv -> AltL -> AltL
caltLSaturate env alts
  = case alts of
      (alt1:_) -> -- (\v -> v `seq` tr "caltLSaturate" ("nr alts" >#< length alts >#< "all" >#< length allAlts) v) $ 
                  listSaturateWith 0 (length allAlts - 1) altIntTag allAlts alts
            where allAlts
                    = case rceEnvDataAlts env (altConTag alt1) of
                        Just ts -> [ (ctagTag t,mkA env t (ctagArity t)) | t <- ts ]
                        _       -> [ (altIntTag a, a) | a <- alts ]
                    where mkA env ct a = Alt_Alt (mkPatCon env ct a Nothing) (rceCaseCont env)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
cpatBindOffsetL :: [FldBind] -> ([FldBind],ValBindL)
cpatBindOffsetL pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b@(FldBind_Fld n t o)
                        ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset n -- hsnPrefix "off_" n
                            in   case o of
                                   Expr_Int _ _ -> (b,[])
                                   _            -> (FldBind_Fld n t (Expr_Var offNm),[mkValBind1 offNm tyInt o])
                    )
               $  pbL
     in   (pbL',concat obL)

caltOffsetL :: Alt -> (Alt,ValBindL)
caltOffsetL alt
  =  case alt of
       Alt_Alt (Pat_Con t r b) e
         ->  (Alt_Alt (Pat_Con t r b') e,offBL)
             where (b',offBL) = cpatBindOffsetL b
       _ ->  (alt,[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(MbPatRest)
type MbPatRest = Maybe (PatRest,Int) -- (pat rest, arity)
%%]

%%[(8 codegen) export(mkExprStrictSatCaseMeta,mkExprStrictSatCase)
{-
Either:
  - make a case expr from alternatives,
    saturating the alternatives with defaults for missing alternatives.
  - or, when only a single alternative binding a single field, bind it directly with a let
-}
mkExprStrictSatCaseMeta :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> AltL -> Expr
mkExprStrictSatCaseMeta env mbNm meta e [Alt_Alt (Pat_Con (CTag tyNm _ _ _ _) PatRest_Empty [FldBind_Fld nm _ _]) ae]
  | dgiIsNewtype dgi
  = mkExprLet ValBindCateg_Plain
      (  [ mkValBind1Meta {- (panicJust "mkExprStrictSatCaseMeta.mbNm" mbNm) -} {- -} nm meta (tyErr "mkExprStrictSatCaseMeta.1") e ]
      ++ maybe [] (\(n,ty) -> [ mkValBind1Meta n meta ty e ]) mbNm
      ) ae
  where dgi = panicJust ("mkExprStrictSatCaseMeta.dgi:" ++ show tyNm) $ dataGamLookup tyNm (rceDataGam env)
mkExprStrictSatCaseMeta env mbNm meta e alts
  = case mbNm of
      Just (n,ty)  -> mkExprStrictInMeta n meta ty e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = mkExprLet ValBindCateg_Strict altOffBL (Expr_Case n (caltLSaturate env (alt':alts)) Nothing {-(rceCaseCont env)-})
          where (alt',altOffBL) = caltOffsetL alt
        mk [] n
          = Expr_Case n [] Nothing {-(rceCaseCont env)-} -- dummy case

mkExprStrictSatCase :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> AltL -> Expr
mkExprStrictSatCase env eNm e alts = mkExprStrictSatCaseMeta env eNm MetaVal_Val e alts
%%]

%%[(8 codegen)
{-
Make a case expr from non-saturated alternatives,
alternatives are given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
-}
mkExprSelsCasesMeta' :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> [(CTag,[(HsName,{-HsName,-}Expr)],MbPatRest,Expr)] -> Expr
mkExprSelsCasesMeta' env mbNm meta e tgSels
  = mkExprStrictSatCaseMeta env mbNm meta e alts
  where  alts = [ Alt_Alt
                    (Pat_Con {- (maybe (exprVar e) id mbNm) -} ct
                       (mkRest mbRest ct)
                       [FldBind_Fld n (tyErr "mkExprSelsCasesMeta'") off | (n,{-lbl,-}off) <- nmLblOffL]
                    )
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (PatRest_Var hsnWild) (\_ _ _ _ _ -> PatRest_Empty) ct

mkExprSelsCases' :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> [(CTag,[(HsName,{-HsName,-}Expr)],MbPatRest,Expr)] -> Expr
mkExprSelsCases' env ne e tgSels = mkExprSelsCasesMeta' env ne MetaVal_Val e tgSels
%%]

%%[(8 codegen)
{-
Make a case expr from a single alternative,
the alternative given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
-}
mkExprSelsCaseMeta' :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> CTag -> [(HsName,{-HsName,-}Expr)] -> MbPatRest -> Expr -> Expr
mkExprSelsCaseMeta' env ne meta e ct nmLblOffL mbRest sel = mkExprSelsCasesMeta' env ne meta e [(ct,nmLblOffL,mbRest,sel)]

mkExprSelsCase' :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> CTag -> [(HsName,{-HsName,-}Expr)] -> MbPatRest -> Expr -> Expr
mkExprSelsCase' env ne e ct nmLblOffL mbRest sel = mkExprSelsCaseMeta' env ne MetaVal_Val e ct nmLblOffL mbRest sel
%%]

%%[(8 codegen) export(mkExprSelCase)
{-
Make a case expr from a single alternative with a single field,
the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr
-}
mkExprSelCase :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> CTag -> HsName -> {- HsName -> -} Expr -> MbPatRest -> Expr
mkExprSelCase env ne e ct n {-lbl-} off mbRest
  = mkExprSelsCase' env ne e ct [(n,{-lbl,-}off)] mbRest (Expr_Var n)
%%]

%%[(8 codegen) export(mkExprSatSelsCases)
{-
Make a case expr from a single alternative with non-saturated fields,
the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
the fields (and alternatives) are saturated according to the tag + rest info
-}
mkExprSatSelsCasesMeta :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest,Expr)] -> Expr
mkExprSatSelsCasesMeta env ne meta e tgSels
  =  mkExprSelsCasesMeta' env ne meta e alts
  where mkOffL ct mbr nol
          = case (ct,mbr) of
              (CTagRec       ,Nothing   ) -> map mklo nol
              (CTagRec       ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a _,_         ) -> mkloL a
          where mklo (n,{-l,-}o) = (n,{-l,-}tcInt o)
                mkloL a = map mklo
                          -- $ (\v -> v `seq` tr "mkCExprSatSelsCasesMeta" ("nr nol" >#< length nol >#< "arity" >#< a) v)
                          $ listSaturateWith 0 (a-1) (\(_,{-_,-}o) -> o) [(o,(l,{-l,-}o)) | (o,l) <- zip [0..a-1] hsnLclSupply] $ nol
        alts = [ (ct,mkOffL ct mbRest nmLblOffL,mbRest,sel) | (ct,nmLblOffL,mbRest,sel) <- tgSels ]

mkExprSatSelsCases :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest,Expr)] -> Expr
mkExprSatSelsCases env ne e tgSels = mkExprSatSelsCasesMeta env ne MetaVal_Val e tgSels
%%]

%%[(8 codegen) export(mkExprSatSelsCaseMeta,mkExprSatSelsCase)
{-
Make a case expr from a single alternative with non-saturated fields,
the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
the fields (and alternatives) are saturated according to the tag + rest info
-}
mkExprSatSelsCaseMeta :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest -> Expr -> Expr
mkExprSatSelsCaseMeta env ne meta e ct nmLblOffL mbRest sel = mkExprSatSelsCasesMeta env ne meta e [(ct,nmLblOffL,mbRest,sel)]

mkExprSatSelsCase :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest -> Expr -> Expr
mkExprSatSelsCase env ne e ct nmLblOffL mbRest sel = mkExprSatSelsCaseMeta env ne MetaVal_Val e ct nmLblOffL mbRest sel
%%]

%%[(8 codegen) hs export(mkExprSatSelCase)
{-
Make a case expr from a single alternative with a single field,
the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
the fields (and alternatives) are saturated according to the tag + rest info
-}
mkExprSatSelCase :: RCEEnv -> Maybe (HsName,Ty) -> Expr -> CTag -> HsName -> {- HsName -> -} Int -> MbPatRest -> Expr
mkExprSatSelCase env ne e ct n {- lbl -} off mbRest
  = mkExprSatSelsCase env ne e ct [(n,{-lbl,-}off)] mbRest (Expr_Var n)
%%]

%%[(8 codegen) export(mkExprSatSelsCaseUpdMeta)
{-
Make a case expr specifically for an update.
-}
mkExprSatSelsCaseUpdMeta :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> CTag -> Int -> [(Int,(Expr,MetaVal))] -> MbPatRest -> Expr
mkExprSatSelsCaseUpdMeta env mbNm meta e ct arity offValL mbRest
  = mkExprSatSelsCaseMeta env mbNm meta e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupply
        nmLblOffL = zip ns [0..] -- zip3 ns ns [0..]
        sel = mkExprTuple' ct (tyErr "TyCore.Utils.mkExprSatSelsCaseUpdMeta")
                $ map (fst.snd)
                -- $ (\v -> v `seq` tr "mkCExprSatSelsCaseUpdMeta" ("nr offValL" >#< length offValL >#< "arity" >#< arity) v)
                $ listSaturateWith 0 (arity-1) fst [(o,(o,(Expr_Var n,MetaVal_Val))) | (n,{-_,-}o) <- nmLblOffL] offValL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List comprehension utilities for deriving, see also HS/ToEH
%%% These functions redo on the Core level the desugaring done in ToEH. Regretfully so ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) hs export(mkListComprehenseGenerator)
mkListComprehenseGenerator :: RCEEnv -> Pat -> (Expr -> Expr) -> Expr -> Expr -> Expr
mkListComprehenseGenerator env patOk mkOk fail e
  = mkExprLam1 x ty
      (mkExprStrictSatCase (env {rceCaseCont = fail}) (Just (xStrict,ty)) (Expr_Var x)
        [Alt_Alt patOk (mkOk e)]
      )
  where x = mkHNmHidden "x"
        xStrict = hsnUniqifyEval x
        ty = tyErr "mkListComprehenseGenerator"
%%]

%%[(99 codegen) hs export(mkListComprehenseTrue)
mkListComprehenseTrue :: RCEEnv -> Ty -> Expr -> Expr
mkListComprehenseTrue env ty e = mkListSingleton (rceEHCOpts env) ty e
%%]

%%[(99 codegen) hs export(mkMatchString)
mkMatchString :: RCEEnv -> String -> Expr -> Expr -> Expr -> Expr
mkMatchString env str ok fail e
  = mkExprLetPlain x ty e
    $ foldr (\(c,ns@(_,xh,_)) ok
               -> matchCons ns
                  $ mkMatchChar opts (Just $ hsnUniqifyEval xh)  c (Expr_Var xh) ok fail
            )
            (matchNil xt ok)
    $ zip str nms
  where env' = env {rceCaseCont = fail}
        matchCons (x,xh,xt) e = mkExprSatSelsCase env' (Just (hsnUniqifyEval x,ty)) (Expr_Var x) constag [(xh,{-xh,-}0),(xt,{-xt,-}1)] (Just (PatRest_Empty,2)) e
        matchNil   x        e = mkExprSatSelsCase env' (Just (hsnUniqifyEval x,ty)) (Expr_Var x) niltag  []                    (Just (PatRest_Empty,0)) e
        constag = ctagCons opts
        niltag  = ctagNil  opts
        opts = rceEHCOpts env
        (nms@((x,_,_):_),(xt,_,_))
          = fromJust $ initlast $ snd
            $ foldr (\n (nt,l) -> (n,(n,hsnUniqifyStr HsNameUniqifier_Field "h" n,nt):l)) (hsnUnknown,[])
            $ take (length str + 1) $ hsnLclSupplyWith (mkHNmHidden "l")
        ty = tyErr "mkMatchString"
%%]

%%[(99 codegen) hs export(mkMatchTuple)
mkMatchTuple :: RCEEnv -> [HsName] -> Expr -> Expr -> Expr
mkMatchTuple env fldNmL ok e
  = mkExprLetPlain x ty e
    $ mkExprSatSelsCase env (Just (hsnUniqifyEval x,ty)) (Expr_Var x) CTagRec ({- zip3 fldNmL -} zip fldNmL [0..]) (Just (PatRest_Empty,length fldNmL)) ok
  where x = mkHNmHidden "x"
        ty = tyErr "mkMatchTuple"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FieldUpdateL,fuL2ExprL,fuL2ExprNodeFldL,fuMap)
type FieldUpdateL e = AssocL HsName (e,Maybe Int)

fuMap :: (HsName -> e -> (e',Int)) -> FieldUpdateL e -> FieldUpdateL e'
fuMap f = map (\(l,(e,_)) -> let (e',o) = f l e in (l,(e',Just o)))

fuL2ExprL' :: (HsName -> e -> x) -> FieldUpdateL e -> [x]
fuL2ExprL' f l = [ f n e | (n,(e,_)) <- l ]

fuL2ExprL :: FieldUpdateL Expr -> [Expr]
fuL2ExprL = fuL2ExprL' (\n e -> exprTupFld e)

fuL2ExprNodeFldL :: Bool -> (HsName -> Maybe HsName) -> FieldUpdateL Expr -> [ExprSeq1]
fuL2ExprNodeFldL yesThunk withLbl
  = fuL2ExprL' (\n e -> mk (withLbl n) e)
  where mk mbNm e = maybe (ExprSeq1_L0Val f Nothing) (\n -> ExprSeq1_L0LblVal n f) mbNm
           where f = mkth $ exprTupFld e
        mkth = if yesThunk then mkExprThunk else id

fuReorder :: EHCOpts -> [HsName] -> FieldUpdateL Expr -> (ValBindL,FieldUpdateL (Expr -> Expr))
fuReorder opts nL fuL
  =  let  (fuL',offL,_,_)
            =  foldl
                 (\(fuL,offL,exts,dels) (n,(_,(f,_)))
                     ->  let  mkOff n lbl o
                                =  let smaller l = rowLabCmp l lbl == LT
                                       off = length (filter smaller dels) - length (filter smaller exts)
                                   in  mkValBind1 n tyInt (tcAddInt opts o off)
                              no = Expr_Var n
                         in   case f of
                                 Expr_TupIns _ t l o e -> ((l,(\r -> Expr_TupIns r t l no e,Nothing)) : fuL,(mkOff n l o):offL,l:exts,dels  )
                                 Expr_TupUpd _ t l o e -> ((l,(\r -> Expr_TupUpd r t l no e,Nothing)) : fuL,(mkOff n l o):offL,exts  ,dels  )
                                 Expr_TupDel _ t l o   -> ((l,(\r -> Expr_TupDel r t l no  ,Nothing)) : fuL,(mkOff n l o):offL,exts  ,l:dels)
                 )
                 ([],[],[],[])
            .  zip nL
            $  fuL
          cmpFU (n1,_ ) (n2,_) = rowLabCmp n1 n2
     in   (offL, sortBy cmpFU fuL')
%%]

%%[(10 codegen) export(fuMkExpr)
fuMkExpr :: EHCOpts -> UID -> FieldUpdateL Expr -> Expr -> Expr
fuMkExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = mkValBind1 n (tyErr "fuMkExpr") r : oL
     in   mkExprLet ValBindCateg_Strict bL $ foldl (\r (_,(f,_)) -> f r) (Expr_Var n) $ fuL'
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
fvLAsArg :: VarIntroMp -> FvS -> VarIntroL
fvLAsArg cvarIntroMp fvS
  =  sortOn (vintroLev . snd)
     $ filter (\(_,cvi) -> vintroLev cvi > cLevModule)
     $ map (\n -> (n,vintroLookup n cvarIntroMp))
     $ Set.toList fvS

mkFvNm :: Int -> HsName -> HsName
mkFvNm i n = hsnUniqifyInt HsNameUniqifier_New i n -- hsnSuffix n ("~" ++ show i)

fvLArgRepl :: Int -> VarIntroL -> (VarIntroL,VarIntroL,VarReplNmMp)
fvLArgRepl uniq argLevL
  =  let  argNL = zipWith (\u (n,i) -> (mkFvNm u n,i)) [uniq..] argLevL
     in   ( argLevL
          , argNL
          , Map.fromList $ zipWith (\(o,_) (n,cvi) -> (o,(vreplFromVarIntro cvi) {vreplRepl = n})) argLevL argNL
          )

fvVarRepl :: VarReplNmMp -> HsName -> Expr
fvVarRepl nMp n = maybe (Expr_Var n) (Expr_Var . vreplRepl) $ Map.lookup n nMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RPatFld -> ValBind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(rpatBindL2ValBindL)
rpatBindL2ValBindL :: RCEEnv -> Bool -> HsName -> CTag -> MbPatRest -> AssocL RPatFld (Maybe Int) -> [ValBind]
rpatBindL2ValBindL env hasSub parNm ct rest pbL 
  = concat
    $  map  (\(RPatFld_Fld l o _ p,mbOff)
                -> let  b n = [mkValBind1 n tyInt (mkc n mbOff)]
                        pn  = parNm
                        pn' = hsnUniqifyEval pn
                        mkc n (Just o) = mkExprSatSelCase env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
                        mkc n Nothing  = mkExprSelCase    env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
                        ty n = tyErr ("rpatBindL2ValBindL: " ++ show n)
                   in   case rcpPNm p of
                            RPatNmOrig n           -> b n
                            RPatNmUniq n | hasSub  -> b n
                            _                      -> []
            )
    $  pbL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FldOffset(..),foffMkOff,foffLabel)
data FldOffset
  = FldKnownOffset      { foffLabel'     :: !HsName, foffOffset   :: !Int      }
  | FldComputeOffset    { foffLabel'     :: !HsName, foffExpr     :: !Expr    }
  | FldImplicitOffset

instance Eq FldOffset where
  (FldKnownOffset _ o1) == (FldKnownOffset _ o2) = o1 == o2
  foff1                 == foff2                 = foffLabel foff1 == foffLabel foff2

instance Ord FldOffset where
  (FldKnownOffset _ o1) `compare` (FldKnownOffset _ o2) = o1 `compare` o2
  foff1                 `compare` foff2                 = foffLabel foff1 `rowLabCmp` foffLabel foff2

foffMkOff :: FldOffset -> Int -> (Int,Expr)
foffMkOff FldImplicitOffset      o = (o,tcInt o)
foffMkOff (FldKnownOffset   _ o) _ = (o,tcInt o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff				= foffLabel' foff
%%]

%%[(8 codegen) export(FieldSplitL,fsL2PatL,fsL2PatOffsetL)
type FieldSplitL = AssocL FldOffset RPat

fsL2PatL :: FieldSplitL -> [RPat]
fsL2PatL = assocLElts

fsL2PatOffsetL :: FieldSplitL -> AssocL RPatFld (Maybe Int)
fsL2PatOffsetL l = [ (RPatFld_Fld n oe (rpatNmNm $ rcpPNm p) p,Just oi) | (o,(foff,p)) <- zip [0..] l, let (oi,oe) = foffMkOff foff o, let n = foffLabel foff ]
%%]

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
                                   in  tcAddInt opts o nrSmaller
                         in   ((FldComputeOffset l (mkOff l exts o),p):fsL,l:exts)
                 )
                 ([],[])
            $  fsL
     in   rowCanonOrderBy compare fsL'
%%]

%%[(8 codegen) export(rpbReorder,patBindLOffset)
rpbReorder :: EHCOpts -> [RPatFld] -> [RPatFld]
rpbReorder opts pbL
  =  let  (pbL',_)
            =  foldr
                 (\(RPatFld_Fld l o n p) (pbL,exts) 
                     ->  let  mkOff lbl exts o
                                =  let nrSmaller = length . filter (\e -> rowLabCmp e lbl == LT) $ exts
                                   in  tcAddInt opts o nrSmaller
                         in   ((RPatFld_Fld l (mkOff l exts o) n p):pbL,l:exts)
                 )
                 ([],[])
            $  pbL
          cmpPB (RPatFld_Fld l1 _ _ _)  (RPatFld_Fld l2 _ _ _) = rowLabCmp l1 l2
     in   sortBy cmpPB pbL'

patBindLOffset :: [RPatFld] -> ([RPatFld],[ValBindL])
patBindLOffset
  =  unzip
  .  map
       (\b@(RPatFld_Fld l o n p@(RPat_Var pn _))
           ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset $ rpatNmNm pn
               in   case o of
                      Expr_Int _ _ -> (b,[])
                      _            -> (RPatFld_Fld l (Expr_Var offNm) n p,[mkValBind1 offNm tyInt o])
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
rceRebinds :: Bool -> (HsName,Ty) -> RCEAltL -> ValBindL
rceRebinds origOnly (nm,ty) alts
  = [ mkValBind1 n ty (Expr_Var nm) | pn <- raltLPatNms alts, alsoUniq || rpatNmIsOrig pn, let n = rpatNmNm pn, n /= nm ]
  where alsoUniq = not origOnly
%%]
rceRebinds :: (HsName,Ty) -> RCEAltL -> ValBindL
rceRebinds (nm,ty) alts = [ mkValBind1 n ty (Expr_Var nm) | (RPatNmOrig n) <- raltLPatNms alts, n /= nm ]

%%[(8 codegen) hs
rceMatchVar :: RCEEnv ->  [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchVar env ((arg,ty):args') alts
  = mkExprLet ValBindCateg_Plain (rceRebinds True (arg,ty) alts) remMatch
  where remMatch  = rceMatch env args' [RAlt_Alt remPats e f | (RAlt_Alt (RPat_Var _ _ : remPats) e f) <- alts]

rceMatchIrrefutable :: RCEEnv ->  [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchIrrefutable env (argty@(arg,ty):args') alts@[RAlt_Alt (RPat_Irrefutable n _ b : remPats) e f]
  = mkExprLet ValBindCateg_Plain (rceRebinds False argty alts) $ mkExprLet ValBindCateg_Plain b remMatch
  where remMatch  = rceMatch env args' [RAlt_Alt remPats e f]

rceMkConAltAndSubAlts :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Alt
rceMkConAltAndSubAlts env ((arg,ty):args) alts@(alt:_)
  = Alt_Alt altPat (mkExprLet ValBindCateg_Plain (rceRebinds True (arg,ty) alts) subMatch)
  where (subAlts,subAltSubs)
          =  unzip
               [ (RAlt_Alt (pats ++ ps) e f, map (\p -> let n = rpatNmNm (rcpPNm p) in (n,rcpTy p)) pats)
               | (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_One _ pbinds) : ps) e f) <- alts
               , let pats = [ p | (RPatFld_Fld _ _ _ p) <- pbinds ]
               ]
        subMatch
          =  rceMatch env (head subAltSubs ++ args) subAlts
        altPat
          =  case alt of
               RAlt_Alt (RPat_Con n _ t (RPatConBind_One r pbL) : _) _ _
                 ->  Pat_Con {- (rpatNmNm n) -} t r pbL'
                     where  pbL' = [ {- FldBind_Fld l o n (Pat_Var (rpatNmNm $ rcpPNm p)) -} FldBind_Fld n (rcpTy p) o | (RPatFld_Fld l o n p) <- pbL ]
        tyerr n = tyErr ("rceMkConAltAndSubAlts: " ++ show n)

rceMatchCon :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchCon env ((arg,ty):args) alts
  = mkExprStrictSatCase env (Just (arg',ty)) (Expr_Var arg) alts'
  where arg'   =  hsnUniqifyEval arg
        alts'  =  map (rceMkConAltAndSubAlts env ((arg',ty):args))
                  $ groupSortOn (ctagTag . rcaTag)
                  $ filter (not . null . rcaPats)
                  $ alts

rceMatchConMany :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchConMany env ((arg,ty):args) [RAlt_Alt (RPat_Con n _ t (RPatConBind_Many bs) : ps) e f]
  = mkExprStrictIn arg' ty (Expr_Var arg)
                    (\_ -> foldr (\mka e -> rceMatch env [(arg',ty)] (mka e)) (rceMatch env ((arg',ty):args) altslast) altsinit)
  where arg'     = hsnUniqifyEval arg
        altsinit = [ \e -> [RAlt_Alt (RPat_Con n ty t b     : []) e f] | b <- bsinit ]
        altslast =         [RAlt_Alt (RPat_Con n ty t blast : ps) e f]
        (bsinit,blast) = panicJust "rceMatchConMany" $ initlast bs

rceMatchConst :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchConst env ((arg,ty):args) alts
  = mkExprStrictIn arg' ty (Expr_Var arg) (\n -> mkExprLet ValBindCateg_Plain (rceRebinds True (arg,ty) alts) (Expr_Case n alts' Nothing {-(rceCaseCont env)-}))
  where arg' = hsnUniqifyEval arg
        alts' = [ Alt_Alt (rpat2Pat p) (tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e) | (RAlt_Alt (p:_) e _) <- alts ]
%%]

%%[(97 codegen) hs
rceMatchBoolExpr :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchBoolExpr env aargs@((arg,ty):args) alts
  = foldr (\(n,c,t) f -> mkIf (rceEHCOpts env) (Just n) c t f) (rceCaseCont env) alts'
  where alts'  =  map (\(u, alts@(RAlt_Alt (RPat_BoolExpr _ b _ _ : _) _ _ : _))
                         -> ( hsnUniqifyInt HsNameUniqifier_Evaluated u arg
                            , mkExprApp b [Expr_Var arg]
                            , rceMatch env args [ RAlt_Alt remPats e f | (RAlt_Alt (RPat_BoolExpr _ _ _ _ : remPats) e f) <- alts ]
                            )
                      )
                  $ zip [0..]
                  $ groupSortOn (rcpMbConst . head . rcaPats)
                  $ filter (not . null . rcaPats)
                  $ alts
%%]
rceMatchBoolExpr2 :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatchBoolExpr2 env ((arg,ty):args) alts
  = foldr (\(n,c,t) f -> mkIf opts (Just n) c t f) (rceCaseCont env) m
  where m = [ ( hsnUniqifyInt HsNameUniqifier_Evaluated u arg -- hsnSuffix arg $ "!" ++ show u
              , mkExprApp b [Expr_Var arg]
              , rceMatch env args [RAlt_Alt remPats e f]
              )
            | (u,RAlt_Alt (RPat_BoolExpr _ _ b _ : remPats) e f) <- zip [0..] alts
            ]
        opts = rceEHCOpts env

%%[(8 codegen) hs
rceMatchSplits :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
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
rceMatch :: RCEEnv -> [(HsName,Ty)] -> RCEAltL -> Expr
rceMatch env [] []    =  rceCaseCont env
rceMatch env [] alts  
  =  case [ e | (RAlt_Alt [] e _) <- alts ] of
       (e:_)  -> tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e
       _      -> rceCaseCont env
rceMatch env args alts
  =  foldr
        (\alts e
           ->  case e of
                  Expr_Var _
                     ->  rceMatchSplits (rceUpdEnv e env) args alts
                  _  ->  mkExprLet ValBindCateg_Plain [mkValBind1 nc (tyErr "rceMatch") e]
                         $ rceMatchSplits (rceUpdEnv (Expr_Var nc) env) args alts
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
rceUpdEnv :: Expr -> RCEEnv -> RCEEnv
rceUpdEnv e env
  = env { rceCaseFailSubst = Map.union (Map.fromList [ (i,e) | i <- Set.toList (rceCaseIds env) ])
                             $ rceCaseFailSubst env
        , rceCaseCont      = e
        }
%%]

