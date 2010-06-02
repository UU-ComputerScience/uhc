%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core and TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}AbstractCore.Utils} import ({%{EH}AbstractCore})
%%]

%%[(8 codegen) import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty})
%%]

%%[(8 codegen) import({%{EH}Gam},{%{EH}Gam.ValGam},{%{EH}Gam.DataGam})
%%]

%%[(8 codegen) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set, Data.Maybe)
%%]

%%[(8 codegen) import(EH.Util.Utils)
%%]

-- debug
%%[(8 codegen) import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstracted/shared types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(CaseFailSubst')
-- | Substitution for fail construct, identified by a UID
type CaseFailSubst' expr = Map.Map UID expr
%%]

%%[(8 codegen) export(RCEEnv'(..),emptyRCEEnv)
-- | Env to support Reordering of Case Expression (RCE)
data RCEEnv' expr
  = RCEEnv
      { rceValGam           :: !ValGam					-- type of value (amongst other)
      , rceTyVarMp          :: !VarMp					-- tvar bindings for ValGam
      , rceDataGam          :: !DataGam					-- data type + constructor info
      , rceCaseFailSubst    :: !(CaseFailSubst' expr)	-- fail continuation map
      , rceCaseIds          :: !UIDS					-- fail ids
      , rceCaseCont         :: !expr					-- continuation
      , rceEHCOpts          :: !EHCOpts					-- options
      -- , rceIsStrict			:: !Bool				-- scrutinee must be evaluated
      }

emptyRCEEnv :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> RCEEnv' e
emptyRCEEnv opts = RCEEnv emptyGam emptyVarMp emptyGam Map.empty (Set.singleton uidStart) (acoreBuiltinUndefined opts) opts -- True
%%]

%%[(8 codegen) export(rceEnvDataAlts)
-- | All tags of the type of the constructor for a tag t
rceEnvDataAlts :: RCEEnv' e -> CTag -> Maybe [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> dataGamTagsOfTy (rceTyVarMp env |=> ty) (rceDataGam env)
              _  -> Nothing
                    -- panic ("rceEnvDataAlts: " ++ show conNm) -- Nothing
                    -- dataGamTagsOfTy (Ty_Con conNm) (rceDataGam env)
      _  -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: Bool
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(9191 codegen) hs export(ctagTrue, ctagFalse)
ctagTrue, ctagFalse :: EHCOpts -> CTag
ctagTrue  opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolTrue)  1 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagFalse opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolFalse) 0 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(9191 codegen) hs export(ctagCons,ctagNil)
ctagCons, ctagNil :: EHCOpts -> CTag
ctagCons opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltCons) 0 2 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagNil  opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltNil ) 1 0 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract syntax for encoding case+pattern rewrite info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RAlt'(..),RPat'(..),RPatConBind'(..),RPatFld'(..))
data RAlt' e t b pr
  = RAlt_Alt			{ rcaPats :: ![RPat' e t b pr], raaExpr :: !e, raaFailS :: UIDS }

data RPat' e t b pr
  = RPat_Var			{ rcpPNm :: !RPatNm, rcpTy :: !t }
  | RPat_Con			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpTag :: !CTag, rcpBinds :: !(RPatConBind' e t b pr) }
  | RPat_Int			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpInt :: !Integer }
  | RPat_Char			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpChar :: !Char }
  | RPat_Irrefutable	{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpValBindL :: ![b] }
%%[[97
  | RPat_BoolExpr		{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpExpr :: !e, rcpMbConst :: Maybe SrcConst }
%%]]

data RPatConBind' e t b pr
  = RPatConBind_One		{ rpcbRest :: !pr, rpcbBinds :: ![RPatFld' e t b pr] }
  | RPatConBind_Many	{ rpcbConBinds :: ![RPatConBind' e t b pr] }

data RPatFld' e t b pr
  = RPatFld_Fld		    { rpbLbl :: !HsName, rpbOffset :: !e, rpbNm :: !HsName, rpbPat :: !(RPat' e t b pr)}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatTagArityMbNms)
acorePatTagArityMbNms :: (AbstractCore e m b bcat mbind t p pr pf a) => CTag -> Int -> Maybe [HsName] -> p
acorePatTagArityMbNms ctag arity mbNmL
  = pat
  where pat = acorePatCon ctag (acorePatRestEmpty) (zipWith mkB nmL [0 .. arity - 1])
        mkB n o = acorePatFldTy (acoreTyErr2 pat "acorePatTagArityMbNms") (n,acoreInt o) n
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100519 AD: to be sorted out further, especially the uncommented panic occurs because of lacking environmental info:

ehc: panic: acoreAltLSaturate.rceEnvDataAlts(1): CTag {ctagTyNm = UHC.Base.$Dict-Real, ctagNm = UHC.Base.$Dict-Real, ctagTag' = 0, ctagArity = 3, ctagMaxArity = 3}

Because these are single alternative records, it does not harm to assume that, but has to be sorted out.


%%[(8 codegen) export(acoreAltLSaturate)
acoreAltLSaturate :: (AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> [a] -> [a]
acoreAltLSaturate env alts
  = case alts of
      (alt1:_) -> listSaturateWith 0 (length allAlts - 1) altIntTag allAlts alts
            where (allAlts,altIntTag)
                    = case acorePatMbCon pat of
                        -- if a con pat, use the tag to dispatch upon
                        Just (CTagRec,_,_)
                          -> ([(0,alt1)], const 0)
                        Just (tg,_,_)
                          -> case rceEnvDataAlts env tg of
                               Just ts    -> ([ (ctagTag t,mkA env t (ctagArity t)) | t <- ts ], ctagTag . panicJust "acoreAltLSaturate.rceEnvDataAlts(2)" . acoreAltMbTag)
                               _          -> -- tr "acoreAltLSaturate" (pp tg) $
                                             ([(0,alt1)], const 0)
                               -- _          -> panic ("acoreAltLSaturate.rceEnvDataAlts(1): " ++ show tg)
                        _ -> case acorePatMbInt pat of
                               -- if an int, use the int to dispatch upon; used internally only (by deriving Enum)
                               Just (_,i) -> ([ (fromInteger i, a) | a <- alts ], fromInteger . snd . panicJust "acoreAltLSaturate.acorePatMbInt(2)" . acorePatMbInt . fst . acoreUnAlt)
                               _          -> panic "acoreAltLSaturate.acorePatMbInt(1)"
                    where (pat,_) = acoreUnAlt alt1
                          mkA env ct a = acoreAlt (acorePatTagArityMbNms ct a Nothing) (rceCaseCont env)
      _     -> []
%%]

Original (TyCore):

caltLSaturate :: RCEEnv -> AltL -> AltL
caltLSaturate env alts
  = case alts of
      (alt1:_) -> -- (\v -> v `seq` tr "caltLSaturate" ("nr alts" >#< length alts >#< "all" >#< length allAlts) v) $ 
                  listSaturateWith 0 (length allAlts - 1) altIntTag allAlts alts
            where allAlts
                    = case rceEnvDataAlts env (panicJust "caltLSaturate" $ acoreAltMbTag alt1) of
                        Just ts -> [ (ctagTag t,mkA env t (ctagArity t)) | t <- ts ]
                        _       -> [ (altIntTag a, a) | a <- alts ]
                    where mkA env ct a = acoreAlt (acorePatTagArityMbNms ct a Nothing) (rceCaseCont env)
      _     -> []

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
acorePatBindOffsetL :: (AbstractCore e m b bcat mbind t p pr pf a) => [pf] -> ([pf],[b])
acorePatBindOffsetL pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b -> let (t,(l,o),n) = acoreUnPatFld b
                               offNm = hsnUniqify HsNameUniqifier_FieldOffset l
                           in  case acoreExprMbInt o of
                                 Just _ -> (b,[])
                                 _      -> (acorePatFldTy t (l,acoreVar offNm) n,[acoreBind1Ty offNm (acoreTyInt o) o])
                    )
               $  pbL
     in   (pbL',concat obL)
%%]

%%[(8 codegen) export(acoreAltOffsetL)
acoreAltOffsetL :: (AbstractCore e m b bcat mbind t p pr pf a) => a -> (a,[b])
acoreAltOffsetL alt
  =  case acorePatMbCon p of
       Just (t,r,b)
         ->  (acoreAlt (acorePatCon t r b') e,offBL)
             where (b',offBL) = acorePatBindOffsetL b
       _ ->  (alt,[])
  where (p,e) = acoreUnAlt alt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(MbPatRest')
type MbPatRest' pr = Maybe (pr,Int) -- (pat rest, arity)
%%]

%%[(8 codegen) export(acoreStrictSatCaseMetaTy,acoreStrictSatCaseTy,acoreStrictSatCaseMeta,acoreStrictSatCase)
-- | Make case expression from alternatives, saturating the alternatives w.r.t. all constructors
-- | Either:
-- |   - make a case expr from alternatives,
-- |     saturating the alternatives with defaults for missing alternatives.
-- |   - or, when only a single alternative binding a single field, bind it directly with a let
acoreStrictSatCaseMetaTy :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName,t) -> m -> e -> [a] -> e
acoreStrictSatCaseMetaTy env mbNm meta e []
  = rceCaseCont env			-- TBD: should be error message "scrutinizing datatype without constructors"
acoreStrictSatCaseMetaTy env mbNm meta e [alt] -- [CAlt_Alt (CPat_Con (CTag tyNm _ _ _ _) CPatRest_Empty [CPatFld_Fld _ _ pnm]) ae]
  | isJust mbPatCon && length flds == 1 && dgiIsNewtype dgi
  = acoreLet cat
      ( [ acoreBind1CatMetaTy cat pnm meta (acoreTyErr e "TBD: mkExprStrictSatCaseMeta.1") e ]
        ++ maybe [] (\(n,ty) -> [ acoreBind1CatMetaTy cat n meta ty e ]) mbNm
      ) ae
  where dgi = panicJust "acoreStrictSatCaseMetaTy.dgi" $ dataGamLookup (ctagTyNm tg) (rceDataGam env)
        (pat,ae) = acoreUnAlt alt
        mbPatCon@(~(Just (tg,_,flds@(~([fld]))))) = acorePatMbCon pat
        (_,_,pnm) = acoreUnPatFld fld
        cat = acoreBindcategPlain e
acoreStrictSatCaseMetaTy env mbNm meta e alts
  = case mbNm of
      Just (n,ty)  -> acoreLetStrictInMetaTy n meta ty e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = acoreLet (acoreBindcategStrict e) altOffBL (acoreCaseDflt n (acoreAltLSaturate env (alt':alts)) (Just $ rceCaseCont env))
          where (alt',altOffBL) = acoreAltOffsetL alt
        mk [] n
          = acoreCaseDflt n [] (Just $ rceCaseCont env) -- dummy case

acoreStrictSatCaseMeta :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName) -> m -> e -> [a] -> e
acoreStrictSatCaseMeta env eNm m e alts = acoreStrictSatCaseMetaTy env (fmap (\n -> (n,acoreTyErr e "acoreStrictSatCaseMeta")) eNm) m e alts

acoreStrictSatCaseTy :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName,t) -> e -> [a] -> e
acoreStrictSatCaseTy env eNm e alts = acoreStrictSatCaseMetaTy env eNm (acoreMetavalDflt e) e alts

acoreStrictSatCase :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName) -> e -> [a] -> e
acoreStrictSatCase env eNm e alts = acoreStrictSatCaseMeta env eNm (acoreMetavalDflt e) e alts
%%]

Export of the following group of defs can be removed after conversion of all utils to acore variants.

%%[(8 codegen) export(acoreSelsCasesMetaTy,acoreSelsCasesMeta,acoreSelsCasesTy,acoreSelsCases)
-- | Make a case expr from non-saturated alternatives,
-- | alternatives are given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
acoreSelsCasesMetaTy :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName,t) -> m -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesMetaTy env mbNm meta e tgSels
  = acoreStrictSatCaseMetaTy env mbNm meta e alts
  where  alts = [ acoreAlt 
                    (acorePatCon ct
                       (mkRest mbRest ct)
                       [acorePatFldTy (acoreTyErr e "TBD: acoreSelsCasesMetaTy") (n,off) n | (n,{-lbl,-}off) <- nmLblOffL]
                    )
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (acorePatRestVar hsnWild) (\_ _ _ _ _ -> acorePatRestEmpty) ct

acoreSelsCasesMeta :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName) -> m -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesMeta env ne meta e tgSels = acoreSelsCasesMetaTy env (fmap (\n -> (n,acoreTyErr e "acoreStrictSatCaseMeta")) ne) meta e tgSels

acoreSelsCasesTy :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName,t) -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesTy env ne e tgSels = acoreSelsCasesMetaTy env ne (acoreMetavalDflt e) e tgSels

acoreSelsCases :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => RCEEnv' e -> Maybe (HsName) -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCases env ne e tgSels = acoreSelsCasesMeta env ne (acoreMetavalDflt e) e tgSels
%%]

Original Core:

mkCExprSelsCasesMeta' :: RCEEnv -> Maybe HsName -> CMetaVal -> CExpr -> [(CTag,[(HsName,HsName,CExpr)],MbCPatRest,CExpr)] -> CExpr
mkCExprSelsCasesMeta' env mbNm meta e tgSels
  = acoreStrictSatCaseMeta env mbNm meta e alts
  where  alts = [ CAlt_Alt
                    (CPat_Con ct
                       (mkRest mbRest ct)
                       [CPatFld_Fld lbl off n | (n,lbl,off) <- nmLblOffL]
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

Original TyCore:

mkExprSelsCasesMeta' :: RCEEnv -> Maybe (HsName,Ty) -> MetaVal -> Expr -> [(CTag,[(HsName,{-HsName,-}Expr)],MbPatRest,Expr)] -> Expr
mkExprSelsCasesMeta' env mbNm meta e tgSels
  = acoreStrictSatCaseMetaTy env mbNm meta e alts
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
