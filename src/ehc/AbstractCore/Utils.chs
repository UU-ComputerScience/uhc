%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core and TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}AbstractCore.Utils} import ({%{EH}AbstractCore})
%%]

%%[(8 codegen) import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty})
%%]

%%[(8 codegen) import({%{EH}Gam},{%{EH}Gam.ValGam},{%{EH}Gam.DataGam})
%%]

%%[(8 codegen) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(8 codegen) import(Data.List, qualified Data.Map as Map, qualified Data.Set as Set, Data.Maybe)
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
type CaseFailSubst' e m b ba t = CSubst' e m b ba t
%%]

%%[(8 codegen) export(RCEEnv'(..),emptyRCEEnv)
-- | Env to support Reordering of Case Expression (RCE)
data RCEEnv' expr metaval bind bindasp ty
  = RCEEnv
      { rceValGam           :: !ValGam					-- type of value (amongst other)
      , rceTyVarMp          :: !VarMp					-- tvar bindings for ValGam
      , rceDataGam          :: !DataGam					-- data type + constructor info
      , rceCaseFailSubst    :: !(CaseFailSubst' expr metaval bind bindasp ty)	-- fail continuation map
      , rceCaseIds          :: !UIDS					-- fail ids
      , rceCaseCont         :: !expr					-- continuation
      , rceEHCOpts          :: !EHCOpts					-- options
      -- , rceIsStrict			:: !Bool				-- scrutinee must be evaluated
      }

emptyRCEEnv :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> RCEEnv' e m b ba t
emptyRCEEnv opts = RCEEnv emptyGam emptyVarMp emptyGam Map.empty (Set.singleton uidStart) (acoreBuiltinUndefined opts) opts -- True
%%]

%%[(8 codegen) export(rceEnvDataAlts)
-- | All tags of the type of the constructor for a tag t
rceEnvDataAlts :: RCEEnv' e m b ba t -> CTag -> Maybe [CTag]
rceEnvDataAlts env t
  = case t of
      CTag _ conNm _ _ _
         -> case valGamTyOfDataCon conNm (rceValGam env) of
              (_,ty,[])
                 -> dataGamTagsOfTy (rceTyVarMp env `varUpd` ty) (rceDataGam env)
              _  -> Nothing
                    -- panic ("rceEnvDataAlts: " ++ show conNm) -- Nothing
                    -- dataGamTagsOfTy (Ty_Con conNm) (rceDataGam env)
      _  -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make pat from tag and arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatTagArityMbNms)
acorePatTagArityMbNms :: (AbstractCore e m b basp bcat mbind t p pr pf a) => CTag -> Int -> Maybe [HsName] -> p
acorePatTagArityMbNms ctag arity mbNmL
  = pat
  where pat = acorePatCon ctag (acorePatRestEmpty) (zipWith mkB nmL [0 .. arity - 1])
        mkB n o = acorePatFldTy (acoreTyErr "acorePatTagArityMbNms") (n,acoreInt o) n
        nmL = maybe (repeat hsnWild) id mbNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100519 AD: to be sorted out further, especially the uncommented panic occurs because of lacking environmental info:

ehc: panic: acoreAltLSaturate.rceEnvDataAlts(1): CTag {ctagTyNm = UHC.Base.$Dict-Real, ctagNm = UHC.Base.$Dict-Real, ctagTag' = 0, ctagArity = 3, ctagMaxArity = 3}

Because these are single alternative records, it does not harm to assume that, but has to be sorted out.


%%[(8 codegen) export(acoreAltLSaturate)
acoreAltLSaturate :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> [a] -> [a]
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
acorePatBindOffsetL :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [pf] -> ([pf],[b])
acorePatBindOffsetL pbL
  =  let  (pbL',obL)
            =  unzip
               .  map
                    (\b -> let (t,(l,o),n) = acoreUnPatFld b
                               offNm = hsnUniqify HsNameUniqifier_FieldOffset l
                           in  case acoreExprMbInt o of
                                 Just _ -> (b,[])
                                 _      -> (acorePatFldTy t (l,acoreVar offNm) n,[acoreBind1Ty offNm (acoreTyInt) o])
                    )
               $  pbL
     in   (pbL',concat obL)
%%]

%%[(8 codegen) export(acoreAltOffsetL)
acoreAltOffsetL :: (AbstractCore e m b basp bcat mbind t p pr pf a) => a -> (a,[b])
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
acoreStrictSatCaseMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> [a] -> e
acoreStrictSatCaseMetaTy env mbNm meta e []
  = rceCaseCont env			-- TBD: should be error message "scrutinizing datatype without constructors"
acoreStrictSatCaseMetaTy env mbNm meta e [alt] -- [CAlt_Alt (CPat_Con (CTag tyNm _ _ _ _) CPatRest_Empty [CPatFld_Fld _ _ pnm _]) ae]
  | isJust mbPatCon && length flds == 1 && not (ctagIsRec tg) && dgiIsNewtype dgi
  = acoreLet cat
      ( [ acoreBind1CatMetaTy cat pnm meta (acoreTyErr "TBD: mkExprStrictSatCaseMeta.1") e ]
        ++ maybe [] (\(n,ty) -> [ acoreBind1CatMetaTy cat n meta ty e ]) mbNm
      ) ae
  where dgi = panicJust "acoreStrictSatCaseMetaTy.dgi" $ dataGamLookup (ctagTyNm tg) (rceDataGam env)
        (pat,ae) = acoreUnAlt alt
        mbPatCon@(~(Just (tg,_,flds@(~([fld]))))) = acorePatMbCon pat
        (_,_,pnm) = acoreUnPatFld fld
        cat = acoreBindcategPlain
acoreStrictSatCaseMetaTy env mbNm meta e alts
  = case mbNm of
      Just (n,ty)  -> acoreLet1StrictInMetaTy n meta ty e $ mk alts
      Nothing -> mk alts e
  where mk (alt:alts) n
          = acoreLet (acoreBindcategStrict) altOffBL (acoreCaseDflt n (acoreAltLSaturate env (alt':alts)) (Just undef))
          where (alt',altOffBL) = acoreAltOffsetL alt
        mk [] n
          = acoreCaseDflt n [] (Just undef) -- dummy case
        undef = acoreBuiltinUndefined (rceEHCOpts env)

acoreStrictSatCaseMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> [a] -> e
acoreStrictSatCaseMeta env eNm m e alts = acoreStrictSatCaseMetaTy env (acoreTyLift "acoreStrictSatCaseMeta" eNm) m e alts
{-# INLINE acoreStrictSatCaseMeta #-}

acoreStrictSatCaseTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> [a] -> e
acoreStrictSatCaseTy env eNm e alts = acoreStrictSatCaseMetaTy env eNm acoreMetavalDflt e alts
{-# INLINE acoreStrictSatCaseTy #-}

acoreStrictSatCase :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> [a] -> e
acoreStrictSatCase env eNm e alts = acoreStrictSatCaseMeta env eNm acoreMetavalDflt e alts
{-# INLINE acoreStrictSatCase #-}
%%]

Export of the following group of defs can be removed after conversion of all utils to acore variants.

%%[(8 codegen) export(acoreSelsCasesMetaTy,acoreSelsCasesMeta,acoreSelsCasesTy,acoreSelsCases)
-- | Make a case expr from non-saturated alternatives,
-- | alternatives are given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
acoreSelsCasesMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesMetaTy env mbNm meta e tgSels
  = acoreStrictSatCaseMetaTy env mbNm meta e alts
  where  alts = [ acoreAlt 
                    (acorePatCon ct
                       (mkRest mbRest ct)
                       [acorePatFldTy (acoreTyErr "TBD: acoreSelsCasesMetaTy") (n,off) n | (n,{-lbl,-}off) <- nmLblOffL]
                    )
                    sel
                | (ct,nmLblOffL,mbRest,sel) <- tgSels
                ]
         mkRest mbr ct
           = case mbr of
               Just (r,_) -> r
               _          -> ctag (acorePatRestVar hsnWild) (\_ _ _ _ _ -> acorePatRestEmpty) ct

acoreSelsCasesMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesMeta env ne meta e tgSels = acoreSelsCasesMetaTy env (acoreTyLift "acoreStrictSatCaseMeta" ne) meta e tgSels
{-# INLINE acoreSelsCasesMeta #-}

acoreSelsCasesTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCasesTy env ne e tgSels = acoreSelsCasesMetaTy env ne acoreMetavalDflt e tgSels
{-# INLINE acoreSelsCasesTy #-}

acoreSelsCases :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> [(CTag,[(HsName,{-HsName,-}e)],MbPatRest' pr,e)] -> e
acoreSelsCases env ne e tgSels = acoreSelsCasesMeta env ne acoreMetavalDflt e tgSels
{-# INLINE acoreSelsCases #-}
%%]

%%[(8 codegen) export(acoreSelsCaseMetaTy,acoreSelsCaseMeta,acoreSelsCaseTy,acoreSelsCase)
-- | Make a case expr from a single alternative,
-- | the alternative given by their tag + fields (name/offset) + rest (for extensible records) + alt expr
acoreSelsCaseMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> CTag -> [(HsName,{-HsName,-}e)] -> MbPatRest' pr -> e -> e
acoreSelsCaseMetaTy env ne meta e ct nmLblOffL mbRest sel = acoreSelsCasesMetaTy env ne meta e [(ct,nmLblOffL,mbRest,sel)]

acoreSelsCaseMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> CTag -> [(HsName,{-HsName,-}e)] -> MbPatRest' pr -> e -> e
acoreSelsCaseMeta env ne meta e ct nmLblOffL mbRest sel = acoreSelsCaseMetaTy env (acoreTyLift "acoreSelsCaseMeta" ne) meta e ct nmLblOffL mbRest sel
{-# INLINE acoreSelsCaseMeta #-}

acoreSelsCaseTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> CTag -> [(HsName,{-HsName,-}e)] -> MbPatRest' pr -> e -> e
acoreSelsCaseTy env ne e ct nmLblOffL mbRest sel = acoreSelsCaseMetaTy env ne acoreMetavalDflt e ct nmLblOffL mbRest sel
{-# INLINE acoreSelsCaseTy #-}

acoreSelsCase :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> CTag -> [(HsName,{-HsName,-}e)] -> MbPatRest' pr -> e -> e
acoreSelsCase env ne e ct nmLblOffL mbRest sel = acoreSelsCaseMeta env ne acoreMetavalDflt e ct nmLblOffL mbRest sel
{-# INLINE acoreSelsCase #-}
%%]

%%[(8 codegen) export(acoreSelCaseTy,acoreSelCase)
-- | Make a case expr from a single alternative with a single field,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr
acoreSelCaseTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> CTag -> HsName -> e -> MbPatRest' pr -> e
acoreSelCaseTy env ne e ct n {-lbl-} off mbRest
  = acoreSelsCaseTy env ne e ct [(n,{-lbl,-}off)] mbRest (acoreVar n)

acoreSelCase :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe HsName -> e -> CTag -> HsName -> e -> MbPatRest' pr -> e
acoreSelCase env ne e ct n {-lbl-} off mbRest
  = acoreSelCaseTy env (acoreTyLift "acoreSelCase" ne) e ct n {-lbl-} off mbRest
{-# INLINE acoreSelCase #-}
%%]

%%[(8 codegen) export(acoreSatSelsCasesMetaTy,acoreSatSelsCasesTy,acoreSatSelsCases,acoreSatSelsCasesMeta)
-- | Make a case expr from a single alternative with non-saturated fields,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreSatSelsCasesMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest' pr,e)] -> e
acoreSatSelsCasesMetaTy env ne meta e tgSels
  =  acoreSelsCasesMetaTy env ne meta e alts
  where mkOffL ct mbr nol
          = case (ct,mbr) of
              (CTagRec       ,Nothing   ) -> map mklo nol
              (CTagRec       ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a _,_         ) -> mkloL a
          where mklo (n,{-l,-}o) = (n,{-l,-}acoreInt o)
                mkloL a = map mklo
                          $ listSaturateWith 0 (a-1) (\(_,{-_,-}o) -> o) [(o,(l,{-l,-}o)) | (o,l) <- zip [0..a-1] hsnLclSupply] $ nol
        alts = [ (ct,mkOffL ct mbRest nmLblOffL,mbRest,sel) | (ct,nmLblOffL,mbRest,sel) <- tgSels ]

acoreSatSelsCasesMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest' pr,e)] -> e
acoreSatSelsCasesMeta env ne meta e tgSels = acoreSatSelsCasesMetaTy env (acoreTyLift "acoreSatSelsCasesMeta" ne) meta e tgSels
{-# INLINE acoreSatSelsCasesMeta #-}

acoreSatSelsCasesTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest' pr,e)] -> e
acoreSatSelsCasesTy env ne e tgSels = acoreSatSelsCasesMetaTy env ne acoreMetavalDflt e tgSels
{-# INLINE acoreSatSelsCasesTy #-}

acoreSatSelsCases :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> [(CTag,[(HsName,{-HsName,-}Int)],MbPatRest' pr,e)] -> e
acoreSatSelsCases env ne e tgSels = acoreSatSelsCasesTy env (acoreTyLift "acoreSatSelsCases" ne) e tgSels
{-# INLINE acoreSatSelsCases #-}
%%]

%%[(8 codegen) export(acoreSatSelsCaseMetaTy,acoreSatSelsCaseMeta,acoreSatSelsCaseTy,acoreSatSelsCase)
-- | Make a case expr from a single alternative with non-saturated fields,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreSatSelsCaseMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest' pr -> e -> e
acoreSatSelsCaseMetaTy env ne meta e ct nmLblOffL mbRest sel = acoreSatSelsCasesMetaTy env ne meta e [(ct,nmLblOffL,mbRest,sel)]

acoreSatSelsCaseMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest' pr -> e -> e
acoreSatSelsCaseMeta env ne meta e ct nmLblOffL mbRest sel = acoreSatSelsCaseMetaTy env (acoreTyLift "acoreSatSelsCaseMeta" ne) meta e ct nmLblOffL mbRest sel
{-# INLINE acoreSatSelsCaseMeta #-}

acoreSatSelsCaseTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest' pr -> e -> e
acoreSatSelsCaseTy env ne e ct nmLblOffL mbRest sel = acoreSatSelsCaseMetaTy env ne acoreMetavalDflt e ct nmLblOffL mbRest sel
{-# INLINE acoreSatSelsCaseTy #-}

acoreSatSelsCase :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> CTag -> [(HsName,{-HsName,-}Int)] -> MbPatRest' pr -> e -> e
acoreSatSelsCase env ne e ct nmLblOffL mbRest sel = acoreSatSelsCaseTy env (acoreTyLift "acoreSatSelsCase" ne) e ct nmLblOffL mbRest sel
{-# INLINE acoreSatSelsCase #-}
%%]

%%[(8 codegen) hs export(acoreExprSatSelCaseTy,acoreExprSatSelCase)
-- | Make a case expr from a single alternative with a single field,
-- | the alternative given by their tag + field (name/offset) + rest (for extensible records) + alt expr,
-- | the fields (and alternatives) are saturated according to the tag + rest info
acoreExprSatSelCaseTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> e -> CTag -> HsName -> {- HsName -> -} Int -> MbPatRest' pr -> e
acoreExprSatSelCaseTy env ne e ct n {- lbl -} off mbRest = acoreSatSelsCaseTy env ne e ct [(n,{-lbl,-}off)] mbRest (acoreVar n)

acoreExprSatSelCase :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> e -> CTag -> HsName -> {- HsName -> -} Int -> MbPatRest' pr -> e
acoreExprSatSelCase env ne e ct n {- lbl -} off mbRest = acoreExprSatSelCaseTy env (acoreTyLift "acoreExprSatSelCase" ne) e ct n {- lbl -} off mbRest
{-# INLINE acoreExprSatSelCase #-}
%%]

%%[(8888 codegen) export(acoreSatSelsCaseUpdMetaTy,acoreSatSelsCaseUpdMeta)
-- | Make a case expr specifically for an update.
acoreSatSelsCaseUpdMetaTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName,t) -> m -> e -> CTag -> Int -> [(Int,(e,m))] -> MbPatRest' pr -> e
acoreSatSelsCaseUpdMetaTy env mbNm meta e ct arity offValL mbRest
  = acoreSatSelsCaseMetaTy env mbNm meta e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupply
        nmLblOffL = zip ns [0..] -- zip3 ns ns [0..]
        sel = acoreTagTupTy ct (acoreTyErr "AbstractCore.Utils.acoreSatSelsCaseUpdMetaTy")
                $ map (fst.snd)
                $ listSaturateWith 0 (arity-1) fst [(o,(o,(acoreVar n,acoreMetavalDflt))) | (n,{-_,-}o) <- nmLblOffL] offValL

acoreSatSelsCaseUpdMeta :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Maybe (HsName) -> m -> e -> CTag -> Int -> [(Int,(e,m))] -> MbPatRest' pr -> e
acoreSatSelsCaseUpdMeta env mbNm meta e ct arity offValL mbRest = acoreSatSelsCaseUpdMetaTy env (acoreTyLift "acoreSatSelsCaseUpdMeta" mbNm) meta e ct arity offValL mbRest
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List comprehension utilities for deriving, see also HS/ToEH
%%% These functions redo on the Core level the desugaring done in ToEH. Regretfully so ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) hs export(acoreMatchString)
acoreMatchString :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> String -> e -> e -> e -> e
acoreMatchString env str ok fail e
  = acoreLet1PlainTy x ty e
    $ foldr (\(c,ns@(_,xh,_)) ok
               -> matchCons ns
                  $ acoreMatchChar opts (Just $ hsnUniqifyEval xh)  c (acoreVar xh) ok fail
            )
            (matchNil xt ok)
    $ zip str nms
  where env' = env {rceCaseCont = fail}
        matchCons (x,xh,xt) e = acoreSatSelsCaseTy env' (Just (hsnUniqifyEval x,ty)) (acoreVar x) constag [(xh,0),(xt,1)] (Just (acorePatRestEmpty,2)) e
        matchNil   x        e = acoreSatSelsCaseTy env' (Just (hsnUniqifyEval x,ty)) (acoreVar x) niltag  []              (Just (acorePatRestEmpty,0)) e
        constag = ctagCons opts
        niltag  = ctagNil  opts
        opts = rceEHCOpts env
        (nms@((x,_,_):_),(xt,_,_))
          = fromJust $ initlast $ snd
            $ foldr (\n (nt,l) -> (n,(n,hsnUniqifyStr HsNameUniqifier_Field "h" n,nt):l)) (hsnUnknown,[])
            $ take (length str + 1) $ hsnLclSupplyWith (mkHNmHidden "l")
        ty = acoreTyErr "acoreMatchString"
%%]

%%[(99 codegen) hs export(acoreMatchTuple)
acoreMatchTuple :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> [HsName] -> e -> e -> e
acoreMatchTuple env fldNmL ok e
  = acoreLet1Plain x e
    $ acoreSatSelsCase env (Just $ hsnUniqifyEval x) (acoreVar x) CTagRec (zip fldNmL [0..]) (Just (acorePatRestEmpty,length fldNmL)) ok
  where x = mkHNmHidden "x"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
rceSplit :: (RAlt' e t b pr -> RCESplitCateg) -> RCEAltL' e t b pr -> [RCEAltL' e t b pr]
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
rceRebinds :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => Bool -> (HsName,t) -> RCEAltL' e t b pr -> [b]
rceRebinds origOnly (nm,ty) alts
  = [ acoreBind1Ty n ty (acoreVar nm) | pn <- raltLPatNms alts, alsoUniq || rpatNmIsOrig pn, let n = rpatNmNm pn, n /= nm ]
  where alsoUniq = not origOnly
%%]

%%[(8 codegen) hs
rceMatchVar :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t ->  [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchVar env ((arg,ty):args') alts
  = -- acoreLet acoreBindcategPlain (rceRebinds True (arg,ty) alts) remMatch
    remMatch
  where -- remMatch  = rceMatchTy env args' [RAlt_Alt remPats e f | a@(RAlt_Alt (RPat_Var _ _ : remPats) e f) <- alts]
        remMatch  = rceMatchTy env args' [RAlt_Alt remPats (acoreLet acoreBindcategPlain (rceRebinds True (arg,ty) [a]) e) f | a@(RAlt_Alt (RPat_Var _ _ : remPats) e f) <- alts]

rceMatchIrrefutable :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t ->  [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchIrrefutable env (argty@(arg,ty):args') alts@[RAlt_Alt (RPat_Irrefutable n _ b : remPats) e f]
  = acoreLet acoreBindcategPlain (rceRebinds False argty alts) $ acoreLet acoreBindcategPlain b remMatch
  where remMatch  = rceMatchTy env args' [RAlt_Alt remPats e f]

rceMkConAltAndSubAlts :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> a
rceMkConAltAndSubAlts env ((arg,ty):args) alts@(alt:_)
  = acoreAlt altPat (acoreLet acoreBindcategPlain (rceRebinds True (arg,ty) alts) subMatch)
  where (subAlts,subAltSubs)
          =  unzip
               [ ( RAlt_Alt (pats ++ ps) e f
                 , map (\p -> let n = rpatNmNm (rcpPNm p) in (n,rcpTy p)) pats
                 )
               | (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_One _ pbinds) : ps) e f) <- alts
               , let pats = [ p | (RPatFld_Fld _ _ _ p) <- pbinds ]
               ]
        subMatch
          =  rceMatchTy env (subAltSub ++ args) subAlts
          where subAltSub = zipWith (\(_,t) (n,ni) -> (ni,t)) (head subAltSubs) altNmIntroAssocL
        (altPat, altNmIntroAssocL)
          =  case alt of
               RAlt_Alt (RPat_Con n _ t (RPatConBind_One r pbL) : _) _ _
                 ->  (acorePatCon t r pbL', nmIntroAssocL)
                     where (pbL',nmIntroAssocL)
                               = unzip
                                   [ ( acorePatFldTy (rcpTy p) (l,o) introNm -- nm
                                     , (nm, introNm)
                                     )
                                   | (RPatFld_Fld l o n p, inx) <- zip pbL [(0 :: Int) ..]
                                   , let nm      = rpatNmNm $ rcpPNm p
                                   , let introNm = hsnUniqifyInt HsNameUniqifier_Field inx nm
                                   ]
        tyerr n = acoreTyErr ("rceMkConAltAndSubAlts: " ++ show n)

rceMatchCon :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchCon env ((arg,ty):args) alts
  = acoreStrictSatCaseTy env (Just (arg',ty)) (acoreVar arg) alts'
  where arg'   =  hsnUniqifyEval arg
        alts'  =  map (rceMkConAltAndSubAlts env ((arg',ty):args))
                  $ groupSortOn (ctagTag . rcaTag)
                  $ filter (not . null . rcaPats)
                  $ alts

rceMatchConMany :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchConMany env ((arg,ty):args) [RAlt_Alt (RPat_Con n _ t (RPatConBind_Many bs) : ps) e f]
  = acoreLet1StrictInTy arg' ty (acoreVar arg)
                        (\_ -> foldr (\mka e -> rceMatchTy env [(arg',ty)] (mka e)) (rceMatchTy env ((arg',ty):args) altslast) altsinit)
  where arg'     = hsnUniqifyEval arg
        altsinit = [ \e -> [RAlt_Alt (RPat_Con n ty t b     : []) e f] | b <- bsinit ]
        altslast =         [RAlt_Alt (RPat_Con n ty t blast : ps) e f]
        (bsinit,blast) = panicJust "rceMatchConMany" $ initlast bs

rceMatchConst :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchConst env ((arg,ty):args) alts
  = acoreLet1StrictInTy arg' ty (acoreVar arg) (\n -> acoreLet cat (rceRebinds True (arg,ty) alts) (acoreCaseDflt n alts' Nothing {-(rceCaseCont env)-}))
  where arg' = hsnUniqifyEval arg
        alts' = [ acoreAlt (acoreRPat2Pat p) (cSubstApp (rceCaseFailSubst env) e {- tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e -}) | (RAlt_Alt (p:_) e _) <- alts ]
        cat = acoreBindcategPlain
%%]

%%[(97 codegen) hs
rceMatchBoolExpr :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchBoolExpr env aargs@((arg,ty):args) alts
  = foldr (\(n,c,t) f -> acoreIf (rceEHCOpts env) (Just n) c t f) (rceCaseCont env) alts'
  where alts'  =  map (\(u, alts@(RAlt_Alt (RPat_BoolExpr _ _ b _ : _) _ _ : _))
                         -> ( hsnUniqifyInt HsNameUniqifier_Evaluated u arg
                            , acoreApp b [acoreVar arg]
                            , rceMatchTy env args [ RAlt_Alt remPats e f | (RAlt_Alt (RPat_BoolExpr _ _ _ _ : remPats) e f) <- alts ]
                      )     )
                  $ zip [0..]
                  $ groupSortOn (rcpMbConst . head . rcaPats)
                  $ filter (not . null . rcaPats)
                  $ alts
%%]

%%[(8 codegen) hs
rceMatchSplits :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
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

%%[(8 codegen) hs export(rceMatchTy)
rceMatchTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName,t)] -> RCEAltL' e t b pr -> e
rceMatchTy env [] []    =  rceCaseCont env
rceMatchTy env [] alts  
  =  case [ e | (RAlt_Alt [] e _) <- alts ] of
       (e:_)  -> cSubstApp (rceCaseFailSubst env) e -- tcSubstCaseAltFail (rceEHCOpts env) (rceCaseFailSubst env) e
       _      -> rceCaseCont env
rceMatchTy env args alts
  =  foldr
        (\alts e
           ->  case acoreExprMbVar e of
                  Just _
                     ->  rceMatchSplits (rceUpdEnv e env) args alts
                  _  ->  acoreLet1PlainTy nc (acoreTyErr "rceMatch") e
                         $ rceMatchSplits (rceUpdEnv (acoreVar nc) env) args alts
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

%%[(8 codegen) hs export(rceMatch)
rceMatch :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a, CSubstitutable e m b ba t e) => RCEEnv' e m b ba t -> [(HsName)] -> RCEAltL' e t b pr -> e
rceMatch env args alts = rceMatchTy env (acoreTyLift "rceMatch" args) alts
%%]

%%[(8 codegen) hs export(rceUpdEnv)
rceUpdEnv :: e -> RCEEnv' e m b ba t -> RCEEnv' e m b ba t
rceUpdEnv e env
  = env { rceCaseFailSubst = Map.union (acoreCSubstFromUidExprL [ (i,e) | i <- Set.toList (rceCaseIds env) ])
                             $ rceCaseFailSubst env
        , rceCaseCont      = e
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%%[(9 codegen) hs export(acoreCSubstFromVarMpImpls)
acoreCSubstFromVarMpImpls :: (AbstractCore e m b basp bcat mbind t p pr pf a) => VarMp -> CSubst' e m b ba t
acoreCSubstFromVarMpImpls c
  =  acoreCSubstFromUidImplsL
        [ (iv,(acoreCoeImplsApp i,acoreCoeImplsLam acoreCoeId i))
        | (iv,VMIImpls i) <- varmpToAssocL c, let (_,mbTl) = implsPredsMbTail i, isNothing mbTl
        ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RPatFld -> binds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreRPatBindL2BindL)
acoreRPatBindL2BindL :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => RCEEnv' e m b ba t -> Bool -> HsName -> CTag -> MbPatRest' pr -> AssocL (RPatFld' e t b pr) (Maybe Int) -> [b]
acoreRPatBindL2BindL env hasSub parNm ct rest pbL 
  = concat
    $  map  (\(RPatFld_Fld l o _ p,mbOff)
                -> let  b n = [acoreBind1CatTy acoreBindcategPlain n acoreTyInt (mkc n mbOff)]
                        pn  = parNm
                        pn' = hsnUniqifyEval pn
                        mkc n (Just o) = acoreExprSatSelCaseTy env (Just (pn',ty pn')) (acoreVar pn) ct n {- l -} o rest
                        mkc n Nothing  = acoreSelCaseTy    env (Just (pn',ty pn')) (acoreVar pn) ct n {- l -} o rest
                        ty n = acoreTyErr ("acoreRPatBindL2BindL: " ++ show n)
                   in   case rcpPNm p of
                            RPatNmOrig n           -> b n
                            RPatNmUniq n | hasSub  -> b n
                            _                      -> []
            )
    $  pbL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RPatFld utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) export(acoreRpbReorder,acoreRPatFldLSplitToOffset)
acoreRpbReorder :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> [RPatFld' e t b pr] -> [RPatFld' e t b pr]
acoreRpbReorder opts pbL
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

acoreRPatFldLSplitToOffset :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => [RPatFld' e t b pr] -> ([RPatFld' e t b pr],[[b]])
acoreRPatFldLSplitToOffset
  =  unzip
  .  map
       (\b@(RPatFld_Fld l o n p@(RPat_Var pn _))
           ->  let  offNm = hsnUniqify HsNameUniqifier_FieldOffset $ rpatNmNm pn
               in   case acoreExprMbInt o of
                      Just _ -> (b,[])
                      _      -> (RPatFld_Fld l (acoreVar offNm) n p,[acoreBind1Ty offNm acoreTyInt o])
       )
%%]



