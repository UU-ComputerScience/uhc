%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Core.Utils} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Core},{%{EH}Gam}) export(RCEEnv(..),emptyRCEEnv)
%%]

%%[8 import(Data.List,EH.Util.Utils) export(FieldUpdateL,fuL2ExprL,fuMkCExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data RCEEnv
  = RCEEnv
      { rceValGam           :: ValGam
      , rceDataGam          :: DataGam
      , rceCaseFailSubst    :: CaseFailSubst
      , rceCaseId           :: UID
      , rceCaseCont         :: CExpr
      }

emptyRCEEnv :: RCEEnv
emptyRCEEnv = RCEEnv emptyGam emptyGam Map.empty uidStart cvarUndefined

rceEnvDataAlts :: RCEEnv -> CTag -> [CTag]
rceEnvDataAlts env t
  =  case t of
       CTag _ conNm _ _
          ->  case valGamLookup conNm (rceValGam env) of
                Just vgi
                   ->  let  ty = snd $ tyArrowArgsRes $ vgiTy $ vgi
                       in   maybe [] id $ dataGamTagsOfTy ty (rceDataGam env)
                _  ->  [t]
       _  ->  [t]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% General purpose saturate (should move to lib, or Common)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
listSaturate :: (Enum a,Ord a) => a -> a -> (x -> a) -> (a -> x) -> [x] -> [x]
listSaturate min max get mk l
  = [ Map.findWithDefault (mk i) i mp | i <- [min..max] ]
  where mp = Map.fromList [ (get x,x) | x <- l ]
%%]

%%[8
listSaturateWith :: (Enum a,Ord a) => a -> a -> (x -> a) -> [(a,x)] -> [x] -> [x]
listSaturateWith min max get missing l
  = listSaturate min max get mk l
  where mp = Map.fromList missing
        mk a = panicJust "listSaturateWith" $ Map.lookup a mp
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
                          mkP     ct a = CPat_Con cpatNmNone ct CPatRest_Empty [mkB o | o <- [0..a-1]]
                          mkB o        = CPatBind_Bind hsnUnknown (CExpr_Int o) (cpatNmNm cpatNmNone) (CPat_Var cpatNmNone)
      _     -> []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extract offsets from pat bindings as separate binding to new/fresh names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpatBindLOffsetL :: CPatBindL -> (CPatBindL,CBindL)
cpatBindLOffsetL pbL
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
     in   (pbL',concat obL)

caltOffsetL :: CAlt -> (CAlt,CBindL)
caltOffsetL alt
  =  case alt of
       CAlt_Alt (CPat_Con n t r pbL : ps) e
         ->  (CAlt_Alt (CPat_Con n t r pbL' : ps) e,offBL)
             where (pbL',offBL) = cpatBindLOffsetL pbL
       _ ->  (alt,[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct case with: strict in expr, offsets strict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type MbCPatRest = Maybe (CPatRest,Int) -- (pat rest, arity)
%%]

%%[8 export(mkCExprStrictSatCase)
mkCExprStrictSatCase :: RCEEnv -> Maybe HsName -> CExpr -> CAltL -> CExpr
mkCExprStrictSatCase env eNm e (alt:alts)
  =  let  (alt',altOffBL) = caltOffsetL alt
          mk n = mkCExprLet CBindStrict altOffBL (CExpr_Case n (caltLSaturate env (alt':alts)) (rceCaseCont env))
     in   case eNm of
            Just n  -> mkCExprStrictIn n e mk
            Nothing -> mk e
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
mkCExprSelsCase' :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,CExpr)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSelsCase' env ne e ct nmLblOffL mbRest sel
  =  let  n = maybe (cexprVar e) id ne
          alt = CAlt_Alt
                  [CPat_Con (CPatNmOrig $ maybe (cexprVar e) id ne) ct rest
                      [CPatBind_Bind lbl off n (CPat_Var (CPatNmOrig n)) | (n,lbl,off) <- nmLblOffL]]
                  sel
          rest = case mbRest of
                   Just (r,_) -> r
                   _          -> ctag (CPatRest_Var hsnWild) (\_ _ _ _ -> CPatRest_Empty) ct
     in   mkCExprStrictSatCase (env {rceCaseCont = cvarUndefined}) ne e [alt]
%%]

%%[8 export(mkCExprSatSelsCase)
mkCExprSatSelsCase :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> [(HsName,HsName,Int)] -> MbCPatRest -> CExpr -> CExpr
mkCExprSatSelsCase env ne e ct nmLblOffL mbRest sel
  =  mkCExprSelsCase' env ne e ct nmLblOffL' mbRest sel
  where nmLblOffL'
          = case (ct,mbRest) of
              (CTagRec     ,Nothing   ) -> map mklo nmLblOffL
              (CTagRec     ,Just (_,a)) -> mkloL a
              (CTag _ _ _ a,_         ) -> mkloL a
        mklo (n,l,o) = (n,l,CExpr_Int o)
        mkloL a = map mklo $ listSaturateWith 0 (a-1) (\(n,l,o) -> o) [(o,(l,l,o)) | (o,l) <- zip [0..a-1] hsnLclSupplyL] $ nmLblOffL
%%]

%%[8 export(mkCExprSatSelsCaseUpd)
mkCExprSatSelsCaseUpd :: RCEEnv -> Maybe HsName -> CExpr -> CTag -> Int -> [(Int,CExpr)] -> MbCPatRest -> CExpr
mkCExprSatSelsCaseUpd env ne e ct arity offValL mbRest
  = mkCExprSatSelsCase env ne e ct nmLblOffL mbRest sel
  where ns = take arity hsnLclSupplyL
        nmLblOffL = zip3 ns ns [0..]
        valMp = Map.fromList offValL
        sel = mkCExprApp (CExpr_Tup ct) [ Map.findWithDefault (CExpr_Var n) o valMp | (n,_,o) <- nmLblOffL ] 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FieldUpdateL e = AssocL HsName (e,Maybe Int)

fuL2ExprL :: FieldUpdateL CExpr -> [CExpr]
fuL2ExprL l = [ e | (_,(CExpr_TupIns _ _ _ _ e,_)) <- l ]

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


