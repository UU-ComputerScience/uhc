% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 import(FiniteMap,EHCommon,EHTy,EHCore,EHGam) export(RCEEnv(..),emptyRCEEnv)
%%]

%%[8 export(mkCExprStrictSatCase,mkCExprSelCase)
%%]

%%[8 import(List) export(FieldUpdateL,fuMkCExpr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data RCEEnv = RCEEnv {rceValGam :: ValGam, rceDataGam :: DataGam}

emptyRCEEnv :: RCEEnv
emptyRCEEnv = RCEEnv emptyGam emptyGam

rceEnvDataAlts :: RCEEnv -> CTag -> [CTag]
rceEnvDataAlts env t
  =  case t of
       CTag _ conNm _ _
          ->  case valGamLookup (rceValGam env) conNm of
                Just vgi
                   ->  let  tyNm = tyAppFunConNm . snd . tyArrowArgsRes . vgiTy $ vgi
                       in   maybe [] (eltsFM . dgiDataTagMp) . gamLookup (rceDataGam env) $ tyNm
                _  ->  []
       _  ->  []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Saturate alt's of case w.r.t. all possible tags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
caltLSaturate :: RCEEnv -> CAltL -> CExpr -> CAltL
caltLSaturate env alts ce
  =  let  altTags = [ t | (CAlt_Alt (CPat_Con _ t _ _ : _) _) <- alts ]
          absentTagArities = filter (\t -> t `notElem` altTags) . rceEnvDataAlts env . head $ altTags
          absentAlts
                 =  [ CAlt_Alt [mkP ct a] ce | ct@(CTag _ _ _ a) <- absentTagArities ]
                 where  mkB o = CPatBind_Bind hsnUnknown (CExpr_Int o) (cpatNmNm cpatNmNone) (CPat_Var cpatNmNone)
                        mkP ct a = CPat_Con cpatNmNone ct CPatRest_Empty [mkB o | o <- [0..a-1]]
     in   sortOn caltTag (alts ++ absentAlts)
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
mkCExprStrictSatCase :: RCEEnv -> HsName -> CExpr -> CAltL -> CExpr -> CExpr
mkCExprStrictSatCase env eNm e (alt:alts) ce
  =  let  (alt',altOffBL) = caltOffsetL alt
     in   mkCExprStrictIn eNm e
            (\n -> mkCExprLet CBindStrict altOffBL (CExpr_Case n (caltLSaturate env (alt':alts) ce) ce))

mkCExprSelCase :: RCEEnv -> HsName -> CExpr -> CTag -> HsName -> HsName -> CExpr -> CExpr
mkCExprSelCase env ne e ct n lbl off
  =  let  alt = CAlt_Alt
                    [CPat_Con (CPatNmOrig ne) ct (CPatRest_Var hsnWild)
                        [CPatBind_Bind lbl off n (CPat_Var (CPatNmOrig n))]]
                    (CExpr_Var n)
     in   mkCExprStrictSatCase env ne e [alt] cvarUndefined
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type FieldUpdateL e = AssocL HsName e

fuReorder :: [HsName] -> FieldUpdateL CExpr -> (CBindL,FieldUpdateL (CExpr -> CExpr))
fuReorder nL fuL
  =  let  (fuL',offL,_,_)
            =  foldl
                 (\(fuL,offL,exts,dels) (n,(_,f))
                     ->  let  mkOff n lbl o
                                =  let smaller l = rowLabCmp l lbl == LT
                                       off = length (filter smaller dels) - length (filter smaller exts)
                                   in  CBind_Bind n (o `mkCExprAddInt` off)
                              no = CExpr_Var n
                         in   case f of
                                 CExpr_TupIns _ t l o e -> ((l,\r -> CExpr_TupIns r t l no e) : fuL,(mkOff n l o):offL,l:exts,dels)
                                 CExpr_TupUpd _ t l o e -> ((l,\r -> CExpr_TupUpd r t l no e) : fuL,(mkOff n l o):offL,exts,dels)
                                 CExpr_TupDel _ t l o   -> ((l,\r -> CExpr_TupDel r t l no) : fuL,(mkOff n l o):offL,exts,l:dels)
                 )
                 ([],[],[],[])
            .  zip nL
            $  fuL
          cmpFU (n1,_ ) (n2,_) = rowLabCmp n1 n2
     in   (offL, sortBy cmpFU fuL')

fuMkCExpr :: UID -> FieldUpdateL CExpr -> CExpr -> CExpr
fuMkCExpr u fuL r
  =  let  (n:nL) = map uidHNm . mkNewLevUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder nL fuL
          bL = CBind_Bind n r : oL
     in   mkCExprLet CBindStrict bL . foldl (\r (_,f) -> f r) (CExpr_Var n) $ fuL'
%%]


