%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) module {%{EH}TyCore.Utils2} import(qualified Data.Map as Map,Data.Maybe,{%{EH}Base.Builtin},{%{EH}Opts},{%{EH}Base.Common}) 
%%]
%%[(8 codegen tycore) import({%{EH}TyCore.Base})
%%]

%%[(8 codegen tycore) hs import({%{EH}AbstractCore})
%%]
%%[(8 codegen tycore) hs import({%{EH}AbstractCore.Utils} hiding (rceMatch)) export(module {%{EH}AbstractCore.Utils})
%%]

%%[(8 codegen tycore) import({%{EH}Gam},{%{EH}VarMp},{%{EH}Substitutable},{%{EH}Gam.ValGam},{%{EH}Gam.DataGam})
%%]

%%[(8 codegen tycore) import(Data.List,qualified Data.Set as Set,Data.List,qualified Data.Map as Map,EH.Util.Utils)
%%]

-- debug
%%[(8 codegen tycore) import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Env to support Reordering of Case Expression (RCE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) export(RCEEnv)
type RCEEnv = RCEEnv' Expr MetaVal ValBind ValBind Ty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Remainder of pattern for extensible records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen tycore) export(MbPatRest)
type MbPatRest = MbPatRest' PatRest
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reorder record Field Update (to sorted on label, upd's first, then ext's)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) export(FieldUpdateL,fuL2ExprL,fuL2ExprNodeFldL,fuMap)
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
  where mk mbNm e = maybe (ExprSeq1_L0Val f) (\n -> ExprSeq1_L0LblVal n f) mbNm
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
                                   in  mkValBind1 n tyInt (acoreBuiltinAddInt opts o off)
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

%%[(10 codegen tycore) export(fuMkExpr)
fuMkExpr :: EHCOpts -> UID -> FieldUpdateL Expr -> Expr -> Expr
fuMkExpr opts u fuL r
  =  let  (n:nL) = map (uidHNm . uidChild) . mkNewUIDL (length fuL + 1) $ u
          (oL,fuL') = fuReorder opts nL fuL
          bL = mkValBind1 n (tyErr "fuMkExpr") r : oL
     in   acoreLet ValBindCateg_Strict bL $ foldl (\r (_,(f,_)) -> f r) (Expr_Var n) $ fuL'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Free var closure, and other utils used by Trf/...GlobalAsArg transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) export(fvsClosure,fvsTransClosure)
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

%%[(8 codegen tycore) export(fvLAsArg,mkFvNm,fvLArgRepl,fvVarRepl)
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

%%[(8888 codegen tycore) hs export(rpatBindL2ValBindL)
rpatBindL2ValBindL :: RCEEnv -> Bool -> HsName -> CTag -> MbPatRest -> AssocL RPatFld (Maybe Int) -> [ValBind]
rpatBindL2ValBindL env hasSub parNm ct rest pbL 
  = concat
    $  map  (\(RPatFld_Fld l o _ p,mbOff)
                -> let  b n = [mkValBind1 n tyInt (mkc n mbOff)]
                        pn  = parNm
                        pn' = hsnUniqifyEval pn
                        -- mkc n (Just o) = mkExprSatSelCase env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
                        mkc n (Just o) = acoreExprSatSelCaseTy env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
                        -- mkc n Nothing  = mkExprSelCase    env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
                        mkc n Nothing  = acoreSelCaseTy    env (Just (pn',ty pn')) (Expr_Var pn) ct n {- l -} o rest
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

%%[(8 codegen tycore) export(FldOffset(..),foffMkOff,foffLabel)
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
foffMkOff FldImplicitOffset      o = (o,acoreInt o)
foffMkOff (FldKnownOffset   _ o) _ = (o,acoreInt o)
foffMkOff (FldComputeOffset _ e) o = (o,e)

foffLabel :: FldOffset -> HsName
foffLabel FldImplicitOffset = hsnUnknown
foffLabel foff				= foffLabel' foff
%%]

%%[(8 codegen tycore) export(FieldSplitL,fsL2PatL,fsL2PatOffsetL)
type FieldSplitL = AssocL FldOffset RPat

fsL2PatL :: FieldSplitL -> [RPat]
fsL2PatL = assocLElts

fsL2PatOffsetL :: FieldSplitL -> AssocL RPatFld (Maybe Int)
fsL2PatOffsetL l = [ (RPatFld_Fld n oe (rpatNmNm $ rcpPNm p) p,Just oi) | (o,(foff,p)) <- zip [0..] l, let (oi,oe) = foffMkOff foff o, let n = foffLabel foff ]
%%]

-- Reordering compensates for the offset shift caused by predicate computation, which is predicate by predicate
-- whereas these sets of patterns are dealt with in one go.
%%[(8 codegen tycore) export(fsLReorder)
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
     in   rowCanonOrderBy compare fsL'
%%]

