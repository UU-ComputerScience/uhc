%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PP configuration and varieties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

1. Tailoring PP implementations

For PP of
- HsName
- HsName used as a constructor name
- Variable of some sort
- UID

and configuring
- whether to follow an underlying AST structure

2. Different uses of PP

As class variations on PP

%%[8 hs module {%{EH}Base.CfgPP}
%%]

%%[8 import({%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Opts.Base},{%{EH}Base.HsName.Builtin},{%{EH}Scanner.Common})
%%]

%%[8 import(Data.Char,qualified Data.Set as Set)
%%]

%%[8 import(UHC.Util.Pretty)
%%]
%%[8 import(UHC.Util.ScanUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ppScanoptsNm)
-- | Prettyprint 'HsName' with scanopts, taking care of properly escaping based on scan info (used when parsing)
ppScanoptsNm :: ScanOpts -> HsName -> PP_Doc
ppScanoptsNm copts n = fst $ ppHsnEscapeWith '$' (hsnOkChars '$' $ copts) (hsnNotOkStrs copts) (`Set.member` leaveAsIs) n
    where leaveAsIs = Set.fromList [hsnRowEmpty]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CfgPP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(CfgPP(..))
class CfgPP x where
  cfgppHsName 		:: x -> HsName -> PP_Doc
  cfgppConHsName 	:: x -> HsName -> PP_Doc
  cfgppUID    		:: x -> UID    -> PP_Doc
  cfgppVarHsName 	:: x -> Maybe HsName -> Maybe UID -> Maybe Int -> Maybe PP_Doc -> PP_Doc
  cfgppVarHsNameFallback
  					:: x -> Maybe HsName -> Maybe UID -> Maybe Int -> Maybe PP_Doc -> PP_Doc
  cfgppFollowAST    :: x -> Bool
  cfgppTyPPVarDflt 	:: x -> String -> UID -> Maybe PP_Doc -> PP_Doc

  cfgppHsName    _              			= pp
  cfgppConHsName _              			= ppCon
  cfgppUID       _              			= pp
  cfgppVarHsName x mn mu mi mp				= cfgppVarHsNameFallback x mn mu mi mp
  cfgppVarHsNameFallback x _ _ _ (Just p)  	= p
  cfgppVarHsNameFallback x _ _ (Just i) _ 	= cfgppHsName x $ mkHNm $ tnUniqRepr i
  cfgppVarHsNameFallback x (Just n) _ _ _ 	= cfgppHsName x n
  cfgppVarHsNameFallback x _ (Just u) _ _ 	= cfgppUID x u
  cfgppFollowAST _              			= False
  cfgppTyPPVarDflt							= \x pre tv mbpp -> cfgppVarHsName x (Just $ mkHNm $ pre ++ "_" ++ show tv) (Just tv) Nothing mbpp
%%]

%%[8 export(CfgPP_Plain(..),CfgPP_Core(..),CfgPP_Grin(..),CfgPP_TyCore(..))
data CfgPP_Plain   = CfgPP_Plain
data CfgPP_Core    = CfgPP_Core
data CfgPP_TyCore  = CfgPP_TyCore
data CfgPP_Grin    = CfgPP_Grin
%%]

%%[8
instance CfgPP CfgPP_Plain
%%]

%%[8
instance CfgPP CfgPP_Core where
  {-
  cfgppHsName    _ n 				= fst $ ppHsnEscapeWith '$' (hsnOkChars '$' $ copts) (hsnNotOkStrs copts) (`Set.member` leaveAsIs) n
    where copts = coreScanOpts emptyEHCOpts
          leaveAsIs = Set.fromList [hsnRowEmpty]
  -}
  cfgppHsName    _ n 				= ppScanoptsNm (coreScanOpts emptyEHCOpts) n
  cfgppConHsName     				= cfgppHsName
  cfgppFollowAST     				= const True
  cfgppUID _       u 				= ppUIDParseable u
  cfgppVarHsName x _ (Just u) _ _ 	= cfgppUID x u
  cfgppVarHsName x mn mu mi mp      = cfgppVarHsNameFallback x mn mu mi mp
%%]

%%[8
ppNmTyCore = ppHsnEscaped (Right $ Set.fromList ['0'..'9']) '$' (hsnEscapeeChars '$' tycoreScanOpts)

instance CfgPP CfgPP_TyCore where
  cfgppHsName    _ = ppNmTyCore
%%]

%%[8
instance CfgPP CfgPP_Grin where
  cfgppHsName    _ = ppHsnNonAlpha grinScanOpts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Print HsName as for Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ppCoreNm)
ppCoreNm :: HsName -> PP_Doc
ppCoreNm = cfgppHsName CfgPP_Core
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Support utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
tnUniqRepr :: Int -> String
tnUniqRepr
  = lrepr
  where lrepr i = if i <= 26
                  then  [repr i]
                  else  let  (d,r) = i `divMod` 26
                        in   (repr d : lrepr r)
        repr    = (chr . (97+))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pp's which should not be here...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.ppCTag export(ppCTag', ppCTagExtensive')
-- intended for parsing
ppCTag' :: CfgPP x => x -> CTag -> PP_Doc
ppCTag' x t
  = case t of
      CTagRec                      -> ppCurly "Rec"
      CTag ty nm tag arity mxarity -> ppCurlysCommas' [ppNm ty, ppNm nm, pp tag {- , pp arity, pp mxarity -}]
  where ppNm n = cfgppHsName x n

-- intended for parsing
ppCTagExtensive' :: CfgPP x => x -> CTag -> PP_Doc
ppCTagExtensive' x t
  = case t of
      CTagRec                      -> ppCurly "Rec"
      CTag ty nm tag arity mxarity -> ppCurlysCommas' [ppNm ty, ppNm nm, pp tag, pp arity, pp mxarity]
  where ppNm n = cfgppHsName x n
%%]

%%[8 export(ppCTagsMp)
ppCTagsMp :: CfgPP x => x -> CTagsMp -> PP_Doc
ppCTagsMp x
  = mkl (mkl (ppCTag' x))
  where mkl pe = ppCurlysSemisBlock . map (\(n,e) -> cfgppHsName x n >-< indent 1 ("=" >#< pe e))
%%]

