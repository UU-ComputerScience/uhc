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

%%[8 import({%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Opts.Base},{%{EH}Base.Builtin},{%{EH}Scanner.Common})
%%]

%%[8 import(Data.Char,qualified Data.Set as Set)
%%]

%%[8 import(EH.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CfgPP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(CfgPP(..))
class CfgPP x where
  cfgppHsName 		:: x -> HsName -> PP_Doc
  cfgppConHsName 	:: x -> HsName -> PP_Doc
  cfgppUID    		:: x -> UID    -> PP_Doc
  cfgppVarHsName 	:: x -> Maybe HsName -> Maybe UID -> Maybe Int -> PP_Doc
  cfgppFollowAST    :: x -> Bool

  cfgppHsName    _              = pp
  cfgppConHsName _              = ppCon
  cfgppUID       _              = pp
  cfgppVarHsName x _ _ (Just i) = cfgppHsName x $ mkHNm $ tnUniqRepr i
  cfgppVarHsName x (Just n) _ _ = cfgppHsName x n
  cfgppVarHsName x _ (Just u) _ = cfgppUID x u
  cfgppFollowAST _              = False
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
  cfgppHsName    _ = ppHsnNonAlpha coreScanOpts'
    where coreScanOpts' = coreScanOpts emptyEHCOpts
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

%%[8.ppCTag export(ppCTag')
-- intended for parsing
ppCTag' :: CfgPP x => x -> CTag -> PP_Doc
ppCTag' x t
  = case t of
      CTagRec                      -> ppCurly "Rec"
      CTag ty nm tag arity mxarity -> ppCurlysCommas' [ppNm ty,ppNm nm,pp tag, pp arity, pp mxarity]
  where ppNm n = cfgppHsName x n
%%]

%%[8 export(ppCTagsMp)
ppCTagsMp :: CfgPP x => x -> CTagsMp -> PP_Doc
ppCTagsMp x
  = mkl (mkl (ppCTag' x))
  where mkl pe = ppCurlysSemisBlock . map (\(n,e) -> cfgppHsName x n >-< indent 1 ("=" >#< pe e))
%%]

