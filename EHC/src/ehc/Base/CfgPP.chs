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

%%[8 import({%{EH}Base.Common},{%{EH}Base.HsName},{%{EH}Base.Builtin},{%{EH}Scanner.Common})
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

%%[8 export(CfgPP_Plain(..),CfgPP_HI(..),CfgPP_Core(..),CfgPP_Grin(..),CfgPP_TyCore(..))
data CfgPP_Plain   = CfgPP_Plain
data CfgPP_HI      = CfgPP_HI
data CfgPP_Core    = CfgPP_Core
data CfgPP_TyCore  = CfgPP_TyCore
data CfgPP_Grin    = CfgPP_Grin
%%]

%%[8
instance CfgPP CfgPP_Plain
%%]

%%[8
instance CfgPP CfgPP_HI where
  cfgppHsName   _ n
%%[[8
    = ppHsnNonAlpha hiScanOpts n
%%][20
    = case n of
        HsName_Pos i
          -> pp i
        _ -> ppHsnNonAlpha hiScanOpts n
%%]]
  cfgppConHsName x n            = {-  -} if n == hsnRowEmpty then hsnORow >#< hsnCRow else cfgppHsName x n
  cfgppVarHsName x _ (Just u) _ = cfgppUID x u
  cfgppUID       _ u            = "uid" >#< ppUID' u
  cfgppFollowAST _              = True
%%]

%%[8
instance CfgPP CfgPP_Core where
  cfgppHsName    _ = ppHsnNonAlpha coreScanOpts
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

%%[8_2 -8.ppCTag export(ppCTag', ppCTag'')

ppCTag'' :: CfgPP x => x -> CTag -> PP_Doc
ppCTag'' x t =
  case t of
    CTagRec         -> ppCurly "Rec"
    CTag _ nm _ _ _ -> pp nm 

-- intended for parsing
ppCTag' :: CfgPP x => x -> CTag -> PP_Doc
ppCTag' x t =
  case t of
    CTagRec                      -> ppCurly "Rec"
    CTag ty nm tag arity mxarity -> pp nm >#< "::" >#< pp ty
%%]

%%[8 export(ppCTagsMp)
ppCTagsMp :: CfgPP x => x -> CTagsMp -> PP_Doc
ppCTagsMp x
  = mkl (mkl (ppCTag' x))
  where mkl pe = ppCurlysSemisBlock . map (\(n,e) -> cfgppHsName x n >-< indent 1 ("=" >#< pe e))
%%]

%%[2020
instance PPForHI CTag where
  ppForHI = ppCTag' CfgPP_HI
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PP variants for: HI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2020 export(PPForHI(..))
class (PP x) => PPForHI x where
  ppForHI :: x -> PP_Doc

  ppForHI = pp
%%]

%%[2020
instance PPForHI UID where
  ppForHI = cfgppUID CfgPP_HI

instance PPForHI HsName where
  ppForHI = cfgppHsName CfgPP_HI

instance PPForHI Int

instance PPForHI String where
  ppForHI = pp . show

instance PPForHI a => PPForHI (AlwaysEq a) where
  ppForHI (AlwaysEq x) = ppForHI x
%%]

%%[2020
instance PPForHI VarUIDHsName where
  ppForHI (VarUIDHs_Name i n) = "varuidnmname" >#< ppCurlysCommasBlock [ppForHI i, ppForHI n]
  ppForHI (VarUIDHs_UID  i  ) = "varuidnmuid"  >#< ppForHI i
  ppForHI (VarUIDHs_Var  i  ) = "varuidnmvar"  >#< ppForHI i
%%]

