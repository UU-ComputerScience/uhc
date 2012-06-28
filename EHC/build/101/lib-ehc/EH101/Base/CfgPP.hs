module EH101.Base.CfgPP
( CfgPP (..)
, CfgPP_Plain (..), CfgPP_Core (..), CfgPP_Grin (..), CfgPP_TyCore (..)
, ppCTag'
, ppCTagsMp )
where
import EH101.Base.Common
import EH101.Base.HsName
import EH101.Opts.Base
import EH101.Base.Builtin
import EH101.Scanner.Common
import Data.Char
import qualified Data.Set as Set
import EH.Util.Pretty

{-# LINE 37 "src/ehc/Base/CfgPP.chs" #-}
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

{-# LINE 54 "src/ehc/Base/CfgPP.chs" #-}
data CfgPP_Plain   = CfgPP_Plain
data CfgPP_Core    = CfgPP_Core
data CfgPP_TyCore  = CfgPP_TyCore
data CfgPP_Grin    = CfgPP_Grin

{-# LINE 61 "src/ehc/Base/CfgPP.chs" #-}
instance CfgPP CfgPP_Plain

{-# LINE 65 "src/ehc/Base/CfgPP.chs" #-}
instance CfgPP CfgPP_Core where
  cfgppHsName    _ = ppHsnNonAlpha coreScanOpts'
    where coreScanOpts' = coreScanOpts emptyEHCOpts

{-# LINE 71 "src/ehc/Base/CfgPP.chs" #-}
ppNmTyCore = ppHsnEscaped (Right $ Set.fromList ['0'..'9']) '$' (hsnEscapeeChars '$' tycoreScanOpts)

instance CfgPP CfgPP_TyCore where
  cfgppHsName    _ = ppNmTyCore

{-# LINE 78 "src/ehc/Base/CfgPP.chs" #-}
instance CfgPP CfgPP_Grin where
  cfgppHsName    _ = ppHsnNonAlpha grinScanOpts

{-# LINE 87 "src/ehc/Base/CfgPP.chs" #-}
tnUniqRepr :: Int -> String
tnUniqRepr
  = lrepr
  where lrepr i = if i <= 26
                  then  [repr i]
                  else  let  (d,r) = i `divMod` 26
                        in   (repr d : lrepr r)
        repr    = (chr . (97+))

{-# LINE 102 "src/ehc/Base/CfgPP.chs" #-}
-- intended for parsing
ppCTag' :: CfgPP x => x -> CTag -> PP_Doc
ppCTag' x t
  = case t of
      CTagRec                      -> ppCurly "Rec"
      CTag ty nm tag arity mxarity -> ppCurlysCommas' [ppNm ty,ppNm nm,pp tag, pp arity, pp mxarity]
  where ppNm n = cfgppHsName x n

{-# LINE 112 "src/ehc/Base/CfgPP.chs" #-}
ppCTagsMp :: CfgPP x => x -> CTagsMp -> PP_Doc
ppCTagsMp x
  = mkl (mkl (ppCTag' x))
  where mkl pe = ppCurlysSemisBlock . map (\(n,e) -> cfgppHsName x n >-< indent 1 ("=" >#< pe e))

