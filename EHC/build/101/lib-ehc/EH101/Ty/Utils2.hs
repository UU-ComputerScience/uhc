module EH101.Ty.Utils2
( foAppLRCoeAsSubst )
where
import EH101.Base.Common
import EH101.Ty
import EH101.Base.Builtin
import EH101.Opts
import EH101.VarMp
import EH101.Ty.FitsInCommon
import EH101.AbstractCore
import EH101.Core
import EH101.Core.Subst



{-# LINE 26 "src/ehc/Ty/Utils2.chs" #-}
foAppLRCoeAsSubst :: EHCOpts -> UID -> FIOut -> VarMp -> CSubst -> CExpr -> (CExpr,CSubst)
foAppLRCoeAsSubst opts uniq fo c cs ce
  = (ce', foCSubst fo `cSubstApp` s1 `cSubstApp` s2)
  where (u',u1,u2) = mkNewLevUID2 uniq
        -- s0 = cs `cSubstApp` foCSubst fo
        (ww ,s1) = lrcoeWipeWeaveAsSubst opts u1 c (foLRCoe fo)
        (ce',s2) = coeEvalOnAsSubst u2 ww ce

