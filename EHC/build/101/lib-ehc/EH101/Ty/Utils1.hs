module EH101.Ty.Utils1
( ppTyS
, tyTopLevelMain )
where
import EH101.Base.Common
import EH101.Substitutable
import EH101.VarMp
import EH101.Ty
import EH101.Ty.Pretty
import EH.Util.Pretty
import EH101.Base.Builtin
import EH101.Opts



{-# LINE 19 "src/ehc/Ty/Utils1.chs" #-}
ppTyS :: VarUpdatable Ty m => m -> Ty -> PP_Doc
ppTyS = ppS ppTy

{-# LINE 24 "src/ehc/Ty/Utils1.chs" #-}
tyTopLevelMain :: EHCOpts -> TyVarId -> Ty
tyTopLevelMain opts uniq = mk1ConApp (ehcOptBuiltin opts ehbnIO) (mkTyVar uniq)

