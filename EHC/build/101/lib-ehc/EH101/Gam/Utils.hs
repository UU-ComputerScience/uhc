module EH101.Gam.Utils
( mentrelFilterMpExtendViaValGam )
where
import Data.List
import EH101.Base.Common
import EH101.Opts
import EH101.Ty
import EH101.Ty.FitsInCommon
import EH101.Ty.FitsIn
import EH101.Error
import EH101.Gam
import EH101.VarMp
import EH101.Substitutable
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Ty.UsedNames
import EH101.Module
import EH101.Gam.ValGam


{-# LINE 66 "src/ehc/Gam/Utils.chs" #-}
-- | Lookup indirectly used identifiers, type constants from types of value bindings.
mentrelFilterMpExtendViaValGam :: HsName -> ValGam -> ModEntRelFilterMp -> ModEntRelFilterMp
mentrelFilterMpExtendViaValGam moduleNm valGam mentrelFilterMp
  = mentrelFilterMpUnions
      (   [ mentrelFilterMp ]
       ++ [ maybe Map.empty (tyUsedNames moduleNm . vgiTy) $ valGamLookup n valGam
          | n <- Set.toList $ Map.findWithDefault Set.empty IdOcc_Val mentrelFilterMp
          ]
      )

