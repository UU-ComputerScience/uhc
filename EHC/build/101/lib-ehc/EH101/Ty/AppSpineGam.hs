module EH101.Ty.AppSpineGam
( module EH101.Gam.AppSpineGam
, asGamLookup
, mkAppSpineGam
, asFOUpdCoe )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts
import EH101.Ty
import EH101.Gam
import EH101.Gam.AppSpineGam
import EH101.Gam.AppSpineGam
import EH101.Ty.FitsInCommon
import EH101.Ty.FIEnv
import EH.Util.Utils
import EH101.AbstractCore
import EH101.Core.Subst



{-# LINE 48 "src/ehc/Ty/AppSpineGam.chs" #-}
arrowAppSpineVertebraeInfoL :: FIEnv -> [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL env
  = [ AppSpineVertebraeInfo polContravariant fioMkStrong
          asFODflt
          (Just dfltFOUpdCoe)
    , AppSpineVertebraeInfo polCovariant id
          asFOArrow
          (Just (\opts [ffo,afo]
                  -> let (u',u1) = mkNewUID (foUniq afo)
                         -- c = lrcoeForLamTyApp opts u1 (foCSubst afo) (foLRCoe ffo) (foLRCoe afo)
                         (c,s) = lrcoeForLamTyAppAsSubst opts u1 (foLRCoe ffo) (foLRCoe afo)
                     in  afo { foUniq = u'
                             , foLRCoe = c
                             , foCSubst = foCSubst afo `cSubstApp` s
                             }
          )     )
    ]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL
  = repeat
    $ AppSpineVertebraeInfo polCovariant id asFODflt
          (Just dfltFOUpdCoe)

{-# LINE 98 "src/ehc/Ty/AppSpineGam.chs" #-}
dfltFOUpdCoe :: AppSpineFOUpdCoe
dfltFOUpdCoe _ x = last' (panic "Ty.AppSpineGam.dfltFOUpdCoe") x

asFOUpdCoe :: AppSpineVertebraeInfo -> AppSpineFOUpdCoe
asFOUpdCoe = maybe dfltFOUpdCoe id . asMbFOUpdCoe

{-# LINE 106 "src/ehc/Ty/AppSpineGam.chs" #-}
asFODflt :: FIOut -> FIOut -> FIOut
asFODflt _ afo = afo

{-# LINE 111 "src/ehc/Ty/AppSpineGam.chs" #-}
asFOArrow :: FIOut -> FIOut -> FIOut
asFOArrow _ afo = afo {foLInstToL = InstTo_Plain : foLInstToL afo, foRInstToL = InstTo_Plain : foRInstToL afo}

{-# LINE 120 "src/ehc/Ty/AppSpineGam.chs" #-}
asGamLookup :: HsName -> AppSpineGam -> Maybe AppSpineInfo
asGamLookup nm g
  = case gamLookup nm g of
      j@(Just _)
        -> j
      Nothing | hsnIsProd nm
        -> Just $ emptyAppSpineInfo {asgiVertebraeL = take (hsnProdArity nm) prodAppSpineVertebraeInfoL}
      _ -> Nothing


{-# LINE 137 "src/ehc/Ty/AppSpineGam.chs" #-}
mkAppSpineGam ::  FIEnv -> AppSpineGam
mkAppSpineGam env
  = assocLToGam
      [ (hsnArrow       , emptyAppSpineInfo {asgiVertebraeL = arrowAppSpineVertebraeInfoL env})
      , (hsnRec         , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
      , (hsnRecUnboxed  , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
      ]

