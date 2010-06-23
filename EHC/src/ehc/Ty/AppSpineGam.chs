%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 module {%{EH}Ty.AppSpineGam} import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts})
%%]
%%[4 import({%{EH}Ty},{%{EH}Gam},{%{EH}Gam.AppSpineGam})
%%]
%%[4 import({%{EH}Gam.AppSpineGam}) export(module {%{EH}Gam.AppSpineGam})
%%]
%%[4 import({%{EH}Ty.FitsInCommon},{%{EH}Ty.FIEnv})
%%]
%%[(8 codegen hmtyinfer) import(qualified {%{EH}TyCore.Full0} as C)
%%]
%%[(9 codegen hmtyinfer) import({%{EH}Core.Subst})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Vertebrae info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).vertebraeInfoL export(arrowAppSpineVertebraeInfoL, prodAppSpineVertebraeInfoL)
arrowAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL
  = [ AppSpineVertebraeInfo
        polContravariant fioMkStrong
        asFODflt
    , AppSpineVertebraeInfo
        polCovariant id
%%[[4
        asFODflt
%%][(8 codegen)
        asFOArrow
%%]]
    ]

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL
  = repeat
    $ AppSpineVertebraeInfo
        polCovariant id
        asFODflt
%%]

%%[(8 hmtyinfer).vertebraeInfoL -4.vertebraeInfoL
arrowAppSpineVertebraeInfoL :: FIEnv -> [AppSpineVertebraeInfo]
arrowAppSpineVertebraeInfoL env
  = [ AppSpineVertebraeInfo polContravariant fioMkStrong
          asFODflt
%%[[(8 codegen)
          (Just dfltFOUpdCoe)
%%]]
    , AppSpineVertebraeInfo polCovariant id
          asFOArrow
%%[[(8 codegen)
          (Just (\opts [ffo,afo]
                  -> let (u',u1) = mkNewUID (foUniq afo)
%%[[9
                         -- c = lrcoeForLamTyApp opts u1 (foCSubst afo) (foLRCoe ffo) (foLRCoe afo)
                         (c,s) = lrcoeForLamTyAppAsSubst opts u1 (foLRCoe ffo) (foLRCoe afo)
%%]]
                         (tc,ts) = {- C.lrcoeForLamTyAppAsSubst -} fireqLRCoeForLamTyAppAsSubst fiReqs opts u1 (C.tyErr ("arrowAppSpineVertebraeInfoL: " ++ show u1)) (foLRTCoe ffo) (foLRTCoe afo)
                     in  afo { foUniq = u'
%%[[9
                             , foLRCoe = c
                             , foCSubst = foCSubst afo `cSubstApp` s
%%]]
                             , foLRTCoe = tc
                             , foTCSubst = fireqCSubstAppSubst fiReqs (foTCSubst afo) ts
                             }
          )     )
%%]]
    ]
  where fiReqs = feFIReqs env

prodAppSpineVertebraeInfoL :: [AppSpineVertebraeInfo]
prodAppSpineVertebraeInfoL
  = repeat
    $ AppSpineVertebraeInfo polCovariant id asFODflt
%%[[(8 codegen)
          (Just dfltFOUpdCoe)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion update
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen hmtyinfer) export(asFOUpdCoe)
dfltFOUpdCoe :: AppSpineFOUpdCoe
dfltFOUpdCoe _ x = last x

asFOUpdCoe :: AppSpineVertebraeInfo -> AppSpineFOUpdCoe
asFOUpdCoe = maybe dfltFOUpdCoe id . asMbFOUpdCoe
%%]

%%[(4 hmtyinfer)
asFODflt :: FIOut -> FIOut -> FIOut
asFODflt _ afo = afo
%%]

%%[(8 codegen hmtyinfer)
asFOArrow :: FIOut -> FIOut -> FIOut
asFOArrow _ afo = afo {foLInstToL = InstTo_Plain : foLInstToL afo, foRInstToL = InstTo_Plain : foRInstToL afo}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).AppSpineGam export(asGamLookup)
asGamLookup :: HsName -> AppSpineGam -> Maybe AppSpineInfo
asGamLookup nm g
  = case gamLookup nm g of
      j@(Just _)
        -> j
      Nothing | hsnIsProd nm
        -> Just $ emptyAppSpineInfo {asgiVertebraeL = take (hsnProdArity nm) prodAppSpineVertebraeInfoL}
      _ -> Nothing

%%]

%%[(4 hmtyinfer).appSpineGam export(mkAppSpineGam)
mkAppSpineGam :: FIEnv -> AppSpineGam
mkAppSpineGam _ =  assocLToGam [(hsnArrow, emptyAppSpineInfo {asgiVertebraeL = arrowAppSpineVertebraeInfoL})]
%%]

%%[(7 hmtyinfer).appSpineGam -4.appSpineGam export(mkAppSpineGam)
mkAppSpineGam ::  FIEnv -> AppSpineGam
mkAppSpineGam env
  = assocLToGam
      [ (hsnArrow       , emptyAppSpineInfo {asgiVertebraeL = arrowAppSpineVertebraeInfoL env})
      , (hsnRec         , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
%%[[18
      , (hsnRecUnboxed  , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
%%]]
      ]
%%]

