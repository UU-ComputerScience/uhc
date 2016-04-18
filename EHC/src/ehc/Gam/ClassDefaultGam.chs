%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization: ClassDefaultGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment for class defaults, introduced by @default@ declarations.
Each class can have multiple types as its default.
Currently only the first one is used.
%%]

%%[(9 hmtyinfer) module {%{EH}Gam.ClassDefaultGam}
%%]

%%[(9 hmtyinfer) hs import ({%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Base.HsName.Builtin})
%%]
%%[(9 hmtyinfer) import({%{EH}Gam},{%{EH}Ty},{%{EH}VarMp})
%%]
%%[(9 hmtyinfer) import(UHC.Util.Substitutable)
%%]
%%[(9 hmtyinfer) import({%{EH}Ty.FitsInCommon2},{%{EH}Ty.FitsIn})
%%]

%%[(9 hmtyinfer) import(Data.Maybe)
%%]

%%[(50 hmtyinfer) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class default gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer).AppSpineGam export(ClassDefaultGamInfo(..))
-- If this changes, also change {%{EH}ConfigInternalVersions}
data ClassDefaultGamInfo
  = ClassDefaultGamInfo
      { cldiDefaultTypes	:: [Ty]
      }
%%[[50
      deriving (Typeable)
%%]]
%%]

%%[(9 hmtyinfer).AppSpineGam export(ClassDefaultGam)
type ClassDefaultGam = Gam HsName ClassDefaultGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Find a defaulting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(clDfGamLookupDefault)
-- | Lookup a matching default for a predicate
clDfGamLookupDefault
  :: ( VarLookup gm
     , VarLookupCmb VarMp gm
     , VarLookupKey gm ~ VarId, VarLookupVal gm ~ VarMpInfo
     )
     => FIIn' gm -> Pred -> ClassDefaultGam
     -> Maybe VarMp
clDfGamLookupDefault fi pr clDfGam
  = case pr of
      Pred_Class t | isJust mbConArgs
        -> do (ClassDefaultGamInfo {cldiDefaultTypes = (tg:_)}) <- gamLookup nm clDfGam
              (_,tyVarMp) <- fitPredIntoPred fi pr (Pred_Class $ appCon1App nm tg)
              return tyVarMp
        where mbConArgs@(~(Just (nm,args))) = appMbConApp t
      _ -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer)
instance Serialize ClassDefaultGamInfo where
  sput (ClassDefaultGamInfo a) = sput a
  sget = liftM ClassDefaultGamInfo sget
%%]

