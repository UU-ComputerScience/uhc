%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization: ClassDefaultGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment for class defaults, introduced by @default@ declarations.
Each class can have multiple types as its default.
Currently only the first one is used.
%%]

%%[(9 hmtyinfer || hmtyast) module {%{EH}Gam.ClassDefaultGam}
%%]

%%[(9 hmtyinfer || hmtyast) hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[(9 hmtyinfer || hmtyast) import({%{EH}Gam},{%{EH}Ty},{%{EH}VarMp})
%%]
%%[(9 hmtyinfer || hmtyast) import({%{EH}Ty.FitsInCommon2},{%{EH}Ty.FitsIn})
%%]

%%[(9 hmtyinfer || hmtyast) import(Data.Maybe)
%%]

%%[(50 hmtyinfer || hmtyast) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class default gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast).AppSpineGam export(ClassDefaultGamInfo(..))
data ClassDefaultGamInfo
  = ClassDefaultGamInfo
      { cldiDefaultTypes	:: [Ty]
      }
%%[[50
      deriving (Data,Typeable)
%%]]
%%]

%%[(9 hmtyinfer || hmtyast).AppSpineGam export(ClassDefaultGam)
type ClassDefaultGam = Gam HsName ClassDefaultGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Find a defaulting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(clDfGamLookupDefault)
-- | Lookup a matching default for a predicate
clDfGamLookupDefault
  :: ( VarLookup gm LabelVarId VarMpInfo
     -- , VarLookup gm Ty VarMpInfo
     , VarLookupCmb VarMp gm
     )
     => FIIn' gm -> Pred -> ClassDefaultGam
     -> Maybe VarMp
clDfGamLookupDefault fi pr clDfGam
  = case pr of
      Pred_Class t | isJust mbConArgs
        -> do (ClassDefaultGamInfo {cldiDefaultTypes = (tg:_)}) <- gamLookup nm clDfGam
              (_,tyVarMp) <- fitPredIntoPred fi pr (Pred_Class $ mk1ConApp nm tg)
              return tyVarMp
        where mbConArgs@(~(Just (nm,args))) = tyMbAppConArgs t
      _ -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer || hmtyast)
instance Serialize ClassDefaultGamInfo where
  sput (ClassDefaultGamInfo a) = sput a
  sget = liftM ClassDefaultGamInfo sget
%%]

