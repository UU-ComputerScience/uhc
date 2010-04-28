%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam.FixityGam}
%%]

%%[1 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[1 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[1 hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[1 hs import ({%{EH}Gam})
%%]
%%[1 hs import({%{EH}Error}) 
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(3 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixity gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(FixityGam, FixityGamInfo(..), defaultFixityGamInfo)
data FixityGamInfo = FixityGamInfo { fgiPrio :: !Int, fgiFixity :: !Fixity } deriving Show

defaultFixityGamInfo = FixityGamInfo fixityMaxPrio Fixity_Infixl

type FixityGam = Gam HsName FixityGamInfo
%%]

%%[20
deriving instance Typeable FixityGamInfo
deriving instance Data FixityGamInfo

%%]

%%[1 export(fixityGamLookup)
fixityGamLookup :: HsName -> FixityGam -> FixityGamInfo
fixityGamLookup nm fg = maybe defaultFixityGamInfo id $ gamLookup nm fg
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999
instance ForceEval FixityGamInfo
%%[[102
  where
    fevCount (FixityGamInfo p f) = cm1 "FixityGamInfo" `cmUnion` fevCount p `cmUnion` fevCount f
%%]]
%%]

%%[20
instance Serialize FixityGamInfo where
  sput (FixityGamInfo a b) = sput a >> sput b
  sget = liftM2 FixityGamInfo sget sget
%%]
