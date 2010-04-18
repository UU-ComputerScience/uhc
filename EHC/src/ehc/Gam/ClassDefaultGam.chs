%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization: ClassDefaultGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment for class defaults, introduced by @default@ declarations.
Each class can have multiple types as its default.
Currently only the first one is used.
%%]

%%[9 module {%{EH}Gam.ClassDefaultGam}
%%]

%%[9 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[(9 hmtyinfer) import({%{EH}Gam},{%{EH}Ty})
%%]

%%[(20 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% App spine gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer).AppSpineGam export(ClassDefaultGamInfo(..))
data ClassDefaultGamInfo
  = ClassDefaultGamInfo
      { cldiDefaultTypes	:: [Ty]
      }
%%[[20
      deriving (Data,Typeable)
%%]]
%%]

%%[(9 hmtyinfer).AppSpineGam export(ClassDefaultGam)
type ClassDefaultGam = Gam HsName ClassDefaultGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 hmtyinfer || hmtyast)
instance Serialize ClassDefaultGamInfo where
  sput (ClassDefaultGamInfo a) = sput a
  sget = liftM ClassDefaultGamInfo sget
%%]

