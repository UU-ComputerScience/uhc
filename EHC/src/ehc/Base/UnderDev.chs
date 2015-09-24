%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Under development config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.UnderDev}
%%]

%%[1 import(UHC.Util.Utils)
%%]

%%[1 import(GHC.Generics(Generic), Data.Typeable)
%%]

%%[8888 import(Control.Monad, Control.Monad.IO.Class)
%%]

%%[1 import(qualified Data.Map as Map)
%%]

%%[8888 import(Data.Sequence((><))) export ((><))
%%]

%%[1 import({%{EH}Base.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Under development flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(UnderDev(..), allUnderDevMp)
-- | Topics under development
data UnderDev
  = UnderDev_Anon					-- something under development not further specified
  | UnderDev_NameAnalysis			-- alternatate (more accurate) name dependency analysis, required for named instances
  deriving (Eq,Ord,Enum,Typeable,Bounded,Generic)

instance DataAndConName UnderDev

instance Show UnderDev where
  show = strToLower . showUnprefixed 1

allUnderDevMp :: Map.Map String UnderDev
allUnderDevMp = str2stMp
%%]

