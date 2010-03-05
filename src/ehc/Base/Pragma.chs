%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.Pragma}
%%]

%%[99 import(qualified Data.Map as Map,Data.List)
%%]
%%[99 import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[99 import({%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[doesWhat doclatex
Internal representation of pragmas.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pragmas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(Pragma(..))
data Pragma
  = Pragma_NoImplicitPrelude				-- no implicit prelude
  deriving (Eq,Ord,Enum,Show,Typeable,Data)
%%]

%%[99 export(allPragmaMp,showAllPragmas',showAllPragmas)
allPragmaMp :: Map.Map String Pragma
allPragmaMp
  = Map.fromList ts
  where ts
          = [ (drop prefixLen $ show t, t)
            | t <-
                  [ Pragma_NoImplicitPrelude
                  ]
            ]
        prefixLen = length "Pragma_"

showAllPragmas' :: String -> String
showAllPragmas'
  = showStringMapKeys allPragmaMp

showAllPragmas :: String
showAllPragmas
  = showAllPragmas' " "
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance Binary Pragma where
  put = putEnum8
  get = getEnum8

instance Serialize Pragma where
  sput = sputPlain
  sget = sgetPlain

%%]


