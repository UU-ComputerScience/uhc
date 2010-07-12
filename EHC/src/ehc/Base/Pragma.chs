%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.Pragma}
%%]

%%[99 import(qualified Data.Map as Map,Data.List)
%%]
%%[99 import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[99 import({%{EH}Base.HsName})
%%]
%%[99 import({%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]
%%[99 import(Control.Monad)
%%]

%%[doesWhat doclatex
Internal representation of pragmas.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pragmas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(Pragma(..))
data Pragma
  = Pragma_NoImplicitPrelude                -- no implicit prelude
  | Pragma_CPP                              -- preprocess with cpp
  | Pragma_Derivable                        -- generic derivability
      { pragmaDerivClassName        :: HsName       -- the class name for which
      , pragmaDerivFieldName        :: HsName       -- this field is derivable
      , pragmaDerivDefaultName      :: HsName       -- using this default value
      }
  deriving (Eq,Ord,Show,Typeable,Data)

%%]
instance Show Pragma where
  show Pragma_NoImplicitPrelude = "NoImplicitPrelude"
  show Pragma_CPP               = "CPP"
  show (Pragma_Derivable _ _ _) = "Derivable"

%%[99 export(allSimplePragmaMp,showAllSimplePragmas',showAllSimplePragmas)
allSimplePragmaMp :: Map.Map String Pragma
allSimplePragmaMp
  = Map.fromList ts
  where ts
          = [ (drop prefixLen $ show t, t)
            | t <-
                  [ Pragma_NoImplicitPrelude
                  , Pragma_CPP
                  -- , Pragma_Derivable undefined undefined undefined
                  ]
            ]
        prefixLen = length "Pragma_"

showAllSimplePragmas' :: String -> String
showAllSimplePragmas'
  = showStringMapKeys allSimplePragmaMp

showAllSimplePragmas :: String
showAllSimplePragmas
  = showAllSimplePragmas' " "
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance Serialize Pragma where
  sput (Pragma_NoImplicitPrelude   ) = sputWord8 0
  sput (Pragma_CPP                 ) = sputWord8 1
  sput (Pragma_Derivable  a b c    ) = sputWord8 2 >> sput a >> sput b >> sput c
  sget = do t <- sgetWord8
            case t of
              0 -> return Pragma_NoImplicitPrelude
              1 -> return Pragma_CPP
              2 -> liftM3 Pragma_Derivable          sget sget sget

%%]

