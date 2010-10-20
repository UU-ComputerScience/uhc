%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.Pragma}
%%]

%%[99 import(qualified Data.Map as Map,Data.List)
%%]
%%[99 import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[99 import({%{EH}Base.HsName},{%{EH}Base.Target})
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
  | Pragma_NoGenericDeriving                -- turn off generic deriving
  | Pragma_GenericDeriving                  -- turn on generic deriving (default)
  | Pragma_ExtensibleRecords                -- turn on extensible records
%%[[(99 codegen)
  | Pragma_ExcludeIfTarget
      { pragmaExcludeTargets   		:: [Target]
      }
%%]]
  deriving (Eq,Ord,Show,Typeable,Data)

%%]

%%[99 export(allSimplePragmaMp,showAllSimplePragmas',showAllSimplePragmas)
allSimplePragmaMp :: Map.Map String Pragma
allSimplePragmaMp
  = Map.fromList ts
  where ts
          = [ (drop prefixLen $ show t, t)
            | t <-
                  [ Pragma_NoImplicitPrelude
                  , Pragma_CPP
                  , Pragma_GenericDeriving
                  , Pragma_NoGenericDeriving
                  , Pragma_ExtensibleRecords
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
%%% Querying pragmas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) export(pragmaIsExcludeTarget)
pragmaIsExcludeTarget :: Target -> Pragma -> Bool
pragmaIsExcludeTarget t (Pragma_ExcludeIfTarget ts) = t `elem` ts
pragmaIsExcludeTarget _ _                           = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance Serialize Pragma where
  sput (Pragma_NoImplicitPrelude        ) = sputWord8 0
  sput (Pragma_CPP                      ) = sputWord8 1
  sput (Pragma_Derivable  a b c         ) = sputWord8 2 >> sput a >> sput b >> sput c
  sput (Pragma_NoGenericDeriving        ) = sputWord8 3
  sput (Pragma_GenericDeriving          ) = sputWord8 4
  sput (Pragma_ExtensibleRecords        ) = sputWord8 5
%%[[(99 codegen)
  sput (Pragma_ExcludeIfTarget a        ) = sputWord8 6 >> sput a
%%]]
  sget = do t <- sgetWord8
            case t of
              0 -> return Pragma_NoImplicitPrelude
              1 -> return Pragma_CPP
              2 -> liftM3 Pragma_Derivable              sget sget sget
              3 -> return Pragma_NoGenericDeriving
              4 -> return Pragma_GenericDeriving
              5 -> return Pragma_ExtensibleRecords
%%[[(99 codegen)
              6 -> liftM  Pragma_ExcludeIfTarget        sget
%%]]

%%]

