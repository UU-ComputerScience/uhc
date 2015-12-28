%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code generation target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}Base.Pragma}
%%]

%%[99 import(qualified Data.Map as Map,Data.List)
%%]
%%[99 import(UHC.Util.Pretty,UHC.Util.Utils)
%%]
%%[99 import({%{EH}Base.HsName},{%{EH}Base.Target})
%%]
%%[99 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]
%%[99 import(Control.Monad)
%%]
%%[99 import({%{EH}Opts.CommandLine})
%%]

%%[doesWhat doclatex
Internal representation of pragmas.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pragmas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(Pragma(..))
-- | Data type representing pragmas.
--   The names of the constructors must begin with "Pragma_", the string used in the map allSimplePragmaMp.
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
  | Pragma_NoBangPatterns               	-- turn off bang patterns
  | Pragma_BangPatterns                  	-- turn on bang patterns (default)
  | Pragma_NoPolyKinds               		-- turn off polymorphic kinds (default)
  | Pragma_PolyKinds                  		-- turn on
  | Pragma_NoOverloadedStrings         		-- turn off overloaded strings (default)
  | Pragma_OverloadedStrings           		-- turn on
  | Pragma_ExtensibleRecords                -- turn on extensible records
  | Pragma_Fusion               			-- turn on fusion syntax
  | Pragma_OptionsUHC              			-- commandline options
      { pragmaOptions   			:: String
      }
%%[[99
  | Pragma_ExcludeIfTarget
      { pragmaExcludeTargets   		:: [Target]
      }
%%]]
  deriving (Eq,Ord,Show,Typeable,Generic)

%%]

%%[99 export(allSimplePragmaMp,showAllSimplePragmas',showAllSimplePragmas)
-- | All pragmas not requiring further arguments, these are accepted as LANGUAGE pragma when included in this map.
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
                  , Pragma_BangPatterns
                  , Pragma_NoBangPatterns
                  , Pragma_NoPolyKinds         
                  , Pragma_PolyKinds           
                  , Pragma_NoOverloadedStrings 
                  , Pragma_OverloadedStrings   
                  , Pragma_ExtensibleRecords
                  , Pragma_Fusion
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

%%[99 export(pragmaInvolvesCmdLine)
pragmaInvolvesCmdLine :: Pragma -> Bool
pragmaInvolvesCmdLine (Pragma_CPP         ) = True
pragmaInvolvesCmdLine (Pragma_OptionsUHC _) = True
pragmaInvolvesCmdLine _                     = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance Serialize Pragma
%%]

%%[9999
instance Serialize Pragma where
  sput (Pragma_NoImplicitPrelude        ) = sputWord8 0
  sput (Pragma_CPP                      ) = sputWord8 1
  sput (Pragma_Derivable  a b c         ) = sputWord8 2 >> sput a >> sput b >> sput c
  sput (Pragma_NoGenericDeriving        ) = sputWord8 3
  sput (Pragma_GenericDeriving          ) = sputWord8 4
  sput (Pragma_ExtensibleRecords        ) = sputWord8 5
  sput (Pragma_ExcludeIfTarget a        ) = sputWord8 6 >> sput a
  sput (Pragma_Fusion        			) = sputWord8 7
  sput (Pragma_NoBangPatterns           ) = sputWord8 8
  sput (Pragma_BangPatterns             ) = sputWord8 9
  sput (Pragma_OptionsUHC      a        ) = sputWord8 10 >> sput a
  sput (Pragma_NoPolyKinds       		) = sputWord8 11
  sput (Pragma_PolyKinds         		) = sputWord8 12
  sput (Pragma_NoOverloadedStrings		) = sputWord8 13
  sput (Pragma_OverloadedStrings 		) = sputWord8 14
  sget = do t <- sgetWord8
            case t of
              0  -> return Pragma_NoImplicitPrelude
              1  -> return Pragma_CPP
              2  -> liftM3 Pragma_Derivable              sget sget sget
              3  -> return Pragma_NoGenericDeriving
              4  -> return Pragma_GenericDeriving
              5  -> return Pragma_ExtensibleRecords
%%[[(99 codegen)
              6  -> liftM  Pragma_ExcludeIfTarget        sget
%%]]
              7  -> return Pragma_Fusion
              8  -> return Pragma_NoBangPatterns
              9  -> return Pragma_BangPatterns
              10 -> liftM  Pragma_OptionsUHC             sget
              11 -> return Pragma_NoPolyKinds       
              12 -> return Pragma_PolyKinds         
              13 -> return Pragma_NoOverloadedStrings
              14 -> return Pragma_OverloadedStrings 

%%]

