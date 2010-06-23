%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module GRINICommon import(System.Console.GetOpt,{%{EH}Base.Common}) export(GRIOpts(..), defaultGRIOpts, griCmdLineOpts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data GRIOpts    = GRIOptions    {  grioptHelp           ::  Bool
                                ,  grioptDebug          ::  Bool
                                ,  grioptSearchPath     ::  [String]
                                ,  grioptVerbosity      ::  Verbosity
                                }
%%]

%%[8
defaultGRIOpts  = GRIOptions    {  grioptHelp           =   False
                                ,  grioptDebug          =   False
                                ,  grioptSearchPath     =   []
                                ,  grioptVerbosity      =   VerboseQuiet
                                }
%%]

%%[8
griCmdLineOpts  
  =  [  Option "d"  ["debug"]         (NoArg oDebug)
          "include debug info, for now: dump extra info in ast pretty print"
     ,  Option "h"  ["help"]          (NoArg oHelp)
          "output this help"
     ,  Option "v"  ["verbose"]       (OptArg oVerbose "0|1|2")
          "be verbose, 0=quiet 1=normal 2=noisy, default=1"
     ]
  where  oHelp           o =  o { grioptHelp          = True    }
         oDebug          o =  o { grioptDebug         = True    }
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { grioptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { grioptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { grioptVerbosity     = VerboseALot        }
                                Nothing     -> o { grioptVerbosity     = VerboseNormal      }
                                _           -> o
%%]
