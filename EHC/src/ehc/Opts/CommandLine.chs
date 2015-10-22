%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options for the commandline of shell/unix commands/tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Opts.CommandLine}
%%]

%%[8 import(Data.List)
%%]

%%[8 import(Data.Typeable(Typeable), Data.Generics(Data))
%%]

%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]
%%[50 import(Control.Monad)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Commandline options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(CmdFlag(..),Cmd(..),CmdLineOpt(..),CmdLineOpts)
data CmdFlag
  = CmdFlag_Define          String      (Maybe String)
  | CmdFlag_Undefine        String
  | CmdFlag_Flag            String
  | CmdFlag_KeyEqualsVal    String      String
  | CmdFlag_KeyWithVal      String      String
  | CmdFlag_IncludeDir      String
  | CmdFlag_Lib      		String
  
  -- (plain) args
  | CmdFlag_Arg      		String
  
  -- modifiers
  | CmdFlag_ModfMin      	CmdFlag		-- double '-'
  deriving (Eq,Typeable)

data Cmd
  = Cmd_CPP_Preprocessing
  | Cmd_CPP
  | Cmd_C
  deriving (Eq,Ord,Enum,Bounded,Typeable)

data CmdLineOpt
  = CmdLineOpt
      { cloptForCmd     :: Cmd
      , cloptFlag       :: CmdFlag
      }
  deriving (Eq,Typeable)

type CmdLineOpts = [CmdLineOpt]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cppOpt,cppOptF,cppOptI,cppArg)
cppOpt :: CmdFlag -> CmdLineOpt
cppOpt = CmdLineOpt Cmd_CPP

cppArg :: String -> CmdLineOpt
cppArg = cppOpt . CmdFlag_Arg

cppOptF :: String -> CmdLineOpt
cppOptF = cppOpt . CmdFlag_Flag

cppOptI :: String -> CmdLineOpt
cppOptI = cppOpt . CmdFlag_IncludeDir
%%]

%%[8 export(gccOpt,gccArg,gccOptF,gccOptOutput,gccOptLib)
gccOpt :: CmdFlag -> CmdLineOpt
gccOpt = CmdLineOpt Cmd_C

gccArg :: String -> CmdLineOpt
gccArg = gccOpt . CmdFlag_Arg

gccOptF :: String -> CmdLineOpt
gccOptF = gccOpt . CmdFlag_Flag

gccOptOutput :: String -> CmdLineOpt
gccOptOutput = gccOpt . CmdFlag_KeyWithVal "o"

gccOptLib :: String -> CmdLineOpt
gccOptLib = gccOpt . CmdFlag_Lib
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unparsing/showing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(showCmdLineOpts,showCmdLineOpts')
showCmdLineOpts' :: [Cmd] -> CmdLineOpts -> [String]
showCmdLineOpts' forCmds opts = map show $ filter (\o -> cloptForCmd o `elem` forCmds) opts

showCmdLineOpts :: CmdLineOpts -> String
showCmdLineOpts = concat . intersperse " " . showCmdLineOpts' [minBound::Cmd .. maxBound]

-- | Show key + value
kv :: String -> String -> Maybe String -> String
kv k sep mv = k ++ maybe "" (\v -> sep ++ v) mv

instance Show CmdFlag where
  show (CmdFlag_Define      	k mv)       = "-D"  ++ kv k "=" mv
  show (CmdFlag_Undefine    	k   )       = "-U"  ++ k
  show (CmdFlag_Flag        	f   )       = "-"   ++ f
  show (CmdFlag_KeyEqualsVal 	k  v)       = "-"   ++ kv k "=" (Just v)
  show (CmdFlag_KeyWithVal 		k  v)       = "-"   ++ kv k " " (Just v)
  show (CmdFlag_IncludeDir  	d   )       = "-I"  ++ d
  show (CmdFlag_Lib    			l   )       = "-l"  ++ l
  show (CmdFlag_Arg    			a   )       =          a
  show (CmdFlag_ModfMin  		f   )       = "-"   ++ show f

instance Show CmdLineOpt where
  show = show . cloptFlag
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(parseCmdLineOpts)
-- | Parse commandline options into CmdLineOpts
parseCmdLineOpts :: Cmd -> String -> (CmdLineOpts,[String])
parseCmdLineOpts cmd s
  = ([ CmdLineOpt cmd $ pOpt o | ('-':o) <- opts ],rest)
  where (opts,rest)    = partition isOpt $ words s
        isOpt ('-':_)  = True
        isOpt _        = False
        pOpt ('-':opt) = CmdFlag_ModfMin $ pOpt opt
        pOpt ('D':def) = uncurry CmdFlag_Define $ pDef def
        pOpt ('I':dir) = CmdFlag_IncludeDir dir
        pOpt ('l':lib) = CmdFlag_Lib lib
        pOpt s         = case pDef s of
                           (k, Just v) -> CmdFlag_KeyEqualsVal k v
                           (k, _     ) -> CmdFlag_Flag        k
        pDef s =
          case break (== '=') s of
            (k,'=':v) -> (k, Just v)
            _         -> (s, Nothing)
%%]

