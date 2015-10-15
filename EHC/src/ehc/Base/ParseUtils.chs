%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc parsing utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Parsing utilities.
These are not put elsewhere in a more general purpose library because these utilities
depend on UHC specific stuff.
%%]

%%[8 module {%{EH}Base.ParseUtils}
%%]

%%[8 import({%{EH}Base.Common})
%%]

-- parsing
%%[8 import(UU.Parsing, UU.Parsing.Offside, UHC.Util.ParseUtils)
%%]
-- scanning
%%[8 import(UHC.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Base.HsName})
%%]


-- general imports 
%%[8 import(qualified Data.Set as Set, qualified Data.Map as Map, Data.Maybe, Data.Version, Data.List)
%%]
%%[8 import(Control.Monad, Control.Monad.IO.Class, System.IO)
%%]
%%[8 import(UHC.Util.Pretty, UHC.Util.FPath)
%%]
%%[8 import({%{EH}Error}, {%{EH}Error.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type sugar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(P)
type P p = PlainParser Token p
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse a string, ignoring error message specifics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(parseString)
parseString :: ScanOpts -> P res -> String -> Maybe res
parseString scanOpts p s
  = if null errs then Just res else Nothing
  where tokens     = scan scanOpts (initPos s) s
        (res,errs) = parseToResMsgs p tokens
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHParseOpts(..), defaultEHParseOpts)
-- | Options to influence parsing
data EHParseOpts
  = EHParseOpts
      { ehpoptsLitMode			:: Bool			-- ^ literal mode?
      , ehpoptsOkToStopAtErr	:: Bool			-- ^ stop prematurely when parse error occurs and be ok with the result parsed until then?
      , ehpoptsForImport		:: Bool			-- ^ for import only?
      }
    deriving (Show)

defaultEHParseOpts :: EHParseOpts
defaultEHParseOpts
  = EHParseOpts
      { ehpoptsLitMode			= False
      , ehpoptsOkToStopAtErr	= False
      , ehpoptsForImport		= False
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstraction of what we want to be able to do with parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHPrs, EHPrsAna, EHPrsOff)
type EHPrs prs inp sym pos a = prs inp Pair sym pos a
type EHPrsAna a = EHPrs AnaParser [Token] Token (Maybe Token) a
type EHPrsOff a = EHPrs OffsideParser [Token] Token (Maybe Token) a
%%]

%%[8 export(EHParser)
class ( Eq symmsg, Show symmsg, Show pos, Position pos, PP (Message symmsg pos)
      -- , IsParser (prs inp Pair sym pos) sym
      -- , InputState inp sym pos
      -- , IsParser prs sym
      )
      => EHParser prs inp sym symmsg pos | prs -> inp sym symmsg pos where
  -- | Scan & parse using a parser coupled with a scanner
  ehScanParseToResMsgs :: ScanOpts -> EHParseOpts -> EHPrs prs inp sym pos a -> FilePath -> Handle -> IO (a,[Message symmsg pos])
  -- ehScanParseToResMsgs :: ScanOpts -> EHParseOpts -> prs a -> FilePath -> Handle -> IO (a,[Message symmsg pos])

-- instance (Show sym, Eq sym, Show pos, Position pos, Symbol sym, InputState [Token] sym pos) => EHParser AnaParser [Token] sym sym pos where
-- instance (Show sym, Ord sym, Show pos, Position pos, Symbol sym, InputState [Token] sym pos) => EHParser AnaParser [Token] sym sym pos where
instance EHParser AnaParser [Token] Token Token (Maybe Token) where
-- instance (Show sym, Ord sym, Show pos, Position pos, Symbol sym, InputState [Token] sym pos) => EHParser (AnaParser [Token] Pair sym pos) [Token] sym sym pos where
-- instance (Show pos, Position pos, InputState [Token] Token pos) => EHParser AnaParser [Token] Token Token pos where
-- instance InputState [Token] Token Pos => EHParser AnaParser [Token] Token Token Pos where
  ehScanParseToResMsgs sopts popts prs fn fh = do
    tokens <- scanHandle sopts fn fh
    return $ parseToResMsgs prs tokens

instance EHParser OffsideParser [Token] Token (OffsideSymbol Token) (Maybe Token) where
-- instance EHParser (OffsideParser [Token] Pair Token (Maybe Token)) [Token] Token (OffsideSymbol Token) (Maybe Token) where
  ehScanParseToResMsgs sopts popts prs fn fh = do
    tokens <- offsideScanHandle sopts fn fh
    return $ if ehpoptsOkToStopAtErr popts
      then parseOffsideToResMsgsStopAtErr prs tokens
      else parseOffsideToResMsgs prs tokens
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generalized parsing invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(parseWithFPath)
-- | The one and only parsing wrapper for parsing from files
parseWithFPath
  :: ( MonadIO m, Monad m
     , EHParser prs inp sym symmsg pos
     )
     => ScanOpts
     -> EHParseOpts
     -> EHPrs prs inp sym pos a
     -- -> prs a
     -> FPath															-- ^ file
     -> m (a,[Err])
parseWithFPath sopts popts p fp = do
    (fn,fh) <- liftIO $ openFPath fp ReadMode False
    let sopts' | ehpoptsLitMode popts = sopts {scoLitmode = True}
               | otherwise            = sopts
    (res,msgs) <- liftIO $ ehScanParseToResMsgs sopts' popts p fn fh
    let errs = map (rngLift emptyRange mkPPErr) msgs
    liftIO $ res `seq` hClose fh
    return (res,errs)
%%]
    
