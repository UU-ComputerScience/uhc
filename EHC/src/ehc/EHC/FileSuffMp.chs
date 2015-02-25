%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File suffix info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}EHC.FileSuffMp}
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]
%%[8 import({%{EH}Base.Target})
%%]

%%[8 import({%{EH}EHC.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Suffix search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FileSuffMp, emptyFileSuffMp, mkFileSuffMpHs)
type FileSuffMp =
  [( FileSuffix				-- suffix
   , EHCompileUnitState		-- initial state
   , Bool					-- visible from commandline
   )]

emptyFileSuffMp :: FileSuffMp
emptyFileSuffMp = []

-- | Allowed suffixes, order is significant.
mkFileSuffMpHs :: EHCOpts -> FileSuffMp
mkFileSuffMpHs opts
  = [ ( Just "hs"  , ECUS_Haskell HSStart, True )
%%[[99
    , ( Just "lhs" , ECUS_Haskell LHSStart, True )
%%]]
    , ( Just "eh"  , ECUS_Eh EHStart, True )
%%[[50
    , ( Just "hi"  , ECUS_Haskell HIStart, False )
%%]]
%%[[(8 grin)
    -- currently not supported
    -- , ( Just "grin", ECUS_Grin, True )
%%]]
%%[[(50 corerunin)
    , ( Just Cfg.suffixDotlessBinaryCoreRun , ECUS_CoreRun CRRStartBinary, True )
%%]]
%%[[(50 corein)
    , ( Just Cfg.suffixDotlessInputOutputTextualCore, ECUS_Core CRStartText, True   )
    , ( Just Cfg.suffixDotlessInputOutputBinaryCore , ECUS_Core CRStartBinary, True )
%%]]
%%[[(50 corebackend)
    , ( Just Cfg.suffixDotlessBinaryCore , ECUS_Core CRStartBinary, False )
%%]]
    ]
%%[[(90 codegen)
    ++ (if targetIsOnUnixAndOrC (ehcOptTarget opts)
        then [ ( Just "c"   , ECUS_C CStart, True )
             , ( Just "o"   , ECUS_O OStart, True )
             ]
        else []
       )
%%]]
%%]

%%[8 export(fileSuffMpHsNoSuff)
-- Suffix map for empty suffix, defaults to .hs
fileSuffMpHsNoSuff :: FileSuffMp
fileSuffMpHsNoSuff
  = [ ( Nothing  , ECUS_Haskell HSStart, False )
    ]
%%]

