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
%%[8 import(UU.Parsing, EH.Util.ParseUtils)
%%]
-- scanning
%%[8 import(EH.Util.ScanUtils, {%{EH}Scanner.Common}, {%{EH}Base.HsName})
%%]


-- general imports 
%%[8 import(qualified Data.Set as Set, qualified Data.Map as Map, Data.Maybe, Data.Version, Data.List)
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
