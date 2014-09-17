%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

This should be in Base.Parser, but because of module circularities in an additional module

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic/shared parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Parser2}
%%]

%%[8 import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Base.Parser})
%%]

%%[8 import(UU.Parsing, UHC.Util.ParseUtils, UHC.Util.ScanUtils)
%%]

%%[8 import({%{EH}Base.FileSearchLocation})
%%]

%%[50 import({%{EH}Module})
%%]

%%[50 import(qualified Data.Set as Set,qualified UHC.Util.Rel as Rel)
%%]

%%[99 import(Data.Version)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing the package name as it is used
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
pPkgKey :: P PkgKey
pPkgKey = (concat <$> pList1_ng (pVarid <|> pConid <|> ("-" <$ pMINUS))) <+> pMb (pMINUS *> pVersion)

pVersion :: P Version
pVersion = (\v -> Version (map read v) []) <$> pList1Sep pDOT pInteger10
%%]

%%[99 export(parsePkgKey, parsePkgKeys)
scanOptsPkgKey = defaultScanOpts {scoSpecChars = Set.fromList ".-", scoAllowFloat = False}

parsePkgKey :: String -> Maybe PkgKey
parsePkgKey = parseString scanOptsPkgKey pPkgKey

parsePkgKeys :: String -> Maybe [PkgKey]
parsePkgKeys = parseString scanOptsPkgKey (pList pPkgKey)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(pModEntRel)
pModEnt :: P ModEnt
pModEnt
  = (\kind occ owns -> ModEnt kind occ owns emptyRange)
    <$  pOCURLY <*> pIdOccKind <* pCOMMA <*> pIdOcc
    <*> pMaybe Set.empty id (Set.fromList <$ pCOMMA <* pOCURLY <*> pListSep pCOMMA pModEnt <* pCCURLY)
    <*  pCCURLY

pModEntRel :: P ModEntRel
pModEntRel
  = Rel.fromList <$> pAssocL pDollNm pModEnt
%%]

