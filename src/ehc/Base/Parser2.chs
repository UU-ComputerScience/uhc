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

%%[8 import({%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Scanner.Scanner}, {%{EH}Base.ParseUtils})
%%]

%%[8 import({%{EH}Base.Parser})
%%]

%%[8 import(UU.Parsing, EH.Util.ParseUtils, EH.Util.ScanUtils)
%%]

%%[8 import({%{EH}Base.FileSearchLocation})
%%]

%%[20 import({%{EH}Module})
%%]

%%[20 import(qualified Data.Set as Set,qualified EH.Util.Rel as Rel)
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

%%[99 export(parsePkgKey)
parsePkgKey :: String -> Maybe PkgKey
parsePkgKey
  = parseString scanOpts pPkgKey
  where scanOpts   = defaultScanOpts {scoSpecChars = Set.fromList ".-", scoAllowFloat = False}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(pModEntRel)
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

