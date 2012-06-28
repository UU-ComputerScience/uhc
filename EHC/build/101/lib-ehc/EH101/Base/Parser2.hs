module EH101.Base.Parser2
( pModEntRel
, parsePkgKey )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Scanner.Common
import EH101.Scanner.Scanner
import EH101.Base.ParseUtils
import EH101.Base.Parser
import UU.Parsing
import EH.Util.ParseUtils
import EH.Util.ScanUtils
import EH101.Base.FileSearchLocation
import EH101.Module
import qualified Data.Set as Set
import qualified EH.Util.Rel as Rel
import Data.Version



{-# LINE 41 "src/ehc/Base/Parser2.chs" #-}
pPkgKey :: P PkgKey
pPkgKey = (concat <$> pList1_ng (pVarid <|> pConid <|> ("-" <$ pMINUS))) <+> pMb (pMINUS *> pVersion)

pVersion :: P Version
pVersion = (\v -> Version (map read v) []) <$> pList1Sep pDOT pInteger10

{-# LINE 49 "src/ehc/Base/Parser2.chs" #-}
parsePkgKey :: String -> Maybe PkgKey
parsePkgKey
  = parseString scanOpts pPkgKey
  where scanOpts   = defaultScanOpts {scoSpecChars = Set.fromList ".-", scoAllowFloat = False}

{-# LINE 60 "src/ehc/Base/Parser2.chs" #-}
pModEnt :: P ModEnt
pModEnt
  = (\kind occ owns -> ModEnt kind occ owns emptyRange)
    <$  pOCURLY <*> pIdOccKind <* pCOMMA <*> pIdOcc
    <*> pMaybe Set.empty id (Set.fromList <$ pCOMMA <* pOCURLY <*> pListSep pCOMMA pModEnt <* pCCURLY)
    <*  pCCURLY

pModEntRel :: P ModEntRel
pModEntRel
  = Rel.fromList <$> pAssocL pDollNm pModEnt

