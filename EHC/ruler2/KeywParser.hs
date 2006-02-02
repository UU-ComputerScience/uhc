-------------------------------------------------------------------------
-- Keyw/sym parser
-------------------------------------------------------------------------

module KeywParser
  ( specialChars, opChars
  , propsSynInhMp, propsOtherMp, propsMp
  , keywordsTextProps, keywordsTextEscapable, keywordsText
  , keywordsOpsEsc, keywordsOpsExplainEsc, keywordsOpsParenEsc
  , keywordsOps
  , mkScan, mkHScan
  , pNmStr, pNmStrI, pSymEscStr, pSymStr
  )
  where

-- import qualified Data.Set as Set
import qualified Data.Map as Map
-- import Data.List
import IO
import UU.Parsing
-- import UU.Parsing.CharParser
-- import UU.Scanner.Position( initPos, Pos, Position(..) )
-- import UU.Scanner.GenToken
import UU.Scanner
import ParseUtils
import AttrProps
-- import Nm
-- import SelParser
-- import Common
-- import Utils (wordsBy)
-- import MainAG

-------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------

specialChars  =  "().`"
opChars       =  "!#$%&*+/<=>?@\\^|-:;,[]{}~"

propsSynInhMp
  =  Map.fromList [ ("thread",AtThread), ("updown",AtUpdown) ]
propsOtherMp
  =  Map.fromList [ ("retain",AtRetain), ("node",AtNode) ]
propsMp
  =  Map.unions [ propsSynInhMp, propsOtherMp ]
keywordsTextProps
  =  Map.keys propsMp
keywordsTextEscapable
  =  keywordsTextProps
     ++ [ "judge", "judgeshape", "judgespec", "judgeuse", "relation"
        , "rule", "rules", "ruleset"
        , "scheme", "view", "hole", "holes", "viewsel", "rulesgroup"
        , "explain"
        -- related to global info
        , "viewhierarchy", "format", "rewrite", "preamble"
        , "extern", "external"
        -- related to formatting (styles)
        , "tex", "ag", "def", "use", "spec"
        -- misc
        , "text"
        ]
keywordsText
  =  [ "unique"
     ] ++ keywordsTextEscapable
keywordsOpsEsc
  =  [ ",", ":", "[", "]", "*", "<" ]
keywordsOpsExplainEsc
  =  [ "=", "-", "." ]
keywordsOpsParenEsc
  =  [ "|" ] ++ keywordsOpsExplainEsc
keywordsOps
  =  keywordsOpsParenEsc ++ keywordsOpsEsc

mkScan :: FilePath -> String -> [Token]
mkScan fn txt = scan keywordsText keywordsOps specialChars opChars (initPos fn) txt

mkHScan :: FilePath -> Handle -> IO [Token]
mkHScan fn fh
  = do  {  txt <- hGetContents fh
        ;  return (mkScan fn txt) 
        }

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

pNmStr, pNmStrI :: (IsParser p Token) => p String

pNmStr = pVarid <|> pConid

pNmStrI = pNmStr <|> pInteger

pSymEscStr :: (IsParser p Token) => ([String],[String]) -> p String
pSymEscStr (kEsc,kpEsc)
  =   pVarsym <|> pConsym
  <|> pAnyKey pKey kEsc
  <|> pKey "`"  *> (   (\n -> "`" ++ n ++ "`") <$> pNmStr
                   <|> concat <$> pList1 (pAnyKey pKey kpEsc)
                   )
               <*  pKey "`"

pSymStr :: (IsParser p Token) => p String
pSymStr = pSymEscStr (keywordsOpsEsc,keywordsOpsParenEsc)

