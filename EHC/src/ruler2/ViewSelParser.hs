-------------------------------------------------------------------------
-- ViewSel parser
-------------------------------------------------------------------------

module ViewSelParser
  ( module KeywParser
  , module NmParser
  , pViewSel, pViewSels
  , pNmSel, pRlSel
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import UU.Parsing
import RulerScanner
import ParseUtils
import Nm
import KeywParser
import NmParser
import ViewSel

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

pViewSel :: (IsParser p Token) => p ViewSel
pViewSel
  = pV <??> ((flip ViewSel_Range) <$ pKey "-" <*> pV)
  where pV =   ViewSel_View <$> pNmVw <|> ViewSel_All <$ pKey "*"
           <|> pParens (ViewSel_Renamed <$> (foldr1 nmStrApd <$> pList1 pNmVw) <* pKey "=" <*> pNmVw)
        

pViewSels :: (IsParser p Token) => p ViewSels
pViewSels = pListSep pComma pViewSel

pNmSel :: (IsParser p Token) => p NmSel
pNmSel = NmSel_All <$ pKey "*" <|> NmSel_Nms <$> pList pNm

pRlSel :: (IsParser p Token) => p RlSel
pRlSel = RlSel_Sel <$> pParens pViewSels <* pKey "." <*> pParens pNmSel <* pKey "." <*> pParens pNmSel
