-------------------------------------------------------------------------
-- Text Parser
-------------------------------------------------------------------------

{-
Text is parsed in 2 steps, the first step is done here by parsing only the meta data required to find out the types of the various chunks of text.
-}

module Text.Parser
  ( module Text.Parser.Common
  
  , pAGItf
  , t2tScanOpts
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Common
import Text
import Text.Parser.Common

-------------------------------------------------------------------------
-- Scanner setup
-------------------------------------------------------------------------

t2tScanOpts :: ScanOptsMp
t2tScanOpts
  = Map.fromList
        [ ( ScTpMetaMeta
          , defaultScanOpts
              { scoKeywordsTxt      =   Set.fromList [ "_", "-", ".", "<", "=", "@", "||", "&&" ]
                                        `Set.union`
                                        Map.keysSet texttypeMp
              , scoSpecChars        =   Set.fromList "(),%{}"
              , scoOpChars          =   Set.fromList "+-=*&^$#@!\\|><~`;:?/_."
              }
          )
        ]

-------------------------------------------------------------------------
-- Parser for meta level, not dealing with content
-------------------------------------------------------------------------

pAGItf				::	T2TPr AGItf
pAGItf				=	AGItf_AGItf <$> pTextItems

pTextItems			::	T2TPr TextItems
pTextItems			=	pList pTextItem

pTextItem			::	T2TPr TextItem
pTextItem			=	pT2T <|> pLine <|> pLineFeed

pLine				::	T2TPr TextItem
pLine				=	TextItem_Line <$> pText

pLineFeed			::	T2TPr TextItem
pLineFeed			=	TextItem_LineFeed <$ pNl

pT2T				::	T2TPr TextItem
pT2T				=	TextItem_T2T <$> (tokPos <$> pBegContent) <*> pTextType <*> pTextItems <* pEndContent

pTextType			::	T2TPr TextType
pTextType			=	pAnyFromMap pKey texttypeMp
