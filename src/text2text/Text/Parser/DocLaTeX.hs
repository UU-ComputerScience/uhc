-------------------------------------------------------------------------
-- DocLaTeX Parser
-------------------------------------------------------------------------

module Text.Parser.DocLaTeX
  ( pItf
  , doclatexScanOptsMp
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified EH.Util.FastSeq as Seq

import Common
import Text
import Text.Parser.Common

-------------------------------------------------------------------------
-- 0 arg cmds
-------------------------------------------------------------------------

cmd0argMp :: Map.Map String TextItem
cmd0argMp
  = Map.fromList
      [ ("maketitle"		, TextItem_MakeTitle		)
      , ("tableofcontents"	, TextItem_TOC		)
      ]

-------------------------------------------------------------------------
-- 1 arg cmds
-------------------------------------------------------------------------

cmd1argMp :: Map.Map String (TextItems -> TextItem)
cmd1argMp
  = Map.fromList
      [ ("title"		, TextItem_Title							)
      , ("author"		, TextItem_Author							)
      , ("label"		, TextItem_Label							)
      , ("textbf"		, TextItem_Styled TextStyle_Bold			)
      , ("usepackage"	, TextItem_Import							)
      , ("paragraph"	, TextItem_Header HeaderLevel_Paragraph		)
      ]
    `Map.union` Map.fromList [ (concat (replicate l "sub") ++ "section", TextItem_Header (HeaderLevel_Level l)) | l <- [0..2] ]

-------------------------------------------------------------------------
-- 2 arg cmds
-------------------------------------------------------------------------

cmd2argMp :: Map.Map String (TextItems -> TextItems -> TextItem)
cmd2argMp
  = Map.fromList
      [ ("href"		, TextItem_RefTo RefType_Global		)
      , ("lref"		, TextItem_RefTo RefType_Local		)
      ]

-------------------------------------------------------------------------
-- Allowed environments
-------------------------------------------------------------------------

grouptypeMp :: Map.Map String GroupType
grouptypeMp
  = Map.fromList
      [ ("verbatim"	, GroupType_Verbatim		)
      , ("pre"		, GroupType_Verbatim		)
      , ("document"	, GroupType_Document		)
      ]

-------------------------------------------------------------------------
-- Allowed styles
-------------------------------------------------------------------------

textstyleMp :: Map.Map String TextStyle
textstyleMp
  = Map.fromList
      [ ("verb"		, TextStyle_Verbatim	)
      ]

-------------------------------------------------------------------------
-- Allowed itemize styles
-------------------------------------------------------------------------

itemizestyleMp :: Map.Map String ItemizeStyle
itemizestyleMp
  = Map.fromList
      [ ("itemize"		, ItemizeStyle_Bullet	)
      , ("enumerate"	, ItemizeStyle_Number	)
      ]

-------------------------------------------------------------------------
-- Allowed document options
-------------------------------------------------------------------------

docoptionMp :: Map.Map String DocumentOption
docoptionMp
  = Map.fromList
      [ ("a4paper"		, DocumentOption_A4Paper	)
      ]

-------------------------------------------------------------------------
-- Scanner setup
-------------------------------------------------------------------------

doclatexScanOpts :: ScanOpts
doclatexScanOpts
  = defaultScanOpts
      { scoKeywordsTxt      =   Map.keysSet grouptypeMp
                                `Set.union` Map.keysSet itemizestyleMp
                                `Set.union` Map.keysSet docoptionMp
      , scoCommandsTxt      =   Set.fromList [ "begin", "end", "item", "documentclass" ]
                                `Set.union` Map.keysSet textstyleMp
                                `Set.union` Map.keysSet cmd0argMp
                                `Set.union` Map.keysSet cmd1argMp
                                `Set.union` Map.keysSet cmd2argMp
      , scoSpecChars        =   Set.fromList "{}[]"
      , scoOpChars          =   Set.fromList ""
      }

doclatexScanOptsMp :: ScanOptsMp
doclatexScanOptsMp
  = Map.fromList
        [ ( ScTpContent TextType_DocLaTeX
          , doclatexScanOpts
          )
        ]

-------------------------------------------------------------------------
-- Parser doclatex content
-------------------------------------------------------------------------

-- itf to outside
pItf                ::  T2TPr (Seq.Seq TextItem)
pItf                =   pTextItemsP' (pTextItem <|> pTextItemDoc)


-- items
pTextItemsP'        ::  T2TPr2' TextItem (Seq.Seq TextItem)
pTextItemsP' pItm   =   (Seq.unions . map Seq.fromList) <$> pList ((:[]) <$> pItm <|> pAST)

pText2ItemsP'       ::  T2TPr3' TextItem TextItem (Seq.Seq TextItem)
pText2ItemsP' p1 p2 =   (\i1 i2 -> Seq.unions (Seq.fromList i1 : map Seq.fromList i2)) <$> pList p1 <*> pList ((:[]) <$> p2 <|> pAST)

pTextItemsP         ::  T2TPr2' TextItem TextItems
pTextItemsP pItm    =   Seq.toList <$> pTextItemsP' pItm

pText2ItemsP        ::  T2TPr3' TextItem TextItem TextItems
pText2ItemsP p1 p2  =   Seq.toList <$> pText2ItemsP' p1 p2

pTextItems          ::  T2TPr TextItems
pTextItems          =   pTextItemsP pTextItem


-- item
pTextItemSpace      ::  T2TPr TextItem
pTextItemSpace      =   TextItem_Space    <$> pWhite
                    <|> TextItem_LineFeed <$  pNl

pTextItemBase       ::  T2TPr TextItem
pTextItemBase       =   TextItem_NonSpace <$> pText
                    <|> uncurry TextItem_Group <$> pBeginEnd pGroupType pTextItems
                    <|> uncurry TextItem_Itemize <$> pBeginEnd pItemizeStyle (pText2ItemsP pTextItemSpace pTextItemItem)
                    <|> pCmd2Arg <*> pArg pTextItems <*> pArg pTextItems
                    <|> pCmd1Arg <*> pArg pTextItems
                    <|> pCmd0Arg
                    -- <|> pAnyKey (\x -> TextItem_NonSpace <$> pKey x) (map (:[]) $ Set.toList $ scoSpecChars doclatexScanOpts)

pTextItemItem       ::  T2TPr TextItem
pTextItemItem       =   TextItem_ItemizeItem <$ pCmd "item" <*> pTextItems

pTextItemDoc        ::  T2TPr TextItem
pTextItemDoc        =   TextItem_DocumentHeader <$ pCmd "documentclass" <*> pMb (pArgOpt (pList pDocumentOption)) <*> pArg pTextItems

pTextItem           ::  T2TPr TextItem
pTextItem           =   pTextItemBase
                    <|> pTextItemSpace


-- begin/end
pBeginEnd           ::  T2TPr3' c TextItems (c,TextItems)
pBeginEnd pEnv pIts =   (,)
                        <$  pCmd "begin" <*> pArg pEnv
                        <*> pIts
                        <*  pCmd "end" <* pArg pEnv


-- arg
pArg                ::  T2TPr2 x
pArg x              =   pKey "{" *> x <* pKey "}"

pArgOpt             ::  T2TPr2 x
pArgOpt x           =   pKey "[" *> x <* pKey "]"


-- various types, kinds, etc
pGroupType          ::  T2TPr GroupType
pGroupType          =   pAnyFromMap pKey grouptypeMp

pTextStyle          ::  T2TPr TextStyle
pTextStyle          =   pAnyFromMap pCmd textstyleMp

pItemizeStyle       ::  T2TPr ItemizeStyle
pItemizeStyle       =   pAnyFromMap pKey itemizestyleMp

pDocumentOption     ::  T2TPr DocumentOption
pDocumentOption     =   pAnyFromMap pKey docoptionMp


-- cmds
pCmd0Arg            ::  T2TPr TextItem
pCmd0Arg            =   pAnyFromMap pCmd cmd0argMp

pCmd1Arg            ::  T2TPr (TextItems -> TextItem)
pCmd1Arg            =   pAnyFromMap pCmd cmd1argMp

pCmd2Arg            ::  T2TPr (TextItems -> TextItems -> TextItem)
pCmd2Arg            =   pAnyFromMap pCmd cmd2argMp

