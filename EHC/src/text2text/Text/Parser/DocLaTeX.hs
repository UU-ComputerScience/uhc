-------------------------------------------------------------------------
-- DocLaTeX Parser
-------------------------------------------------------------------------

module Text.Parser.DocLaTeX
  ( pItf
  , doclatexScanOptsMp
  )
  where

import Data.Maybe
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
      [ ("maketitle"        , TextItem_MakeTitle        )
      , ("tableofcontents"  , TextItem_TOC              )
      , ("hline"            , TextItem_HorRuler         )
      -- , ("\\"                , TextItem_BreakLine        )
      ]

-------------------------------------------------------------------------
-- 1 arg cmds
-------------------------------------------------------------------------

cmd1argMp :: Map.Map String (TextItems -> TextItem)
cmd1argMp
  = Map.fromList
      [ ("title"        , TextItem_Title                            )
      , ("author"       , TextItem_Author                           )
      , ("label"        , TextItem_Label                            )
      , ("textbf"       , TextItem_Styled TextStyle_Bold            )
      , ("textit"       , TextItem_Styled TextStyle_Italic          )
      , ("texttt"       , TextItem_Styled TextStyle_Teletype        )
      , ("emph"         , TextItem_Styled TextStyle_Emphasized      )
      , ("usepackage"   , TextItem_Import                           )
      , ("paragraph"    , TextItem_Header HeaderLevel_Paragraph     )
      ]
    `Map.union` Map.fromList [ (concat (replicate l "sub") ++ "section", TextItem_Header (HeaderLevel_Level l)) | l <- [0..2] ]

-------------------------------------------------------------------------
-- 2 arg cmds
-------------------------------------------------------------------------

cmd2argMp :: Map.Map String (TextItems -> TextItems -> TextItem)
cmd2argMp
  = Map.fromList
      [ ("href"     , TextItem_RefTo RefType_Global     )
      , ("lref"     , TextItem_RefTo RefType_Local      )
      , ("eref"     , TextItem_RefTo RefType_EhcWeb     )
      , ("sref"     , TextItem_RefTo RefType_EhcSrc     )
      ]

-------------------------------------------------------------------------
-- Allowed environments
-------------------------------------------------------------------------

grouptypeMp :: Map.Map String GroupType
grouptypeMp
  = Map.fromList
      [ ("verbatim" , GroupType_Verbatim        )
      , ("pre"      , GroupType_Verbatim        )
      -- , ("document" , GroupType_Document        )
      -- , ("tabular"  , GroupType_Tabular         )
      ]

-------------------------------------------------------------------------
-- Allowed styles
-------------------------------------------------------------------------

{-
textstyleMp :: Map.Map String TextStyle
textstyleMp
  = Map.fromList
      [ ("verb"     , TextStyle_Verbatim    )
      ]
-}

-------------------------------------------------------------------------
-- Allowed itemize styles
-------------------------------------------------------------------------

itemizestyleMp :: Map.Map String ItemizeStyle
itemizestyleMp
  = Map.fromList
      [ ("itemize"      , ItemizeStyle_Bullet   )
      , ("enumerate"    , ItemizeStyle_Number   )
      ]

-------------------------------------------------------------------------
-- Allowed table column formattings
-------------------------------------------------------------------------

tablecolfmtMp :: Map.Map Char TableColFormat
tablecolfmtMp
  = Map.fromList
      [ ('l'            , TableColFormat_JustifyLeft    )
      , ('c'            , TableColFormat_JustifyCenter  )
      , ('r'            , TableColFormat_JustifyRight   )
      ]

-------------------------------------------------------------------------
-- Allowed document options
-------------------------------------------------------------------------

docoptionMp :: Map.Map String DocumentOption
docoptionMp
  = Map.fromList
      [ ("a4paper"      , DocumentOption_A4Paper    )
      ]

-------------------------------------------------------------------------
-- Scanner setup
-------------------------------------------------------------------------

specCharsOpenClose          =   "{}[]"
specCharsTable1             =   "|"
specCharsTable2             =   "&"

doclatexScanOpts :: ScanOpts
doclatexScanOpts
  = defaultScanOpts
      { scoKeywordsTxt      =   Map.keysSet grouptypeMp
                                `Set.union` Map.keysSet itemizestyleMp
                                `Set.union` Map.keysSet docoptionMp
                                `Set.union` Set.fromList [ "tabular", "document" ]
      , scoCommandsTxt      =   Set.fromList [ "begin", "end", "item", "documentclass" ]
                                -- `Set.union` Map.keysSet textstyleMp
                                `Set.union` Map.keysSet cmd0argMp
                                `Set.union` Map.keysSet cmd1argMp
                                `Set.union` Map.keysSet cmd2argMp
      , scoSpecChars        =   Set.fromList (specCharsOpenClose ++ specCharsTable1 ++ specCharsTable2)
      , scoOpChars          =   Set.fromList ""
      , scoVerbOpenClose    =   [ ("\\begin{pre}","\\end{pre}")
                                , ("\\begin{verbatim}","\\end{verbatim}")
                                ]
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
pItf                =   pTextItemsP' (pTextItemAll <|> pTextItemDoc)


-- items
pTextItemsP'        ::  T2TPr2' TextItem (Seq.Seq TextItem)
pTextItemsP' pItm   =   (Seq.unions . map Seq.fromList) <$> pList ((:[]) <$> pItm <|> pAST)

pText2ItemsP'       ::  T2TPr3' TextItem TextItem (Seq.Seq TextItem)
pText2ItemsP' p1 p2 =   (\i1 i2 -> Seq.unions (Seq.fromList i1 : map Seq.fromList i2)) <$> pList p1 <*> pList ((:[]) <$> p2 <|> pAST)

pText3ItemsP'       ::  T2TPr4' TextItem TextItem TextItem (Seq.Seq TextItem)
pText3ItemsP' p1 p2 p3
					=   (\i1 i2 i3 -> Seq.unions (Seq.fromList i1 : Seq.singleton i2 : map Seq.fromList i3))
					    <$> pList p1 <*> p2 <*> pList ((:[]) <$> p3 <|> pAST)

pTextItemsP         ::  T2TPr2' TextItem TextItems
pTextItemsP pItm    =   Seq.toList <$> pTextItemsP' pItm

pText2ItemsP        ::  T2TPr3' TextItem TextItem TextItems
pText2ItemsP p1 p2  =   Seq.toList <$> pText2ItemsP' p1 p2

pText3ItemsP        ::  T2TPr4' TextItem TextItem TextItem TextItems
pText3ItemsP p1 p2 p3
					=   Seq.toList <$> pText3ItemsP' p1 p2 p3

pTextItemsAll       ::  T2TPr TextItems
pTextItemsAll       =   pTextItemsP pTextItemAll

pTextItemsArg       ::  T2TPr TextItems
pTextItemsArg       =   pTextItemsP pTextItemArg

pTextItemsTbl       ::  T2TPr TextItems
pTextItemsTbl       =   (:) <$> pTextItemTbl1 <*> pTextItemsP pTextItemTbl2

pTableItemsAftRowSep::  T2TPr TextItems
pTableItemsAftRowSep=   pTextItemsP pTextItemSpace

pTableItemsAftFldSep::  T2TPr TextItems
pTableItemsAftFldSep=   pTextItemsP pTextItemSpace


-- item
pTextItemSpace      ::  T2TPr TextItem
pTextItemSpace      =   TextItem_Space     <$> pWhite
                    <|> TextItem_LineFeed  <$  pNl
                    <|> TextItem_CommentLF <$> pCmtLF

pTextItemBase       ::  T2TPr TextItem
pTextItemBase       =   TextItem_NonSpace <$> pText
                    <|> (\(c:v) -> TextItem_VerbatimInline [c] v) <$> pVerbInline
                    <|> uncurry TextItem_Group <$> pBeginEnd pGroupType pTextItemsAll
                    <|> (\(_,t) -> TextItem_DocumentContent t) <$> pBeginEnd (pKey "document") pTextItemsAll
                    <|> (\(_,(fmt,(extra,rows))) -> TextItem_Table fmt extra rows)
                        <$> pBeginEndArg (pKey "tabular") pTableFormat (pTableItemsAftRowSep <+> pTableRows)
                    <|> uncurry TextItem_Itemize <$> pBeginEnd pItemizeStyle (pText2ItemsP pTextItemSpace pTextItemItem)
                    <|> pCmd2Arg <*> pArg pTextItemsArg <*> pArg pTextItemsArg
                    <|> pCmd1Arg <*> pArg pTextItemsArg
                    <|> pCmd0Arg

pTextItemSpecs      ::  (IsParser p Tok) => [String] -> p TextItem
pTextItemSpecs s    =   pAnyKey (\x -> TextItem_NonSpace <$> pKey x) s

pTextItemSpecs2     ::  (IsParser p Tok) => [Char] -> p TextItem
pTextItemSpecs2 s   =   pTextItemSpecs (map (:[]) s)

pTextItemSpecsOC    ::  T2TPr TextItem
pTextItemSpecsOC    =   pTextItemSpecs2 specCharsOpenClose

pTextItemSpecsTbl1  ::  T2TPr TextItem
pTextItemSpecsTbl1  =   pTextItemSpecs2 specCharsTable1

pTextItemSpecsTbl2  ::  T2TPr TextItem
pTextItemSpecsTbl2  =   pTextItemSpecs2 specCharsTable2

pTextItemKeyws      ::  T2TPr TextItem
pTextItemKeyws      =   pTextItemSpecs (Set.toList $ scoKeywordsTxt doclatexScanOpts)

pTextItemItem       ::  T2TPr TextItem
pTextItemItem       =   TextItem_ItemizeItem <$ pCmd "item" <*> pTextItemsAll

{-
pTextItemizeItems	::	T2TPr TextItems
pTextItemizeItems	=	pCmd "item" <*> <*> pList pTextItemSpace
-}

pTextItemDoc        ::  T2TPr TextItem
pTextItemDoc        =   TextItem_DocumentHeader <$ pCmd "documentclass" <*> pMb (pArgOpt (pList pDocumentOption)) <*> pArg pTextItemsArg

pTextItemArg        ::  T2TPr TextItem
pTextItemArg        =   pTextItemBase
                    <|> pTextItemSpace
                    <|> pCmdBreakLine

pTextItemAll        ::  T2TPr TextItem
pTextItemAll        =   pTextItemArg
                    <|> pTextItemSpecsOC
                    <|> pTextItemSpecsTbl1
                    <|> pTextItemSpecsTbl2
                    <|> pTextItemKeyws

pTextItemTbl1       ::  T2TPr TextItem
pTextItemTbl1       =   pTextItemBase
                    <|> pTextItemSpecsOC
                    <|> pTextItemKeyws
                    <|> pTextItemSpecsTbl1

pTextItemTbl2       ::  T2TPr TextItem
pTextItemTbl2       =   pTextItemTbl1
                    <|> pTextItemSpace


-- begin/end
pBeginEnd           ::  T2TPr3' c its (c,its)
pBeginEnd pEnv pIts =   (,)
                        <$  pCmd "begin" <*> pArg pEnv
                        <*> pIts
                        <*  pCmd "end" <* pArg pEnv

pBeginEndArg        ::  T2TPr4' c arg its (c,(arg,its))
pBeginEndArg pEnv pA pIts
                    =   pBeginEnd pEnv (pArg pA <+> pIts)

-- table
pTableRowSep        ::  T2TPr TextItems
pTableRowSep        =   pCmdBreakLine *> pTableItemsAftRowSep

pTableRows          ::  T2TPr TableRows
pTableRows          =   pList ((\row rowsep -> TableRow_Row (row []) rowsep)
                               <$> pChainr ((\s l r -> \s' -> l s' ++ r s) <$ pKey "&" <*> pTableItemsAftFldSep)
                                           ((\i s -> [TableField_Fld s i]) <$> pTextItemsTbl)
                               <*> pTableRowSep
                              )

pTableFormat        ::  T2TPr TableFormat
pTableFormat        =   concat
                        <$> pList1 (   (\s -> catMaybes [Map.lookup c tablecolfmtMp | c <- s])
                                       <$> pText
                                   <|> [TableColFormat_SepbyLine]
                                       <$  pKey "|"
                                   )

-- arg
pArg                ::  T2TPr2 x
pArg x              =   pKey "{" *> x <* pKey "}"

pArgOpt             ::  T2TPr2 x
pArgOpt x           =   pKey "[" *> x <* pKey "]"


-- various types, kinds, etc
pGroupType          ::  T2TPr GroupType
pGroupType          =   pAnyFromMap pKey grouptypeMp

{-
pTextStyle          ::  T2TPr TextStyle
pTextStyle          =   pAnyFromMap pCmd textstyleMp
-}

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

pCmdBreakLine       ::  T2TPr TextItem
pCmdBreakLine       =   TextItem_BreakLine <$ pCmd "\\\\"
