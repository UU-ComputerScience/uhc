% $Id: EHParser.chs 276 2005-08-31 11:54:35Z atze $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module EHScannerCommon import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, UU.Scanner.GenTokenParser, EHCommon)
%%]

%%[1.Scanner import(UU.Scanner) export(module UU.Scanner)
%%]

%%[1 export(module EHScannerCommon)
%%]

%%[7.Scanner -1.Scanner import(EHScanner) export(module EHScanner)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner options: keywords etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ScanOpts
data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  [String]
        ,   scoKeywordsOps      ::  [String]
        ,   scoSpecChars        ::  String
        ,   scoOpChars          ::  String
        }
%%]

%%[1.scanOpts
scanOpts :: ScanOpts
scanOpts
%%]
%%[1.defaultScanOpts
  =  ScanOpts
%%]
%%[7 -1.defaultScanOpts
  =  defaultScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =   
                [ "in"
%%]
%%[4
                , "forall", "exists"
%%]
%%[5
                , "data", "case", "if", "then", "else"
%%]
%%[8
                , "foreign", "import", "jazy"
%%]
%%[9
                , "class", "instance"
%%]
%%[1
                ] ++ offsideTrigs
        ,   scoKeywordsOps      =
                [ "=", "\\", show hsnArrow, "::", "@"
%%]
%%[2
                , "..."
%%]
%%[3
                , "%"
%%]
%%[4
                , ".", "~"
%%]
%%[5
                , "|"
%%]
%%[6
                , "*"
%%]
%%[7
                , ":="
%%]
%%[9
                , show hsnPrArrow, "<:"
%%]
%%[10
                , show hsnDynVar
%%]
%%[1
                ]
        ,   scoSpecChars        =
                "();,[]{}"
        ,   scoOpChars          =
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7 -1.ScanOpts
        ,   scoSpecPairs        =
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
%%]
%%[9
                ,  show hsnOImpl, show hsnCImpl
%%]
%%[7
                ]
%%]
%%[1
        }
%%]

%%[1.offsideTrigs
offsideTrigs  =  [ "let" ]
%%]

%%[5.offsideTrigs -1.offsideTrigs
offsideTrigs  =  [ "let", "of" ]
%%]

%%[9.offsideTrigs -5.offsideTrigs
offsideTrigs  =  [ "let", "of", "where" ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scan file/handle to tokenlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return (scan (scoKeywordsTxt opts) (scoKeywordsOps opts) (scoSpecChars opts) (scoOpChars opts) (initPos fn) txt) 
        }
%%]

%%[7 -1.scanHandle
%%]

%%[1.offsideScanHandle
offsideScanHandle fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        ;  return (scanOffside moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved "let" noPos
          oBrace    = reserved "{" noPos
          cBrace    = reserved "}" noPos
          triggers  = [ reserved x noPos | x <- offsideTrigs ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related parser abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pKeyTk, pKeyTk'         ::  IsParser p Token
                              => String -> p Token
pKeyTk  key             =   pCostReserved' 9 key
pKeyTk' key             =   pCostReserved' 8 key

pKeyw                   ::  (IsParser p Token,Show k) => k -> p Token
pKeyw k                 =   pKeyTk (show k)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extraction from Token
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
tokGetVal :: Token -> String
tokGetVal x
  = case x of
      ValToken _ v p -> v
      Reserved v p   -> v

pV :: (IsParser p Token) => p Token -> p String
pV p = tokGetVal <$> p

pHNm :: (IsParser p Token) => p Token -> p HsName
pHNm p = (HNm . tokGetVal) <$> p
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pMODULE        ,
    pWHERE     ,
    pSEMI      ,
    pDCOLON    ,
    pOBRACK    ,
    pCBRACK    ,
    pOCURLY    ,
    pCCURLY    ,
    pAT        ,
    pPERCENT   ,
    pDOT       ,
    pCOMMA     ,
    pOPAREN    ,
    pCPAREN    ,
    pINFIX     ,
    pINFIXL    ,
    pINFIXR    ,
    pMINUS     ,
    pSTAR      ,
    pBANG      ,
    pEQUAL     ,
    pRARROW    ,
    pBACKQUOTE ,
    pLET       ,
    pLAM       ,
    pUNDERSCORE,
    pIN        
  :: IsParser p Token => p Token
%%]

%%[1
pMODULE          = pKeyTk "module"
pWHERE           = pKeyTk "where"
pSEMI            = pKeyTk ";"
pDCOLON          = pKeyTk "::"
pOBRACK          = pKeyTk "["
pCBRACK          = pKeyTk "]"
pOCURLY          = pKeyTk "{"
pCCURLY          = pKeyTk "}"
pAT              = pKeyTk "@"
pPERCENT         = pKeyTk "%"
pDOT             = pKeyTk "."
pCOMMA           = pKeyTk ","
pOPAREN          = pKeyTk "("
pCPAREN          = pKeyTk ")"
pINFIX           = pKeyTk "infix"
pINFIXL          = pKeyTk "infixl"
pINFIXR          = pKeyTk "infixr"
pMINUS           = pKeyTk "-"
pSTAR            = pKeyTk "*"
pBANG            = pKeyTk "!"
pEQUAL           = pKeyTk "="
pRARROW          = pKeyTk "->"
pBACKQUOTE       = pKeyTk "`"
pLET             = pKeyTk "let"
pLAM             = pKeyTk "\\"
pUNDERSCORE      = pKeyTk "_"
pIN              = pKeyTk "in"
%%]

%%[2
pTDOT    
  :: IsParser p Token => p Token
%%]

%%[2
pTDOT            = pKeyTk "..."
%%]

%%[3
%%]

%%[4
pFORALL       ,
    pEXISTS   ,
    pTILDE    
  :: IsParser p Token => p Token
%%]

%%[4
pFORALL          = pKeyTk "forall"
pEXISTS          = pKeyTk "exists"
pTILDE           = pKeyTk "~"
%%]

%%[5
pLARROW        ,
    pVBAR      ,
    pDDOT      ,
    pDATA      ,
    pCASE      ,
    pIF        ,
    pTHEN      ,
    pELSE      
  :: IsParser p Token => p Token
%%]

%%[5
pLARROW          = pKeyTk "<-"
pVBAR            = pKeyTk "|"
pDDOT            = pKeyTk "dotdot"
pDATA            = pKeyTk "data"
pCASE            = pKeyTk "case"
pIF              = pKeyTk "if"
pTHEN            = pKeyTk "then"
pELSE            = pKeyTk "else"
%%]

%%[6
%%]

%%[7
%%]

%%[8
pLABEL          ,
    pSAFE       ,
    pUNSAFE     ,
    pTHREADSAFE ,
    pCCALL      ,
    pSTDCALL    ,
    pDYNAMIC    ,
    pFOREIGN    ,
    pIMPORT     ,
    pJAZY       ,
    pDO         
  :: IsParser p Token => p Token
%%]

%%[8
pLABEL           = pKeyTk "label"
pSAFE            = pKeyTk "safe"
pUNSAFE          = pKeyTk "unsafe"
pTHREADSAFE      = pKeyTk "threadsafe"
pCCALL           = pKeyTk "ccallconv"
pSTDCALL         = pKeyTk "stdcallconv"
pDYNAMIC         = pKeyTk "dynamic"
pFOREIGN         = pKeyTk "foreign"
pDO              = pKeyTk "do"
pIMPORT          = pKeyTk "import"
pJAZY            = pKeyTk "jazy"
%%]

%%[9
pDARROW         ,
    pCLASS      ,
    pINSTANCE   ,
    pDERIVING   ,
    pDEFAULT    
  :: IsParser p Token => p Token
%%]

%%[9
pDARROW          = pKeyTk "=>"
pCLASS           = pKeyTk "class"
pINSTANCE        = pKeyTk "instance"
pDERIVING        = pKeyTk "deriving"    
pDEFAULT         = pKeyTk "default"
%%]

%%[10
%%]

%%[11
%%]

%%[12
pTYPE      ,
    pNEWTYPE   
  :: IsParser p Token => p Token
%%]

%%[12
pTYPE            = pKeyTk "type"
pNEWTYPE         = pKeyTk "newtype"
%%]

%%[13
pEXPORT          ,
    pQUALIFIED   ,
    pAS          ,
    pHIDING      ,
    pDOTNET      
  :: IsParser p Token => p Token
%%]

%%[13
pEXPORT          = pKeyTk "export"
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"
pDOTNET          = pKeyTk "dotnet"
%%]

%%[99
pDEPRECATED_prag = pKeyTk "deprecated_prag"
pCOLON           = pKeyTk ":"
pCLOSE_prag      = pKeyTk "close_prag"
pDOTDOT          = pKeyTk "dotdot"
pVOCURLY         = pKeyTk "vocurly"
pVCCURLY         = pKeyTk "vccurly"
pSOURCE_prag     = pKeyTk "source_prag"
pRULES_prag      = pKeyTk "rules_prag"
pESCAPE_open     = pKeyTk "parenEscape"
pUNPACK_prag     = pKeyTk "unpack_prag"
pOUBXPAREN       = pKeyTk "oubxparen"
pCUBXPAREN       = pKeyTk "cubxparen"
pINLINE_prag     = pKeyTk "inline_prag"
pNOINLINE_prag   = pKeyTk "noinline_prag"
pSPECIALISE_prag = pKeyTk "specialise_prag"
pCORE_prag       = pKeyTk "core_prag"
pREC             = pKeyTk "rec"
pPARENESCAPE     = pKeyTk "parenEscape"
pOEXPQUOTE       = pKeyTk "openExpQuote"
pCQUOTE          = pKeyTk "closeQuote"
pOTYPQUOTE       = pKeyTk "openTypQuote"
pOPATQUOTE       = pKeyTk "openPatQuote"
pODECQUOTE       = pKeyTk "openDecQuote"
pOPARENBAR       = pKeyTk "oparenbar"
pCPARENBAR       = pKeyTk "cparenbar"
pLARROWTAIL      = pKeyTk "larrowtail"      -- -<
pRARROWTAIL      = pKeyTk "rarrowtail"      -- >-
pDLARROWTAIL     = pKeyTk "Larrowtail"      -- -<<
pDRARROWTAIL     = pKeyTk "Rarrowtail"      -- >>-
pREIFY_TYPE      = pKeyTk "reifyType"
pREIFY_DECL      = pKeyTk "reifyDecl"
pREIFY_FIXITY    = pKeyTk "reifyFixity"
pOCURLYBAR       = pKeyTk "ocurlybar"
pCCURLYBAR       = pKeyTk "ccurlybar"
pCPABRACK        = pKeyTk "cpabrack"
pOPABRACK        = pKeyTk "opabrack"
p_SCC_           = pKeyTk "scc"
pSCC_prag        = pKeyTk "scc_prag"
pMDO             = pKeyTk "mdo"
pPROC            = pKeyTk "proc"
pOF              = pKeyTk "of"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position) 
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)
%%]

