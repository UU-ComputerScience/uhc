%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.ScannerCommon} import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, UU.Scanner.GenTokenParser, EH.Util.ScanUtils(), {%{EH}Base.Common})
%%]

%%[1 import(EH.Util.ScanUtils)
%%]

%%[1.Scanner import(UU.Scanner) export(module UU.Scanner)
%%]

%%[1 export(module {%{EH}Base.ScannerCommon})
%%]

%%[5.Scanner -1.Scanner import({%{EH}Base.Scanner}) export(module {%{EH}Base.Scanner})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner options: keywords etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ehScanOpts
ehScanOpts :: ScanOpts
ehScanOpts
  =  defaultScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =
                tokKeywStrsEH1
                ++ offsideTrigs
%%]
%%[4
                ++ tokKeywStrsEH4
%%]
%%[5
                ++ tokKeywStrsEH5
%%]
%%[8
                ++ tokKeywStrsEH8
%%]
%%[9
                ++ tokKeywStrsEH9
%%]
%%[1
        ,   scoKeywordsOps      =
                tokOpStrsEH1
%%]
%%[2
                ++ tokOpStrsEH2
%%]
%%[3
                ++ tokOpStrsEH3
%%]
%%[4
                ++ tokOpStrsEH4
%%]
%%[5
                ++ tokOpStrsEH5
%%]
%%[6
                ++ tokOpStrsEH6
%%]
%%[7
                ++ tokOpStrsEH7
%%]
%%[9
                ++ tokOpStrsEH9
%%]
%%[10
                ++ tokOpStrsEH10
%%]
%%[1
        ,   scoSpecChars        =
                "();,[]{}`"
        ,   scoOpChars          =
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7
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
        ,   scoOffsideTrigs     =   offsideTrigs
        ,   scoOffsideModule    =   "let"
        ,   scoOffsideOpen      =   "{"
        ,   scoOffsideClose     =   "}"
        }
  where offsideTrigs     =
            [  "let"
%%]
%%[5
            ,  "of"
%%]
%%[9
            ,  "where"
%%]
%%[1
            ]
%%]

%%[1
hsScanOpts :: ScanOpts
hsScanOpts
  = ehScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =
                scoKeywordsTxt ehScanOpts
                ++ offsideTrigs
                ++ tokKeywStrsHS1
%%]
%%[4
                ++ tokKeywStrsHS4
%%]
%%[5
                ++ tokKeywStrsHS5
%%]
%%[8
                ++ tokKeywStrsHS8
%%]
%%[9
                ++ tokKeywStrsHS9
%%]
%%[1
        ,   scoKeywordsOps      =
                scoKeywordsOps ehScanOpts
                ++ tokOpStrsHS1
%%]
%%[2
                ++ tokOpStrsHS2
%%]
%%[3
                ++ tokOpStrsHS3
%%]
%%[4
                ++ tokOpStrsHS4
%%]
%%[5
                ++ tokOpStrsHS5
%%]
%%[6
                ++ tokOpStrsHS6
%%]
%%[7
                ++ tokOpStrsHS7
%%]
%%[9
                ++ tokOpStrsHS9
%%]
%%[10
                ++ tokOpStrsHS10
%%]
%%[1
        ,   scoOffsideTrigs     =
                scoOffsideTrigs ehScanOpts
                ++ offsideTrigs
        ,   scoOffsideModule    =   "module"
        }
  where offsideTrigs     =
            [  "where"
            ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scan file/handle to tokenlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return $ scan (scoKeywordsTxt opts)
                         (scoKeywordsOps opts)
                         (scoSpecChars opts)
                         (scoOpChars opts)
                         (initPos fn) 
                  $ txt
        }
%%]

%%[5 -1.scanHandle
%%]

%%[1.offsideScanHandle
offsideScanHandle scanOpts fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        ;  return (scanOffside moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved (scoOffsideModule scanOpts) noPos
          oBrace    = reserved (scoOffsideOpen scanOpts) noPos
          cBrace    = reserved (scoOffsideClose scanOpts) noPos
          triggers  = [ reserved x noPos | x <- scoOffsideTrigs scanOpts ]
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

%%[1
pStringTk, pCharTk,
  pInteger8Tk, pInteger10Tk, pInteger16Tk, pFractionTk,
%%]
%%[8
  pQVaridTk, pQConidTk,
  pQVarsymTk, pQConsymTk,
%%]
%%[1
  pVaridTk, pConidTk,
  pTextnmTk, pTextlnTk, pIntegerTk, pVarsymTk, pConsymTk
    :: IsParser p Token => p Token

pStringTk     =   pCostValToken' 9 TkString    ""        
pCharTk       =   pCostValToken' 9 TkChar      "\NUL"    
pInteger8Tk   =   pCostValToken' 9 TkInteger8  "0"       
pInteger10Tk  =   pCostValToken' 9 TkInteger10 "0"       
pInteger16Tk  =   pCostValToken' 9 TkInteger16 "0"
pFractionTk   =   pCostValToken' 9 TkFraction  "0.0"
pVaridTk      =   pCostValToken' 9 TkVarid     "<identifier>" 
pConidTk      =   pCostValToken' 9 TkConid     "<Identifier>" 
pConsymTk     =   pCostValToken' 9 TkConOp     "<conoperator>"
pVarsymTk     =   pCostValToken' 9 TkOp        "<operator>" 
pTextnmTk     =   pCostValToken' 9 TkTextnm    "<name>"       
pTextlnTk     =   pCostValToken' 9 TkTextln    "<line>"     
pIntegerTk    =   pInteger10Tk
%%]
%%[8
pQVaridTk     =   pCostValToken' 9 TkQVarid     "<identifier>" 
pQConidTk     =   pCostValToken' 9 TkQConid     "<Identifier>" 
pQConsymTk    =   pCostValToken' 9 TkQConOp     "<conoperator>"
pQVarsymTk    =   pCostValToken' 9 TkQOp        "<operator>" 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pCONID, pCONSYM, pVARID, pVARSYM :: IsParser p Token => p Token

pCONID           = pConidTk
pCONSYM          = pConsymTk
pVARID           = pVaridTk
pVARSYM          = pVarsymTk
%%]

%%[8
pQCONID, pQCONSYM, pQVARID, pQVARSYM :: IsParser p Token => p Token

pQCONID          = pQConidTk
pQCONSYM         = pQConsymTk
pQVARID          = pQVaridTk
pQVARSYM         = pQVarsymTk
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
    pCOLON     ,
    pOBRACK    ,
    pCBRACK    ,
    pOCURLY    ,
    pCCURLY    ,
    pVOCURLY   ,
    pVCCURLY   ,
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
pCOLON           = pKeyTk ":"
pDCOLON          = pKeyTk "::"
pOBRACK          = pKeyTk "["
pCBRACK          = pKeyTk "]"
pOCURLY          = pKeyTk "{"
pCCURLY          = pKeyTk "}"
pVOCURLY         = pKeyTk "{-layout"
pVCCURLY         = pKeyTk "}-layout"
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
pRARROW          = pKeyTk (show hsnArrow)
pBACKQUOTE       = pKeyTk "`"
pLET             = pKeyTk "let"
pLAM             = pKeyTk "\\"
pUNDERSCORE      = pKeyTk "_"
pIN              = pKeyTk "in"

tokKeywStrsEH1 = [ "in", "let" ]
tokKeywStrsHS1 = [ "module", "where", "infix", "infixl", "infixr" ]
tokOpStrsEH1   = [ "=", "\\", show hsnArrow, "::", "@" ]
tokOpStrsHS1   = [ "-", "*", "!", "_", "%", "." ]
%%]

%%[2
pTDOT    
  :: IsParser p Token => p Token
%%]

%%[2
pTDOT            = pKeyTk "..."

tokOpStrsEH2   = [ "..." ]
tokOpStrsHS2   = [  ]
%%]

%%[3
tokOpStrsEH3   = [ "%" ]
tokOpStrsHS3   = [  ]
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

tokKeywStrsEH4 = [ "forall", "exists" ]
tokKeywStrsHS4 = [  ]
tokOpStrsEH4   = [ ".", "~" ]
tokOpStrsHS4   = [  ]
%%]

%%[5
pLARROW        ,
    pVBAR      ,
    pDATA      ,
    pCASE      ,
    pOF        ,
    pIF        ,
    pTHEN      ,
    pELSE      ,
    pDOTDOT    
  :: IsParser p Token => p Token
%%]

%%[5
pLARROW          = pKeyTk "<-"
pVBAR            = pKeyTk "|"
pDATA            = pKeyTk "data"
pCASE            = pKeyTk "case"
pOF              = pKeyTk "of"
pIF              = pKeyTk "if"
pTHEN            = pKeyTk "then"
pELSE            = pKeyTk "else"
pDOTDOT          = pKeyTk ".."

tokKeywStrsEH5 = [ "data", "case", "if", "then", "else", "of" ]
tokKeywStrsHS5 = [  ]
tokOpStrsEH5   = [ "|" ]
tokOpStrsHS5   = [ "<-", ".." ]
%%]

%%[6
tokOpStrsEH6   = [ "*" ]
tokOpStrsHS6   = [  ]
%%]

%%[7
pOROWREC        ,
    pCROWREC    ,
    pOROWROW    ,
    pCROWROW    ,
    pOROWSUM    ,
    pCROWSUM    ,
    pCOLEQUAL
  :: IsParser p Token => p Token
%%]

%%[7
pOROWREC         = pKeyTk (show hsnORec)
pCROWREC         = pKeyTk (show hsnCRec)
pOROWROW         = pKeyTk (show hsnORow)
pCROWROW         = pKeyTk (show hsnCRow)
pOROWSUM         = pKeyTk (show hsnOSum)
pCROWSUM         = pKeyTk (show hsnCSum)
pCOLEQUAL        = pKeyTk ":="
%%]

%%[7
tokOpStrsEH7   = [ ":=" ]
tokOpStrsHS7   = [  ]
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
    pDO         ,
    pEXPORT     ,
    pQUALIFIED  ,
    pAS         ,
    pHIDING     
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
pEXPORT          = pKeyTk "export"
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"

tokKeywStrsEH8 = [ "foreign", "import", "jazy" ]
tokKeywStrsHS8 = [ "export", "qualified", "as", "hiding", "label", "safe", "threadsafe", "ccallconv", "stdcallconv", "dynamic", "do" ]
%%]

%%[9
pDARROW         ,
    pLTCOLON    ,
    pOIMPL      ,
    pCIMPL      ,
    pCLASS      ,
    pINSTANCE   ,
    pDERIVING   ,
    pDEFAULT    
  :: IsParser p Token => p Token
%%]

%%[9
pDARROW          = pKeyTk (show hsnPrArrow)
pLTCOLON         = pKeyTk "<:"
pOIMPL           = pKeyTk (show hsnOImpl)
pCIMPL           = pKeyTk (show hsnCImpl)
pCLASS           = pKeyTk "class"
pINSTANCE        = pKeyTk "instance"
pDERIVING        = pKeyTk "deriving"
pDEFAULT         = pKeyTk "default"

tokKeywStrsEH9 = [ "class", "instance" ]
tokKeywStrsHS9 = [ "deriving", "default" ]
tokOpStrsEH9   = [ show hsnPrArrow, "<:" ]
tokOpStrsHS9   = [  ]
%%]

%%[10
tokOpStrsEH10  = [ show hsnDynVar ]
tokOpStrsHS10  = [  ]
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
pDOTNET      
  :: IsParser p Token => p Token
%%]

%%[13
pDOTNET          = pKeyTk "dotnet"
%%]

%%[99
pDEPRECATED_prag = pKeyTk "deprecated_prag"
pCLOSE_prag      = pKeyTk "close_prag"
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
pCPABRACK        = pKeyTk "[:"
pOPABRACK        = pKeyTk "]"
p_SCC_           = pKeyTk "scc"
pSCC_prag        = pKeyTk "scc_prag"
pMDO             = pKeyTk "mdo"
pPROC            = pKeyTk "proc"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
%%]
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position) 
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)

