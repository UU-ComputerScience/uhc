%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Scanner.Common} import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, UU.Scanner.GenTokenParser, EH.Util.ScanUtils(), {%{EH}Base.Builtin}, {%{EH}Base.Common})
%%]

%%[1 import(EH.Util.ScanUtils)
%%]

%%[1.Scanner import(UU.Scanner) export(module UU.Scanner)
%%]

%%[1 export(module {%{EH}Scanner.Common})
%%]

%%[5.Scanner -1.Scanner import({%{EH}Scanner.Scanner}) export(module {%{EH}Scanner.Scanner})
%%]

%%[99 import (Data.Ratio)
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
%%[11
                ++ tokKeywStrsEH11
%%]
%%[12
                ++ tokKeywStrsEH12
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
%%[11
                ++ tokOpStrsEH11
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
%%[[5
            ,  "of"
%%]]
%%[[8
            ,  "letstrict"
%%]]
%%[[9
            ,  "where"
%%]]
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
%%[11
                ++ tokKeywStrsHS11
%%]
%%[12
                ++ tokKeywStrsHS12
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
%%[11
                ++ tokOpStrsHS11
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
%%% Scan opts for other parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
coreScanOpts :: ScanOpts
coreScanOpts
  =  grinScanOpts
        {   scoKeywordsTxt      =   [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                    , "Int", "Char", "Float", "String", "Tag", "Rec"
                                    , "module", "default"
%%[[12
                                    , "Integer", "Double" 
%%]
                                    ]
                                    ++ scoKeywordsTxt tyScanOpts
                                    ++ scoKeywordsTxt hsScanOpts
        ,   scoKeywordsOps      =   scoKeywordsOps grinScanOpts ++ scoKeywordsOps hsScanOpts
        ,   scoDollarIdent      =   True
        ,   scoOpChars          =          scoOpChars   grinScanOpts ++ scoOpChars   hsScanOpts
        ,   scoSpecChars        =   "!=" ++ scoSpecChars grinScanOpts ++ scoSpecChars hsScanOpts
        }
%%]

%%[8
grinScanOpts :: ScanOpts
grinScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   [ "eval", "apply"
                                    , "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi", "fetchupdate"
                                    , "throw", "try", "catch", "ctags", "applymap", "evalmap"
                                    , "C", "F", "P", "A", "R", "H", "U", "W"
                                    ]
        ,   scoKeywordsOps      =   [ "<-", "->", "=", "+=", "-=", ":=", "-", "*" ]
        ,   scoSpecChars        =   "();{}#/\\|,"
        ,   scoOpChars          =   "<->:=+*"
        ,   scoDollarIdent      =   True
        }
%%]

%%[8
hiScanOpts :: ScanOpts
hiScanOpts
  =  hsScanOpts
        {   scoKeywordsTxt      =   [ "value", "fixity", "stamp", "uid", "rule", "var", "ctxt", "sup", "iddef"
                                    , "Value", "Pat", "Type", "Kind", "Class", "Instance", "Default", "Any", "Data"
                                    , "True", "False"
                                    ]
                                    ++ scoKeywordsTxt hsScanOpts
                                    ++ scoKeywordsTxt tyScanOpts
        ,   scoOpChars          =   scoOpChars coreScanOpts
        ,   scoDollarIdent      =   True
        ,   scoSpecChars        =   scoSpecChars coreScanOpts
        ,   scoKeywordsOps      =   [ "??" ] ++ scoKeywordsOps coreScanOpts
        }
%%]

%%[8
tyScanOpts :: ScanOpts
tyScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   [ "uid" ]
        }
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
%%% Splitting up a rational into nominator/denominator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
floatDenot2NomDenom :: String -> (Integer,Integer)
floatDenot2NomDenom denot
  = (numerator f,denominator f)
  where (n,m,e) = getRational denot
        f :: Rational
        f = ((read n * md + mn) * en) % (ed * md)
        en, ed, mn, md :: Integer
        (en,ed) = case e of
                    Just (Just "-",e) -> (1,10 ^ read e)
                    Just (_,e)        -> (10 ^ read e,1)
                    _                 -> (1,1)
        (mn,md) = case m of
                    Just m -> (read m,10 ^ length m)
                    _      -> (1,1)
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
%%[12
  pQVaridTk, pQConidTk,
  pQVarsymTk, pQConsymTk,
%%]
%%[1
  pVaridTk , pConidTk ,
  pVaridTk', pConidTk',
  pTextnmTk, pTextlnTk, pIntegerTk, pVarsymTk, pConsymTk
    :: IsParser p Token => p Token

pStringTk     =   pCostValToken' 7 TkString    ""        
pCharTk       =   pCostValToken' 7 TkChar      "\NUL"    
pInteger8Tk   =   pCostValToken' 7 TkInteger8  "0"       
pInteger10Tk  =   pCostValToken' 7 TkInteger10 "0"       
pInteger16Tk  =   pCostValToken' 7 TkInteger16 "0"
pFractionTk   =   pCostValToken' 7 TkFraction  "0.0"
pVaridTk      =   pCostValToken' 7 TkVarid     "<identifier>" 
pVaridTk'     =   pCostValToken' 6 TkVarid     "<identifier>" 
pConidTk      =   pCostValToken' 7 TkConid     "<Identifier>" 
pConidTk'     =   pCostValToken' 6 TkConid     "<Identifier>" 
pConsymTk     =   pCostValToken' 7 TkConOp     "<conoperator>"
pVarsymTk     =   pCostValToken' 7 TkOp        "<operator>" 
pTextnmTk     =   pCostValToken' 7 TkTextnm    "<name>"       
pTextlnTk     =   pCostValToken' 7 TkTextln    "<line>"     
pIntegerTk    =   pInteger10Tk
%%]
%%[12
pQVaridTk     =   pCostValToken' 7 TkQVarid     "<identifier>" 
pQConidTk     =   pCostValToken' 7 TkQConid     "<Identifier>" 
pQConsymTk    =   pCostValToken' 7 TkQConOp     "<conoperator>"
pQVarsymTk    =   pCostValToken' 7 TkQOp        "<operator>" 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pCONID, pCONID', pCONSYM, pVARID, pVARID', pVARSYM :: IsParser p Token => p Token

pCONID           = pConidTk
pCONID'          = pConidTk
pCONSYM          = pConsymTk
pVARID           = pVaridTk
pVARID'          = pVaridTk'
pVARSYM          = pVarsymTk
%%]

%%[12
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
    pNEWTYPE   ,
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
pNEWTYPE         = pKeyTk "newtype"
pCASE            = pKeyTk "case"
pOF              = pKeyTk "of"
pIF              = pKeyTk "if"
pTHEN            = pKeyTk "then"
pELSE            = pKeyTk "else"
pDOTDOT          = pKeyTk ".."

tokKeywStrsEH5 = [ "data", "case", "if", "then", "else", "of" ]
tokKeywStrsHS5 = [ "newtype" ]
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
    pCOLEQUAL   ,
    pHASH
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
pHASH            = pKeyTk "#"
%%]

%%[7
tokOpStrsEH7   = [ ":=", "#" ]
tokOpStrsHS7   = [  ]
%%]

%%[8
pLABEL          ,
    pLETSTRICT  ,
    pSAFE       ,
    pUNSAFE     ,
    pTHREADSAFE ,
    pCCALL      ,
    pSTDCALL    ,
    pDYNAMIC    ,
    pFOREIGN    ,
    pJAZY       ,
    pIMPORT     ,
    pEXPORT
  :: IsParser p Token => p Token
%%]

%%[8
pLABEL           = pKeyTk "label"
pLETSTRICT       = pKeyTk "letstrict"
pSAFE            = pKeyTk "safe"
pUNSAFE          = pKeyTk "unsafe"
pTHREADSAFE      = pKeyTk "threadsafe"
pCCALL           = pKeyTk "ccall"
pSTDCALL         = pKeyTk "stdcall"
pDYNAMIC         = pKeyTk "dynamic"
pFOREIGN         = pKeyTk "foreign"
pJAZY            = pKeyTk "jazy"
pIMPORT          = pKeyTk "import"
pEXPORT          = pKeyTk "export"

tokKeywStrsEH8 = [ "letstrict", "foreign", "import", "jazy" ]
tokKeywStrsHS8 = [ "export", "label", "safe", "unsafe", "threadsafe", "ccall", "stdcall", "dynamic" ]
%%]

%%[9
pDARROW         ,
    pLTCOLON    ,
    pOIMPL      ,
    pCIMPL      ,
    pCLASS      ,
    pINSTANCE   ,
    pDERIVING   ,
    pDEFAULT    ,
    pDO         
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
pDO              = pKeyTk "do"

tokKeywStrsEH9 = [ "class", "instance" ]
tokKeywStrsHS9 = [ "deriving", "default", "do" ]
tokOpStrsEH9   = [ show hsnPrArrow, "<:" ]
tokOpStrsHS9   = [  ]
%%]

%%[10
tokOpStrsEH10  = [ show hsnDynVar ]
tokOpStrsHS10  = [  ]
%%]

%%[50
%%]

%%[11
pTYPE
  :: IsParser p Token => p Token
%%]

%%[11
pTYPE            = pKeyTk "type"
%%]

%%[11
tokKeywStrsEH11 = [ "type" ]
tokKeywStrsHS11 = [  ]
tokOpStrsEH11   = [  ]
tokOpStrsHS11   = [  ]
%%]

%%[12
pQUALIFIED      ,
    pQUESTQUEST ,
    pAS         ,
    pHIDING     ,
    pNUMBER     
  :: IsParser p Token => p Token
%%]

%%[12
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"
pNUMBER          = pKeyTk "#"
pQUESTQUEST      = pKeyTk "??"

tokKeywStrsEH12 = [  ]
tokKeywStrsHS12 = [ "qualified", "as", "hiding" ]
%%]

%%[13
pDOTNET      
  :: IsParser p Token => p Token
%%]

%%[13
pDOTNET          = pKeyTk "dotnet"
%%]

%%[90
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

