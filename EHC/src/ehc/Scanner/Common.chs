%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

Note: everything is exported.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Scanner.Common} import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, UU.Scanner.GenTokenParser, EH.Util.ScanUtils(), {%{EH}Base.Builtin}, {%{EH}Base.Common})
%%]

%%[1 import(qualified Data.Set as Set)
%%]

%%[1 import(EH.Util.ScanUtils)
%%]

%%[1.Scanner import(UU.Scanner) export(module UU.Scanner)
%%]

%%[1 export(module {%{EH}Scanner.Common})
%%]

%%[5.Scanner -1.Scanner import({%{EH}Scanner.Scanner}) export(module {%{EH}Scanner.Scanner})
%%]

%%[(8 codegen) import (EH.Util.ParseUtils)
%%]
%%[(8 codegen) import ({%{EH}Base.Target})
%%]

%%[97 import (Data.Ratio)
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
                Set.fromList $
                       tokKeywStrsEH1
                    ++ offsideTrigs
%%]
%%[4
                    ++ tokKeywStrsEH4
%%]
%%[5
                    ++ tokKeywStrsEH5
%%]
%%[6
                    ++ tokKeywStrsEH6
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
%%[20
                    ++ tokKeywStrsEH12
%%]
%%[94
                    ++ tokKeywStrsEH94
%%]
%%[95
                    ++ tokKeywStrsEH95
%%]
%%[1
        ,   scoKeywordsOps      =
                Set.fromList $
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
        ,   scoSpecChars        = Set.fromList $
                "();,[]{}`"
        ,   scoOpChars          = Set.fromList $
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7
        ,   scoSpecPairs        = Set.fromList $
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
%%[[9
                ,  show hsnOImpl, show hsnCImpl
%%]]
%%[[18
                -- ,  show hsnOParensUnboxed, show hsnCParensUnboxed
%%]]
                ]
%%]
%%[1
        ,   scoOffsideTrigs     =   offsideTrigs
%%[[9
        ,   scoOffsideTrigsGE   =   offsideTrigsGE
%%]]
        ,   scoOffsideModule    =   "let"
        ,   scoOffsideOpen      =   "{"
        ,   scoOffsideClose     =   "}"
        }
  where offsideTrigs     =
            [  "let", "where"
%%[[5
            ,  "of"
%%]]
%%[[8
            ,  "letstrict"
%%]]
            ]
%%[[9
        offsideTrigsGE   =
            [  "do"
            ]
%%]]
%%]

%%[1
hsScanOpts :: ScanOpts
hsScanOpts
  = ehScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =
                scoKeywordsTxt ehScanOpts `Set.union`
                (Set.fromList $
                       offsideTrigs
                    ++ tokKeywStrsHS1
%%[[4
                    ++ tokKeywStrsHS4
%%]]
%%[[5
                    ++ tokKeywStrsHS5
%%]]
%%[[6
                    ++ tokKeywStrsHS6
%%]]
%%[[8
                    ++ tokKeywStrsHS8
%%]]
%%[[9
                    ++ tokKeywStrsHS9
%%]]
%%[[11
                    ++ tokKeywStrsHS11
%%]]
%%[[20
                    ++ tokKeywStrsHS12
%%]]
%%[[94
                    ++ tokKeywStrsHS94
%%]]
                )
%%]
%%[1
        ,   scoKeywordsOps      =
                scoKeywordsOps ehScanOpts
                `Set.union`
                (Set.fromList $ 
                       tokOpStrsHS1
%%[[2
                    ++ tokOpStrsHS2
%%]]
%%[[3
                    ++ tokOpStrsHS3
%%]]
%%[[4
                    ++ tokOpStrsHS4
%%]]
%%[[5
                    ++ tokOpStrsHS5
%%]]
%%[[6
                    ++ tokOpStrsHS6
%%]]
%%[[7
                    ++ tokOpStrsHS7
%%]]
%%[[9
                    ++ tokOpStrsHS9
%%]]
%%[[10
                    ++ tokOpStrsHS10
%%]]
%%[[11
                    ++ tokOpStrsHS11
%%]]
                )
%%]
%%[1
        ,   scoOffsideTrigs     =
                scoOffsideTrigs ehScanOpts
                ++ offsideTrigs
        ,   scoOffsideTrigsGE   =
                scoOffsideTrigsGE ehScanOpts
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
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
%%[[9
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED"
%%]]
%%[[20
                                        , "Integer" 
%%]]
%%[[94
                                        , "foreignexport" 
%%]]
                                        ])
                                    `Set.union` scoKeywordsTxt tyScanOpts
                                    `Set.union` scoKeywordsTxt hsScanOpts
        ,   scoKeywordsOps      =   scoKeywordsOps grinScanOpts `Set.union` scoKeywordsOps hsScanOpts
        ,   scoDollarIdent      =   True
        ,   scoOpChars          =   scoOpChars grinScanOpts `Set.union` scoOpChars hsScanOpts
        ,   scoSpecChars        =   Set.fromList "!=" `Set.union` scoSpecChars grinScanOpts `Set.union` scoSpecChars hsScanOpts
        ,   scoSpecPairs        =   scoSpecPairs hsScanOpts
        }
%%]

Todo:

%%[8
tycoreScanOpts :: ScanOpts
tycoreScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
%%[[9
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED"
%%]]
%%[[20
                                        , "Integer" 
%%]]
%%[[94
                                        , "foreignexport" 
%%]]
                                        ])
        ,   scoKeywordsOps      =   Set.fromList [ "->", "=", ":", "::", "|", "\\" ]
        ,   scoSpecChars        =   Set.fromList "();{},[]"
        ,   scoOpChars          =   Set.fromList "|\\:=-<>"
        ,   scoDollarIdent      =   True
        }
%%]

                "();,[]{}`"
        ,   scoOpChars          = Set.fromList $
                "!#$%&*+/<=>?@\\^|-:.~"

%%[8
grinScanOpts :: ScanOpts
grinScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList $
                                        [ "eval", "apply"
                                        , "call"
                                        , "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi", "fetchupdate"
                                        , "throw", "try", "catch", "ctags", "applymap", "evalmap"
                                        , "C", "F", "P", "A", "R", "H", "U", "W"
                                        , "basicnode", "enumnode", "opaquenode", "ptrnode", "basicannot", "enumannot", "opaqueannot", "ptrannot"
                                        , "annotfromtaggedptr", "annottotaggedptr", "annotdflt"
                                        , "word"
                                        , "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED", "SPECIALIZED"
                                        , "_"
%%[[97
                                        , "float", "double"
%%]]
%%[[99
                                        , "True", "False"  -- for FFI annotation
%%]]
                                        ]
%%[[94
                                        ++ map show allFFIWays
%%]]
        ,   scoKeywordsOps      =   Set.fromList [ "<-", "->", "=", "+=", "-=", ":=", "-", "*" ]
        ,   scoSpecChars        =   Set.fromList "();{}#/\\|,"
        ,   scoOpChars          =   Set.fromList "<->:=+*"
        ,   scoDollarIdent      =   True
        }
%%]

%%[8
hiScanOpts :: ScanOpts
hiScanOpts
  =  hsScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "value", "fixity", "stamp", "uid", "rule", "var", "ctxt", "sup", "iddef", "arity", "grInline"
                                        , "Value", "Pat", "Type", "Kind", "Class", "Instance", "Default", "Any", "Data"
                                        , "True", "False"
                                        , "tykind", "tykinm", "tykivar"
                                        , "settings"
%%[[9
                                        , "chr", "chrstore"
                                        , "Assume", "Prove", "Reduction"
                                        , "scope"
                                        , "HasStrictCommonScope", "IsStrictParentScope", "IsVisibleInScope", "EqualScope", "NotEqualScope"
                                        , "redhowinst", "redhowsuper", "redhowprove", "redhowassume", "redhowscope", "redhoweqsym", "redhoweqtrans", "redhoweqcongr"
                                        , "varuidnmname", "varuidnmuid", "varuidnmvar"
                                        , "cxtscope1"
%%]]
%%[[10
                                        , "label", "offset"
                                        , "NonEmptyRowLacksLabel"
                                        , "redhowlabel"
%%]]
%%[[17
                                        , "typolarity"
%%]]
%%[[13
                                        , "redhowlambda"
%%]]
%%[[20
                                        , "visibleno", "visibleyes"
                                        , "importmodules"
%%]]
                                        ]
%%[[20
                                        ++ tokKeywStrsHI6
%%]]
                                    )
                                    `Set.union` scoKeywordsTxt hsScanOpts
                                    `Set.union` scoKeywordsTxt tyScanOpts
                                    `Set.union` scoKeywordsTxt grinScanOpts
        ,   scoOpChars          =   scoOpChars coreScanOpts
        ,   scoDollarIdent      =   True
        ,   scoSpecChars        =   scoSpecChars coreScanOpts
        ,   scoKeywordsOps      =   Set.fromList [ "??" ] `Set.union` scoKeywordsOps coreScanOpts
        }
%%]

%%[8
tyScanOpts :: ScanOpts
tyScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList [ "uid" ]
        }
%%]

%%[94
foreignEntScanOpts :: ScanOpts
foreignEntScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList [ "dynamic", "wrapper", "h", "static" ]
        ,   scoSpecChars        =   Set.fromList ".&"
        ,   scoDollarIdent      =   False
        , 	scoAllowQualified	=	False
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scan file/handle to tokenlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return $ scan (Set.toList $ scoKeywordsTxt opts)
                         (Set.toList $ scoKeywordsOps opts)
                         (Set.toList $ scoSpecChars opts)
                         (Set.toList $ scoOpChars opts)
                         (initPos fn) 
                  $ txt
        }
%%]

%%[5 -1.scanHandle
%%]

%%[1.offsideScanHandle
offsideScanHandle scanOpts fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        -- ;  putStrLn (" tokens: " ++ show tokens)
        ;  return (scanOffsideWithTriggers moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved (scoOffsideModule scanOpts) noPos
          oBrace    = reserved (scoOffsideOpen scanOpts) noPos
          cBrace    = reserved (scoOffsideClose scanOpts) noPos
          triggers  =  [ (Trigger_IndentGT,reserved x noPos) | x <- scoOffsideTrigs   scanOpts ]
                    ++ [ (Trigger_IndentGE,reserved x noPos) | x <- scoOffsideTrigsGE scanOpts ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Splitting up a rational into nominator/denominator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
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

intDenot2Integer :: Int -> String -> Integer
intDenot2Integer b s = getBaseNumber (toInteger b) s
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
%%[18
  pVaridUnboxedTk, pConidUnboxedTk,
  pVarsymUnboxedTk, pConsymUnboxedTk,
%%]
%%[20
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
%%[18
pVaridUnboxedTk      =   pCostValToken' 7 TkVaridUnboxed     "<identifier#>" 
pConidUnboxedTk      =   pCostValToken' 7 TkConidUnboxed     "<Identifier#>" 
pConsymUnboxedTk     =   pCostValToken' 7 TkConOpUnboxed     "<conoperator#>"
pVarsymUnboxedTk     =   pCostValToken' 7 TkOpUnboxed        "<operator#>" 
%%]
%%[20
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

%%[18
pCONIDUNBOXED, pCONSYMUNBOXED, pVARIDUNBOXED, pVARSYMUNBOXED :: IsParser p Token => p Token

pCONIDUNBOXED    = pConidUnboxedTk
pCONSYMUNBOXED   = pConsymUnboxedTk
pVARIDUNBOXED    = pVaridUnboxedTk
pVARSYMUNBOXED   = pVarsymUnboxedTk
%%]

%%[20
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
pHNm p = (hsnFromString . tokGetVal) <$> p
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
tokOpStrsHS5   = [ "<-", "..", ":" ]
%%]

%%[6
pFFORALL      ,
    pEEXISTS  
  :: IsParser p Token => p Token
%%]

%%[6
pFFORALL         = pKeyTk "Forall"
pEEXISTS         = pKeyTk "Exists"

tokKeywStrsEH6 = [  ]
tokKeywStrsHS6 = [  ]
tokKeywStrsHI6 = [ "Forall", "Exists" ]
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
    pFOREIGN    ,
    pIMPORT     ,
    pEXPORT
  :: IsParser p Token => p Token
%%]

%%[8
pLABEL           = pKeyTk "label"
pLETSTRICT       = pKeyTk "letstrict"
pSAFE            = pKeyTk "safe"
pFOREIGN         = pKeyTk "foreign"
pIMPORT          = pKeyTk "import"
pEXPORT          = pKeyTk "export"

tokKeywStrsEH8
  =  [ "letstrict", "foreign", "import" ]
%%[[(8 codegen)
  ++ map show allFFIWays
%%]]
tokKeywStrsHS8 = [ "export", "label", "safe" ]
%%]

%%[(8 codegen)
pFFIWay :: IsParser p Token => p (FFIWay,Token)
pFFIWay
  =   pAnyKey (\way -> (,) way <$> pKeyTk (show way)) allFFIWays
  <?> "pFFIWay"
%%]

%%[9
pDARROW         ,
    pLTCOLON    ,
    pOIMPL      ,
    pCIMPL      ,
    pCLASS      ,
    pINSTANCE   ,
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
pDEFAULT         = pKeyTk "default"
pDO              = pKeyTk "do"

tokKeywStrsEH9 = [ "class", "instance" ]
tokKeywStrsHS9 = [ "default", "do" ]
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

%%[20
pQUALIFIED      ,
    pQUESTQUEST ,
    pAS         ,
    pHIDING     ,
    pNUMBER     
  :: IsParser p Token => p Token
%%]

%%[20
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"
pNUMBER          = pKeyTk "#"
pQUESTQUEST      = pKeyTk "??"

tokKeywStrsEH12 = [  ]
tokKeywStrsHS12 = [ "qualified", "as", "hiding" ]
%%]

%%[95
pDERIVING   
  :: IsParser p Token => p Token

pDERIVING        = pKeyTk "deriving"

tokKeywStrsEH95 = [ "deriving" ]
%%]

%%[94
pUNSAFE     ,
    pTHREADSAFE ,
    pDYNAMIC    ,
    pWRAPPER    ,
    pSTATIC     ,
    pH          ,
    pAMPERSAND
  :: IsParser p Token => p Token

pUNSAFE          = pKeyTk "unsafe"
pTHREADSAFE      = pKeyTk "threadsafe"
pDYNAMIC         = pKeyTk "dynamic"
pWRAPPER         = pKeyTk "wrapper" -- not a HS keyword, but only for foreign function entity
pSTATIC          = pKeyTk "static" -- not a HS keyword, but only for foreign function entity
pH               = pKeyTk "h" -- not a HS keyword, but only for foreign function entity
pAMPERSAND       = pKeyTk "&" -- not a HS keyword, but only for foreign function entity

tokKeywStrsEH94 = [  ]
tokKeywStrsHS94 = [ "unsafe", "threadsafe", "dynamic" ]
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

