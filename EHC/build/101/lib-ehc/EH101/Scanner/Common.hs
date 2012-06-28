module EH101.Scanner.Common
( module EH101.Scanner.Common
, module EH101.Scanner.Scanner )
where
import System.IO
import UU.Parsing
import UU.Parsing.Offside
import UU.Scanner.Position
import UU.Scanner.GenToken
import UU.Scanner.GenTokenParser
import EH.Util.ScanUtils ()
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts.Base
import qualified Data.Set as Set
import EH.Util.ScanUtils
import EH101.Scanner.Scanner
import EH.Util.ParseUtils
import EH101.Base.Target
import Data.Ratio





{-# LINE 49 "src/ehc/Scanner/Common.chs" #-}
ehScanOpts :: EHCOpts -> ScanOpts
ehScanOpts opts
  =  defaultScanOpts
{-# LINE 54 "src/ehc/Scanner/Common.chs" #-}
        {   scoKeywordsTxt      =
                Set.fromList $
                       tokKeywStrsEH1
                    ++ offsideTrigs
{-# LINE 60 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH4
{-# LINE 63 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH5
{-# LINE 66 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH6
{-# LINE 69 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH8
{-# LINE 72 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH9
{-# LINE 75 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH11
{-# LINE 78 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH12
{-# LINE 81 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH90
{-# LINE 84 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokKeywStrsEH91
{-# LINE 87 "src/ehc/Scanner/Common.chs" #-}
                    ++ (if ehcOptFusion opts then tokKeywStrsEH93 else [])
{-# LINE 90 "src/ehc/Scanner/Common.chs" #-}
        ,   scoKeywordsOps      =
                Set.fromList $
                       tokOpStrsEH1
{-# LINE 95 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH2
{-# LINE 98 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH3
{-# LINE 101 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH4
{-# LINE 104 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH5
{-# LINE 107 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH6
{-# LINE 110 "src/ehc/Scanner/Common.chs" #-}
                    ++ (if ehcOptExtensibleRecords opts then tokOpStrsEH7 else [])
{-# LINE 113 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH9
{-# LINE 116 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH10
{-# LINE 119 "src/ehc/Scanner/Common.chs" #-}
                    ++ tokOpStrsEH11
{-# LINE 122 "src/ehc/Scanner/Common.chs" #-}
        ,   scoSpecChars        = Set.fromList $
                "();,[]{}`"
        ,   scoOpChars          = Set.fromList $
                "!#$%&*+/<=>?@\\^|-:.~"
{-# LINE 128 "src/ehc/Scanner/Common.chs" #-}
        ,   scoSpecPairs        = Set.fromList $
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
                ,  show hsnOImpl, show hsnCImpl
                -- ,  show hsnOParensUnboxed, show hsnCParensUnboxed
                ]
{-# LINE 140 "src/ehc/Scanner/Common.chs" #-}
        ,   scoOffsideTrigs     =   offsideTrigs
        ,   scoOffsideTrigsGE   =   offsideTrigsGE
        ,   scoOffsideModule    =   "let"
        ,   scoOffsideOpen      =   "{"
        ,   scoOffsideClose     =   "}"
        }
  where offsideTrigs     =
            [  "let", "where"
            ,  "of"
            ,  "letstrict"
            ]
        offsideTrigsGE   =
            [  "do"
            ]

{-# LINE 165 "src/ehc/Scanner/Common.chs" #-}
hsScanOpts :: EHCOpts -> ScanOpts
hsScanOpts opts
  = ehScanOpts'
{-# LINE 170 "src/ehc/Scanner/Common.chs" #-}
        {   scoKeywordsTxt      =
                scoKeywordsTxt ehScanOpts' `Set.union`
                (Set.fromList $
                       offsideTrigs
                    ++ tokKeywStrsHS1
                    ++ tokKeywStrsHS4
                    ++ tokKeywStrsHS5
                    ++ tokKeywStrsHS6
                    ++ tokKeywStrsHS8
                    ++ tokKeywStrsHS9
                    ++ tokKeywStrsHS11
                    ++ tokKeywStrsHS12
                    ++ tokKeywStrsHS90
                    ++ (if ehcOptFusion opts then tokKeywStrsHS93 else [])
                )
{-# LINE 205 "src/ehc/Scanner/Common.chs" #-}
        ,   scoPragmasTxt      =
                (Set.fromList $
                       tokPragmaStrsHS99
                )
{-# LINE 211 "src/ehc/Scanner/Common.chs" #-}
        ,   scoKeywordsOps      =
                scoKeywordsOps ehScanOpts'
                `Set.union`
                (Set.fromList $
                       tokOpStrsHS1
                    ++ tokOpStrsHS2
                    ++ tokOpStrsHS3
                    ++ tokOpStrsHS4
                    ++ tokOpStrsHS5
                    ++ tokOpStrsHS6
                    ++ tokOpStrsHS7
                    ++ tokOpStrsHS9
                    ++ tokOpStrsHS10
                    ++ tokOpStrsHS11
                )
{-# LINE 246 "src/ehc/Scanner/Common.chs" #-}
        ,   scoOffsideTrigs     =
                scoOffsideTrigs ehScanOpts'
                ++ offsideTrigs
        ,   scoOffsideTrigsGE   =
                scoOffsideTrigsGE ehScanOpts'
        ,   scoOffsideModule    =   "module"
        }
  where offsideTrigs     =
            [  "where"
            ]
        ehScanOpts' = ehScanOpts opts

{-# LINE 264 "src/ehc/Scanner/Common.chs" #-}
coreScanOpts :: EHCOpts -> ScanOpts
coreScanOpts opts
  =  grinScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED"
                                        , "Integer"
                                        , "foreignexport"
                                        ])
                                    `Set.union` scoKeywordsTxt tyScanOpts
                                    `Set.union` scoKeywordsTxt hsScanOpts'
        ,   scoKeywordsOps      =   scoKeywordsOps grinScanOpts `Set.union` scoKeywordsOps hsScanOpts'
        ,   scoDollarIdent      =   True
        ,   scoOpChars          =   scoOpChars grinScanOpts `Set.union` scoOpChars hsScanOpts'
        ,   scoSpecChars        =   Set.fromList "!=" `Set.union` scoSpecChars grinScanOpts `Set.union` scoSpecChars hsScanOpts'
        ,   scoSpecPairs        =   scoSpecPairs hsScanOpts'
        }
  where hsScanOpts' = hsScanOpts opts

{-# LINE 297 "src/ehc/Scanner/Common.chs" #-}
tycoreScanOpts :: ScanOpts
tycoreScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED"
                                        , "Integer"
                                        , "foreignexport"
                                        ])
        ,   scoKeywordsOps      =   Set.fromList [ "->", "=", ":", "::", "|", "\\" ]
        ,   scoSpecChars        =   Set.fromList "();{},[]"
        ,   scoOpChars          =   Set.fromList "|\\:=-<>"
        ,   scoDollarIdent      =   True
        }

{-# LINE 328 "src/ehc/Scanner/Common.chs" #-}
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
                                        , "float", "double"
                                        , "True", "False"  -- for FFI annotation
                                        ]
                                        ++ map show allFFIWays
        ,   scoKeywordsOps      =   Set.fromList [ "<-", "->", "=", "+=", "-=", ":=", "-", "*" ]
        ,   scoSpecChars        =   Set.fromList "();{}#/\\|,"
        ,   scoOpChars          =   Set.fromList "<->:=+*"
        ,   scoDollarIdent      =   True
        }

{-# LINE 360 "src/ehc/Scanner/Common.chs" #-}
hiScanOpts :: EHCOpts -> ScanOpts
hiScanOpts opts
  =  hsScanOpts'
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "value", "fixity", "stamp", "uid", "rule", "var", "ctxt", "sup", "iddef", "arity", "grInline"
                                        , "Value", "Pat", "Type", "Kind", "Class", "Instance", "Default", "Any", "Data"
                                        , "True", "False"
                                        , "tykind", "tykinm", "tykivar"
                                        , "settings"
                                        , "chr", "chrstore"
                                        , "Assume", "Prove", "Reduction"
                                        , "scope"
                                        , "HasStrictCommonScope", "IsStrictParentScope", "IsVisibleInScope", "EqualScope", "NotEqualScope"
                                        , "redhowinst", "redhowsuper", "redhowprove", "redhowassume", "redhowscope", "redhoweqsym", "redhoweqtrans", "redhoweqcongr"
                                        , "varuidnmname", "varuidnmuid", "varuidnmvar"
                                        , "cxtscope1"
                                        , "label", "offset"
                                        , "NonEmptyRowLacksLabel"
                                        , "redhowlabel"
                                        , "typolarity"
                                        , "redhowlambda"
                                        , "visibleno", "visibleyes"
                                        , "importmodules"
                                        ]
                                        ++ tokKeywStrsHI6
                                    )
                                    `Set.union` scoKeywordsTxt hsScanOpts'
                                    `Set.union` scoKeywordsTxt tyScanOpts
                                    `Set.union` scoKeywordsTxt grinScanOpts
        ,   scoOpChars          =   scoOpChars coreScanOpts'
        ,   scoDollarIdent      =   True
        ,   scoSpecChars        =   scoSpecChars coreScanOpts'
        ,   scoKeywordsOps      =   Set.fromList [ "??" ] `Set.union` scoKeywordsOps coreScanOpts'
        }
  where hsScanOpts' = hsScanOpts opts
        coreScanOpts' = coreScanOpts opts

{-# LINE 411 "src/ehc/Scanner/Common.chs" #-}
tyScanOpts :: ScanOpts
tyScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList [ "uid" ]
        }

{-# LINE 419 "src/ehc/Scanner/Common.chs" #-}
foreignEntScanOpts :: FFIWay -> ScanOpts
foreignEntScanOpts way
  =  o {   scoKeywordsTxt      =   Set.fromList [ "dynamic", "wrapper", "h", "static", "new" ]
       ,   scoSpecChars        =   Set.fromList ",.&%[]()*{}"
       ,   scoDollarIdent      =   False
       ,   scoKeywExtraChars   =   Set.fromList wayKeywExtraChars
       ,   scoAllowQualified   =   False
       ,   scoStringDelims     =   scoStringDelims o ++ wayStringDelims
       }
  where o = defaultScanOpts
        (wayKeywExtraChars,wayStringDelims)
          = case way of
              FFIWay_JavaScript -> ("$", "'")
              _                 -> ("" , "" )


{-# LINE 458 "src/ehc/Scanner/Common.chs" #-}
splitTokensOnModuleTrigger :: ScanOpts -> [Token] -> Maybe ([Token],[Token])
splitTokensOnModuleTrigger scanOpts ts
  = case break ismod ts of
      (ts1,ts2@[]) -> Nothing
      tss          -> Just tss
  where ismod (Reserved s _) | s == scoOffsideModule scanOpts = True
        ismod _                                               = False

{-# LINE 468 "src/ehc/Scanner/Common.chs" #-}
offsideScanHandle scanOpts fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        -- ;  putStrLn (" tokens: " ++ show tokens)
        ;  case splitTokensOnModuleTrigger scanOpts tokens of
             Just (ts1,ts2) -> return $ scanLiftTokensToOffside ts1
                                      $ scanOffsideWithTriggers moduleT oBrace cBrace triggers ts2
             _              -> return $ scanOffsideWithTriggers moduleT oBrace cBrace triggers tokens
        }
  where   moduleT   = reserved (scoOffsideModule scanOpts) noPos
          oBrace    = reserved (scoOffsideOpen scanOpts) noPos
          cBrace    = reserved (scoOffsideClose scanOpts) noPos
          triggers  =  [ (Trigger_IndentGT,reserved x noPos) | x <- scoOffsideTrigs   scanOpts ]
                    ++ [ (Trigger_IndentGE,reserved x noPos) | x <- scoOffsideTrigsGE scanOpts ]

{-# LINE 492 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 517 "src/ehc/Scanner/Common.chs" #-}
pKeyTk, pKeyTk'         ::  IsParser p Token
                              => String -> p Token
pKeyTk  key             =   pCostReserved' 9 key
pKeyTk' key             =   pCostReserved' 8 key

pKeyw                   ::  (IsParser p Token,Show k) => k -> p Token
pKeyw k                 =   pKeyTk (show k)

{-# LINE 527 "src/ehc/Scanner/Common.chs" #-}
pStringTk, pCharTk,
  pInteger8Tk, pInteger10Tk, pInteger16Tk, pFractionTk,
{-# LINE 531 "src/ehc/Scanner/Common.chs" #-}
  pVaridUnboxedTk, pConidUnboxedTk,
  pVarsymUnboxedTk, pConsymUnboxedTk,
{-# LINE 535 "src/ehc/Scanner/Common.chs" #-}
  pQVaridTk, pQConidTk,
  pQVarsymTk, pQConsymTk,
{-# LINE 539 "src/ehc/Scanner/Common.chs" #-}
  pVaridTk , pConidTk ,
  pVaridTk', pConidTk',
  pTextnmTk, pTextlnTk, pIntegerTk, pVarsymTk, pConsymTk
    :: IsParser p Token => p Token

pStringTk     =   pHsCostValToken' 7 TkString    ""
pCharTk       =   pHsCostValToken' 7 TkChar      "\NUL"
pInteger8Tk   =   pHsCostValToken' 7 TkInteger8  "0"
pInteger10Tk  =   pHsCostValToken' 7 TkInteger10 "0"
pInteger16Tk  =   pHsCostValToken' 7 TkInteger16 "0"
pFractionTk   =   pHsCostValToken' 7 TkFraction  "0.0"
pVaridTk      =   pHsCostValToken' 7 TkVarid     "<identifier>"
pVaridTk'     =   pHsCostValToken' 6 TkVarid     "<identifier>"
pConidTk      =   pHsCostValToken' 7 TkConid     "<Identifier>"
pConidTk'     =   pHsCostValToken' 6 TkConid     "<Identifier>"
pConsymTk     =   pHsCostValToken' 7 TkConOp     "<conoperator>"
pVarsymTk     =   pHsCostValToken' 7 TkOp        "<operator>"
pTextnmTk     =   pHsCostValToken' 7 TkTextnm    "<name>"
pTextlnTk     =   pHsCostValToken' 7 TkTextln    "<line>"
pIntegerTk    =   pInteger10Tk
{-# LINE 561 "src/ehc/Scanner/Common.chs" #-}
pVaridUnboxedTk      =   pHsCostValToken' 7 TkVaridUnboxed     "<identifier#>"
pConidUnboxedTk      =   pHsCostValToken' 7 TkConidUnboxed     "<Identifier#>"
pConsymUnboxedTk     =   pHsCostValToken' 7 TkConOpUnboxed     "<conoperator#>"
pVarsymUnboxedTk     =   pHsCostValToken' 7 TkOpUnboxed        "<operator#>"
{-# LINE 567 "src/ehc/Scanner/Common.chs" #-}
pQVaridTk     =   pHsCostValToken' 7 TkQVarid     "<identifier>"
pQConidTk     =   pHsCostValToken' 7 TkQConid     "<Identifier>"
pQConsymTk    =   pHsCostValToken' 7 TkQConOp     "<conoperator>"
pQVarsymTk    =   pHsCostValToken' 7 TkQOp        "<operator>"

{-# LINE 578 "src/ehc/Scanner/Common.chs" #-}
pCONID, pCONID', pCONSYM, pVARID, pVARID', pVARSYM :: IsParser p Token => p Token

pCONID           = pConidTk
pCONID'          = pConidTk
pCONSYM          = pConsymTk
pVARID           = pVaridTk
pVARID'          = pVaridTk'
pVARSYM          = pVarsymTk

{-# LINE 589 "src/ehc/Scanner/Common.chs" #-}
pCONIDUNBOXED, pCONSYMUNBOXED, pVARIDUNBOXED, pVARSYMUNBOXED :: IsParser p Token => p Token

pCONIDUNBOXED    = pConidUnboxedTk
pCONSYMUNBOXED   = pConsymUnboxedTk
pVARIDUNBOXED    = pVaridUnboxedTk
pVARSYMUNBOXED   = pVarsymUnboxedTk

{-# LINE 598 "src/ehc/Scanner/Common.chs" #-}
pQCONID, pQCONSYM, pQVARID, pQVARSYM :: IsParser p Token => p Token

pQCONID          = pQConidTk
pQCONSYM         = pQConsymTk
pQVARID          = pQVaridTk
pQVARSYM         = pQVarsymTk

{-# LINE 611 "src/ehc/Scanner/Common.chs" #-}
tokGetVal :: Token -> String
tokGetVal x
  = tokenVal x

pV :: (IsParser p Token) => p Token -> p String
pV p = tokGetVal <$> p

pHNm :: (IsParser p Token) => p Token -> p HsName
pHNm p = (hsnFromString . tokGetVal) <$> p

{-# LINE 633 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 667 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 705 "src/ehc/Scanner/Common.chs" #-}
pTDOT    	,
    pQDOT
  :: IsParser p Token => p Token

{-# LINE 711 "src/ehc/Scanner/Common.chs" #-}
pTDOT            = pKeyTk "..."
pQDOT            = pKeyTk "...."

tokOpStrsEH2   = [ "...", "...." ]
tokOpStrsHS2   = [  ]

{-# LINE 719 "src/ehc/Scanner/Common.chs" #-}
tokOpStrsEH3   = [ "%" ]
tokOpStrsHS3   = [  ]

{-# LINE 724 "src/ehc/Scanner/Common.chs" #-}
pFORALL       ,
    pEXISTS   ,
    pTILDE
  :: IsParser p Token => p Token

{-# LINE 731 "src/ehc/Scanner/Common.chs" #-}
pFORALL          = pKeyTk "forall"
pEXISTS          = pKeyTk "exists"
pTILDE           = pKeyTk (show hsnEqTilde)

tokKeywStrsEH4 = [ "forall", "exists" ]
tokKeywStrsHS4 = [  ]
tokOpStrsEH4   = [ ".", show hsnEqTilde ]
tokOpStrsHS4   = [  ]

{-# LINE 742 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 756 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 774 "src/ehc/Scanner/Common.chs" #-}
pFFORALL      ,
    pEEXISTS
  :: IsParser p Token => p Token

{-# LINE 780 "src/ehc/Scanner/Common.chs" #-}
pFFORALL         = pKeyTk "Forall"
pEEXISTS         = pKeyTk "Exists"

tokKeywStrsEH6 = [  ]
tokKeywStrsHS6 = [  ]
tokKeywStrsHI6 = [ "Forall", "Exists" ]
tokOpStrsEH6   = [ "*" ]
tokOpStrsHS6   = [  ]

{-# LINE 791 "src/ehc/Scanner/Common.chs" #-}
pOROWREC        ,
    pCROWREC    ,
    pOROWROW    ,
    pCROWROW    ,
    pOROWSUM    ,
    pCROWSUM    ,
    pCOLEQUAL   ,
    pHASH
  :: IsParser p Token => p Token

{-# LINE 803 "src/ehc/Scanner/Common.chs" #-}
pOROWREC         = pKeyTk (show hsnORec)
pCROWREC         = pKeyTk (show hsnCRec)
pOROWROW         = pKeyTk (show hsnORow)
pCROWROW         = pKeyTk (show hsnCRow)
pOROWSUM         = pKeyTk (show hsnOSum)
pCROWSUM         = pKeyTk (show hsnCSum)
pCOLEQUAL        = pKeyTk ":="
pHASH            = pKeyTk "#"

{-# LINE 814 "src/ehc/Scanner/Common.chs" #-}
tokOpStrsEH7   = [ ":=", "#" ]
tokOpStrsHS7   = [  ]

{-# LINE 819 "src/ehc/Scanner/Common.chs" #-}
pLABEL          ,
    pLETSTRICT  ,
    pSAFE       ,
    pFOREIGN    ,
    pIMPORT     ,
    pEXPORT
  :: IsParser p Token => p Token

{-# LINE 829 "src/ehc/Scanner/Common.chs" #-}
pLABEL           = pKeyTk "label"
pLETSTRICT       = pKeyTk "letstrict"
pSAFE            = pKeyTk "safe"
pFOREIGN         = pKeyTk "foreign"
pIMPORT          = pKeyTk "import"
pEXPORT          = pKeyTk "export"

tokKeywStrsEH8
  =  [ "letstrict", "foreign", "import" ]
  ++ map show allFFIWays
tokKeywStrsHS8 = [ "export", "label", "safe" ]

{-# LINE 845 "src/ehc/Scanner/Common.chs" #-}
pFFIWay :: IsParser p Token => p (FFIWay,Token)
pFFIWay
  =   pAnyKey (\way -> (,) way <$> pKeyTk (show way)) allFFIWays
  <?> "pFFIWay"

{-# LINE 852 "src/ehc/Scanner/Common.chs" #-}
pDARROW         ,
    pLTCOLON    ,
    pOIMPL      ,
    pCIMPL      ,
    pCLASS      ,
    pINSTANCE   ,
    pDEFAULT    ,
    pDO
  :: IsParser p Token => p Token

{-# LINE 864 "src/ehc/Scanner/Common.chs" #-}
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

{-# LINE 880 "src/ehc/Scanner/Common.chs" #-}
tokOpStrsEH10  = [] -- [ show hsnDynVar ]
tokOpStrsHS10  = [  ]

{-# LINE 888 "src/ehc/Scanner/Common.chs" #-}
pTYPE
  :: IsParser p Token => p Token

{-# LINE 893 "src/ehc/Scanner/Common.chs" #-}
pTYPE            = pKeyTk "type"

{-# LINE 897 "src/ehc/Scanner/Common.chs" #-}
tokKeywStrsEH11 = [ "type" ]
tokKeywStrsHS11 = [  ]
tokOpStrsEH11   = [  ]
tokOpStrsHS11   = [  ]

{-# LINE 904 "src/ehc/Scanner/Common.chs" #-}
pQUALIFIED      ,
    pQUESTQUEST ,
    pAS         ,
    pHIDING     ,
    pNUMBER
  :: IsParser p Token => p Token

{-# LINE 913 "src/ehc/Scanner/Common.chs" #-}
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"
pNUMBER          = pKeyTk "#"
pQUESTQUEST      = pKeyTk "??"

tokKeywStrsEH12 = [  ]
tokKeywStrsHS12 = [ "qualified", "as", "hiding" ]

{-# LINE 924 "src/ehc/Scanner/Common.chs" #-}
pDERIVING
  :: IsParser p Token => p Token

pDERIVING        = pKeyTk "deriving"

tokKeywStrsEH91 = [ "deriving" ]

{-# LINE 933 "src/ehc/Scanner/Common.chs" #-}
pUNSAFE     ,
    pTHREADSAFE ,
    pDYNAMIC    ,
    pWRAPPER    ,
    pSTATIC     ,
    pH          ,
    pNEW        ,
    pAMPERSAND
  :: IsParser p Token => p Token

pUNSAFE          = pKeyTk "unsafe"
pTHREADSAFE      = pKeyTk "threadsafe"
pDYNAMIC         = pKeyTk "dynamic"
pWRAPPER         = pKeyTk "wrapper" -- not a HS keyword, but only for foreign function entity
pSTATIC          = pKeyTk "static" -- not a HS keyword, but only for foreign function entity
pH               = pKeyTk "h" -- not a HS keyword, but only for foreign function entity
pAMPERSAND       = pKeyTk "&" -- not a HS keyword, but only for foreign function entity
pNEW             = pKeyTk "new"

tokKeywStrsEH90  = [  ]
tokKeywStrsHS90  = [ "unsafe", "threadsafe", "dynamic" ]

{-# LINE 957 "src/ehc/Scanner/Common.chs" #-}
pFUSE         ,
    -- pWITH     ,
    pCONVERT
  :: IsParser p Token => p Token

pFUSE    = pKeyTk "fuse"
-- pWITH    = pKeyTk "with"
pCONVERT = pKeyTk "convert"

tokKeywStrsEH93  = [  ]
tokKeywStrsHS93  = [ "fuse", "convert" ]

{-# LINE 971 "src/ehc/Scanner/Common.chs" #-}
pLANGUAGE_prag  		,
	-- pOPTIONSGHC_prag  	,
	pDERIVABLE_prag		,
	pEXCLUDEIFTARGET_prag,
    pOPRAGMA    		,
    pCPRAGMA
  :: IsParser p Token => p Token

pLANGUAGE_prag   = pKeyTk "LANGUAGE"
pDERIVABLE_prag  = pKeyTk "DERIVABLE"
pEXCLUDEIFTARGET_prag  = pKeyTk "EXCLUDE_IF_TARGET"
-- pOPTIONSGHC_prag = pKeyTk "OPTIONS_GHC"
pOPRAGMA         = pKeyTk "{-#"
pCPRAGMA         = pKeyTk "#-}"

tokPragmaStrsHS99= [ "LANGUAGE", "DERIVABLE", "EXCLUDE_IF_TARGET" {-, "OPTIONS_GHC" , "INLINE", "NOINLINE", "SPECIALIZE" -} ]


