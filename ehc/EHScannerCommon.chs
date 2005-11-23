% $Id: EHParser.chs 276 2005-08-31 11:54:35Z atze $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module EHScannerCommon import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, EHCommon)
%%]

%%[1.Scanner import(UU.Scanner) export(module UU.Scanner)
%%]

%%[1 export(offsideScanHandle)
%%]

%%[7.Scanner -1.Scanner import(EHScanner) export(module EHScanner)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
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

%%[1
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position) 
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)
%%]

