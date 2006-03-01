module RulerScanner
 ( module RulerScannerMachine
 , module UU.Scanner.Token
 , module UU.Scanner.TokenParser
 , module UU.Scanner.Position
 
 , SPos, emptySPos

 , scanFile, scanHandle
 , offsideScanHandle
 )
 where

import IO
import RulerScannerMachine
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.Position
import ScanUtils
-- import UUTest.Parsing.Offside
import UU.Parsing.Offside

-- instances
import UU.Scanner.TokenShow()
import UU.Scanner.GenTokenOrd()
import UU.Scanner.GenTokenSymbol()

-------------------------------------------------------------------------
-- Symbol position
-------------------------------------------------------------------------

type SPos = (String,Pos)

emptySPos = ("",noPos)

-------------------------------------------------------------------------
-- Wrappers around scan
-------------------------------------------------------------------------

scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return (scan opts (initPos fn) txt) 
        }

scanFile :: ScanOpts -> FilePath -> IO [Token]
scanFile opts fn = 
        do txt <- readFile fn
           return (scan opts (initPos fn) txt) 

offsideScanHandle :: ScanOpts -> FilePath -> Handle -> IO (OffsideInput [Token] Token (Maybe Token))
offsideScanHandle opts fn fh
  = do  {  tokens <- scanHandle opts fn fh
        ;  return (scanOffside moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved (scoOffsideModule opts) noPos
          oBrace    = reserved (scoOffsideOpen opts) noPos
          cBrace    = reserved (scoOffsideClose opts) noPos
          triggers  = [ reserved x noPos | x <- scoOffsideTrigs opts ]
