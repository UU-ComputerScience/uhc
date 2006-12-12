%%[1 hs module (Scanner)
%%]

%%[1 hs export (module ScannerMachine)
%%]

%%[1 hs export (module UU.Scanner.Token)
%%]

%%[1 hs export (module UU.Scanner.TokenParser)
%%]

%%[1 hs export (module UU.Scanner.Position)
%%]

%%[1 hs export (SPos, emptySPos)
%%]

%%[1 hs export (scanFile, scanHandle)
%%]

%%[1 hs export (offsideScanHandle)
%%]

%%[1 hs import (IO)
%%]

%%[1 hs import (ScannerMachine)
%%]

%%[1 hs import (UU.Scanner.Token)
%%]

%%[1 hs import (UU.Scanner.TokenParser)
%%]

%%[1 hs import (UU.Scanner.Position)
%%]

%%[1 hs import (EH.Util.ScanUtils)
%%]

%%[1 hs import (UU.Parsing.Offside)
%%]

-- instances
%%[1 hs import (UU.Scanner.TokenShow())
%%]

%%[1 hs import (UU.Scanner.GenTokenOrd())
%%]

%%[1 hs import (UU.Scanner.GenTokenSymbol())
%%]


%%[1 hs

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

%%]
