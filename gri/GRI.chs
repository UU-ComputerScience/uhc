% $Id: GRI.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO)
%%]

%%[8 import(UU.Parsing, GRICommon,EHScanner)
%%]

%%[8 import (GRICommon,GRIParser,FPath,FiniteMap,Maybe,List,Directory)
%%]

%%[8 import (GrinCode)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  putStrLn "gri: not available at this version (of gri)"
%%]

%%[8.main -1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute griCmdLineOpts args
                 opts           = foldr ($) defaultGRIOpts o
         ;  if grioptHelp opts
            then  putStrLn (usageInfo "Usage: gri [options] [file]\n\noptions:" griCmdLineOpts)
            else  if null errs
                  then  doCompileRun (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interpreter driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
parseGrin :: FPath -> GRIOpts -> IO GrModule
parseGrin fp opts
  = do {  let fNm    = fpathToStr fp
       ;  (fn,fh) <-  if fpathIsEmpty fp
                      then  return ("<stdin>",stdin)
                      else  do  {  h <- openFile fNm ReadMode
                                ;  return (fNm,h)
                                }
       ;  tokens <- scanHandle scanOpts fn fh
       ;  gr <- parseIO (pModule) tokens
       ;  return gr
       }
%%]

%%[8
doCompileRun :: String -> GRIOpts -> IO ()
doCompileRun fn opts
  = do { let fp             = mkTopLevelFPath "grin" fn
       ; gr <- parseGrin fp opts
       ; return ()
       }
%%]

