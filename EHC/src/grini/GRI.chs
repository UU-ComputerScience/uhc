% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, System.Console.GetOpt, IO)
%%]

%%[8 import(UU.Parsing, UU.Pretty, EHCommon,GRICommon,EHScanner)
%%]

%%[8 import (FPath,qualified Data.Map as Map,Data.Maybe,Data.List,Directory)
%%]

%%[8 import (GRICommon,GRIParser,GRIRun,GRISetup,GrinCode)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  putStrLn "gri: not available for this version (of ehc/gri)"
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
runGrin :: GRIOpts -> RunState -> IO RunState
runGrin opts rs
  =  do  {  rs' <- run1Step rs
         ;  case rsHalted rs' of
              Just msg
                ->  return rs'
              Nothing
                ->  do  {  if grioptDebug opts
                           then do  {  putPPLn (pp "-------")
                                    ;  prs <- ppRunState rs'
                                    ;  putPPLn prs
                                    }
                           else return ()
                        ;  runGrin opts rs'
                        }
         }
%%]

%%[8
doCompileRun :: String -> GRIOpts -> IO ()
doCompileRun fn opts
  = do { let fp = mkTopLevelFPath "grin" fn
       ; gr <- parseGrin fp opts
       ; rs <- grGriSetup gr
       ; if grioptDebug opts
         then do  {  prs <- ppRunState rs
                  ;  putPPLn prs
                  }
         else return ()
       ; rs' <- runGrin opts rs
       ; putPPLn (fromJust . rsHalted $ rs')
       }
%%]

