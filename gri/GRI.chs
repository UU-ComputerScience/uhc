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

%%[8 import(UU.Parsing, UU.Pretty, EHCommon,GRICommon,EHScanner)
%%]

%%[8 import (FPath,FiniteMap,Maybe,List,Directory)
%%]

%%[8 import (GRICommon,GRIParser,GRIRun,GRISetup,GrinCode)
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
runGrin :: GRIOpts -> RunState -> IO RunState
runGrin opts rs
  =  do  {  rs' <- run1Step rs
         ;  case rsHalted rs' of
              Just msg
                ->  return rs'
              Nothing
                ->  do  {  if grioptDebug opts
                           then do  {  putPPLn (pp "-------")
                                    ;  putPPLn (pp rs')
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
       ; let rs = grGriSetup gr
       ; if grioptDebug opts
         then putPPLn (pp rs)
         else return ()
       ; rs' <- runGrin opts rs
       ; putPPLn (fromJust . rsHalted $ rs')
       }
%%]

