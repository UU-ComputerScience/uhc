% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System, GetOpt, IO, UU.Pretty, UU.Parsing, UU.Parsing.Offside, EHCommon, EHParser, EHMainAG)
%%]

%%[8 import (EHCodeJava,EHCodePretty,EHError,FPath,FiniteMap)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main, compiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute cmdLineOpts args
                 opts           = foldr ($) defaultEHCOpts o
         ;  if ehcoptHelp opts
            then  putStrLn (usageInfo "Usage ehc [options] [file]\n\noptions:" cmdLineOpts)
            else  if null errs
                  then  doCompile (if null n then "" else head n) opts
                  else  putStr (head errs)
         }
%%]

%%[doCompileA.1
  =  do  {  (fn,fh) <-  if null filename
                        then  return ("<stdin>",stdin)
                        else  do  {  h <- openFile filename ReadMode
                                  ;  return (filename,h)
                                  }
         ;  tokens <- offsideScanHandle fn fh
         ;  let steps = parseOffside (pAGItf) tokens
         ;  (res,_) <- evalStepsIO show steps
%%]

%%[doCompileB.1
         ;  let wrRes = wrap_AGItf res (Inh_AGItf {opts_Inh_AGItf = opts})
%%]

%%[doCompileB.8
         ;  let fp = mkFPath fn
                wrRes = wrap_AGItf res (Inh_AGItf {opts_Inh_AGItf = opts, baseName_Inh_AGItf = fpathBase fp})
%%]

%%[doCompileC.1
         ;  case ehcoptDumpPP opts of
              Just "pp"   ->  putStrLn (disp (pp_Syn_AGItf wrRes) 70 "")
              Just "ast"  ->  putStrLn (disp (ppAST_Syn_AGItf wrRes) 1000 "")
              _           ->  return ()
%%]

%%[doCompileC.8
         ;  let codePP = ppCode (cmodule_Syn_AGItf wrRes)
         ;  case ehcoptDumpPP opts of
              Just "pp"    ->  putStrLn (disp (pp_Syn_AGItf wrRes) 70 "")
              Just "ast"   ->  putStrLn (disp (ppAST_Syn_AGItf wrRes) 1000 "")
              Just "code"  ->  putStrLn (disp codePP 120 "")
              _            ->  return ()
%%]

%%[doCompileD.1
         ;  if ehcoptShowTopTyPP opts
            then  putStr (disp (topTyPP_Syn_AGItf wrRes) 1000 "")
            else  return ()
%%]

%%[1.doCompile
doCompile :: String -> EHCOpts -> IO ()
doCompile filename opts
%%@doCompileA.1
%%@doCompileB.1
%%@doCompileC.1
%%@doCompileD.1
         }
%%]

%%[8.doCompile -1.doCompile
doCompile :: String -> EHCOpts -> IO ()
doCompile filename opts
%%@doCompileA.1
%%@doCompileB.8
%%@doCompileC.8
%%@doCompileD.1
         ;  if ehcoptCode opts
            then  do  {  writeFile (fpathToStr (fpathSetSuff "code" fp))
                            (disp codePP 120 "")
                      }
            else  return ()
         ;  if ehcoptCodeJava opts
            then  do  {  let (jBase,jPP) = cexprJavaSrc (cmodule_Syn_AGItf wrRes)
                             jFP = fpathSetBase jBase fp
                      ;  writeFile (fpathToStr (fpathSetSuff "java" jFP))
                            (disp jPP 120 "")
                      }
            else  return ()
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data CompileUnitState
  = CUSUnknown | CUSHaskell | CUSFail
  deriving (Show,Eq)

data CompileUnit
  = CompileUnit
      { cuFilePath          :: FPath
      , cuModNm             :: HsName
      , cuOut               :: Syn_AGItf
      , cuState             :: CompileUnitState
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data CompileRunState
  = CRSOk
  | CRSFail
  | CRSFailErrL ErrL

data CompileRun
  = CompileRun
      { crCUCache       :: FiniteMap HsName CompileUnit
      , crOpts          :: EHCOpts
      , crState         :: CompileRunState
      , crIn            :: Inh_AGItf
      , crNextUID       :: UID
      , crHereUID       :: UID
      }

crHandle1 :: (a -> IO b) -> (a -> b) -> (a -> CompileRunState) -> IO a -> IO b
crHandle1 action err errs it
  = do { v <- it
       ; case errs v of
           CRSFailErrL es
             -> do { return (err v)
                   }
           CRSFail
             -> return (err v)
           CRSOk
             -> action v
       }

crSetFail :: CompileRun -> CompileRun
crSetFail cr = cr {crState = CRSFail}

crSetOk :: CompileRun -> CompileRun
crSetOk cr = cr {crState = CRSOk}

crSeq :: [CompileRun -> IO CompileRun] -> CompileRun -> IO CompileRun
crSeq []      cr = return cr
crSeq (a:as)  cr
  = crHandle1 cont crSetFail crState (a cr)
  where cont cr = crSeq as (crSetOk cr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler driver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
%%]

