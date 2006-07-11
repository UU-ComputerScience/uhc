%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module Main import(System.Environment, System.Console.GetOpt, Control.Monad.Error, Control.Monad.State)
%%]

%%[8 import({%{EH}Base.Common}, {%{EH}GrinCode})
%%]

%%[8 import (EH.Util.FPath, {%{GRIN}GRINCCommon}, {%{GRIN}CompilerDriver}, {%{EH}Base.Opts})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.main
main :: IO ()
main = doCompileGrin
%%]

%%[8.main -1.main
main :: IO ()
main
  =  do  {  args <- getArgs
         ;  let  oo@(o,n,errs)  = getOpt Permute ehcCmdLineOpts args
                 opts           = foldr ($) defaultEHCOpts o
         ;  if ehcOptHelp opts
            then  putStrLn (usageInfo "Usage: grinc [options] [file]\n\noptions:" ehcCmdLineOpts)
            else  if null errs
                  then  doCompileGrin (Left (if null n then "" else head n)) opts
                  else  mapM_ (\o -> putStr $ "grinc: " ++ o) errs
         }
%%]
