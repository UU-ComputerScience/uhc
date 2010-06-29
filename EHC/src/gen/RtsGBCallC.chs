%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate C Call code for grin bytecode interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
20100126: obsolete, but still left as example code. The work described
below is now directly generated into C source files corresponding the a
HS module.

RtsGBCallC generates a C header and source file for a C calling wrapper.
Arguments must be passed between the GB and C stack. This is important
especially for float and double as these are not directly passed via the
C stack. The general solution is to let the C compiler do this job by
invoking a C function via a wrapper function which passes arguments and
result. This is not the most efficient solution, but it works, certainly
in an interpreter setting.
%%]

%%[8 module Main
%%]

%%[8 import(System, Data.Char, Data.List, Control.Monad)
%%]

%%[8 import(EH.Util.Pretty)
%%]

%%[8 import(qualified {%{EH}Config} as EHCfg)
%%]

%%[8 import({%{EH}Config},{%{EH}Base.BasicAnnot},{%{EH}Base.GenC})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
main :: IO ()
main
  = do { args <- getArgs
       ; prog <- getProgName
       ; let quit = error ("usage: " ++ prog ++ " (h|c) max-call-args")
       ; unless (length args == 2) quit
       ; let maxCCallArgs :: Int
             maxCCallArgs
               = read (args !! 1)
             resPP
               = case args !! 0 of
                   "h" -> mkH maxCCallArgs
                   "c" -> mkC maxCCallArgs
                   _   -> quit
       ; putPPLn resPP
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make it: toplevel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
mkH :: Int -> PP_Doc
mkH maxCCallArgs
  =   gencCmt maxCCallArgs
  >-< vlist [ gencBasicSizeFunTyDef "GB_CFun_" (res:args) | (_,fs) <- allFunTyL maxCCallArgs, (res,argss) <- fs, args <- argss ]
  >-< gencFunExtern "void" "gb_callc" [pp "GB_Word", pp "GB_Word"]
%%]

%%[8
mkC :: Int -> PP_Doc
mkC maxCCallArgs
  =   gencCmt maxCCallArgs
  >-< gencInclude "../rts.h"
  >-< gencInclude "../bc/interpreter.h"
  >-< gencInclude "ccall.h"
  >-< gencFunDef "void" "gb_callc" [gencFunDefArg "GB_Word" nargs, gencFunDefArg "GB_Word" callenc ]
        (  gencWrapperCFunDef "GB_CFun_" (Just nargs)
                              (pp nargs) allGrinBasicSize
                              (Right
                                ( \enc a -> gencSwitchcase enc a
                                , \as -> gencSwitch callenc as
                                                    ( gencSwitchdefault $ gencStat
                                                    $ gencCall "gb_panic1_1" [gencStr "no C gencCall for gencCall encoding", pp callenc]
                                                    )
                                , [ (show $ basicGrinSizeLEncoding ra,ra)
                                  | (_,fs) <- allFunTyL maxCCallArgs
                                  , (res,argss) <- fs
                                  , args <- argss
                                  , let ra = res : args
                                  ]
                              ) )
        )
  where r = "res_"
        r' t = r ++ (gbtyAsIs $ basicGBTy t)
        f = "func"
        szRes = "sz"
        a = "args"
        pc = "pc"
        sp = "sp"
        callenc = "callenc"
        nargs = "nargs"
        -- rw = "res_wordEquiv"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
allFunTyL :: Int -> [(Int,[(BasicSize,[[BasicSize]])])]
allFunTyL maxCCallArgs
  = [ ( nrArg
      , [ ( res
          , combine $ replicate nrArg allGrinBasicSize
          )
        | res <- allGrinBasicSize
        ]
      )
    | nrArg <- [0..maxCCallArgs]
    ]
  where combine [] = [[]]
        combine (a:as) = [ (x:xs) | x <- a, xs <- combine as]
%%]


