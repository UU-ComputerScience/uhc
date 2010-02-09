%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate C Call code for grin bytecode interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module Main
%%]

%%[8 import(System, Data.Char, Data.List, Control.Monad)
%%]

%%[8 import(EH.Util.Pretty)
%%]

%%[8 import(qualified {%{EH}Config} as EHCfg)
%%]

%%[8 import({%{EH}Config},{%{EH}Base.BasicAnnot})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
main :: IO ()
main
  = do { args <- getArgs
       ; prog <- getProgName
       ; let quit = error ("usage: " ++ prog ++ " (h|c) max-ccall-args")
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
  =   ppCmt maxCCallArgs
  >-< vlist [ funTyDef (res:args) | (_,fs) <- allFunTyL maxCCallArgs, (res,argss) <- fs, args <- argss ]
  >-< funext "void" "gb_callc" [pp "GB_Word", pp "GB_Word"]
%%]

%%[8
mkC :: Int -> PP_Doc
mkC maxCCallArgs
  =   ppCmt maxCCallArgs
  >-< include "../rts.h"
  >-< include "../bc/interpreter.h"
  >-< include "ccall.h"
  >-< fundef "void" "gb_callc" [fundefarg "GB_Word" nargs, fundefarg "GB_Word" callenc ]
        (  [ localvar "GB_WordPtr" a (Just $ cast "GB_WordPtr" "GB_SPRel(1)")
           , localvar "GB_Word" f (Just $ pp "GB_TOS")
           , localvar "Word" szRes Nothing
           ]
        -- ++ [ localvar "GB_WordEquiv" rw Nothing ]
        ++ [ localvar (gbtyAsIs $ basicGBTy t) (r' t) Nothing | t <- allGrinBasicSize ]
        ++ [ stat (call "GB_SetTOS" [pp nargs])
           , stat (call "GB_Push" [pp pc])
           , stat "GB_BP_Link"
           , switch callenc
               [ switchcase (show $ basicGrinSizeLEncoding ra)
                   (let (cl,sz) = funCCall ra f a
                        resgbty = basicGBTy res
                        restyStck = gbtyOnStack resgbty
                        restyAsIs = gbtyAsIs    resgbty
                    in  [ {- stat (call "GB_Push" [call "GB_Word_MkGC" [nargs >|< "+3 /* nargs, PC, MP */"]])
                        , -}
                          assign (r' res) cl
%%[[96
                        , stat (call "GB_PassExcWith" [empty,empty,op ">" "gb_ThrownException_NrOfEvalWrappers" "0",pp "return"])
%%]]
                        , stat "GB_BP_UnlinkSP"
                        , stat (call "GB_PopCastIn" ["GB_BytePtr",pc])
                        , stat (call "GB_PopIn" [nargs])
                        , assign szRes (op "-" (op "*" nargs (sizeof "GB_Word")) (sizeof restyStck))
                        , assign sp (call "GB_RegByteRel" [pp "GB_Word", pp sp, pp szRes])
                        , stat (call "GB_SetCallCResult" [pp restyStck, pp (gbtyWordEquiv resgbty), pp sp, pp "0", pp $ r' res])
                        ]
                   )
               | (_,fs) <- allFunTyL maxCCallArgs
               , (res,argss) <- fs
               , args <- argss
               , let ra = res : args
               ]
               (switchdefault $ stat $ call "gb_panic1_1" [str "no C call for call encoding", pp callenc])
           ]
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% C constructs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
funext :: (PP res,PP nm) => res -> nm -> [PP_Doc] -> PP_Doc
funext res nm args = endsemic ("extern" >#< res >#< nm >|< ppParensCommas args)

fundef :: (PP res,PP nm) => res -> nm -> [PP_Doc] -> [PP_Doc] -> PP_Doc
fundef res nm args exprs = res >#< nm >|< ppParensCommas args >-< ppCurlysBlock exprs

fundefarg :: (PP ty,PP nm) => ty -> nm -> PP_Doc
fundefarg ty nm = ty >#< nm

endsemic, stat :: PP x => x -> PP_Doc
endsemic x = x >#< ";"
stat = endsemic

typedef :: (PP res,PP ty) => res -> ty -> PP_Doc
typedef res ty = endsemic ("typedef" >#< res >#< ty)

cast :: (PP ty,PP val) => ty -> val -> PP_Doc
cast ty val = "Cast" >|< ppParensCommas [pp ty,pp val]

assign :: (PP lval,PP rval) => lval -> rval -> PP_Doc
assign lval rval = stat (lval >#< "=" >#< rval)

call :: (PP nm,PP arg) => nm -> [arg] -> PP_Doc
call nm args = nm >|< ppParensCommas args

sizeof :: PP x => x -> PP_Doc
sizeof x = call "sizeof" [pp x]

op :: (PP op,PP e1,PP e2) => op -> e1 -> e2 -> PP_Doc
op o e1 e2 = ppParens (e1 >#< o >#< e2)

switch :: (PP sel) => sel -> [PP_Doc] -> PP_Doc -> PP_Doc
switch sel cases dflt = "switch" >#< ppParens sel >-< ppCurlysBlock (cases ++ [dflt])

switchcase' :: PP sel => sel -> PP_Doc -> PP_Doc
switchcase' sel stat = sel >#< ":" >-< indent 2 (stat >-< endsemic "break")

switchcase :: PP sel => sel -> [PP_Doc] -> PP_Doc
switchcase sel stats = switchcase' ("case" >#< sel) (vlist stats)

switchdefault :: PP_Doc -> PP_Doc
switchdefault = switchcase' "default"

str :: String -> PP_Doc
str = pp . show

localvar :: (PP ty,PP var) => ty -> var -> Maybe PP_Doc -> PP_Doc
localvar ty var mbinit = endsemic (ty >#< var >#< maybe empty (\i -> "=" >#< i) mbinit)

include :: PP f => f -> PP_Doc
include f = "#include \"" >|< f >|< "\""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
funCCall :: (PP fun,PP argbase) => [BasicSize] -> fun -> argbase -> (PP_Doc,PP_Doc)
funCCall ty@(res:args) fun argbase
  = (call (cast ({- ppTyDrf $ -} funTyNm ty) fun) (reverse argsStack),sz)
  where (argsStack,sz)
          = foldl
              (\(stk,off) bt
                 -> let t = basicGBTy bt
                    in  (call "GB_RegByteRelCastx" [pp (gbtyAsIs t),pp argbase,pp off] : stk, op "+" off (sizeof $ gbtyOnStack t))
              )
              ([],pp (0::Int)) args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
funTyNm :: [BasicSize] -> String
funTyNm (res:args)
  = "GB_CFun_" ++ [basicGrinSizeCharEncoding res] ++ show (length args) ++ map basicGrinSizeCharEncoding args
%%]

E.g.:
typedef GB_Word (*GB_CFun_w4wwww)(GB_Word,GB_Word,GB_Word,GB_Word);

%%[8
funTyDef :: [BasicSize] -> PP_Doc
funTyDef ty@(res:args)
  = typedef (gbtyAsIs $ basicGBTy res) (ppParens (ppDrf (funTyNm ty)) >|< ppParensCommas (ppargs args))
  where ppargs []         = ["void"]
        ppargs args@(_:_) = map (gbtyAsIs . basicGBTy) args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PP Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
ppCmt :: PP a => a -> PP_Doc
ppCmt = ppPacked "/* " " */"

ppDrf :: PP a => a -> PP_Doc
ppDrf x = "*" >|< x

ppTyDrf :: PP a => a -> PP_Doc
ppTyDrf x = (x >|< "*")
%%]

