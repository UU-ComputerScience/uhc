%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generation of C
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Various C code generation snippets.
%%]

%%[(8 codegen) hs module {%{EH}Base.GenC}
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,Data.Bits, Data.List)
%%]

%%[(8 codegen) hs import(EH.Util.Pretty)
%%]

%%[(8 codegen) hs import({%{EH}Base.BasicAnnot})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level building blocks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencCmt,gencDeref,gencTyDeref)
gencCmt :: PP a => a -> PP_Doc
gencCmt = ppPacked "/* " " */"

gencDeref :: PP a => a -> PP_Doc
gencDeref x = "*" >|< x

gencTyDeref :: PP a => a -> PP_Doc
gencTyDeref x = (x >|< "*")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencCast,gencAssign,gencCall,gencSizeof,gencOp,gencStr)
gencCast :: (PP ty,PP val) => ty -> val -> PP_Doc
-- gencCast ty val = "Cast" >|< ppParensCommas [pp ty,pp val]
gencCast ty val = ppParens (ppParens ty >|< ppParens val)

gencAssign :: (PP lval,PP rval) => lval -> rval -> PP_Doc
gencAssign lval rval = gencStat (lval >#< "=" >#< rval)

gencCall :: (PP nm,PP arg) => nm -> [arg] -> PP_Doc
gencCall nm args = nm >|< ppParensCommas args

gencSizeof :: PP x => x -> PP_Doc
gencSizeof x = gencCall "sizeof" [pp x]

gencOp :: (PP op,PP e1,PP e2) => op -> e1 -> e2 -> PP_Doc
gencOp o e1 e2 = ppParens (e1 >#< o >#< e2)

gencStr :: String -> PP_Doc
gencStr = pp . show

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencFunExtern,gencFunDef,gencFunDefArg)
gencFunExtern :: (PP res,PP nm) => res -> nm -> [PP_Doc] -> PP_Doc
gencFunExtern res nm args = gencEndsemic ("extern" >#< res >#< nm >|< ppParensCommas args)

gencFunDef :: (PP res,PP nm) => res -> nm -> [PP_Doc] -> [PP_Doc] -> PP_Doc
gencFunDef res nm args exprs = res >#< nm >|< ppParensCommas args >-< ppCurlysBlock exprs

gencFunDefArg :: (PP ty,PP nm) => ty -> nm -> PP_Doc
gencFunDefArg ty nm = ty >#< nm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencEndsemic,gencStat)
gencEndsemic, gencStat :: PP x => x -> PP_Doc
gencEndsemic x = x >#< ";"
gencStat = gencEndsemic
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencSwitch,gencSwitchcase,gencSwitchcase',gencSwitchdefault)
gencSwitch :: (PP sel) => sel -> [PP_Doc] -> PP_Doc -> PP_Doc
gencSwitch sel cases dflt = "switch" >#< ppParens sel >-< ppCurlysBlock (cases ++ [dflt])

gencSwitchcase' :: PP sel => sel -> PP_Doc -> PP_Doc
gencSwitchcase' sel stat = sel >#< ":" >-< indent 2 (stat >-< gencEndsemic "break")

gencSwitchcase :: PP sel => sel -> [PP_Doc] -> PP_Doc
gencSwitchcase sel stats = gencSwitchcase' ("case" >#< sel) (vlist stats)

gencSwitchdefault :: PP_Doc -> PP_Doc
gencSwitchdefault = gencSwitchcase' "default"

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type, defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencTypedef,gencLocalvar)
gencTypedef :: (PP res,PP ty) => res -> ty -> PP_Doc
gencTypedef res ty = gencEndsemic ("typedef" >#< res >#< ty)

gencLocalvar :: (PP ty,PP var) => ty -> var -> Maybe PP_Doc -> PP_Doc
gencLocalvar ty var mbinit = gencEndsemic (ty >#< var >#< maybe empty (\i -> "=" >#< i) mbinit)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencInclude)
gencInclude :: PP f => f -> PP_Doc
gencInclude f = "#include \"" >|< f >|< "\""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate function type for [BasicSize]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencBasicSizeGBFunTyNm)
gencBasicSizeGBFunTyNm :: String -> [BasicSize] -> String
gencBasicSizeGBFunTyNm funPrefix (res:args)
  = funPrefix ++ [basicGrinSizeCharEncoding res] ++ show (length args) ++ map basicGrinSizeCharEncoding args
%%]

E.g.:
typedef GB_Word (*GB_CFun_w4wwww)(GB_Word,GB_Word,GB_Word,GB_Word);

%%[8 export(gencBasicSizeFunTyDef)
gencBasicSizeFunTyDef :: String -> [BasicSize] -> PP_Doc
gencBasicSizeFunTyDef funPrefix ty@(res:args)
  = gencTypedef (gbtyAsIs $ basicGBTy res)
                (ppParens (gencDeref (gencBasicSizeGBFunTyNm funPrefix ty)) >|< ppParensCommas (ppargs args))
  where ppargs []         = ["void"]
        ppargs args@(_:_) = map (gbtyAsIs . basicGBTy) args
%%]

%%[8 export(gencBasicSizeFunPrefix)
gencBasicSizeFunPrefix :: String
gencBasicSizeFunPrefix = "gb_callc_"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate function call for [BasicSize]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencBasicSizeFunCall)
gencBasicSizeFunCall :: (PP fun,PP argbase) => String -> [BasicSize] -> fun -> argbase -> (PP_Doc,PP_Doc)
gencBasicSizeFunCall tyPre ty@(res:args) fun argbase
  = (gencCall (gencCast ({- gencTyDeref $ -} gencBasicSizeGBFunTyNm tyPre ty) fun) (reverse argsStack),sz)
  where (argsStack,sz)
          = foldl
              (\(stk,off) bt
                 -> let t = basicGBTy bt
                    in  (gencCall "GB_RegByteRelCastx" [pp (gbtyAsIs t),pp argbase,pp off] : stk, gencOp "+" off (gencSizeof $ gbtyOnStack t))
              )
              ([],pp (0::Int)) args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate wrapper function between BC and C
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is highly specific for the BC backend.

%%[8 export(gencWrapperCFunDef)
gencWrapperCFunDef
  :: String -> (Maybe String)
  -> PP_Doc -> [BasicSize] -> Either [BasicSize] (x -> [PP_Doc] -> PP_Doc,[PP_Doc] -> PP_Doc,[(x,[BasicSize])])
  -> [PP_Doc]
gencWrapperCFunDef tyPre mbNArgsNm
                   nargs allResBasicSizes allCallArgRes
  =    [ gencLocalvar "GB_WordPtr" argsNm (Just $ gencCast "GB_WordPtr" "GB_SPRel(1)")
       , gencLocalvar "GB_Word" funcNm (Just $ pp "GB_TOS")
       ]
    ++ nargsDef
    ++ [ gencLocalvar (mkTyNm t) (r' t) Nothing | t <- allResBasicSizes ]
    ++ [ gencStat (gencCall "GB_SetTOS" [nargs])
       , gencStat (gencCall "GB_Push" [pp pcNm])
       , gencStat "GB_BP_Link"
       ]
    ++ ( case allCallArgRes of
           Left  resArgs
             -> {- [ gencStat (gencCall "printf" [show $ "CallEnc wrap " ++ gencBasicSizeGBFunTyNm "" resArgs ++ "\n"])
                , gencStat (gencCall "fflush" ["stdout"])
                ]
                ++ -} mkPost resArgs
           Right (mkAlt,mkAlts,resArgss)
             -> [mkAlts [ mkAlt x $ mkPost resArgs | (x,resArgs) <- resArgss ]]
       )
  where (nargsNm,nargsDef) = maybe (let a = "nargs" in (a,[gencLocalvar "Word" a Nothing])) (\n -> (n,[])) mbNArgsNm
        spNm = "sp"
        pcNm = "pc"
        resNm = "res_"
        funcNm = "func"
        argsNm = "args"
        r' t = resNm ++ mkTyNm t
        mkTyNm = gbtyAsIs . basicGBTy
        mkPost resArgs@(res:args)
          = [ gencAssign (r' res) cl
%%[[96
            , gencStat (gencCall "GB_PassExcWith" [empty,empty,gencOp ">" "gb_ThrownException_NrOfEvalWrappers" "0",pp "return"])
%%]]
            , gencStat "GB_BP_UnlinkSP"
            , gencStat (gencCall "GB_PopCastIn" ["GB_BytePtr",pcNm])
            , gencStat (gencCall "GB_PopIn" [nargsNm])
            -- , gencStat (gencCall "printf" [show $ "CallEnc pop args %d\n", nargsNm])
            , gencAssign spNm (gencCall "GB_RegByteRel" [pp "GB_Word", pp spNm, gencOp "-" (gencOp "*" nargsNm (gencSizeof "GB_Word")) (gencSizeof restyStck)])
            , gencStat (gencCall "GB_SetCallCResult" [pp restyStck, pp (gbtyWordEquiv resgbty), pp spNm, pp "0", pp $ r' res])
            ]
          where (cl,sz) = gencBasicSizeFunCall tyPre resArgs funcNm argsNm
                resgbty = basicGBTy res
                restyStck = gbtyOnStack resgbty
                restyAsIs = gbtyAsIs    resgbty
%%]
