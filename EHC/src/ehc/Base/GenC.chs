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
%%% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(GenC)
type GenC = PP_Doc
%%]

%%[8 export(genc)
genc :: PP x => x -> GenC
genc = pp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% General: comment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencCmt)
gencCmt :: PP a => a -> GenC
gencCmt = ppPacked "/* " " */"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pointer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencDeref,gencTyDeref)
gencDeref :: PP a => a -> GenC
gencDeref x = "*" >|< x

gencTyDeref :: PP a => a -> GenC
gencTyDeref x = (x >|< "*")
%%]

%%[8 export(gencAddrOf)
-- | C address of: &(x)
gencAddrOf :: PP a => a -> GenC
gencAddrOf x = "&" >|< ppParens x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencNULL)
-- | C constant: NULL
gencNULL :: GenC
gencNULL = genc "NULL"

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencAssign,gencUpdAssign)
-- | C update assign: lval <op>= rval ;
gencUpdAssign :: (PP lval,PP rval) => String -> lval -> rval -> GenC
gencUpdAssign op lval rval = gencStat (lval >#< op >|< "=" >#< rval)

-- | C assign: lval = rval ;
gencAssign :: (PP lval,PP rval) => lval -> rval -> GenC
gencAssign = gencUpdAssign ""

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expressions: general
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencCast,gencCall,gencSizeof,gencOp,gencStr)
-- | C cast: ((ty)(val))
gencCast :: (PP ty,PP val) => ty -> val -> GenC
-- gencCast ty val = "Cast" >|< ppParensCommas [genc ty,genc val]
gencCast ty val = ppParens (ppParens ty >|< ppParens val)

-- | C call: nm( args )
gencCall :: (PP nm,PP arg) => nm -> [arg] -> GenC
gencCall nm args = nm >|< ppParensCommas args

-- | C sizeof: sizeof( x )
gencSizeof :: PP x => x -> GenC
gencSizeof x = gencCall "sizeof" [genc x]

-- | C binary operator expression: e1 o e2
gencOp :: (PP op,PP e1,PP e2) => op -> e1 -> e2 -> GenC
gencOp o e1 e2 = ppParens (e1 >#< o >#< e2)

-- | C string: "str"
gencStr :: String -> GenC
gencStr = genc . show

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expressions: array
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencArray,gencArrayV,gencArrayV',gencArrayAt,gencAddrArrayAt)
-- | C array: { ... }
gencArray :: PP a => [a] -> GenC
gencArray = ppCurlysCommasBlock

-- | C array, rendered as list, with termination: { vals } term
gencArrayV' :: PP a => String -> [a] -> [GenC]
gencArrayV' term vals = ppBlock' "{ " ("}" ++ term) ", " vals

-- | C array, rendered as list: { ... }
gencArrayV :: PP a => [a] -> [GenC]
gencArrayV = gencArrayV' ""

-- | C array indexing, vertically rendered: a[o]
gencArrayAt :: (PP a,PP o) => a -> o -> GenC
gencArrayAt a o = a >|< "[" >|< o >|< "]"

-- | C address of: &a[o]
gencAddrArrayAt :: (PP a,PP o) => a -> o -> GenC
gencAddrArrayAt a o = gencAddrOf $ gencArrayAt a o
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Declarations: type, value defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencTypeDecl,gencVarDecl,gencVarDeclInit,gencVarDeclInitV)
gencTypeDecl :: (PP res,PP ty) => res -> ty -> GenC
gencTypeDecl res ty = gencEndsemic ("typedef" >#< res >#< ty)

-- | C value def, with optional init: ty var ;
gencVarDecl :: (PP ty,PP var) => ty -> var -> GenC
gencVarDecl ty var = gencEndsemic (ty >#< var)

-- | C value def, with init: ty var = init
gencVarDeclInit :: (PP ty,PP var,PP init) => ty -> var -> init -> GenC
gencVarDeclInit ty var init = gencEndsemic (ty >#< var >#< "=" >#< init)

-- | C value def, with init, vertically rendered: ty var = init
gencVarDeclInitV :: (PP ty,PP var,PP init) => ty -> var -> init -> GenC
gencVarDeclInitV ty var init = gencEndsemic (ty >#< var >#< "=" >-< indent 2 init)
%%]

%%[8 export(gencStatic,gencExtern)
-- | C modifier static: static x
gencStatic :: PP x => x -> GenC
gencStatic x = "static" >#< x

-- | C modifier static: extern x
gencExtern :: PP x => x -> GenC
gencExtern x = "extern" >#< x

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Declarations: functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencFunExtern,gencFunDef,gencFunDefArg)
gencFunExtern :: (PP res,PP nm) => res -> nm -> [GenC] -> GenC
gencFunExtern res nm args = gencEndsemic ("extern" >#< res >#< nm >|< ppParensCommas args)

gencFunDef :: (PP res,PP nm) => res -> nm -> [GenC] -> [GenC] -> GenC
gencFunDef res nm args exprs = res >#< nm >|< ppParensCommas args >-< ppCurlysBlock exprs

gencFunDefArg :: (PP ty,PP nm) => ty -> nm -> GenC
gencFunDefArg ty nm = ty >#< nm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Statements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencEndsemic,gencStat)
gencEndsemic, gencStat :: PP x => x -> GenC
gencEndsemic x = x >#< ";"
gencStat = gencEndsemic
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencSwitch,gencSwitchcase,gencSwitchcase',gencSwitchdefault)
gencSwitch :: (PP sel) => sel -> [GenC] -> GenC -> GenC
gencSwitch sel cases dflt = "switch" >#< ppParens sel >-< ppCurlysBlock (cases ++ [dflt])

gencSwitchcase' :: PP sel => sel -> GenC -> GenC
gencSwitchcase' sel stat = sel >#< ":" >-< indent 2 (stat >-< gencEndsemic "break")

gencSwitchcase :: PP sel => sel -> [GenC] -> GenC
gencSwitchcase sel stats = gencSwitchcase' ("case" >#< sel) (vlist stats)

gencSwitchdefault :: GenC -> GenC
gencSwitchdefault = gencSwitchcase' "default"

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(gencInclude)
gencInclude :: PP f => f -> GenC
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
gencBasicSizeFunTyDef :: String -> [BasicSize] -> GenC
gencBasicSizeFunTyDef funPrefix ty@(res:args)
  = gencTypeDecl (gbtyAsIs $ basicGBTy res)
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
gencBasicSizeFunCall :: (PP fun,PP argbase) => String -> [BasicSize] -> fun -> argbase -> (GenC,GenC)
gencBasicSizeFunCall tyPre ty@(res:args) fun argbase
  = (gencCall (gencCast ({- gencTyDeref $ -} gencBasicSizeGBFunTyNm tyPre ty) fun) (reverse argsStack),sz)
  where (argsStack,sz)
          = foldl
              (\(stk,off) bt
                 -> let t = basicGBTy bt
                    in  (gencCall "GB_RegByteRelCastx" [genc (gbtyAsIs t),genc argbase,genc off] : stk, gencOp "+" off (gencSizeof $ gbtyOnStack t))
              )
              ([],genc (0::Int)) args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generate wrapper function between BC and C
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is highly specific for the BC backend.

%%[8 export(gencWrapperCFunDef)
gencWrapperCFunDef
  :: String -> (Maybe String)
  -> GenC -> [BasicSize] -> Either [BasicSize] (x -> [GenC] -> GenC,[GenC] -> GenC,[(x,[BasicSize])])
  -> [GenC]
gencWrapperCFunDef tyPre mbNArgsNm
                   nargs allResBasicSizes allCallArgRes
  =    [ gencVarDeclInit "GB_WordPtr" argsNm (gencCast "GB_WordPtr" "GB_SPRel(1)")
       , gencVarDeclInit "GB_Word" funcNm "GB_TOS"
       ]
    ++ nargsDef
    ++ [ gencVarDecl (mkTyNm t) (r' t) | t <- allResBasicSizes ]
    ++ [ gencStat (gencCall "GB_SetTOS" [nargs])
       , gencStat (gencCall "GB_Push" [genc pcNm])
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
  where (nargsNm,nargsDef) = maybe (let a = "nargs" in (a,[gencVarDecl "Word" a])) (\n -> (n,[])) mbNArgsNm
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
            , gencStat (gencCall "GB_PassExcWith" [empty,empty,gencOp ">" "gb_ThrownException_NrOfEvalWrappers" "0",genc "return"])
%%]]
            , gencStat "GB_BP_UnlinkSP"
            , gencStat (gencCall "GB_PopCastIn" ["GB_BytePtr",pcNm])
            , gencStat (gencCall "GB_PopIn" [nargsNm])
            -- , gencStat (gencCall "printf" [show $ "CallEnc pop args %d\n", nargsNm])
            , gencAssign spNm (gencCall "GB_RegByteRel" [genc "GB_Word", genc spNm, gencOp "-" (gencOp "*" nargsNm (gencSizeof "GB_Word")) (gencSizeof restyStck)])
            , gencStat (gencCall "GB_SetCallCResult" [genc restyStck, genc (gbtyWordEquiv resgbty), genc spNm, genc "0", genc $ r' res])
            ]
          where (cl,sz) = gencBasicSizeFunCall tyPre resArgs funcNm argsNm
                resgbty = basicGBTy res
                restyStck = gbtyOnStack resgbty
                restyAsIs = gbtyAsIs    resgbty
%%]
