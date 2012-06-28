module EH101.Base.GenC
( GenC
, genc
, gencCmt
, gencDeref, gencTyDeref
, gencAddrOf
, gencNULL
, gencInt
, gencAssign, gencUpdAssign
, gencCast, gencCall, gencSizeof, gencStr
, gencOp, gencOpL1Pre, gencOpL2
, gencArray, gencArrayV, gencArrayV', gencArrayAt, gencAddrArrayAt
, gencTypeDecl, gencVarDecl, gencVarDeclInit, gencVarDeclInitV
, gencStatic, gencExtern
, gencFunExtern, gencFunDef, gencFunDefArg
, gencEndsemic, gencStat
, gencSwitch, gencSwitchcase, gencSwitchcase', gencSwitchdefault
, gencLabel
, gencInclude, gencInclude'
, gencBasicSizeGBFunTyNm
, gencBasicSizeFunTyDef
, gencBasicSizeFunPrefix
, gencBasicSizeFunCall
, gencWrapperCFunDef )
where
import qualified Data.Map as Map
import Data.Bits
import Data.List
import EH.Util.Pretty
import EH101.Base.BasicAnnot


{-# LINE 26 "src/ehc/Base/GenC.chs" #-}
type GenC = PP_Doc

{-# LINE 30 "src/ehc/Base/GenC.chs" #-}
genc :: PP x => x -> GenC
genc = pp

{-# LINE 39 "src/ehc/Base/GenC.chs" #-}
gencCmt :: PP a => a -> GenC
gencCmt = ppPacked "/* " " */"

{-# LINE 48 "src/ehc/Base/GenC.chs" #-}
gencDeref :: PP a => a -> GenC
gencDeref x = "*" >|< x

gencTyDeref :: PP a => a -> GenC
gencTyDeref x = (x >|< "*")

{-# LINE 56 "src/ehc/Base/GenC.chs" #-}
-- | C address of: &(x)
gencAddrOf :: PP a => a -> GenC
gencAddrOf x = "&" >|< ppParens x


{-# LINE 67 "src/ehc/Base/GenC.chs" #-}
-- | C constant: NULL
gencNULL :: GenC
gencNULL = genc "NULL"


{-# LINE 74 "src/ehc/Base/GenC.chs" #-}
-- | C constant: int
gencInt :: (PP ty, PP int) => Maybe ty -> (GenC -> GenC) -> int -> GenC
gencInt mbTy mkL int
  = maybe i (\t -> gencCast t $ mkL i) $ mbTy
  where i = pp int


{-# LINE 87 "src/ehc/Base/GenC.chs" #-}
-- | C update assign: lval <op>= rval ;
gencUpdAssign :: (PP lval,PP rval) => String -> lval -> rval -> GenC
gencUpdAssign op lval rval = gencStat (lval >#< op >|< "=" >-< indent 1 rval)

-- | C assign: lval = rval ;
gencAssign :: (PP lval,PP rval) => lval -> rval -> GenC
gencAssign = gencUpdAssign ""


{-# LINE 102 "src/ehc/Base/GenC.chs" #-}
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

-- | C string: "str"
gencStr :: String -> GenC
gencStr = genc . show


{-# LINE 122 "src/ehc/Base/GenC.chs" #-}
-- | C binary operator expression: o e, [] variant
gencOpL1Pre :: (PP op,PP e) => op -> [e] -> GenC
gencOpL1Pre o [e] = ppParens (o >#< e)

-- | C binary operator expression: e1 o e2, [] variant
gencOpL2 :: (PP op,PP e) => op -> [e] -> GenC
gencOpL2 o [e1,e2] = ppParens (e1 >#< o >#< e2)

-- | C binary operator expression: e1 o e2
gencOp :: (PP op,PP e) => op -> e -> e -> GenC
gencOp o e1 e2 = gencOpL2 o [e1,e2]


{-# LINE 141 "src/ehc/Base/GenC.chs" #-}
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

{-# LINE 167 "src/ehc/Base/GenC.chs" #-}
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

{-# LINE 184 "src/ehc/Base/GenC.chs" #-}
-- | C modifier static: static x
gencStatic :: PP x => x -> GenC
gencStatic x = "static" >#< x

-- | C modifier static: extern x
gencExtern :: PP x => x -> GenC
gencExtern x = "extern" >#< x


{-# LINE 199 "src/ehc/Base/GenC.chs" #-}
gencFunExtern :: (PP res,PP nm) => res -> nm -> [GenC] -> GenC
gencFunExtern res nm args = gencEndsemic ("extern" >#< res >#< nm >|< ppParensCommas args)

gencFunDef :: (PP res,PP nm) => res -> nm -> [GenC] -> [GenC] -> GenC
gencFunDef res nm args exprs = res >#< nm >|< ppParensCommas args >-< ppCurlysBlock exprs

gencFunDefArg :: (PP ty,PP nm) => ty -> nm -> GenC
gencFunDefArg ty nm = ty >#< nm

{-# LINE 214 "src/ehc/Base/GenC.chs" #-}
gencEndsemic, gencStat :: PP x => x -> GenC
gencEndsemic x = x >#< ";"
gencStat = gencEndsemic

{-# LINE 224 "src/ehc/Base/GenC.chs" #-}
gencSwitch :: (PP sel) => sel -> [GenC] -> GenC -> GenC
gencSwitch sel cases dflt = "switch" >#< ppParens sel >-< ppCurlysBlock (cases ++ [dflt])

gencSwitchcase' :: PP sel => [sel] -> GenC -> GenC
gencSwitchcase' sels stat = vlist (map (>#< ":") sels) >-< indent 2 (stat >-< gencEndsemic "break")

gencSwitchcase :: PP sel => [sel] -> [GenC] -> GenC
gencSwitchcase sels stats = gencSwitchcase' (map ("case" >#<) sels) (vlist stats)

gencSwitchdefault :: GenC -> GenC
gencSwitchdefault = gencSwitchcase' ["default"]


{-# LINE 243 "src/ehc/Base/GenC.chs" #-}
gencLabel :: PP l => l -> GenC
gencLabel l = l >|< ":"

{-# LINE 252 "src/ehc/Base/GenC.chs" #-}
gencInclude' :: PP f => String -> f -> GenC
gencInclude' suff f = "#include \"" >|< f >|< (if null suff then empty else ("." >|< suff)) >|< "\""

gencInclude :: PP f => f -> GenC
gencInclude = gencInclude' ""

{-# LINE 264 "src/ehc/Base/GenC.chs" #-}
gencBasicSizeGBFunTyNm :: String -> [BasicSize] -> String
gencBasicSizeGBFunTyNm funPrefix (res:args)
  = funPrefix ++ basicGrinSizeCharEncoding res ++ show (length args) ++ concatMap basicGrinSizeCharEncoding args

{-# LINE 273 "src/ehc/Base/GenC.chs" #-}
gencBasicSizeFunTyDef :: String -> [BasicSize] -> GenC
gencBasicSizeFunTyDef funPrefix ty@(res:args)
  = gencTypeDecl (gbtyAsIs $ basicGBTy res)
                (ppParens (gencDeref (gencBasicSizeGBFunTyNm funPrefix ty)) >|< ppParensCommas (ppargs args))
  where ppargs []         = ["void"]
        ppargs args@(_:_) = map (gbtyAsIs . basicGBTy) args

{-# LINE 282 "src/ehc/Base/GenC.chs" #-}
gencBasicSizeFunPrefix :: String
gencBasicSizeFunPrefix = "gb_callc_"

{-# LINE 291 "src/ehc/Base/GenC.chs" #-}
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

{-# LINE 310 "src/ehc/Base/GenC.chs" #-}
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
            , gencStat (gencCall "GB_PassExcWith" [empty,empty,gencOp ">" "gb_ThrownException_NrOfEvalWrappers" "0",genc "return"])
            , gencStat "GB_BP_UnlinkSP"
            , gencStat (gencCall "GB_PopCastIn" ["GB_BytePtr",pcNm])
            , gencStat (gencCall "GB_PopIn" [nargsNm])
            -- , gencStat (gencCall "printf" [show $ "CallEnc pop args %d\n", nargsNm])
            , gencAssign spNm (gencCall "GB_RegByteRel" [genc "GB_Word", genc spNm, gencOp "-" (gencOp "*" (pp nargsNm) (gencSizeof "GB_Word")) (gencSizeof restyStck)])
            , gencStat (gencCall "GB_SetCallCResult" [genc restyStck, genc (gbtyAsReturned resgbty), genc spNm, genc "0", genc $ r' res])
            -- , gencStat (gencCall "printf" [genc $ show $ "TOS %lld %lld %llx " ++ r' res ++ " %d " ++ " %lld\n", gencDeref $ genc spNm, gencCall "GB_Int2GBInt" [gencDeref $ genc spNm], gencDeref $ genc spNm, genc $ r' res, genc $ r' res])
            ]
          where (cl,sz) = gencBasicSizeFunCall tyPre resArgs funcNm argsNm
                resgbty = basicGBTy res
                restyStck = gbtyOnStack resgbty
                restyAsIs = gbtyAsIs    resgbty
