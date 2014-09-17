%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for dealing with FFI, for now part of Core, meant for Grin gen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.FFI} import({%{EH}Base.HsName.Builtin},{%{EH}Base.Builtin2},{%{EH}Opts},{%{EH}Base.Target},{%{EH}Base.Common},{%{EH}Base.TermLike})
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,Data.List,Data.Maybe)
%%]
%%[(8 codegen hmtyinfer) hs import({%{EH}Base.BasicAnnot},{%{EH}Ty},{%{EH}Gam.DataGam})
%%]
%%[(8 codegen) hs import({%{EH}AbstractCore})
%%]
%%[(8 codegen) hs import({%{EH}Core},{%{EH}Core.Utils})
%%]
%%[(8 codegen) hs import(qualified {%{EH}Core.SysF.AsTy} as SysF)
%%]
%%[(8 codegen) hs import({%{EH}GrinCode})
%%]
%%[(8 codegen) hs import({%{EH}Foreign.Extract}, {%{EH}Foreign.Boxing})
%%]
%%[(90 codegen) hs import({%{EH}BuiltinPrims})
%%]

-- debug
%%[(8888 codegen) import({%{EH}Base.Debug},UHC.Util.Pretty)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Builtin info about (un)boxedness
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(tyNmFFIBoxBasicAnnot)
-- | is ty going to be passed unboxed to ffi, return info about it if so?
tyNmFFIBoxBasicAnnot :: EHCOpts -> HsName -> Maybe BasicAnnot
tyNmFFIBoxBasicAnnot opts
  = \n -> fmap biGrinBoxAnnot $ m n
  where m = builtinGrinInfo opts

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates: Grin bytecode specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(tyNmGBMayLiveAsTaggedPtr,tyNmGBTagPtrBasicAnnot)
-- | is ty living as a tagged pointer?
tyNmGBMayLiveAsTaggedPtr :: EHCOpts -> HsName -> Maybe BuiltinInfo
tyNmGBMayLiveAsTaggedPtr opts
  | targetIsGrinBytecode (ehcOptTarget opts) = builtinGrinInfo opts
  | otherwise                                = const Nothing

-- | BasicAnnot when unboxing also means living as tagged pointer
tyNmGBTagPtrBasicAnnot :: EHCOpts -> Bool -> HsName -> BasicAnnot -> BasicAnnot
tyNmGBTagPtrBasicAnnot opts box t annot
  = case tyNmGBMayLiveAsTaggedPtr opts t of
      Just x  -> if biGbcMayLiveUnboxed x
                 then annot {baTagging = if box then BasicAnnotTagging_ToPtr else BasicAnnotTagging_FromPtr, baIsSigned = biIsSigned x} 
                 else annot
      Nothing -> annot
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct code fragments based on FFI info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(ffiMkArgUnpack)
-- | make argument, i.e. wrap given argument name in proper introduction with annotation about what it is
ffiMkArgUnpack
  :: EHCOpts
     -> DataGam
     -> (HsName -> BasicAnnot -> HsName -> intro)     -- make intro: node around basic type
     -> (HsName -> HsName -> intro)             -- make intro: enum
     -> (HsName -> HsName -> intro)             -- make intro: var
     -> (HsName -> HsName -> intro)             -- make intro: opaque
     -> (HsName -> HsName -> intro)             -- make intro: pointer
     -> HsName                              -- arg name
     -> Ty                                  -- its type
     -> intro
ffiMkArgUnpack
     opts dataGam
     mkNodeI mkEnumI mkVarI mkOpaqI mkPtrI
     argNm ty
  = mk
  where tyNm  = tyAppFunConNm ty
        mbAnn = tyNmFFIBoxBasicAnnot opts tyNm
        mk | isJust mbAnn                   = mkNodeI tyNm (tyNmGBTagPtrBasicAnnot opts False tyNm (fromJust mbAnn)) argNm
           | tyNmIsFFIEnumable dataGam tyNm = mkEnumI tyNm argNm
           | isJust (recMbRecRow ty)        = mkVarI  tyNm argNm
           | tyNmIsFFIOpaque dataGam tyNm   = mkOpaqI tyNm argNm
           | otherwise                      = mkPtrI  tyNm argNm
%%]

%%[(8 codegen grin) hs export(ffiGrinMkArgUnpack)
-- | make argument, specialization of ffiMkArgUnpack for GrinCode
ffiGrinMkArgUnpack
  :: EHCOpts
     -> DataGam
     -> HsName                              -- arg name
     -> Ty                                  -- its type
     -> GrPatLam
ffiGrinMkArgUnpack
     opts dataGam
     argNm ty
  | ehcOptGenBoxGrin opts
    = let unbox t n = GrPatLam_Box (tyNm2Boxing opts dataGam t) n
      in  ffiMkArgUnpack
             opts dataGam
             (\t _ n -> unbox t n) unbox (const GrPatLam_Var) unbox unbox
             argNm ty
  | otherwise
    = ffiMkArgUnpack
         opts dataGam
         (const GrPatLam_BasicNode) (const GrPatLam_EnumNode) (const GrPatLam_Var) (const GrPatLam_OpaqueNode) (const GrPatLam_PtrNode)
         argNm ty
%%]

%%[(8 codegen) hs export(ffiMkResPack)
-- | make result, i.e. wrap given argument name in proper adaption with annotation about what it is
-- Note: 0-tuple is assumed to be Enumerable (change this here, and in the RTS, if the 0-tuple is to be regarded as a basis for extensible rows)
ffiMkResPack
  :: EHCOpts
     -> DataGam
     -> (BasicAnnot -> HsName -> intro)     -- make intro: node around basic type
     -> (HsName -> HsName -> intro)         -- make intro: enum
     -> (HsName -> intro)                   -- make intro: opaque
     -> (HsName -> HsName -> intro)         -- make intro: pointer
     -> (e -> intro -> e -> e)              -- make bind: let .. in
     -> (Ty -> HsName -> HsName -> e)       -- make expr: node
     -> (Ty -> HsName -> HsName -> e)       -- make expr: enum
     -> (Ty -> HsName -> HsName -> e)       -- make expr: opaq
     -> (Ty -> HsName -> HsName -> e)       -- make expr: ptr
     -> HsName                              -- arg name
     -> Ty                                  -- its type
     -> e                                   -- res value
     -> e
ffiMkResPack
     opts dataGam
     mkNodeI mkEnumI mkOpaqI mkPtrI
     mkBindE
     mkNodeE mkEnumE mkOpaqE mkPtrE
     resNm resTy res
  = mk
  where resTyNm  = tyAppFunConNm resTy
        mbAnn = tyNmFFIBoxBasicAnnot opts resTyNm
        mkE e = e resTy resTyNm resNm
        mk | isJust mbAnn                   	= mkBindE res (mkNodeI (tyNmGBTagPtrBasicAnnot opts True resTyNm (fromJust mbAnn)) resNm) (mkE mkNodeE)
           | tyNmIsFFIEnumable dataGam resTyNm	= mkBindE res (mkEnumI resTyNm                                                     resNm) (mkE mkEnumE)
           | isRec && arity == 0            	= mkBindE res (mkEnumI recNm                                                       resNm) (mkE mkEnumE)
           | isRec                          	= mkBindE res (mkPtrI  recNm                                                       resNm) (mkE mkPtrE )
           | tyNmIsFFIOpaque dataGam resTyNm    = mkBindE res (mkOpaqI                                                             resNm) (mkE mkOpaqE)
           | otherwise                      	= mkBindE res (mkPtrI  resTyNm                                                     resNm) (mkE mkPtrE )
           where isRec = isJust $ recMbRecRow resTy
                 arity = length $ snd $ tyRecExts resTy
                 recNm = builtinRecNm arity
%%]

%%[(8 codegen grin) hs export(ffiGrinMkResPack)
-- | make result, specialization of ffiMkResPack for GrinCode
ffiGrinMkResPack
  :: EHCOpts
     -> DataGam
     -> HsName                              -- arg name
     -> Ty                                  -- its type
     -> GrExpr                              -- res value
     -> GrExpr
ffiGrinMkResPack
     opts dataGam
     resNm resTy res
  {- 
  -}
  -- 20130910 AD: not completed
  | ehcOptGenBoxGrin opts
    = let box _ tn n = u $ GrVal_Box (tyNm2Boxing opts dataGam tn) (GrVal_Var n)
      in  ffiMkResPack
             opts dataGam
             (const GrPatLam_Var) (const GrPatLam_Var) GrPatLam_Var (const GrPatLam_Var)
             GrExpr_Seq
             box box box box
             resNm resTy res
  | otherwise
    = ffiMkResPack
         opts dataGam
         GrPatLam_BasicAnnot GrPatLam_EnumAnnot {- GrPatLam_Var -} GrPatLam_OpaqueAnnot GrPatLam_PtrAnnot
         GrExpr_Seq
         (\_ tyNm n -> u (GrVal_BasicNode (GrTag_Con (mkGrTagAnn 1 1) 0 (mkTyIsConTagInfo tyNm)) n))
         (\_ _    n -> u (GrVal_EnumNode                                                         n))
         (\_ _    n -> u (GrVal_OpaqueNode                                                       n))
         (\_ _    n -> u (GrVal_PtrNode                                                          n))
         resNm resTy res
  where u | ehcOptGenBoxGrin opts = GrExpr_Store
          | otherwise             = flip GrExpr_Unit GrType_None
%%]

%%[(8 codegen grin) export(ffiGrinMk)
-- | make ffi call, specifically for Grin
ffiGrinMk
  :: EHCOpts
     -> DataGam
     -> UID
     -> HsName
%%[[8
     -> String					-- name
%%][90
     -> FFIWay					-- calling convention
     -> ForeignEnt				-- reference to the foreign entity
%%]]
%%[[99
     -> Bool					-- result is evaluated
%%]]
     -> [(GrVal,Ty)]			-- arguments info
     -> Ty						-- result info
     -> GrExpr
ffiGrinMk
     opts dataGam uniq modNm
%%[[8
     impEnt
%%][90
     callconv
     impEnt
%%]]
%%[[99
     isEvaluated
%%]]
     args resTy
  = foldr (\((v1,ty),n2) e 
             -> GrExpr_Seq (GrExpr_Unit v1 GrType_None)
                           (ffiGrinMkArgUnpack opts dataGam n2 ty)
                           e
          )
          (ffiGrinMkResPack opts dataGam nmRes resTy
%%[[8
              (GrExpr_FFI impEnt
%%][90
              (GrExpr_FFI callconv impEnt
%%]]
%%[[99
                          (GrFFIAnnot_IsResEval isEvaluated)
%%]]
                          (map GrVal_Var nmArgPatL)
          )   )
          (zip args nmArgPatL)
  where (nmRes:nmArgPatL) = take (length args + 1) (map (uidQualHNm modNm) (iterate uidNext uniq))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates: is result type an IO?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat.IO doclatex
We deal with IO as follow:
\begin{itemize}
\item for f defined as FFI with type f :: a -> IO r, we transform the interface and internal wrapping of f = \a -> r into f2 = \a s -> (s, r),
  the wrapping is similar to the one done in EHC.Prelude.ioFromPrim.
\item when f :: .. -> IO (), the ffi call result is discarded and () returned instead.
\item the extra parameter s is left unevaluated.
\end{itemize}
%%]

%%[(98 codegen) hs export(ffiMbIORes)
-- | is type an IO type, if so return the IO type argument (result returned by IO)
ffiMbIORes :: EHCOpts -> Ty -> Maybe Ty
ffiMbIORes opts resTy
  = case appMbConApp resTy of
      Just (n,[a]) | ehcOptBuiltin opts ehbnIO == n
        -> Just a
      _ -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO adaption of FFI call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(98 codegen) hs export(ffiIOAdapt)
-- | adapt type etc for IO ffi call
ffiIOAdapt
  :: EHCOpts
     -> (UID -> HsName)				-- make unique name (if needed so)
     -> (HsName -> Ty -> e -> e)              -- handle unit result
     -> (HsName -> Ty -> HsName -> Ty -> e -> e)    -- make tupled result, for state representation
     -> UID                             
     -> Ty								-- IO result type
     -> ( [Ty]							-- type of additional arguments
        , [HsName]						-- names of additional arguments
        , e -> e						-- wrapping/adaption of result
        )
ffiIOAdapt
     opts
     mkUniqNm
     mkUnitRes
     mkTupledRes
     uniq iores
  = ([tyState],[nmState],wrapRes)
  where tyState = appCon $ ehcOptBuiltin opts ehbnRealWorld
        [nmState,nmRes,nmIgnoreRes] = take 3 (map (mkUniqNm) (iterate uidNext uniq))
        wrapRes = mkTupledRes nmState (appDbg "ffiIOAdapt.mkTupledRes.state") nmRes (appDbg "ffiIOAdapt.mkTupledRes.res") . dealWithUnitRes
                where dealWithUnitRes
                        = case tyMbRecExts iores of
                            Just (_,[]) -> mkUnitRes nmIgnoreRes (appDbg "ffiIOAdapt.mkUnitRes")
                            _           -> id
%%]

%%[(9898 codegen grin) export(ffiGrinIOAdapt)
-- | adapt type etc for IO ffi call, specialized for Grin
ffiGrinIOAdapt
  :: EHCOpts
     -> UID                             
     -> HsName                          -- module name
     -> Ty								-- IO result type
     -> ([Ty],[HsName],GrExpr -> GrExpr)
ffiGrinIOAdapt
     opts
     uniq modNm iores
  = ffiIOAdapt
      opts
      (uidQualHNm modNm)
      (\          nmIgnoreRes _ r -> GrExpr_Seq r (GrPatLam_Var nmIgnoreRes) (GrExpr_Unit (mkGrRecNode []) GrType_None))
      (\nmState _ nmRes       _ r -> GrExpr_Seq (unit2store r) (GrPatLam_Var nmRes) (GrExpr_Unit (mkGrRecNode $ map GrVal_Var [nmState,nmRes]) GrType_None))
      uniq iores
  where unit2store :: GrExpr -> GrExpr
        unit2store (GrExpr_Seq e p (GrExpr_Unit x _))  =  GrExpr_Seq e p (GrExpr_Store x)
        unit2store e                                   =  error $ "ffiGrinIOAdapt: unit2store applied to non-unit " ++ show e
%%]

%%[(98 codegen) export(ffiCoreIOAdapt)
-- | adapt type etc for IO ffi call, specialized for Core
ffiCoreIOAdapt
  :: EHCOpts
     -> UID                             
     -> Ty								-- IO result type
     -> ([Ty],[HsName],CExpr -> CExpr)
ffiCoreIOAdapt
     opts
     uniq iores
  = ffiIOAdapt
      opts
      mkHNm
      (\          nmIgnoreRes ty r -> acoreLet1StrictTy nmIgnoreRes (SysF.ty2TyCforFFI opts ty) r $ acoreTup []                               )
      (\nmState _ nmRes       ty r -> acoreLet1StrictTy nmRes       (SysF.ty2TyCforFFI opts ty) r $ acoreTup [acoreVar nmState,acoreVar nmRes])
      uniq iores
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evalution adaption of FFI call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ffiEvalAdapt)
-- | evaluate value etc for ffi call
ffiEvalAdapt
  :: ((HsName,Ty,intro,Bool) -> e -> e)		-- construct arg w.r.t. eval need, and bind to intro
     -> ((HsName,Ty,e,Bool) -> e)				-- construct result w.r.t. eval need
     -> [(HsName,Ty,intro,Bool)]				-- arg name + introduction + eval need
     -> (HsName,Ty,e,Bool)						-- result
     -> e
ffiEvalAdapt
     evalBindArg
     evalRes
     args
     res
  = foldr evalBindArg (evalRes res) args
%%]

%%[(8 codegen grin) export(ffiGrinEvalAdapt)
-- | evaluate value etc for ffi call, specialized for Grin
ffiGrinEvalAdapt
  :: [(HsName,GrPatLam,Bool)]				-- arg name + introduction + eval need
     -> (HsName,GrExpr,Bool)				-- result
     -> GrExpr
ffiGrinEvalAdapt args res
  = ffiEvalAdapt
      (\(n,_,i,ev) e -> GrExpr_Seq (if ev then GrExpr_Eval n else GrExpr_Unit (GrVal_Var n) GrType_None) i e)
      (\(n,_,e,ev) -> if ev then GrExpr_Seq e (GrPatLam_Var n) (GrExpr_Eval n) else e)
      (map addTy args) (addTy res)
  where addTy (n,x,b) = (n,appDbg "ffiGrinEvalAdapt",x,b)
%%]

%%[(8 codegen) export(ffiCoreEvalAdapt)
-- | evaluate value etc for ffi call, specialized for Core
ffiCoreEvalAdapt
  :: EHCOpts
     -> [(HsName,Ty,HsName,Bool)]			-- arg name + introduction + eval need
     -> (HsName,Ty,CExpr,Bool)				-- result
     -> CExpr
ffiCoreEvalAdapt opts
  = ffiEvalAdapt
      (\(n,ty,i,ev) e -> (if ev then acoreLet1StrictTy                                              else acoreLet1PlainTy) i (SysF.ty2TyCforFFI opts ty) (acoreVar n) e)
      (\(n,ty,e,ev)   ->  if ev then acoreLet1StrictTy n (SysF.ty2TyCforFFI opts ty) e (acoreVar n) else e               )

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construct FFI/FFE code + type, specific for Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ffiCoreMk)
-- | Construct Core code for FFI
ffiCoreMk
  :: EHCOpts
     -> ( Ty -> CExpr			-- make FFI call
        )
     -> UID
     -> RCEEnv
     -> ForeignExtraction		-- the ffi info
     -> Ty						-- original type signature of FFI
     -> CExpr
ffiCoreMk
     opts
     (mkFFI)
     uniq rceEnv
     foreignEntInfo
     tyFFI
  = acoreLamTy (zip nmArgL (map (SysF.ty2TyCforFFI opts) argTyL) ++ zip nmArgLExtra (repeat $ acoreTyErr "ffiCoreMk.nmArgLExtra.TBD"))
    $ ffiCoreEvalAdapt opts
        ( zip4 nmArgL argTyL nmArgPatL primArgNeedsEvalL )
        ( nmEvalRes
        , resTyAdapted
        , wrapRes
          $ acoreApp (mkFFI $ argTyL `appArr` resTyAdapted)
          $ map acoreVar nmArgPatL
        , primResNeedsEval
        )
  where (argTyL,resTy) = appUnArr tyFFI
        argLen = length argTyL
        (_,u1,u2) = mkNewLevUID2 uniq
        (nmRes:nmEvalRes:nmArgL) = take (argLen + 2) (map mkHNm (iterate uidNext u1))
        nmArgPatL = map (hsnUniqify HsNameUniqifier_FFIArg) nmArgL
        (resTyAdapted,argTyLExtra,nmArgLExtra,wrapRes)
           =
%%[[98
               case ffiMbIORes opts resTy of
                 Just iores
                   -> (iores,a,n,w)
                   where (a,n,w) = ffiCoreIOAdapt opts u2 iores
                 _ ->
%%]]
                      (resTy,[],[],id)
%%[[8
        primArgNeedsEvalL
                        =   take argLen $ repeat True
        primResNeedsEval
                        =   False
%%][96
        mbPrimNeedEval  =   maybe Nothing lookupPrimNeedsEval $ forextractMbEnt foreignEntInfo
        primArgNeedsEvalL
                        =   take argLen $ maybe (repeat True) (\p -> primArgNeedEval p ++ repeat True) mbPrimNeedEval
        primResNeedsEval
                        =   maybe False primResNeedEval mbPrimNeedEval
%%]]
%%]

%%[(90 codegen) export(ffeCoreMk)
-- | Construct Core code for FFE
ffeCoreMk
  :: EHCOpts
     -> UID
     -> RCEEnv
     -> Ty						-- original type signature of FFE
     -> ( CExpr -> CExpr		-- ffe wrapper
        , Ty					-- corresponding type
        )
ffeCoreMk
     opts uniq rceEnv
     tyFFE
  = ( \e ->
          acoreLamTy (zipWith (\a t -> (a, SysF.ty2TyCforFFI opts t)) nmArgL argTyL)
          $ acoreLet1StrictTy nmEvalRes (SysF.ty2TyCforFFI opts resTyAdapted)
              (wrapRes $ acoreApp e $ map acoreVar nmArgL ++ argLExtra)
              (acoreVar nmEvalRes)
	, argTyL `appArr` resTyAdapted
	)
  where (argTyL,resTy) = appUnArr tyFFE
        argLen = length argTyL
        (nmRes:nmEvalRes:nmIOEvalRes:nmArgL) = map mkHNm $ mkNewLevUIDL (argLen+3) uniq
        (resTyAdapted,argLExtra,wrapRes)
           =
%%[[98
               case ffiMbIORes opts resTy of
                 Just iores
                   -> ( iores
                      , [acoreTup []]       -- (), unit, the world
                      , \e -> acoreExprSatSelCaseTy rceEnv (Just (nmIOEvalRes,acoreTyErr "ffeCoreMk.wrapRes")) e CTagRec nmIOEvalRes 1 Nothing
                      )
                 _ ->
%%]]
                      (resTy,[],id)
%%]
