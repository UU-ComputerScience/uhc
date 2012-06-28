module EH101.Core.FFI
( tyNmFFIBoxBasicAnnot
, tyNmGBMayLiveAsTaggedPtr, tyNmGBTagPtrBasicAnnot
, tyIsFFIOpaque
, tyIsFFIEnumable
, ffiMkArgUnpack
, ffiGrinMkArgUnpack
, ffiMkResPack
, ffiGrinMkResPack
, ffiGrinMk
, ffiEvalAdapt
, ffiGrinEvalAdapt
, ffiCoreEvalAdapt
, ffiCoreMk
, ffeCoreMk
, ffiMbIORes
, ffiIOAdapt
, ffiCoreIOAdapt )
where
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.Opts
import EH101.Base.Target
import EH101.Base.Common
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import EH101.Base.BasicAnnot
import EH101.Ty
import EH101.Gam.DataGam
import EH101.AbstractCore
import EH101.Core
import EH101.Core.Utils
import EH101.GrinCode
import EH101.Foreign.Extract
import EH101.BuiltinPrims



{-# LINE 37 "src/ehc/Core/FFI.chs" #-}
-- | is ty going to be passed unboxed to ffi, return info about it if so?
tyNmFFIBoxBasicAnnot :: EHCOpts -> HsName -> Maybe BasicAnnot
tyNmFFIBoxBasicAnnot opts
  = \n -> fmap biGrinBoxAnnot $ m n
  where m = builtinGrinInfo opts


{-# LINE 50 "src/ehc/Core/FFI.chs" #-}
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

{-# LINE 71 "src/ehc/Core/FFI.chs" #-}
-- | is type (name) living opaque w.r.t. ffi?
tyIsFFIOpaque :: Ty -> DataGam -> Bool
tyIsFFIOpaque t dataGam = maybe True null (dataGamTagsOfTy t dataGam)

{-# LINE 77 "src/ehc/Core/FFI.chs" #-}
-- | is type (name) Enumable, that is, representable by an Int?
tyIsFFIEnumable :: HsName -> DataGam -> Bool
tyIsFFIEnumable tn dataGam = maybe False dgiIsEnumable (dataGamLookup tn dataGam)

{-# LINE 89 "src/ehc/Core/FFI.chs" #-}
-- | make argument, i.e. wrap given argument name in proper introduction with annotation about what it is
ffiMkArgUnpack
  :: EHCOpts
     -> DataGam
     -> (BasicAnnot -> HsName -> intro)     -- make intro: node around basic type
     -> (HsName -> intro)                   -- make intro: enum
     -> (HsName -> intro)                   -- make intro: var
     -> (HsName -> intro)                   -- make intro: opaque
     -> (HsName -> intro)                   -- make intro: pointer
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
        mk | isJust mbAnn                   = mkNodeI (tyNmGBTagPtrBasicAnnot opts False tyNm (fromJust mbAnn)) argNm
           | tyIsFFIEnumable tyNm dataGam   = mkEnumI argNm
           | isJust (tyMbRecRow ty)         = mkVarI  argNm
           | tyIsFFIOpaque ty dataGam       = mkOpaqI argNm
           | otherwise                      = mkPtrI  argNm

{-# LINE 116 "src/ehc/Core/FFI.chs" #-}
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
  = ffiMkArgUnpack
         opts dataGam
         GrPatLam_BasicNode GrPatLam_EnumNode GrPatLam_Var GrPatLam_OpaqueNode GrPatLam_PtrNode
         argNm ty

{-# LINE 133 "src/ehc/Core/FFI.chs" #-}
-- | make result, i.e. wrap given argument name in proper adaption with annotation about what it is
-- Note: 0-tuple is assumed to be Enumerable (change this here, and in the RTS, if the 0-tuple is to be regarded as a basis for extensible rows)
ffiMkResPack
  :: EHCOpts
     -> DataGam
     -> (BasicAnnot -> HsName -> intro)     -- make intro: node around basic type
     -> (HsName -> HsName -> intro)         -- make intro: enum
     -- -> (HsName -> intro)                    -- make intro: var
     -> (HsName -> intro)                   -- make intro: opaque
     -> (HsName -> HsName -> intro)         -- make intro: pointer
     -> (e -> intro -> e -> e)              -- make bind: let .. in
     -> (HsName -> HsName -> e)             -- make expr: node
     -> (HsName -> e)                       -- make expr: enum
     -> (HsName -> e)                       -- make expr: opaq
     -> (HsName -> e)                       -- make expr: ptr
     -> HsName                              -- arg name
     -> Ty                                  -- its type
     -> e                                   -- res value
     -> e
ffiMkResPack
     opts dataGam
     mkNodeI mkEnumI {- mkVarI -} mkOpaqI mkPtrI
     mkBindE
     mkNodeE mkEnumE mkOpaqE mkPtrE
     resNm resTy res
  = mk
  where resTyNm  = tyAppFunConNm resTy
        mbAnn = tyNmFFIBoxBasicAnnot opts resTyNm
        mk | isJust mbAnn                   = mkBindE res (mkNodeI (tyNmGBTagPtrBasicAnnot opts True resTyNm (fromJust mbAnn)) resNm) (mkNodeE resTyNm resNm)
           | tyIsFFIEnumable resTyNm dataGam= mkBindE res (mkEnumI resTyNm                                                     resNm) (mkEnumE         resNm)
           | isRec && arity == 0            = mkBindE res (mkEnumI recNm                                                       resNm) (mkEnumE         resNm)
           | isRec                          = mkBindE res (mkPtrI  recNm                                                       resNm) (mkPtrE          resNm)
           | tyIsFFIOpaque resTy dataGam    = mkBindE res (mkOpaqI                                                             resNm) (mkOpaqE         resNm)
           | otherwise                      = mkBindE res (mkPtrI  resTyNm                                                     resNm) (mkPtrE          resNm)
           where isRec = isJust $ tyMbRecRow resTy
                 arity = length $ snd $ tyRecExts resTy
                 recNm = builtinRecNm arity

{-# LINE 173 "src/ehc/Core/FFI.chs" #-}
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
  = ffiMkResPack
         opts dataGam
         GrPatLam_BasicAnnot GrPatLam_EnumAnnot {- GrPatLam_Var -} GrPatLam_OpaqueAnnot GrPatLam_PtrAnnot
         GrExpr_Seq
         (\tyNm n -> u (GrVal_BasicNode (GrTag_Con (mkGrTagAnn 1 1) 0 tyNm) n))
         (\     n -> u (GrVal_EnumNode                                      n))
         (\     n -> u (GrVal_OpaqueNode                                    n))
         (\     n -> u (GrVal_PtrNode                                       n))
         resNm resTy res
  where u x = GrExpr_Unit x GrType_None

{-# LINE 197 "src/ehc/Core/FFI.chs" #-}
-- | make ffi call, specifically for Grin
ffiGrinMk
  :: EHCOpts
     -> DataGam
     -> UID
     -> HsName
     -> FFIWay					-- calling convention
     -> ForeignEnt				-- reference to the foreign entity
     -> Bool					-- result is evaluated
     -> [(GrVal,Ty)]			-- arguments info
     -> Ty						-- result info
     -> GrExpr
ffiGrinMk
     opts dataGam uniq modNm
     callconv
     impEnt
     isEvaluated
     args resTy
  = foldr (\((v1,ty),n2) e
             -> GrExpr_Seq (GrExpr_Unit v1 GrType_None)
                           (ffiGrinMkArgUnpack opts dataGam n2 ty)
                           e
          )
          (ffiGrinMkResPack opts dataGam nmRes resTy
              (GrExpr_FFI callconv impEnt
                          (GrFFIAnnot_IsResEval isEvaluated)
                          (map GrVal_Var nmArgPatL)
          )   )
          (zip args nmArgPatL)
  where (nmRes:nmArgPatL) = take (length args + 1) (map (uidQualHNm modNm) (iterate uidNext uniq))

{-# LINE 262 "src/ehc/Core/FFI.chs" #-}
-- | is type an IO type, if so return the IO type argument (result returned by IO)
ffiMbIORes :: EHCOpts -> Ty -> Maybe Ty
ffiMbIORes opts resTy
  = case tyMbAppConArgs resTy of
      Just (n,[a]) | ehcOptBuiltin opts ehbnIO == n
        -> Just a
      _ -> Nothing

{-# LINE 276 "src/ehc/Core/FFI.chs" #-}
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
  where tyState = Ty_Con $ ehcOptBuiltin opts ehbnRealWorld
        [nmState,nmRes,nmIgnoreRes] = take 3 (map (mkUniqNm) (iterate uidNext uniq))
        wrapRes = mkTupledRes nmState (acoreTyErr "ffiIOAdapt.mkTupledRes.state") nmRes (acoreTyErr "ffiIOAdapt.mkTupledRes.res") . dealWithUnitRes
                where dealWithUnitRes
                        = case tyMbRecExts iores of
                            Just (_,[]) -> mkUnitRes nmIgnoreRes (acoreTyErr "ffiIOAdapt.mkUnitRes")
                            _           -> id

{-# LINE 327 "src/ehc/Core/FFI.chs" #-}
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
      (\          nmIgnoreRes ty r -> acoreLet1StrictTy nmIgnoreRes ty r $ acoreTup []                               )
      (\nmState _ nmRes       ty r -> acoreLet1StrictTy nmRes       ty r $ acoreTup [acoreVar nmState,acoreVar nmRes])
      uniq iores

{-# LINE 349 "src/ehc/Core/FFI.chs" #-}
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

{-# LINE 365 "src/ehc/Core/FFI.chs" #-}
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
  where addTy (n,x,b) = (n,acoreTyErr "ffiGrinEvalAdapt",x,b)

{-# LINE 379 "src/ehc/Core/FFI.chs" #-}
-- | evaluate value etc for ffi call, specialized for Core
ffiCoreEvalAdapt
  :: [(HsName,Ty,HsName,Bool)]				-- arg name + introduction + eval need
     -> (HsName,Ty,CExpr,Bool)				-- result
     -> CExpr
ffiCoreEvalAdapt
  = ffiEvalAdapt
      (\(n,ty,i,ev) e -> (if ev then acoreLet1StrictTy                     else acoreLet1PlainTy) i ty (acoreVar n) e)
      (\(n,ty,e,ev)   ->  if ev then acoreLet1StrictTy n ty e (acoreVar n) else e               )


{-# LINE 396 "src/ehc/Core/FFI.chs" #-}
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
  = acoreLamTy (zip nmArgL argTyL ++ zip nmArgLExtra (repeat $ acoreTyErr "ffiCoreMk.nmArgLExtra.TBD"))
    $ ffiCoreEvalAdapt
        ( zip4 nmArgL argTyL nmArgPatL primArgNeedsEvalL )
        ( nmEvalRes
        , resTyAdapted
        , wrapRes
          $ acoreApp (mkFFI $ argTyL `mkArrow` resTyAdapted)
          $ map acoreVar nmArgPatL
        , primResNeedsEval
        )
  where (argTyL,resTy) = tyArrowArgsRes tyFFI
        argLen = length argTyL
        (_,u1,u2) = mkNewLevUID2 uniq
        (nmRes:nmEvalRes:nmArgL) = take (argLen + 2) (map mkHNm (iterate uidNext u1))
        nmArgPatL = map (hsnUniqify HsNameUniqifier_FFIArg) nmArgL
        (resTyAdapted,argTyLExtra,nmArgLExtra,wrapRes)
           =
               case ffiMbIORes opts resTy of
                 Just iores
                   -> (iores,a,n,w)
                   where (a,n,w) = ffiCoreIOAdapt opts u2 iores
                 _ ->
                      (resTy,[],[],id)
        mbPrimNeedEval  =   maybe Nothing lookupPrimNeedsEval $ forextractMbEnt foreignEntInfo
        primArgNeedsEvalL
                        =   take argLen $ maybe (repeat True) (\p -> primArgNeedEval p ++ repeat True) mbPrimNeedEval
        primResNeedsEval
                        =   maybe False primResNeedEval mbPrimNeedEval

{-# LINE 452 "src/ehc/Core/FFI.chs" #-}
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
          acoreLamTy (zip nmArgL argTyL)
          $ acoreLet1StrictTy nmEvalRes resTyAdapted
              (wrapRes $ acoreApp e $ map acoreVar nmArgL ++ argLExtra)
              (acoreVar nmEvalRes)
	, argTyL `mkArrow` resTyAdapted
	)
  where (argTyL,resTy) = tyArrowArgsRes tyFFE
        argLen = length argTyL
        (nmRes:nmEvalRes:nmIOEvalRes:nmArgL) = map mkHNm $ mkNewLevUIDL (argLen+3) uniq
        (resTyAdapted,argLExtra,wrapRes)
           =
               case ffiMbIORes opts resTy of
                 Just iores
                   -> ( iores
                      , [acoreTup []]       -- (), unit, the world
                      , \e -> acoreExprSatSelCaseTy rceEnv (Just (nmIOEvalRes,acoreTyErr "ffeCoreMk.wrapRes")) e CTagRec nmIOEvalRes 1 Nothing
                      )
                 _ ->
                      (resTy,[],id)
