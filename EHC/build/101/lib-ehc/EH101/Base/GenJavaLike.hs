module EH101.Base.GenJavaLike
( CVarInfo' (..), CVarMp' (..)
, cvarGlob
, cvarToRef
, javalikeArgsPack
, javalikeArgsUnpack
, Scrutinee (..)
, javalikeMkFFICall
, JBind' (..), JBinds'
, jBind' )
where
import qualified Data.Map as Map
import Data.Bits
import Data.List
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.BuiltinPrims
import EH.Util.Pretty
import EH.Util.Utils
import qualified EH.Util.FastSeq as Seq
import EH101.Opts.Base
import EH101.Base.HsName
import EH101.Base.Common
import EH101.Base.BasicAnnot
import EH101.Foreign.Extract

{-# LINE 32 "src/ehc/Base/GenJavaLike.chs" #-}
data CVarInfo' ty varref tupfldref
  = CVarInfo_This                           -- this object
      { cvarType            :: ty
      }
  | CVarInfo_Local                          -- a local on the stack
      { cvarType            :: ty
      , cvarOffset          :: varref
      }
  | CVarInfo_DataFld                        -- a field of a datatype alternative
      { cvarType            :: ty
      , cvarData            :: CVarInfo' ty varref tupfldref
      , cvarClassLocNm      :: HsName
      , cvarFldNm           :: String
      }
  | CVarInfo_TupFld                         -- a field of a tuple
      { cvarType            :: ty
      , cvarTuple           :: CVarInfo' ty varref tupfldref
      , cvarInx             :: Either tupfldref HsName
      }
  | CVarInfo_Global                         -- a global
      { cvarType            :: ty
      , cvarClassLocNm      :: HsName
      , cvarFldNm           :: String
      }
  | CVarInfo_None

type CVarMp' ty varref tupfldref = Map.Map HsName (CVarInfo' ty varref tupfldref)

{-# LINE 62 "src/ehc/Base/GenJavaLike.chs" #-}
-- | global reference
cvarGlob :: ty -> HsName -> HsName -> HsName -> CVarInfo' ty varref tupfldref
cvarGlob ty clNm nm safeVarNm
  = CVarInfo_Global ty clNm' (show safeVarNm)
  where clNm' = maybe clNm (\m -> hsnSetQual m $ hsnQualified m) $ hsnQualifier nm

{-# LINE 75 "src/ehc/Base/GenJavaLike.chs" #-}
-- | generate ref
cvarToRef
  :: ( ty -> e                  		-- make for 'this',
     , ty -> varref -> e        		-- local,
     , ty -> HsName -> String -> e		-- global,
     , ty -> e -> HsName -> String -> e	-- data field,
     ,       e -> e -> e				-- tuple field
     , tupfldref -> e					-- offset
     )
     -> CVarMp' ty varref tupfldref -> CVarInfo' ty varref tupfldref -> e
cvarToRef
     (mkThis,mkLocal,mkGlobal,mkDataFld,mkTupFld,mkOffset)
     cvarMp vi
  = ref vi
  where ref vi
         = case vi of
             CVarInfo_This   t
               -> mkThis t
             CVarInfo_Local   t o
               -> mkLocal t o
             CVarInfo_Global  t cl   f
               -> mkGlobal t cl f
             CVarInfo_DataFld t cvid cl f
               -> mkDataFld t (ref cvid) cl f
             CVarInfo_TupFld  t cvit f
               -> mkTupFld (ref cvit) o
               where o = case f of
                           Left  o -> mkOffset o
                           Right n -> ref $ panicJust "GenJavaLike.cvarToRef.CVarInfo_TupFld" $ Map.lookup n cvarMp
             CVarInfo_None
               -> panic "GenJavaLike.cvarToRef.CVarInfo_None"

{-# LINE 113 "src/ehc/Base/GenJavaLike.chs" #-}
-- pack > 5 args into tuple, otherwise normal
javalikeArgsPack
  :: Int								-- limit after which arguments are passed via array
     -> (ty,ty,[e]->e,Int->String)
     -> [e]
     -> (String,ty,[e])
javalikeArgsPack limit (tyTup,tyObj,mkTup,mkAppNm) args
  | nArgs > limit = (nm,tyTup,[mkTup args])
  | otherwise = (nm,tyObj,args)
  where nArgs = length args
        nm    = mkAppNm nArgs

{-# LINE 127 "src/ehc/Base/GenJavaLike.chs" #-}
-- unpack > 5 args from tuple, otherwise normal
javalikeArgsUnpack
  :: Enum fldref
     => Int								-- limit after which arguments are passed via array
     -> ( ty							-- tuple type
        , ty							-- object/default type
        , ty							-- int type
        , [HsName] -> [ref]				--
        , Int -> e						-- make int
        , Int -> [ref] -> [ref]			-- make <= 5 argument references
        , Int          -> [ref]			-- make >  5 argument reference
        )
     -> [HsName]
     -> ( [(ref,ty)]
        , [(e,ty)]
        , [(HsName,CVarInfo' ty ref fldref)]
        )
javalikeArgsUnpack limit (tyTup,tyObj,tyInt,toRef,mkInt,mkArgRefs,mkArgRefs5) args
  | overLimit = ([(off0,tyTup)]         , [(mkInt nArgs,tyInt)], mkMp [ CVarInfo_TupFld tyObj tup (Left o) | o <- [toEnum 0..] ])
  | otherwise = (zip offs (repeat tyObj), []                   , mkMp [ CVarInfo_Local  tyObj           o  | o <- offs         ])
  where nArgs = length args
        overLimit = nArgs > limit
        offs@(off0:_)
          | overLimit = mkArgRefs5 nArgs
          | otherwise = mkArgRefs  nArgs $ toRef args
        tup   = CVarInfo_Local tyTup off0
        mkMp  = zip args

{-# LINE 161 "src/ehc/Base/GenJavaLike.chs" #-}
data Scrutinee
  = Scrutinee_Tag   CTag
  | Scrutinee_Int   Int
  | Scrutinee_Var   HsName
  | Scrutinee_Other String

{-# LINE 173 "src/ehc/Base/GenJavaLike.chs" #-}
-- | construct the ffi call
javalikeMkFFICall
  ::    ( BuiltinInfo -> basicinfo          			-- extract machine specific info of builtin info
        , Bool -> basicinfo -> (e->e, ty)   			-- unbox
        ,         basicinfo -> (e->e, ty)   			-- box
        , [ty] -> ty -> ForeignExtraction -> e			-- make prim function
        , [ty] -> ty -> e -> [e] -> e					-- make prim call
        , Int -> [e] -> e                      			-- make wrapper
        , Int -> [e] -> e                      			-- make dynamic
        , e->e											-- evaluate
        , ty											-- default ty
        )
     -> ForeignExtraction					-- the extracted entity info
     -> EHCOpts
     -> Bool                        		-- do eval of args
     -> [Maybe HsName]              		-- list of (possibly) type constructor names of arguments
     -> Maybe HsName                		-- and result
     -> ( [e -> e]    						-- additional unwrapping for each argument
        ,  e -> e     						-- and result
        , [e] -> e                 			-- and primitive call constructor
        )
javalikeMkFFICall
     (getInfo,unbox,box,mkPrimFun,mkPrim,mkWrap,mkDyn,jiEvl,jtyObj)
     impExtract
     opts doArgEval
     argMbConL resMbCon
  = (mkArgsE,mkResE,primE)
  where lkupBuiltin = \n -> Map.lookup n m
          where m = builtinKnownBoxedTyMp opts
        mkxxbox evl how mbCon
          = case mbCon of
              Just c -> case lkupBuiltin c of
                          Just bi -> how (getInfo bi)
                          _       -> dflt
              _      -> dflt
          where dflt = (evl,jtyObj)
        evl | doArgEval = jiEvl
            | otherwise = id
        mkunbox = mkxxbox evl (unbox doArgEval)
        mkbox   = mkxxbox evl box
        (mkArgsE,argsTy)
                = unzip $ map mkunbox argMbConL
        (mkResE,resTy)
                = mkbox resMbCon
    	ffi     = mkPrimFun argsTy resTy impExtract
        -- 20101207 TBD: wrap/dyna, 20111124 JS fixed
        primE   = case impExtract of
                    ForeignExtraction_Plain {forextractEnt = impEntNm}
                      -> mkPrim argsTy resTy ffi
                    ForeignExtraction_Wrapper
                      -> \as -> mkWrap (length as) as
                    ForeignExtraction_Dynamic
                      -> \as -> mkDyn (length as) as

{-# LINE 237 "src/ehc/Base/GenJavaLike.chs" #-}
data JBind' ty e fld
  = JBind
      { jbindOrigNm :: HsName
      , jbindNm     :: HsName
      , jbindTy     :: ty
      , jbindJI     :: e
      , jbindFld    :: fld
      }
type JBinds' ty e fld = Seq.Seq (JBind' ty e fld)

{-# LINE 249 "src/ehc/Base/GenJavaLike.chs" #-}
jBind'
  :: ( ty					-- default type of binding
     , HsName -> HsName		-- make field name
     , HsName -> fld		-- make field
     )
     -> HsName				-- original name
     -> HsName				-- name
     -> e					-- bound expr
     -> JBinds' ty e fld
jBind'
     (tyDefault,mkFldNm,mkFld)
     nmOrig nm e
  = Seq.singleton
      $ JBind nmOrig
              nm'
              tyDefault {- @expr.jty -}
              e
              (mkFld nm')
  where nm' = mkFldNm nm -- hsnJavaLikeVarToFld nm

