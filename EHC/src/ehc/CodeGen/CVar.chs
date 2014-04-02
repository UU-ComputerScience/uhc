%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils for encoding differences between variable references in generated code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Utils for encoding differences between variable references in generated code
%%]

%%[(8 codegen) hs module {%{EH}CodeGen.CVar}
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,Data.Bits, Data.List, Data.Char)
%%]

%%[(8 codegen) hs import({%{EH}Base.Builtin},{%{EH}Base.Builtin2},{%{EH}BuiltinPrims})
%%]

%%[(8 codegen) hs import(UHC.Util.Pretty, UHC.Util.Utils, qualified UHC.Util.FastSeq as Seq)
%%]

%%[(8 codegen) hs import({%{EH}Opts.Base},{%{EH}Base.HsName},{%{EH}Base.Common},{%{EH}Base.BasicAnnot})
%%]

%%[(8 codegen) hs import({%{EH}Foreign.Extract})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment info for code variables (CVar)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CVarInfo'(..))
data CVarInfo' tag ty varref datafldref tupfldref
  = CVar_This                           -- this object
      { cvarType            :: ty
      }
  | CVar_Local                          -- a local on the stack
      { cvarType            :: ty
      , cvarOffset          :: varref
      }
  | CVar_DataFld                        -- a field of a datatype alternative
      { cvarType            :: ty
      , cvarData            :: CVarInfo' tag ty varref datafldref tupfldref
      , cvarTag      		:: tag
      , cvarFld             :: datafldref
      }
  | CVar_DataTag                        -- the tag of a datatype alternative
      { cvarType            :: ty
      , cvarData            :: CVarInfo' tag ty varref datafldref tupfldref
      }
  | CVar_TupFld                         -- a field of a tuple
      { cvarType            :: ty
      , cvarTuple           :: CVarInfo' tag ty varref datafldref tupfldref
      , cvarInx             :: Either tupfldref HsName
      }
  | CVar_GlobalExt                         -- a global, external
      { cvarType            :: ty
      , cvarModNm      		:: HsName
      , cvarFldNm           :: HsName
      }
  | CVar_GlobalInt                         -- a global, internal to current module
      { cvarType            :: ty
      , cvarModNm      		:: HsName
      , cvarFldNm           :: HsName
      }
  | CVar_None
%%]

%%[(8 codegen) hs export(CVarMp'(..), cvarMpLookup)
type CVarMp' tag ty varref datafldref tupfldref = Map.Map HsName (CVarInfo' tag ty varref datafldref tupfldref)

cvarMpLookup :: HsName -> CVarMp' tag ty varref datafldref tupfldref -> Maybe (CVarInfo' tag ty varref datafldref tupfldref)
cvarMpLookup = Map.lookup
{-# INLINE cvarMpLookup #-}
%%]

%%[(8 codegen) hs
instance (Show datafldref, Show tupfldref, Show varref) => Show (CVarInfo' tag ty varref datafldref tupfldref) where
  show (CVar_This 		{									})	= "this"
  show (CVar_Local 		{					  cvarOffset=n	})	= show n
  show (CVar_DataFld 	{cvarData=d			, cvarFld=n		}) 	= show d ++ "." ++ show n
  show (CVar_DataTag 	{cvarData=d							}) 	= show d ++ ".tag"
  show (CVar_TupFld 	{cvarTuple=d		, cvarInx=n		}) 	= show d ++ "." ++ either show show n
  show (CVar_GlobalExt 	{cvarModNm=d		, cvarFldNm=n	}) 	= show d ++ "." ++ show n
  show (CVar_GlobalInt 	{					  cvarFldNm=n	}) 	= show n
  show _													 	= "?"

instance (Show datafldref, Show tupfldref, Show varref) => PP (CVarInfo' tag ty varref datafldref tupfldref) where
  pp = pp . show
%%]

%%[(8 codegen) hs export(cvarLoc)
-- | local reference
cvarLoc :: ty -> varref -> CVarInfo' tag ty varref datafldref tupfldref
cvarLoc = CVar_Local
%%]

%%[(8 codegen) hs export(cvarGlob)
-- | global reference
cvarGlob :: ty -> CVarNmModuleCfg -> HsName -> HsName -> CVarInfo' tag ty varref datafldref tupfldref
cvarGlob ty cfg nm varNm
  = CVar_GlobalExt ty clNm' varNm
%%[[8
  where clNm' = cvnmcfgModInTop cfg
%%][50
  where clNm' = maybe (cvnmcfgModInTop cfg) (\m -> hsnSetQual m $ hsnQualified m) $ hsnQualifier nm
%%]]
%%]


%%[(8 codegen) hs export(CVarNmModuleCfg(..), emptyCVarNmModuleCfg)
-- | Per module name configuration used by name generation to take into account differences between local, global, etc 
data CVarNmModuleCfg = CVarNmModuleCfg
  { cvnmcfgPkg			:: HsName
  , cvnmcfgTopInPkg		:: HsName
  , cvnmcfgModInTop		:: HsName
  }

emptyCVarNmModuleCfg = CVarNmModuleCfg hsnUnknown hsnUnknown hsnUnknown
%%]


%%[(8 codegen) hs export(cvarToRef)
-- | generate ref 
cvarToRef
  :: ( ty -> e                  		-- make for 'this',
     , ty -> varref -> e        		-- local,
     , ty -> HsName -> HsName -> e		-- global external, additionally getting a safe name variant
     , ty -> HsName -> HsName -> e		-- global internal, additionally getting a safe name variant
     , ty -> e -> taginfo -> datafldref -> e	-- data field,
     , ty -> e -> e						-- data constr tag,
     ,       e -> e -> e				-- tuple field
     , tupfldref -> e					-- offset
     , CVarNmModuleCfg -> tag -> taginfo
     , CVarNmModuleCfg -> Bool -> HsName -> HsName
     )
     -> CVarNmModuleCfg
     -> CVarMp'   tag ty varref datafldref tupfldref
     -> CVarInfo' tag ty varref datafldref tupfldref
     -> e
cvarToRef
     (mkThis,mkLocal,mkGlobalExt,mkGlobalInt,mkDataFld,mkDataTag,mkTupFld,mkOffset,mkTag,mkSafeName)
     cfg cvarMp vi
  = ref vi
  where ref vi
         = case vi of
             CVar_This   t
               -> mkThis t
             CVar_Local   t o
               -> mkLocal t o
             CVar_GlobalExt  t m f
               -> mkGlobalExt t m (mkSafeName cfg True f)
             CVar_GlobalInt  t m f
               -> mkGlobalInt t m (mkSafeName cfg True f)
             CVar_DataFld t cvid cl f
               -> mkDataFld t (ref cvid) (mkTag cfg cl) f
             CVar_DataTag t cvid
               -> mkDataTag t (ref cvid)
             CVar_TupFld  t cvit f
               -> mkTupFld (ref cvit) o
               where o = case f of
                           Left  o -> mkOffset o
                           Right n -> ref $ panicJust "GenJavaLike.cvarToRef.CVar_TupFld" $ cvarMpLookup n cvarMp
             CVar_None
               -> panic "GenJavaLike.cvarToRef.CVar_None"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface/abstraction for CVarInfo generation (temporary only)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) hs export(CVARINFO(..))
class CVARINFO x i where
  mkCVarInfo :: ty -> x -> CVarInfo' tag ty varref datafldref tupfldref

instance CVARINFO HsName where
  mkCVarInfo = CVar_Local

instance CVARINFO HsName where
  mkCVarInfo = CVar_Local
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mangle name into a name which can be dealt with by a codegen target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(hsnJavaLikeVar)
-- safe name of a variable
hsnJavaLikeVar
  :: ( Bool -> HsName -> HsName		-- adapt for particular platform, before mangling here
     , HsName -> HsName             -- post prefix
     , String -> String             -- adapt module qualifiers
     )
     -> CVarNmModuleCfg
     -> Bool						-- is a global name?
     -> HsName
     -> HsName
hsnJavaLikeVar (preadapt, postprefix, updqual) cfg isglob v
%%[[8
  = hsnSafeJavaLike v
%%][50
  = postprefix $ hsnSafeJavaLike $ handleUpper $ qual $ preadapt isglob v
  where handleUpper v
          = case hsnBaseUnpack v of
               Just (s@(c:vs), mk) | isUpper c -> mk (s ++ "_")
               _ -> v
        qual v
          = case hsnBaseUnpack' v of
               Just (q, s, mk) -> mk (map updqual q) s
               _ -> v
%%]]
%%]

%%[(8 jazy || javascript) hs export(hsnJavaLikeVarCls)
-- name of the class of a variable
hsnJavaLikeVarCls :: CVarNmModuleCfg -> HsName -> HsName
hsnJavaLikeVarCls cfg v
%%[[8
  = hsnSuffix (cvnmcfgPkg cfg) ("-" ++ show v)
%%][50
  = hsnSetQual (cvnmcfgTopInPkg cfg) v
%%]]
%%]

%%[(8 jazy || javascript) hs export(hsnJavaLikeVarToFld)
-- field name of var name
hsnJavaLikeVarToFld :: HsName -> HsName
hsnJavaLikeVarToFld v
%%[[8
  = v
%%][50
  = hsnQualified v
%%]]
%%]

%%[(8 jazy || javascript) hs export(hsnJavaLikeDataTy, hsnJavaLikeDataCon, hsnJavaLikeDataFldAt, hsnJavaLikeDataFlds)
-- name of class of data type
hsnJavaLikeDataTy :: CVarNmModuleCfg -> HsName -> HsName
hsnJavaLikeDataTy _ d = hsnSafeJavaLike d `hsnSuffix` "_Ty"

-- name of class of data constructor
hsnJavaLikeDataCon :: CVarNmModuleCfg -> HsName -> HsName
hsnJavaLikeDataCon _ d = hsnSafeJavaLike d `hsnSuffix` "_Con"

-- name of field of data
hsnJavaLikeDataFldAt :: Int -> String
hsnJavaLikeDataFldAt i = show i

-- all names of fields of data
hsnJavaLikeDataFlds :: Int -> [String]
hsnJavaLikeDataFlds arity = map hsnJavaLikeDataFldAt [0..arity-1]
%%]

