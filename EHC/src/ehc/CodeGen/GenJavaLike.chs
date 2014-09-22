%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils aiding generation of Java like backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Various Java like code generation utility snippets.
%%]

%%[(8 jazy || javascript) hs module {%{EH}CodeGen.GenJavaLike}
%%]

%%[(8 jazy || javascript) hs import(qualified Data.Map as Map,Data.Bits, Data.List)
%%]

%%[(8 jazy || javascript) hs import({%{EH}Base.HsName.Builtin},{%{EH}CodeGen.BuiltinSizeInfo},{%{EH}CodeGen.BuiltinPrims})
%%]

%%[(8 jazy || javascript) hs import(UHC.Util.Pretty, UHC.Util.Utils, qualified UHC.Util.FastSeq as Seq)
%%]

%%[(8 jazy || javascript) hs import({%{EH}Opts.Base},{%{EH}Base.HsName},{%{EH}Base.Common},{%{EH}CodeGen.BasicAnnot})
%%]

%%[(8 jazy || javascript) hs import({%{EH}Foreign.Extract})
%%]

%%[(8 jazy || javascript) hs import({%{EH}CodeGen.CVar})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dealing with >5 args
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 jazy || javascript) hs export(javalikeArgsPack)
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
%%]

%%[(8 jazy || javascript) hs export(javalikeArgsUnpack)
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
        , [(HsName,CVarInfo' tag ty ref datafldref fldref)]
        )
javalikeArgsUnpack limit (tyTup,tyObj,tyInt,toRef,mkInt,mkArgRefs,mkArgRefs5) args
  | overLimit = ([(off0,tyTup)]         , [(mkInt nArgs,tyInt)], mkMp [ CVar_TupFld tyObj tup (Left o) | o <- [toEnum 0..] ])
  | otherwise = (zip offs (repeat tyObj), []                   , mkMp [ CVar_Local  tyObj           o  | o <- offs         ])
  where nArgs = length args
        overLimit = nArgs > limit
        offs@(off0:_)
          | overLimit = mkArgRefs5 nArgs
          | otherwise = mkArgRefs  nArgs $ toRef args
        tup   = CVar_Local tyTup off0
        mkMp  = zip args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Case: scrutinee type (i.e. tag)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 jazy || javascript) hs export(Scrutinee(..))
data Scrutinee
  = Scrutinee_Tag   CTag
  | Scrutinee_Int   Int
  | Scrutinee_Var   HsName
  | Scrutinee_Other String
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FFI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 jazy || javascript) hs export(javalikeMkFFICall)
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
%%[[8
        primE   = mkPrim argsTy resTy ffi
%%][90
        -- 20101207 TBD: wrap/dyna, 20111124 JS fixed
        primE   = case impExtract of
                    ForeignExtraction_Plain {forextractEnt = impEntNm}
                      -> mkPrim argsTy resTy ffi
                    ForeignExtraction_Wrapper
                      -> \as -> mkWrap (length as) as
                    ForeignExtraction_Dynamic
                      -> \as -> mkDyn (length as) as
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 jazy || javascript) hs export(JBind'(..),JBinds')
data JBind' ty e fld
  = JBind
      { jbindOrigNm :: HsName
      , jbindNm     :: HsName
      , jbindTy     :: ty
      , jbindJI     :: e
      , jbindFld    :: fld
      }
type JBinds' ty e fld = Seq.Seq (JBind' ty e fld)
%%]

%%[(8 jazy || javascript) hs export(jBind')
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
%%]

