%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Factored out from various grin based backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs module {%{EH}CodeGen.ValAccess}
%%]

%%[(8 codegen grin) hs import({%{EH}Base.Common},qualified {%{EH}Config} as Cfg)
%%]

%%[(8 codegen grin) hs import(UHC.Util.Utils,UHC.Util.Pretty as Pretty,Data.Bits,Data.Maybe,qualified UHC.Util.FastSeq as Seq,qualified Data.Map as Map)
%%]

%%[(8 codegen grin) hs import({%{EH}Base.BasicAnnot}) export(module {%{EH}Base.BasicAnnot})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environmental info for name resolution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(ValAccessAnnot(..))
-- type ValAccessAnnot = Either BasicAnnot BasicSize			-- either we have to deal with the annotation or it has already been done partly and we need to propagate the size

data ValAccessAnnot			-- either we have to deal with the annotation or it has already been done partly and we need to propagate the size
  = ValAccessAnnot_Annot !BasicAnnot
  | ValAccessAnnot_Basic !BasicSize  !GCPermit
  deriving Show

valAccessAnnot :: (BasicAnnot -> x) -> (BasicSize -> GCPermit -> x) -> ValAccessAnnot -> x
valAccessAnnot f _ (ValAccessAnnot_Annot a  ) = f a
valAccessAnnot _ f (ValAccessAnnot_Basic s p) = f s p

vaAnnotBasicSize :: ValAccessAnnot -> BasicSize
vaAnnotBasicSize = valAccessAnnot grinBasicAnnotSize const

vaAnnotGCPermit :: ValAccessAnnot -> GCPermit
vaAnnotGCPermit = valAccessAnnot grinBasicAnnotGCPermit (flip const)
%%]

%%[(8 codegen grin) hs export(ValAccess(..),ValAccessGam)
data ValAccess lref gref mref meref
  = Val_Local           { vaRef :: !lref, vaAnnot :: !ValAccessAnnot }		-- value on the stack
  | Val_NodeFldLocal    { vaRef :: !lref, vaAnnot :: !ValAccessAnnot }		-- field 0 of node on the stack
  | Val_GlobEntry       { vaGlobRef :: !gref }
  | Val_Int             { vaInt :: Integer }
%%[[50
  | Val_ImpEntry        { vaModRef :: !mref, vaModEntryRef :: !meref }
%%]]

instance (Show lref, Show gref, Show mref, Show meref) => Show (ValAccess lref gref mref meref) where
  show (Val_Local 			{vaRef=r					}) = show r
  show (Val_NodeFldLocal 	{vaRef=r					}) = show r
  show (Val_GlobEntry 		{vaGlobRef=r				}) = show r
  show (Val_Int 			{vaInt=i					}) = show i
%%[[50
  show (Val_ImpEntry 		{vaModRef=m, vaModEntryRef=e}) = show m ++ "." ++ show e
%%]]

instance (Show lref, Show gref, Show mref, Show meref) => PP (ValAccess lref gref mref meref) where
  pp = pp . show

type ValAccessGam lref gref mref meref = Map.Map HsName (ValAccess lref gref mref meref)
%%]

%%[(8 codegen grin) hs
vaHasAnnot :: ValAccess lref gref mref meref -> Bool
vaHasAnnot (Val_Local        _ _) = True
vaHasAnnot (Val_NodeFldLocal _ _) = True
vaHasAnnot _                      = False
%%]

%%[(50 codegen grin) hs export(ImpNmMp)
type ImpNmMp = Map.Map HsName Int
%%]

%%[(8 codegen grin) hs export(NmEnv(..))
data NmEnv lref gref mref meref
  = NmEnv
      { neVAGam     :: ValAccessGam lref gref mref meref
%%[[50
      , neImpNmMp   :: HsName2RefMpMp mref meref
      , neLamMp     :: LamMp
%%]]
      }
%%]

%%[(8 codegen grin).nmEnvLookup hs export(nmEnvLookup)
nmEnvLookup :: HsName -> NmEnv lref gref mref meref -> Maybe (ValAccess lref gref mref meref)
nmEnvLookup nm env = Map.lookup nm $ neVAGam env
%%]

%%[(50 codegen grin) -8.nmEnvLookup hs export(nmEnvLookup)
nmEnvLookup :: HsName -> NmEnv lref gref mref meref -> Maybe (ValAccess lref gref mref meref)
nmEnvLookup nm env
  = case Map.lookup nm $ neVAGam env of
      Nothing
        -> do { q <- hsnQualifier nm
              ; (mo,entryMp) <- Map.lookup q $ neImpNmMp env
              ; eo <- Map.lookup nm entryMp
              ; return (Val_ImpEntry mo eo
                                     {- (maybe (-1) id
                                      $ do { li <- Map.lookup nm $ neLamMp env
                                           ; fi <- laminfoGrinByteCode li
                                           ; return (gblaminfoFuninfoKey fi)
                                           }
                       ) -}            )
              }
      v -> v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Access to module entries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs export(HsName2RefMp,HsName2RefMpMp)
type HsName2RefMp meref = Map.Map HsName meref
type HsName2RefMpMp mref meref = Map.Map HsName (mref, HsName2RefMp meref)
%%]

