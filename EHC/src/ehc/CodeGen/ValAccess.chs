%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Factored out from various grin based backends
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}CodeGen.ValAccess}
%%]

%%[(8 codegen) hs import({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]

%%[(8888 codegen) hs import(qualified {%{EH}Config} as Cfg)
%%]

%%[(8 codegen) hs import(UHC.Util.Utils,UHC.Util.Pretty as Pretty,Data.Bits,Data.Maybe,qualified UHC.Util.FastSeq as Seq,qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[(8 codegen) hs import(Control.Monad, Control.Monad.State)
%%]

%%[(8 codegen) hs import({%{EH}Base.BasicAnnot}) export(module {%{EH}Base.BasicAnnot})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environmental info for name resolution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(ValAccessAnnot(..))
data ValAccessAnnot			-- either we have to deal with the annotation or it has already been done partly and we need to propagate the size
  = ValAccessAnnot_Annot !BasicAnnot
  | ValAccessAnnot_Basic !BasicSize  !GCPermit
  deriving Show
%%]

%%[(8 codegen grin) hs export(valAccessAnnot, vaAnnotBasicSize, vaAnnotGCPermit)
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
  | Val_Int             { vaInt :: !Integer }
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

%%[(8 codegen grin) hs export(vaHasAnnot)
vaHasAnnot :: ValAccess lref gref mref meref -> Bool
vaHasAnnot (Val_Local        _ _) = True
vaHasAnnot (Val_NodeFldLocal _ _) = True
vaHasAnnot _                      = False
%%]

%%[(50 codegen grin) hs export(ImpNmMp)
type ImpNmMp mref = Map.Map HsName mref
%%]

%%[(8 codegen grin) hs export(NmEnv(..))
data NmEnv lref gref mref meref extra
  = NmEnv
      { neVAGam     :: ValAccessGam lref gref mref meref
%%[[50
      , neImpNmMp   :: HsName2FldMpMp
      , neExtra     :: extra
%%]]
      }
%%]

%%[(8 codegen grin)
-- cvtValAccessFromFld :: ValAccess Fld Fld Fld Fld -> ValAccess lref gref mref meref
%%]

%%[(8 codegen grin).nmEnvLookup hs export(nmEnvLookup)
nmEnvLookup :: HsName -> NmEnv lref gref mref meref extra -> Maybe (ValAccess lref gref mref meref)
nmEnvLookup nm env = Map.lookup nm $ neVAGam env
%%]

%%[(50 codegen grin) -8.nmEnvLookup hs export(nmEnvLookup)
nmEnvLookup :: (RefOfFld mref, RefOfFld meref) => HsName -> NmEnv lref gref mref meref extra -> Maybe (ValAccess lref gref mref meref)
nmEnvLookup nm env
  = case Map.lookup nm $ neVAGam env of
      Nothing
        -> do { q <- hsnQualifier nm
              ; (mo,entryMp) <- Map.lookup q $ neImpNmMp env
              ; eo <- Map.lookup nm entryMp
              ; return (Val_ImpEntry (refOfFld mo) (refOfFld eo)
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
%%% Reference generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RefGenerator(refGen1),refGen)
type RefGenMonad ref = State Int ref

class RefGenerator ref where
  -- | Generate for 1 name
  refGen1M :: Int -> HsName -> RefGenMonad ref
  refGen1  :: Int -> Int -> HsName -> (ref, Int)
  
  -- defaults
  refGen1M dir nm = do 
    seed <- get
    let (r,seed') = refGen1 seed dir nm
    put seed'
    return r
  
  refGen1 seed dir nm = runState (refGen1M dir nm) seed

instance RefGenerator HsName where
  refGen1M _ = return

instance RefGenerator Int where
  refGen1 seed dir nm  = (seed, seed+dir)

instance RefGenerator Fld where
  refGen1 seed dir nm  = (Fld (Just nm) (Just seed), seed+dir)

-- | Generate for names, starting at a seed in a direction
refGenM :: RefGenerator ref => Int -> [HsName] -> RefGenMonad (AssocL HsName ref)
refGenM dir nmL
  = forM nmL $ \nm -> do
      r <- refGen1M dir nm
      return (nm,r)

refGen :: RefGenerator ref => Int -> Int -> [HsName] -> AssocL HsName ref
refGen seed dir nmL = evalState (refGenM dir nmL) seed
%%]

%%[(8 codegen) hs export(patNmL2DepL)
-- | Generate references starting at offset 0 with additional direction tweaking
patNmL2DepL :: RefGenerator r => (Int -> Int) -> ([HsName] -> [HsName]) -> [HsName] -> AssocL HsName r
patNmL2DepL fd fl nmL = refGen 0 (fd 1) (fl nmL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Access to module entries, name to offset (in a record)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs export(HsName2RefMp,HsName2RefMpMp)
type HsName2RefMp meref = Map.Map HsName meref
type HsName2RefMpMp mref meref = Map.Map HsName (mref, HsName2RefMp meref)
%%]

%%[(50 codegen) hs export(HsName2FldMpMp,HsName2FldMp)
type HsName2FldMp   = HsName2RefMp   Fld
type HsName2FldMpMp = HsName2RefMpMp Fld Fld
%%]

%%[(50 codegen) hs export(offMpKeysSorted,offMpMpKeysSet)
-- | Module names, sorted on import order, which is included as 0-based offset (used as index in import entry table)
offMpKeysSorted :: (Ord mref, RefOfFld mref) => HsName2FldMpMp -> AssocL HsName mref
offMpKeysSorted m = sortOn snd [ (n, refOfFld o) | (n,(o,_)) <- Map.toList m ]

offMpMpKeysSet :: HsName2RefMpMp mref meref -> HsNameS
offMpMpKeysSet m = Set.unions [ Map.keysSet m' | (_,m') <- Map.elems m ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Access to fetched/introduced idents in a case alt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(AltFetch(..))
data AltFetch lref
  = AltFetch_Many   [HsName]                -- multiple introduced names
  | AltFetch_One    HsName lref             -- single introduced name, field ref in node (excluding header)
  | AltFetch_Zero
  deriving Eq
%%]


