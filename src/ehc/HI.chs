%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell importable interface to HI/AbsSyn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs module {%{EH}HI} import({%{EH}Base.Common},{%{EH}Opts},{%{EH}Base.Builtin},{%{EH}NameAspect})
%%]

%%[(20 hmtyinfer || hmtyast) hs import ({%{EH}Gam.Full},{%{EH}Gam.ClassDefaultGam})
%%]

%%[(20 hmtyinfer || hmtyast) hs import({%{EH}Ty})
%%]

%%[(20 codegen) hs import({%{EH}Base.Target})
%%]
%%[(20 codegen) hs import({%{EH}Core}, {%{EH}LamInfo})
%%]
%%[(20 codegen tycore) hs import(qualified {%{EH}TyCore} as C)
%%]

%%[(20 codegen grin) hs import({%{EH}GrinCode})
%%]
%%[(20 codegen grin) hs import({%{EH}GrinByteCode})
%%]

%%[20 hs import({%{EH}Config},{%{EH}Module})
%%]

%%[(20 hmtyinfer) hs import({%{EH}Pred.ToCHR},{%{EH}CHR.Solve},qualified {%{EH}Pred} as Pr)
%%]

%%[20 hs import(qualified Data.Set as Set,qualified Data.Map as Map,qualified EH.Util.Rel as Rel,qualified EH.Util.FastSeq as Seq,EH.Util.Utils)
%%]

%%[2020 hs export(AGItf(..),Module(..),Binding(..),Bindings)
%%]

%%[20 hs export(Visible(..))
%%]

%%[20 hs import(Control.Monad, {%{EH}Base.Binary})
%%]
%%[20 hs import(Data.Typeable(Typeable), Data.Generics(Data), {%{EH}Base.Serialize})
%%]

%%[9999 hs import({%{EH}Base.ForceEval})
%%]

%%[(9999 codegen grin) hs import({%{EH}GrinCode.Trf.ForceEval})
%%]

-- for debug
%%[20 hs import({%{EH}Base.Debug},EH.Util.Pretty)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional defs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs
data Visible
  = VisibleNo | VisibleYes
  deriving Eq

instance Show Visible where
  show VisibleNo  = "visibleno"
  show VisibleYes = "visibleyes"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HI info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs export(HIInfo(..))
data HIInfo
  = HIInfo
      { hiiValidity             :: !HIValidity								-- a valid HI info?
      , hiiOrigin               :: !HIOrigin								-- where did the HI come from
      , hiiSrcSig               :: !String									-- compiler source signature (md5)
      , hiiTargetFlavor         :: !TargetFlavor							-- for which flavor the hi is generated
      , hiiCompiler             :: !String									-- compiler version info
      , hiiCompileFlags         :: !String									-- flags
      , hiiHasMain              :: !Bool									-- has file a main?
      , hiiModuleNm             :: !HsName									-- module name
      , hiiSrcTimeStamp         :: !String									-- timestamp of compiler source
      , hiiSrcVersionMajor      :: !String									-- major (etc) version numbers
      , hiiSrcVersionMinor      :: !String
      , hiiSrcVersionMinorMinor :: !String
      , hiiSrcVersionSvn        :: !String									-- svn version

      , hiiExps                 :: !ModEntRel								-- exported stuff
      , hiiHiddenExps           :: !ModEntRel								-- exported, but hidden otherwise (instances, optimized code variants, ...)
      , hiiFixityGam            :: !FixityGam								-- fixity of identifiers
      -- , hiiIdDefAssocL          :: !(AssocL IdOcc IdOcc) -- IdDefOccGam		-- name mappings
      -- , hiiIdDefHIIdGam         :: !(Gam IdOcc IdOcc) -- IdDefOccGam		-- name mappings
      , hiiHIDeclImpModL        :: ![HsName]								-- declared imports
      , hiiHIUsedImpModL        :: ![HsName]								-- used imports, usually indirectly via renaming
%%[[(20 hmtyinfer)
      , hiiValGam               :: !ValGam									-- value identifier environment
      , hiiTyGam                :: !TyGam									-- type identifier env
      , hiiTyKiGam              :: !TyKiGam									-- type/tyvar kind env
      , hiiPolGam               :: !PolGam									-- polarity env
      , hiiDataGam              :: !DataGam									-- datatype info env
      , hiiClGam                :: !Pr.ClGam								-- class env
      , hiiClDfGam              :: !ClassDefaultGam							-- class defaults env
      , hiiCHRStore             :: !ScopedPredStore							-- rule database
%%]]
%%[[(20 codegen)
      , hiiLamMp                :: !LamMp									-- codegen info for identifiers
%%]]
%%[[(20 codegen grin)
      , hiiGrInlMp              :: !GrInlMp									-- grin inlineable code
%%]]
%%[[99
      , hiiImpHIMp              :: !ImpHIMp									-- cache of HIInfo's of imported modules, filtered for visibility
%%]]
      }
%%[[20
  deriving (Typeable, Data)
%%]]
%%]

%%[20 hs export(emptyHIInfo)
emptyHIInfo :: HIInfo
emptyHIInfo 
  = HIInfo HIValidity_Absent HIOrigin_FromFile
           "" defaultTargetFlavor "" "" False hsnUnknown "" "" "" "" ""
           Rel.empty Rel.empty emptyGam -- emptyGam
           [] []
           -- emptyHiSettings
%%[[(20 hmtyinfer)
           emptyGam emptyGam emptyGam emptyGam emptyGam emptyGam emptyGam emptyCHRStore
%%]]
%%[[(20 codegen)
           Map.empty
%%]]
%%[[(20 codegen grin)
           Map.empty
%%]]
%%[[99
           Map.empty
%%]]
%%]

%%[20 hs export(hiiIsEmpty)
-- | not empty if ok
hiiIsEmpty :: HIInfo -> Bool
hiiIsEmpty hii = hiiValidity hii /= HIValidity_Ok
%%]

%%[20 hs export(hiiIdDefOccGam)
hiiIdDefOccGam :: HIInfo -> IdDefOccGam
hiiIdDefOccGam hii = hiiIdDefOccGamFromHIIdGam $ mentrelToIdDefOccGam (hiiModuleNm hii) (hiiExps hii)
%%]

%%[2020 hs export(hiiCHRStore)
hiiCHRStore :: HIInfo -> ScopedPredStore
hiiCHRStore = hiiScopedPredStoreFromList . hiiCHRStoreL
%%]

%%[20 hs
instance Show HIInfo where
  show _ = "HIInfo"

instance PP HIInfo where
  pp i = "HIInfo" >#< (   "ModNm  =" >|< pp      	(hiiModuleNm  	  i)
                      >-< "DeclImp=" >|< ppCommas 	(hiiHIDeclImpModL i)
                      >-< "UsedImp=" >|< ppCommas 	(hiiHIUsedImpModL i)
                      >-< "Exps="    >|< pp       	(hiiExps          i)
                      >-< "Exps(H)=" >|< pp       	(hiiHiddenExps    i)
                      >-< "ValGam =" >|< pp       	(hiiValGam 	      i)
                      >-< "TyGam  =" >|< pp       	(hiiTyGam 	      i)
                      -- >-< "IdGam  =" >|< pp       	(hiiIdDefHIIdGam  i)
%%[[99
                      >-< "Cached =" >|< ppAssocLV 	(assocLMapElt pp $ Map.toList $ hiiImpHIMp    i)
%%]]
                      )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils for caching imported HIInfos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 hs export(ImpHIMp)
type ImpHIMp = Map.Map HsName HIInfo

%%]

%%[99 hs export(hiiUnion)
-- | combine HI info for a single module, as extracted from the cached hiiImpHIMp of the module importing these combined modules
hiiUnion :: HIInfo -> HIInfo -> HIInfo
hiiUnion m1 m2
  = m1 { hiiFixityGam			= hiiFixityGam 		m1 `gamUnion` 		hiiFixityGam 	m2
       -- , hiiIdDefHIIdGam        = hiiIdDefHIIdGam 	m1 `gamUnion`		hiiIdDefHIIdGam m2
%%[[(99 hmtyinfer)
       , hiiValGam              = hiiValGam 		m1 `gamUnion` 		hiiValGam 		m2
       , hiiTyGam               = hiiTyGam 			m1 `gamUnion` 		hiiTyGam 		m2
       , hiiTyKiGam             = hiiTyKiGam 		m1 `gamUnion` 		hiiTyKiGam 		m2
       , hiiPolGam              = hiiPolGam 		m1 `gamUnion` 		hiiPolGam 		m2
       , hiiDataGam             = hiiDataGam 		m1 `gamUnion` 		hiiDataGam 		m2
       , hiiClGam               = hiiClGam 			m1 `gamUnion` 		hiiClGam 		m2
       , hiiClDfGam             = hiiClDfGam 		m1 `gamUnion` 		hiiClDfGam 		m2
       , hiiCHRStore            = hiiCHRStore 		m1 `chrStoreUnion`	hiiCHRStore	 	m2
%%]]
%%[[(99 codegen)
       , hiiLamMp               = hiiLamMp 			m1 `Map.union` 		hiiLamMp 		m2
%%]]
%%[[(99 codegen grin)
       , hiiGrInlMp             = hiiGrInlMp 		m1 `Map.union` 		hiiGrInlMp 		m2
%%]]
       }
%%]

%%[99 hs
-- | restrict envs to the ones being in the filter map, so only those visible relative to that map remain
hiiRestrictToFilterMp :: ModEntRelFilterMp -> HIInfo -> HIInfo
hiiRestrictToFilterMp mfm hii
  = hii
      { hiiFixityGam			= fg expVT $ hiiFixityGam 		hii
      -- , hiiIdDefHIIdGam         = fg (\o -> exp (ioccKind o) (ioccNm o))
      --                                      $ hiiIdDefHIIdGam    hii
%%[[(99 hmtyinfer)
      , hiiValGam              	= fg expV  $ hiiValGam 			hii
      , hiiTyGam               	= fg expT  $ hiiTyGam 			hii
      , hiiTyKiGam             	= fg expT' $ hiiTyKiGam 		hii
      , hiiPolGam              	= fg expT  $ hiiPolGam 			hii
      , hiiDataGam             	= fg expT  $ hiiDataGam 		hii
      , hiiClGam               	= fg expC  $ hiiClGam 			hii
      , hiiClDfGam             	= fg expC  $ hiiClDfGam 		hii
%%]]
%%[[(99 codegen)
      , hiiLamMp               	= fm expV  $ hiiLamMp 			hii
%%]]
%%[[(99 codegen grin)
      , hiiGrInlMp             	= fm expV  $ hiiGrInlMp 		hii
%%]]
      }
  where exp k  = (`Set.member` Map.findWithDefault Set.empty k mfm)
        expV   = exp IdOcc_Val
        expT   = exp IdOcc_Type
        expT'  = maybe False (exp IdOcc_Type) . tyKiKeyMbName
        expVT x= expV x || expT x
        expC   = expT -- exp IdOcc_Class
        fg p   = fst . gamPartition (\k _ -> p k)
        fm p   = Map.filterWithKey (\k _ -> p k)
%%]

%%[99 hs
-- | restrict envs to the ones being exported, so only the visible part remains
hiiRestrictToExported :: HIInfo -> HIInfo
hiiRestrictToExported hii = hiiRestrictToFilterMp (mentrelToFilterMp [] (hiiExps hii) `mentrelFilterMpUnion` mentrelToFilterMp [] (hiiHiddenExps hii)) hii
%%]

%%[99 hs export(hiiIncludeCacheOfImport)
-- | include the imported HIInfos in this one, restricted to their exports, to be done just before saving
hiiIncludeCacheOfImport :: (HsName -> HIInfo) -> ModEntRelFilterMp -> HIInfo -> HIInfo
hiiIncludeCacheOfImport imp mfm hii
  = hii
      { hiiImpHIMp = Map.map reset $ Map.unions [top, subtop]
      }
  where -- imports of this module
        top    = Map.unions [ Map.singleton i $ hiiRestrictToFilterMp mfm $ {- (\x -> tr "hiiIncludeCacheOfImport.1" (i >#< x) x) $ -} imp i | i <- hiiHIDeclImpModL hii ++ hiiHIUsedImpModL hii ]

        -- the closure of the imports w.r.t. import relationship
        subtop = Map.map (hiiRestrictToFilterMp mfm) $ Map.unionsWith hiiUnion $ map hiiImpHIMp $ Map.elems top
        
        -- reset some info in cached hii's
        reset hii = hii { hiiImpHIMp                = Map.empty
                        , hiiExps                   = Rel.empty
                        , hiiHiddenExps             = Rel.empty
                        , hiiHIDeclImpModL          = []
                        , hiiHIUsedImpModL          = []
                        , hiiCHRStore               = emptyCHRStore     -- this cannot be, but no solution for filtering this...
                        , hiiSrcSig                 = ""
                        , hiiCompiler               = ""
                        , hiiSrcTimeStamp           = ""
                        , hiiSrcVersionMajor        = ""
                        , hiiSrcVersionMinor        = ""
                        , hiiSrcVersionMinorMinor   = ""
                        , hiiSrcVersionSvn          = ""
                        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reconstruction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(mentrelToIdDefOccGam)
mentrelToIdDefOccGam :: HsName -> ModEntRel -> Gam IdOcc IdOcc -- IdDefOccGam
mentrelToIdDefOccGam modNm r
  = gamFromAssocL
      [ ( IdOcc n' k
        -- , mkIdDefOcc (IdOcc (ioccNm $ mentIdOcc e) k) IdAsp_Any nmLevOutside emptyRange
        , IdOcc (ioccNm $ mentIdOcc e) k
        )
      | (n,e) <- Rel.toList r
      , let k  = ioccKind $ mentIdOcc e
            n' = hsnSetQual modNm n
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs export(hiiIdDefOccGamToHIIdGam,hiiIdDefOccGamFromHIIdGam)
hiiIdDefOccGamToHIIdGam :: IdDefOccGam -> Gam IdOcc IdOcc
hiiIdDefOccGamToHIIdGam = gamMap (\(k,v) -> (k,doccOcc v))

hiiIdDefOccGamFromHIIdGam :: Gam IdOcc IdOcc -> IdDefOccGam
hiiIdDefOccGamFromHIIdGam = gamMap (\(k,v) -> (k,mkIdDefOcc v IdAsp_Any nmLevOutside emptyRange))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Validity, origin of HI file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs export(HIOrigin(..))
data HIOrigin
  = HIOrigin_FromFile								-- from .hi file
  | HIOrigin_FromImportedBy HsNameS					-- reconstructed from modules which imported this hi
  deriving (Eq,Show,Typeable,Data)
%%]

%%[20 hs export(HIValidity(..))
data HIValidity
  = HIValidity_Ok				-- ok
  | HIValidity_Inconsistent		-- inconsistent with compiler
  | HIValidity_Absent			-- not available
  deriving (Eq,Enum,Show,Typeable,Data)
%%]

%%[2020 hs export(hiiPostCheckValidity)
hiiPostCheckValidity :: EHCOpts -> HIInfo -> HIInfo
hiiPostCheckValidity opts i
  = i { hiiIsValid
          =    hiiIsValid i
            -- && optsDiscrRecompileRepr opts == hiiCompileFlags  i
            && ehcOptTargetFlavor    opts == hiiTargetFlavor i
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam flattening
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs
gamFlatten :: Ord k => Gam k v -> Gam k v
gamFlatten = id -- gamFromAssocL . gamToAssocL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs
instance Serialize HIValidity where
  sput = sputEnum8
  sget = sgetEnum8
%%]

%%[20 hs export(sgetHIInfo)
sgetHIInfo :: EHCOpts -> SGet HIInfo
sgetHIInfo opts = do
  { hi_sig  <- sget
  ; hi_ts   <- sget
  ; hi_tv   <- sget
  ; hi_fl   <- sget
  ; hi_comp <- sget
  ; if (    hi_sig == verSig version
         && hi_ts  == verTimestamp version
         && hi_tv  == ehcOptTargetFlavor opts
       )
%%[[99
       || not (ehcOptHiValidityCheck opts)
%%]]
    then do { hi_nm     <- sget
            ; hi_hm     <- sget
            ; hi_m      <- sget
            ; hi_mm     <- sget
            ; hi_mmm    <- sget
            ; hi_svn    <- sget
            ; e         <- sget
            ; he        <- sget
            ; fg        <- sget
            -- ; idg       <- sget
            ; impd      <- sget
            ; impu      <- sget
%%[[(20 hmtyinfer)
            ; vg        <- sget
            ; tg        <- sget
            ; tkg       <- sget
            ; pg        <- sget
            ; dg        <- sget
            ; cg        <- sget
            ; cdg       <- sget
            ; cs        <- sget
%%]]
%%[[(20 codegen)
            ; am        <- sget
%%]]
%%[[(20 codegen grin)
            ; im        <- sget
%%]]
%%[[99
            ; him       <- sget
%%]]
            ; return 
                (emptyHIInfo
                  { hiiValidity             = HIValidity_Ok
                  , hiiSrcSig               = hi_sig
                  , hiiCompiler             = hi_comp
                  , hiiCompileFlags         = hi_fl
                  , hiiTargetFlavor         = hi_tv
                  , hiiHasMain              = hi_hm
                  , hiiSrcTimeStamp         = hi_ts
                  , hiiModuleNm             = hi_nm
                  , hiiSrcVersionMajor      = hi_m
                  , hiiSrcVersionMinor      = hi_mm
                  , hiiSrcVersionMinorMinor = hi_mmm
                  , hiiSrcVersionSvn        = hi_svn
                  , hiiExps                 = e
                  , hiiHiddenExps           = he
                  , hiiFixityGam            = fg
                  -- , hiiIdDefHIIdGam         = {- tr "HIInfo.Binary.get idGam" (pp $ lookup (IdOcc (mkHNm "Prelude.putStrLn") IdOcc_Val) idg) $ -}
                  --                             mentrelToIdDefOccGam hi_nm e -- idg
                  , hiiHIDeclImpModL        = impd
                  , hiiHIUsedImpModL        = impu
%%[[(20 hmtyinfer)
                  , hiiValGam               = vg
                  , hiiTyGam                = tg
                  , hiiTyKiGam              = tkg
                  , hiiPolGam               = pg
                  , hiiDataGam              = dg
                  , hiiClGam                = cg
                  , hiiClDfGam              = cdg
                  , hiiCHRStore             = cs
%%]]
%%[[(20 codegen)
                  , hiiLamMp                = am
%%]]
%%[[(20 codegen grin)
                  , hiiGrInlMp              = im
%%]]
%%[[99
                  , hiiImpHIMp              = him
%%]]
                  })
            }
    else do { return
                (emptyHIInfo
                  { hiiValidity             = HIValidity_Inconsistent
                  , hiiSrcSig               = hi_sig
                  , hiiSrcTimeStamp         = hi_ts
                  , hiiCompileFlags         = hi_fl
                  , hiiCompiler             = hi_comp
                  , hiiTargetFlavor         = hi_tv
                  })
            }
  }
%%]

%%[20 hs
instance Serialize HIInfo where
  sput       (HIInfo
                  { hiiSrcSig               = hi_sig
                  , hiiTargetFlavor         = hi_tv
                  , hiiCompiler             = hi_comp
                  , hiiCompileFlags         = hi_fl
                  , hiiModuleNm             = hi_nm
                  , hiiHasMain              = hi_hm
                  , hiiSrcTimeStamp         = hi_ts
                  , hiiSrcVersionMajor      = hi_m
                  , hiiSrcVersionMinor      = hi_mm
                  , hiiSrcVersionMinorMinor = hi_mmm
                  , hiiSrcVersionSvn        = hi_svn
                  , hiiExps                 = e
                  , hiiHiddenExps           = he
                  , hiiFixityGam            = fg
                  -- , hiiIdDefHIIdGam         = idg
                  , hiiHIDeclImpModL        = impd
                  , hiiHIUsedImpModL        = impu
%%[[(20 hmtyinfer)
                  , hiiValGam               = vg
                  , hiiTyGam                = tg
                  , hiiTyKiGam              = tkg
                  , hiiPolGam               = pg
                  , hiiDataGam              = dg
                  , hiiClGam                = cg
                  , hiiClDfGam              = cdg
                  , hiiCHRStore             = cs
%%]]
%%[[(20 codegen)
                  , hiiLamMp                = am
%%]]
%%[[(20 codegen grin)
                  , hiiGrInlMp              = im
%%]]
%%[[99
                  , hiiImpHIMp              = him
%%]]
                  })
              =    sput hi_sig
                >> sput hi_ts
                >> sput hi_tv
                >> sput hi_fl
                >> sput hi_comp
                >> sput hi_nm
                >> sput hi_hm
                >> sput hi_m
                >> sput hi_mm
                >> sput hi_mmm
                >> sput hi_svn
                >> sput e
                >> sput he
                >> sput (gamFlatten fg)
                -- >> sput idg
                >> sput impd
                >> sput impu
%%[[(20 hmtyinfer)
                >> sput (gamFlatten vg)
                >> sput (gamFlatten tg)
                >> sput (gamFlatten tkg)
                >> sput (gamFlatten pg)
                >> sput (gamFlatten dg)
                >> sput (gamFlatten cg)
                >> sput (gamFlatten cdg)
                >> sput cs
%%]]
%%[[(20 codegen)
                >> sput am
%%]]
%%[[(20 codegen grin)
                >> sput im
%%]]
%%[[99
                >> sput him
%%]]

  sget = sgetHIInfo (defaultEHCOpts
%%[[99
                       { ehcOptHiValidityCheck = False
                       }
%%]]
                    )
%%]

