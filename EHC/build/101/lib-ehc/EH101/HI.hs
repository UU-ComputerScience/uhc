module EH101.HI
( Visible (..)
, HIInfoUsedModMp, HIInfo (..)
, emptyHIInfo
, hiiIsEmpty
, hiiIdDefOccGam
, mentrelToIdDefOccGam
, hiiIdDefOccGamToHIIdGam, hiiIdDefOccGamFromHIIdGam
, HIOrigin (..)
, HIValidity (..)
, sgetHIInfo
, ImpHIMp
, hiiUnion
, hiiIncludeCacheOfImport )
where
import EH101.Base.Common
import EH101.Opts
import EH101.Base.Builtin
import EH101.NameAspect
import EH101.Gam.Full
import EH101.Gam.ClassDefaultGam
import EH101.Ty
import EH101.Base.Target
import EH101.Core
import EH101.LamInfo
import EH101.GrinCode
import EH101.GrinByteCode
import EH101.Config
import EH101.Module
import EH101.Pred.ToCHR
import EH101.CHR.Solve
import qualified EH101.Pred as Pr
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified EH.Util.Rel as Rel
import qualified EH.Util.FastSeq as Seq
import EH.Util.Utils
import Control.Monad
import EH101.Base.Binary
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Base.Serialize
import qualified EH101.Config as Cfg
import EH101.Base.Debug
import EH.Util.Pretty







{-# LINE 67 "src/ehc/HI.chs" #-}
data Visible
  = VisibleNo | VisibleYes
  deriving Eq

instance Show Visible where
  show VisibleNo  = "visibleno"
  show VisibleYes = "visibleyes"

{-# LINE 81 "src/ehc/HI.chs" #-}
type HIInfoUsedModMp = (Map.Map HsName (Set.Set HsName))

data HIInfo
  = HIInfo
      { hiiValidity             :: !HIValidity                              -- a valid HI info?
      , hiiOrigin               :: !HIOrigin                                -- where did the HI come from
      , hiiSrcSig               :: !String                                  -- compiler source signature (md5)
      , hiiTarget               :: !Target                                  -- for which backend the hi is generated
      , hiiTargetFlavor         :: !TargetFlavor                            -- for which flavor the hi is generated
      , hiiCompiler             :: !String                                  -- compiler version info
      , hiiCompileFlags         :: !String                                  -- flags
      , hiiHasMain              :: !Bool                                    -- has file a main?
      , hiiModuleNm             :: !HsName                                  -- module name
      , hiiSrcTimeStamp         :: !String                                  -- timestamp of compiler source
      , hiiSrcVersionMajor      :: !String                                  -- major (etc) version numbers
      , hiiSrcVersionMinor      :: !String
      , hiiSrcVersionMinorMinor :: !String
      , hiiSrcVersionSvn        :: !String                                  -- svn version

      , hiiExps                 :: !ModEntRel                               -- exported stuff
      , hiiHiddenExps           :: !ModEntRel                               -- exported, but hidden otherwise (instances, optimized code variants, ...)
      , hiiFixityGam            :: !FixityGam                               -- fixity of identifiers
      , hiiHIDeclImpModS        :: !(Set.Set HsName)                        -- declared imports
      , hiiHIUsedImpModS        :: !(Set.Set HsName)                        -- used imports, usually indirectly via renaming
      , hiiTransClosedUsedModMp :: !HIInfoUsedModMp       					-- used modules with their imports, required to be linked together, transitively closed/cached over imported modules
      , hiiTransClosedOrphanModS:: !(Set.Set HsName)                        -- orphan modules, required to read its .hi file, transitively closed/cached over imported modules
      , hiiMbOrphan             :: !(Maybe (Set.Set HsName))                -- is orphan module, carrying the module names required
      , hiiValGam               :: !ValGam                                  -- value identifier environment
      , hiiTyGam                :: !TyGam                                   -- type identifier env
      , hiiTyKiGam              :: !TyKiGam                                 -- type/tyvar kind env
      , hiiPolGam               :: !PolGam                                  -- polarity env
      , hiiDataGam              :: !DataGam                                 -- datatype info env
      , hiiClGam                :: !Pr.ClGam                                -- class env
      , hiiClDfGam              :: !ClassDefaultGam                         -- class defaults env
      , hiiCHRStore             :: !ScopedPredStore                         -- rule database
      , hiiLamMp                :: !LamMp                                   -- codegen info for identifiers
      , hiiGrInlMp              :: !GrInlMp                                 -- grin inlineable code
      , hiiImpHIMp              :: !ImpHIMp                                 -- cache of HIInfo's of imported modules, filtered for visibility
      }
  deriving (Typeable, Data)

{-# LINE 134 "src/ehc/HI.chs" #-}
emptyHIInfo :: HIInfo
emptyHIInfo
  = HIInfo HIValidity_Absent HIOrigin_FromFile
           "" defaultTarget defaultTargetFlavor "" "" False hsnUnknown "" "" "" "" ""
           Rel.empty Rel.empty emptyGam
           Set.empty Set.empty
           Map.empty Set.empty Nothing
           emptyGam emptyGam emptyGam emptyGam emptyGam emptyGam emptyGam emptyCHRStore
           Map.empty
           Map.empty
           Map.empty

{-# LINE 156 "src/ehc/HI.chs" #-}
-- | not empty if ok
hiiIsEmpty :: HIInfo -> Bool
hiiIsEmpty hii = hiiValidity hii /= HIValidity_Ok

{-# LINE 162 "src/ehc/HI.chs" #-}
hiiIdDefOccGam :: HIInfo -> IdDefOccGam
hiiIdDefOccGam hii = hiiIdDefOccGamFromHIIdGam $ mentrelToIdDefOccGam (hiiModuleNm hii) (hiiExps hii)

{-# LINE 172 "src/ehc/HI.chs" #-}
instance Show HIInfo where
  show _ = "HIInfo"

instance PP HIInfo where
  pp i = "HIInfo" >#< (   "ModNm  =" >#< pp         (             hiiModuleNm              i)
                      >-< "DeclImp=" >#< ppCommas   (Set.toList $ hiiHIDeclImpModS         i)
                      >-< "UsedImp=" >#< ppCommas   (Set.toList $ hiiHIUsedImpModS         i)
                      >-< "AllUsed=" >#< ppAssocLV  (assocLMapElt (ppCommas . Set.toList) $ Map.toList $ hiiTransClosedUsedModMp i)
                      >-< "AllOrph=" >#< ppCommas   (Set.toList $ hiiTransClosedOrphanModS i)
                      >-< "MbOrph =" >#< ppCommas   (maybe [] Set.toList $ hiiMbOrphan     i)
                      -- >-< "Exps="    >#< pp         (hiiExps          i)
                      -- >-< "Exps(H)=" >#< pp         (hiiHiddenExps    i)
                      -- >-< "ValGam =" >#< pp         (hiiValGam        i)
                      -- >-< "TyGam  =" >#< pp         (hiiTyGam         i)
                      -- >-< "Cached =" >#< ppAssocLV  (assocLMapElt pp $ Map.toList $ hiiImpHIMp    i)
                      )

{-# LINE 211 "src/ehc/HI.chs" #-}
type ImpHIMp = Map.Map HsName HIInfo


{-# LINE 216 "src/ehc/HI.chs" #-}
-- | combine HI info for a single module, as extracted from the cached hiiImpHIMp of the module importing these combined modules
hiiUnion :: HIInfo -> HIInfo -> HIInfo
hiiUnion m1 m2
  = m1 { hiiFixityGam           = hiiFixityGam      m1 `gamUnion`       hiiFixityGam    m2
       -- , hiiIdDefHIIdGam        = hiiIdDefHIIdGam    m1 `gamUnion`       hiiIdDefHIIdGam m2
       , hiiValGam              = hiiValGam         m1 `gamUnion`       hiiValGam       m2
       , hiiTyGam               = hiiTyGam          m1 `gamUnion`       hiiTyGam        m2
       , hiiTyKiGam             = hiiTyKiGam        m1 `gamUnion`       hiiTyKiGam      m2
       , hiiPolGam              = hiiPolGam         m1 `gamUnion`       hiiPolGam       m2
       , hiiDataGam             = hiiDataGam        m1 `gamUnion`       hiiDataGam      m2
       , hiiClGam               = hiiClGam          m1 `gamUnion`       hiiClGam        m2
       , hiiClDfGam             = hiiClDfGam        m1 `gamUnion`       hiiClDfGam      m2
       , hiiCHRStore            = hiiCHRStore       m1 `chrStoreUnion`  hiiCHRStore     m2
       , hiiLamMp               = hiiLamMp          m1 `Map.union`      hiiLamMp        m2
       , hiiGrInlMp             = hiiGrInlMp        m1 `Map.union`      hiiGrInlMp      m2
       }

{-# LINE 241 "src/ehc/HI.chs" #-}
-- | restrict envs to the ones being in the filter map, so only those visible relative to that map remain
hiiRestrictToFilterMp :: ModEntRelFilterMp -> HIInfo -> HIInfo
hiiRestrictToFilterMp mfm hii
  = hii
      { hiiFixityGam            = fg expVT $ hiiFixityGam       hii
      -- , hiiIdDefHIIdGam         = fg (\o -> exp (ioccKind o) (ioccNm o))
      --                                      $ hiiIdDefHIIdGam    hii
      , hiiValGam               = fg expV  $ hiiValGam          hii
      , hiiTyGam                = fg expT  $ hiiTyGam           hii
      , hiiTyKiGam              = fg expT' $ hiiTyKiGam         hii
      , hiiPolGam               = fg expT  $ hiiPolGam          hii
      , hiiDataGam              = fg expT  $ hiiDataGam         hii
      , hiiClGam                = fg expC  $ hiiClGam           hii
      , hiiClDfGam              = fg expC  $ hiiClDfGam         hii
      , hiiLamMp                = fm expV  $ hiiLamMp           hii
      , hiiGrInlMp              = fm expV  $ hiiGrInlMp         hii
      }
  where exp k  = (`Set.member` Map.findWithDefault Set.empty k mfm)
        expV   = exp IdOcc_Val
        expT   = exp IdOcc_Type
        expT'  = maybe False (exp IdOcc_Type) . tyKiKeyMbName
        expVT x= expV x || expT x
        expC   = expT -- exp IdOcc_Class
        fg p   = fst . gamPartition (\k _ -> p k)
        fm p   = Map.filterWithKey (\k _ -> p k)

{-# LINE 275 "src/ehc/HI.chs" #-}
-- | restrict envs to the ones being exported, so only the visible part remains
hiiRestrictToExported :: HIInfo -> HIInfo
hiiRestrictToExported hii = hiiRestrictToFilterMp (mentrelToFilterMp [] (hiiExps hii) `mentrelFilterMpUnion` mentrelToFilterMp [] (hiiHiddenExps hii)) hii

{-# LINE 281 "src/ehc/HI.chs" #-}
-- | include the imported HIInfos in this one, restricted to their exports, to be done just before saving
hiiIncludeCacheOfImport :: (HsName -> HIInfo) -> ModEntRelFilterMp -> HIInfo -> HIInfo
hiiIncludeCacheOfImport imp mfm hii
  = hii
      { hiiImpHIMp = Map.map reset $ Map.unions [top, subtop]
      }
  where -- imports of this module
        top    = Map.unions [ Map.singleton i $ hiiRestrictToFilterMp mfm $ {- (\x -> tr "hiiIncludeCacheOfImport.1" (i >#< x) x) $ -} imp i | i <- Set.toList $ hiiHIDeclImpModS hii `Set.union` hiiHIUsedImpModS hii ]

        -- the closure of the imports w.r.t. import relationship
        subtop = Map.map (hiiRestrictToFilterMp mfm) $ Map.unionsWith hiiUnion $ map hiiImpHIMp $ Map.elems top

        -- reset some info in cached hii's
        reset hii = hii { hiiImpHIMp                = Map.empty
                        , hiiExps                   = Rel.empty
                        , hiiHiddenExps             = Rel.empty
                        , hiiHIDeclImpModS          = Set.empty
                        , hiiHIUsedImpModS          = Set.empty
                        , hiiCHRStore               = emptyCHRStore     -- this cannot be, but no solution for filtering this...
                        , hiiSrcSig                 = ""
                        , hiiCompiler               = ""
                        , hiiSrcTimeStamp           = ""
                        , hiiSrcVersionMajor        = ""
                        , hiiSrcVersionMinor        = ""
                        , hiiSrcVersionMinorMinor   = ""
                        , hiiSrcVersionSvn          = ""
                        }

{-# LINE 315 "src/ehc/HI.chs" #-}
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

{-# LINE 333 "src/ehc/HI.chs" #-}
hiiIdDefOccGamToHIIdGam :: IdDefOccGam -> Gam IdOcc IdOcc
hiiIdDefOccGamToHIIdGam = gamMap (\(k,v) -> (k,doccOcc v))

hiiIdDefOccGamFromHIIdGam :: Gam IdOcc IdOcc -> IdDefOccGam
hiiIdDefOccGamFromHIIdGam = gamMap (\(k,v) -> (k,mkIdDefOcc v IdAsp_Any nmLevOutside emptyRange))

{-# LINE 345 "src/ehc/HI.chs" #-}
data HIOrigin
  = HIOrigin_FromFile                               -- from .hi file
  | HIOrigin_FromImportedBy HsNameS                 -- reconstructed from modules which imported this hi
  deriving (Eq,Show,Typeable,Data)

{-# LINE 352 "src/ehc/HI.chs" #-}
data HIValidity
  = HIValidity_Ok               -- ok
  | HIValidity_WrongMagic       -- wrong magic number
  | HIValidity_Inconsistent     -- inconsistent with compiler
  | HIValidity_Absent           -- not available
  deriving (Eq,Enum,Show,Typeable,Data)

{-# LINE 365 "src/ehc/HI.chs" #-}
gamFlatten :: Ord k => Gam k v -> Gam k v
gamFlatten = id -- gamFromAssocL . gamToAssocL

{-# LINE 374 "src/ehc/HI.chs" #-}
instance Serialize HIValidity where
  sput = sputEnum8
  sget = sgetEnum8

{-# LINE 380 "src/ehc/HI.chs" #-}
sgetHIInfo :: EHCOpts -> SGet HIInfo
sgetHIInfo opts = do
  { hi_magic <- sequence $ replicate (length Cfg.magicNumberHI) sgetWord8
  ; if hi_magic == Cfg.magicNumberHI
    then do { hi_sig   <- sget
            ; hi_ts    <- sget
            ; hi_t     <- sget
            ; hi_tv    <- sget
            ; hi_fl    <- sget
            ; hi_comp  <- sget
            ; if (    hi_sig == verSig version
                   && hi_ts  == verTimestamp version
                   && hi_t   == ehcOptTarget       opts
                   && hi_tv  == ehcOptTargetFlavor opts
                 )
                 || not (ehcOptHiValidityCheck opts)
              then do { hi_nm     <- sget
                      ; hi_hm     <- sget
                      ; hi_m      <- sget
                      ; hi_mm     <- sget
                      ; hi_mmm    <- sget
                      ; hi_svn    <- sget
                      ; e         <- sget
                      ; he        <- sget
                      ; fg        <- sget
                      ; impd      <- sget
                      ; impu      <- sget
                      ; tclused   <- sget
                      ; tclorph   <- sget
                      ; isorph    <- sget
                      ; vg        <- sget
                      ; tg        <- sget
                      ; tkg       <- sget
                      ; pg        <- sget
                      ; dg        <- sget
                      ; cg        <- sget
                      ; cdg       <- sget
                      ; cs        <- sget
                      ; am        <- sget
                      ; im        <- sget
                      ; him       <- sget
                      ; return
                          (emptyHIInfo
                            { hiiValidity             = HIValidity_Ok
                            , hiiSrcSig               = hi_sig
                            , hiiCompiler             = hi_comp
                            , hiiCompileFlags         = hi_fl
                            , hiiTarget               = hi_t
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
                            , hiiHIDeclImpModS        = impd
                            , hiiHIUsedImpModS        = impu
                            , hiiTransClosedUsedModMp = tclused
                            , hiiTransClosedOrphanModS= tclorph
                            , hiiMbOrphan             = isorph
                            , hiiValGam               = vg
                            , hiiTyGam                = tg
                            , hiiTyKiGam              = tkg
                            , hiiPolGam               = pg
                            , hiiDataGam              = dg
                            , hiiClGam                = cg
                            , hiiClDfGam              = cdg
                            , hiiCHRStore             = cs
                            , hiiLamMp                = am
                            , hiiGrInlMp              = im
                            , hiiImpHIMp              = him
                            })
                      }
              else return $
                     emptyHIInfo
                       { hiiValidity             = HIValidity_Inconsistent
                       , hiiSrcSig               = hi_sig
                       , hiiSrcTimeStamp         = hi_ts
                       , hiiCompileFlags         = hi_fl
                       , hiiCompiler             = hi_comp
                       , hiiTarget               = hi_t
                       , hiiTargetFlavor         = hi_tv
                       }
            }
    else return $
           emptyHIInfo
             { hiiValidity             = HIValidity_WrongMagic
             }
  }

{-# LINE 494 "src/ehc/HI.chs" #-}
instance Serialize HIInfo where
  sput       (HIInfo
                  { hiiSrcSig               = hi_sig
                  , hiiTarget               = hi_t
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
                  , hiiHIDeclImpModS        = impd
                  , hiiHIUsedImpModS        = impu
                  , hiiTransClosedUsedModMp = tclused
                  , hiiTransClosedOrphanModS= tclorph
                  , hiiMbOrphan             = isorph
                  , hiiValGam               = vg
                  , hiiTyGam                = tg
                  , hiiTyKiGam              = tkg
                  , hiiPolGam               = pg
                  , hiiDataGam              = dg
                  , hiiClGam                = cg
                  , hiiClDfGam              = cdg
                  , hiiCHRStore             = cs
                  , hiiLamMp                = am
                  , hiiGrInlMp              = im
                  , hiiImpHIMp              = him
                  })
              =    mapM sputWord8 Cfg.magicNumberHI
                >> sput hi_sig
                >> sput hi_ts
                >> sput hi_t
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
                >> sput impd
                >> sput impu
                >> sput tclused
                >> sput tclorph
                >> sput isorph
                >> sput (gamFlatten vg)
                >> sput (gamFlatten tg)
                >> sput (gamFlatten tkg)
                >> sput (gamFlatten pg)
                >> sput (gamFlatten dg)
                >> sput (gamFlatten cg)
                >> sput (gamFlatten cdg)
                >> sput cs
                >> sput am
                >> sput im
                >> sput him

  sget = sgetHIInfo (defaultEHCOpts
                       { ehcOptHiValidityCheck = False
                       }
                    )

