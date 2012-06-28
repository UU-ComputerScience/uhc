module EH101.Gam.Base
( emptyGam, gamMap, gamLookup, gamLookupDup, gamPushNew, gamPop, gamTop, gamAddGam, gamAdd, gamPushGam, gamToAssocL, gamToAssocDupL, assocLToGam, assocDupLToGam, gamKeys
, gamSingleton, gamInsert, gamUnion, gamUnions, gamFromAssocL
, assocLToGamWithDups
, gamToOnlyDups
, gamPartition
, gamMapElts
, gamMapThr
, gamUnzip
, gamElts
, gamNoDups
, Gam )
where
import Data.List
import EH.Util.Utils
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.NameAspect
import qualified Data.Set as Set
import EH101.Opts.Base
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.Gam.ScopeMapGam







{-# LINE 47 "src/ehc/Gam/Base.chs" #-}
type Gam k v        =   SGam k v

{-# LINE 55 "src/ehc/Gam/Base.chs" #-}
emptyGam            ::            Gam k v
gamSingleton        ::            k -> v        -> Gam k v
gamLookup           ::  Ord k =>  k -> Gam k v  -> Maybe v
gamToAssocL         ::  Ord k =>  Gam k v       -> AssocL k v
gamPushNew          ::            Gam k v       -> Gam k v
gamPushGam          ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamPop              ::  Ord k =>  Gam k v       -> (Gam k v,Gam k v)
gamAddGam           ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamAdd              ::  Ord k =>  k -> v        -> Gam k v -> Gam k v

{-# LINE 80 "src/ehc/Gam/Base.chs" #-}
gamToAssocL     g                   = [ (k,v) | (k,(v:_)) <- gamToAssocDupL g ]
gamLookup       k g                 = fmap head $ gamLookupDup k g
assocLToGam                         = gamUnions . map (uncurry gamSingleton)

{-# LINE 86 "src/ehc/Gam/Base.chs" #-}
emptyGam                            = emptySGam
gamSingleton                        = sgamSingleton
gamPushNew      g                   = sgamPushNew g
gamPushGam      g1 g2               = sgamPushGam g1 g2
gamPop          g                   = sgamPop g
gamAddGam       g1 g2               = sgamUnion g1 g2
gamAdd          k v g               = sgamUnion (sgamSingleton k v) g

{-# LINE 106 "src/ehc/Gam/Base.chs" #-}
gamTop              ::  Ord k =>  Gam k v     -> Gam k v
assocLToGam         ::  Ord k =>  AssocL k v  -> Gam k v

{-# LINE 116 "src/ehc/Gam/Base.chs" #-}
gamTop                              = sgamTop

{-# LINE 129 "src/ehc/Gam/Base.chs" #-}
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = sgamFromAssocDupL

{-# LINE 139 "src/ehc/Gam/Base.chs" #-}
assocLToGamWithDups :: Ord k => AssocL k v -> Gam k v
assocLToGamWithDups = assocDupLToGam . assocLGroupSort

{-# LINE 149 "src/ehc/Gam/Base.chs" #-}
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL g = sgamToAssocDupL g

{-# LINE 159 "src/ehc/Gam/Base.chs" #-}
gamToOnlyDups :: Ord k => Gam k v -> AssocL k [v]
gamToOnlyDups g = [ x | x@(n,(_:_:_)) <- gamToAssocDupL g ]

{-# LINE 169 "src/ehc/Gam/Base.chs" #-}
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups g = sgamNoDups g

{-# LINE 184 "src/ehc/Gam/Base.chs" #-}
gamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap = sgamMap

{-# LINE 194 "src/ehc/Gam/Base.chs" #-}
gamMapElts :: Ord k => (v -> v') -> Gam k v -> Gam k v'
gamMapElts f = gamMap (\(n,v) -> (n,f v))

{-# LINE 207 "src/ehc/Gam/Base.chs" #-}
gamPartition :: Ord k => (k -> v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition = sgamPartitionWithKey

{-# LINE 232 "src/ehc/Gam/Base.chs" #-}
gamMapThr :: (Ord k,Ord k') => ((k,v) -> t -> ((k',v'),t)) -> t -> Gam k v -> (Gam k' v',t)
gamMapThr = sgamMapThr

{-# LINE 242 "src/ehc/Gam/Base.chs" #-}
gamKeys :: Ord k => Gam k v -> [k]
gamKeys = assocLKeys . gamToAssocL

{-# LINE 247 "src/ehc/Gam/Base.chs" #-}
gamElts :: Ord k => Gam k v -> [v]
gamElts = assocLElts . gamToAssocL

{-# LINE 257 "src/ehc/Gam/Base.chs" #-}
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k g = sgamLookupDup k g

{-# LINE 274 "src/ehc/Gam/Base.chs" #-}
gamUnzip :: Ord k => Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip g = sgamUnzip g

{-# LINE 283 "src/ehc/Gam/Base.chs" #-}
gamInsert :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsert = gamAdd

gamUnion :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnion = gamAddGam

gamFromAssocL ::  Ord k =>  AssocL k v  -> Gam k v
gamFromAssocL = assocLToGam

{-# LINE 294 "src/ehc/Gam/Base.chs" #-}
gamUnions :: Ord k => [Gam k v] -> Gam k v
gamUnions [] = emptyGam
gamUnions gs = foldr1 gamUnion gs

