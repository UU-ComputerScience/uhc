module EH101.Base.UID
( mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, mkNewLevUID6, mkNewLevUID7, mkNewLevUID8, uidNext, mkNewUID, mkNewUIDL, uidNull, uidChild, mkInfNewUIDL
, UID (..)
, mkUID
, uidFromInt
, uidStart, uidUnused
, nextUnique
, UIDS
, mkNewLevUIDL, mkInfNewLevUIDL
, ppUID' )
where
import EH.Util.Pretty
import qualified Data.Set as Set
import Data.List
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import EH101.Base.Hashable





{-# LINE 33 "src/ehc/Base/UID.chs" #-}
data UID = UID { uidHash :: !Hash, uidInts :: [Int] }
  deriving (Eq,Ord)

{-# LINE 42 "src/ehc/Base/UID.chs" #-}
mkUID :: [Int] -> UID
mkUID is = UID (hashList is) is

{-# LINE 51 "src/ehc/Base/UID.chs" #-}
type UIDL = [UID]

{-# LINE 55 "src/ehc/Base/UID.chs" #-}
type UIDS = Set.Set UID

{-# LINE 59 "src/ehc/Base/UID.chs" #-}
instance Show UID where
  show uid = concat . intersperse "_" . map show . reverse $ uidInts uid

{-# LINE 64 "src/ehc/Base/UID.chs" #-}
uidNext :: UID -> UID
uidNext (UID _ (n:ns)) = mkUID (n+1:ns)

uidChild :: UID -> UID
uidChild (UID _ ns) = mkUID (0:ns)

{-# LINE 80 "src/ehc/Base/UID.chs" #-}
mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u = (uidNext u, uidChild u)

{-# LINE 85 "src/ehc/Base/UID.chs" #-}
uidFromInt :: Int -> UID
uidFromInt i = mkUID [i]

{-# LINE 90 "src/ehc/Base/UID.chs" #-}
uidStart :: UID
uidStart = uidFromInt 0

uidUnused :: UID
uidUnused = uidFromInt (-1)

{-# LINE 98 "src/ehc/Base/UID.chs" #-}
mkNewLevUID2 u = let { (u',u1)          = mkNewLevUID   u; (u'',u2)          = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3)          = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4)       = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4,u5)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)
mkNewLevUID6 u = let { (u',u1,u2,u3)    = mkNewLevUID3  u; (u'',u4,u5,u6)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6)
mkNewLevUID7 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7)    = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6,u7)
mkNewLevUID8 u = let { (u',u1,u2,u3,u4) = mkNewLevUID4  u; (u'',u5,u6,u7,u8) = mkNewLevUID4  u'} in (u'',u1,u2,u3,u4,u5,u6,u7,u8)

uidNull :: UID
uidNull  = mkUID []

mkNewUID :: UID -> (UID,UID)
mkNewUID   uid = (uidNext uid,uid)

mkInfNewUIDL' :: (UID -> (UID,UID)) -> UID -> [UID]
mkInfNewUIDL' mk uid
  =  let  l = drop 1 $ iterate (\(nxt,uid) -> mk nxt) $ mkNewUID uid
     in   map snd l

mkNewUIDL' :: (UID -> (UID,UID)) -> Int -> UID -> [UID] -- assume sz > 0
mkNewUIDL' mk sz uid
  =  take sz (mkInfNewUIDL' mk uid)

mkNewUIDL :: Int -> UID -> [UID] -- assume sz > 0
mkNewUIDL = mkNewUIDL' mkNewUID

mkInfNewUIDL :: UID -> [UID]
mkInfNewUIDL = mkInfNewUIDL' mkNewUID

instance PP UID where
  pp = text . show

{-# LINE 132 "src/ehc/Base/UID.chs" #-}
ppUID' :: UID -> PP_Doc
ppUID' uid = ppCurlysCommas $ uidInts uid

{-# LINE 137 "src/ehc/Base/UID.chs" #-}
mkInfNewLevUIDL :: UID -> [UID]
mkInfNewLevUIDL = mkInfNewUIDL' mkNewLevUID

mkNewLevUIDL :: Int -> UID -> [UID]
mkNewLevUIDL = mkNewUIDL' mkNewLevUID

{-# LINE 149 "src/ehc/Base/UID.chs" #-}
nextUnique = mkNewLevUID

{-# LINE 157 "src/ehc/Base/UID.chs" #-}
deriving instance Typeable UID
deriving instance Data UID

{-# LINE 166 "src/ehc/Base/UID.chs" #-}
instance Hashable UID where
  hash = uidHash

{-# LINE 175 "src/ehc/Base/UID.chs" #-}
instance Binary UID where
  put (UID a b) = put a >> put b
  get = liftM2 UID get get

instance Serialize UID where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

