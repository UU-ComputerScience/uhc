%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UID: unique identifiers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.UID}
%%]

%%[1 export(mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, mkNewLevUID6, mkNewLevUID7, mkNewLevUID8, uidNext, mkNewUID, mkNewUIDL, uidStart, uidNull, uidChild, mkInfNewUIDL)
%%]

%%[1 import(EH.Util.Pretty)
%%]

%%[1 import(qualified Data.Set as Set, Data.List)
%%]

%%[7 export(mkNewLevUIDL,mkInfNewLevUIDL)
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[99 import({%{EH}Base.Hashable})
%%]
%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.UID.Base export(UID(..))
%%[[1
newtype UID = UID { uidInts :: [Int] }
%%][99
data UID = UID { uidHash :: !Hash, uidInts :: [Int] }
%%]]
  deriving (Eq,Ord)
%%]

%%[1 export(mkUID)
mkUID :: [Int] -> UID
%%[[1
mkUID is = UID is
%%][99
mkUID is = UID (hashList is) is
%%]]
%%]

%%[1.UID.UIDL
type UIDL = [UID]
%%]

%%[2 export(UIDS)
type UIDS = Set.Set UID
%%]

%%[1.UID.Show
instance Show UID where
  show uid = concat . intersperse "_" . map show . reverse $ uidInts uid
%%]

%%[1.UID.mkNewLevUID
uidNext :: UID -> UID
%%[[1
uidNext (UID   (n:ns)) = mkUID (n+1:ns)
%%][99
uidNext (UID _ (n:ns)) = mkUID (n+1:ns)
%%]]

uidChild :: UID -> UID
%%[[1
uidChild (UID   ns) = mkUID (0:ns)
%%][99
uidChild (UID _ ns) = mkUID (0:ns)
%%]]
%%]

%%[1.UID.mkNewLevUID
mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u = (uidNext u, uidChild u)
%%]

%%[1 export(uidFromInt)
uidFromInt :: Int -> UID
uidFromInt i = mkUID [i]
%%]

%%[1
uidStart :: UID
uidStart = uidFromInt 0
%%]

%%[1.UID.Utils
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
  =  let  l = iterate (\(nxt,uid) -> mk nxt) . mkNewUID $ uid
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
%%]

%%[8 export(ppUID')
ppUID' :: UID -> PP_Doc
ppUID' uid = ppCurlysCommas $ uidInts uid
%%]

%%[7
mkInfNewLevUIDL :: UID -> [UID]
mkInfNewLevUIDL = mkInfNewUIDL' mkNewLevUID

mkNewLevUIDL :: Int -> UID -> [UID]
mkNewLevUIDL = mkNewUIDL' mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to unique mechanism of AG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(nextUnique)
nextUnique = mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
deriving instance Typeable UID
deriving instance Data UID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance Hashable UID where
  hash = uidHash
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize, ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
instance Binary UID where
%%[[20
  put (UID a) = put a
  get = liftM UID get
%%][99
  put (UID a b) = put a >> put b
  get = liftM2 UID get get
%%]]

instance Serialize UID where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain
%%]

