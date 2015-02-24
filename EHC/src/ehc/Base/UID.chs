%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UID: unique identifiers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.UID}
%%]

%%[1 export(mkNewLevUID, mkNewLevUID2, mkNewLevUID3, mkNewLevUID4, mkNewLevUID5, mkNewLevUID6, mkNewLevUID7, mkNewLevUID8, uidNext, mkNewUID, mkNewUIDL, uidNull, uidChild, mkInfNewUIDL)
%%]

%%[1 import(UHC.Util.Pretty)
%%]
%%[1 import(UHC.Util.Hashable)
%%]

%%[1 import(qualified Data.Set as Set, Data.List)
%%]
%%[1 import(Control.Monad.State, Control.Monad.Identity)
%%]

%%[7 export(mkNewLevUIDL,mkInfNewLevUIDL)
%%]

%%[1 import(Data.Typeable(Typeable), Data.Generics(Data))
%%]

%%[50 import(Control.Monad, UHC.Util.Binary as B, UHC.Util.Serialize)
%%]

%%[1 import(GHC.Generics)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monadic interface to Unique id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(FreshUidT, FreshUid, runFreshUidT, runFreshUid, evalFreshUid)
type FreshUidT m   = StateT UID m
type FreshUid      = FreshUidT Identity

{-
freshUID :: MonadState UID m => m UID
freshUID = state $ \x -> (x, uidNext x)
-}

runFreshUidT :: Monad m => FreshUidT m a -> UID -> m (a,UID)
runFreshUidT f u = runStateT f u
{-# INLINE runFreshUidT #-}

runFreshUid :: FreshUid a -> UID -> (a,UID)
runFreshUid f u = runIdentity $ runFreshUidT f u
{-# INLINE runFreshUid #-}

evalFreshUid :: FreshUid a -> UID -> a
evalFreshUid f u = fst $ runIdentity $ runFreshUidT f u
{-# INLINE evalFreshUid #-}
%%]

%%[1 export(MonadFreshUID(..))
class Monad m => MonadFreshUID m where
  -- | Fresh single UID
  freshUID :: m UID
  freshUID = freshInfUID

  -- | Fresh infinite range of UID 
  freshInfUID :: m UID
  freshInfUID = freshUID

-- TBD: flip results of mkNewLevUID (etc) to be in agreement with behavior of state
instance Monad m => MonadFreshUID (FreshUidT m) where
  freshUID = state $ \x -> (x, uidNext x)
  {-# INLINE freshUID #-}

  freshInfUID = state $ \x -> (uidChild x, uidNext x)
  {-# INLINE freshInfUID #-}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unique id's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.UID.Base export(UID(..))
%%[[1
newtype UID = UID { uidInts :: [Int] }
%%][99
data UID = UID { uidHash :: !Int, uidInts :: ![Int] }
%%]]
  deriving (Eq,Ord,Generic)
%%]

%%[1 export(mkUID)
mkUID :: [Int] -> UID
%%[[1
mkUID is = UID is
%%][99
mkUID is = UID (hash is) is
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

%%[1
%%[[1
instance Hashable UID
%%][99
instance Hashable UID where
  hashWithSalt salt (UID h _) = salt `hashWithSalt` h
%%]]
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

%%[1 export(uidStart,uidUnused)
uidStart :: UID
uidStart = uidFromInt 0

uidUnused :: UID
uidUnused = uidFromInt (-1)
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
%%]

%%[8 export(showUIDParseable, ppUIDParseable)
-- | Inverse of pUID
showUIDParseable :: UID -> String
-- showUIDParseable uid = "%[" ++ (concat $ intersperse "/" $ map show $ uidInts uid) ++ "]"
showUIDParseable uid = "`{" ++ (concat $ intersperse "," $ map show $ uidInts uid) ++ "}"

-- | Inverse of pUID
ppUIDParseable :: UID -> PP_Doc
ppUIDParseable = pp . showUIDParseable
%%]

%%[7
mkInfNewLevUIDL :: UID -> [UID]
mkInfNewLevUIDL = mkInfNewUIDL' mkNewLevUID

mkNewLevUIDL :: Int -> UID -> [UID]
mkNewLevUIDL = mkNewUIDL' mkNewLevUID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplifications, used by codegen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(uidSimplifications)
-- | Simplifications obtained by omitting all but 1 of the Int's, then re-adding one by one, omitting the original
uidSimplifications :: UID -> [UID]
uidSimplifications = map (mkUID . reverse) . drop 1 . init . inits . reverse . uidInts
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

%%[1
deriving instance Typeable UID
deriving instance Data UID
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999
instance Hashable UID where
  hash = uidHash
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize, ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Binary UID where
%%[[50
  put (UID a) = B.put a
  get = liftM UID B.get
%%][99
  put (UID a b) = B.put a >> B.put b
  get = liftM2 UID B.get B.get
%%]]

instance Serialize UID where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain
%%]

