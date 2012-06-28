module EH101.Base.Serialize
( SPut
, SGet
, Serialize (..)
, sputPlain, sgetPlain
, sputUnshared, sputShared
, sgetShared
, sputWord8, sgetWord8
, sputWord16, sgetWord16
, sputEnum8, sgetEnum8
, runSPut, runSGet
, serialize, unserialize
, putSPutFile, getSGetFile
, putSerializeFile, getSerializeFile )
where
import qualified EH101.Base.Binary as Bn
import qualified Data.ByteString.Lazy as L
import System.IO
import System.IO (openBinaryFile)
import EH.Util.Utils
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Word
import Data.Array
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Trans

{-# LINE 103 "src/ehc/Base/Serialize.chs" #-}
data SCmd
  = SCmd_Unshared
  | SCmd_ShareDef   | SCmd_ShareRef			--
  | SCmd_ShareDef16 | SCmd_ShareRef16
  | SCmd_ShareDef8  | SCmd_ShareRef8
  deriving (Enum)

instance Bn.Binary SCmd where
  put = Bn.putEnum8
  get = Bn.getEnum8

scmdTo16 :: SCmd -> SCmd
scmdTo16 SCmd_ShareDef = SCmd_ShareDef16
scmdTo16 SCmd_ShareRef = SCmd_ShareRef16
scmdTo16 c             = c

scmdTo8 :: SCmd -> SCmd
scmdTo8 SCmd_ShareDef = SCmd_ShareDef8
scmdTo8 SCmd_ShareRef = SCmd_ShareRef8
scmdTo8 c             = c

scmdToNrBits :: SCmd -> Int
scmdToNrBits SCmd_ShareDef16 = 16
scmdToNrBits SCmd_ShareRef16 = 16
scmdToNrBits SCmd_ShareDef8  = 8
scmdToNrBits SCmd_ShareRef8  = 8
scmdToNrBits _               = 32

scmdFrom :: SCmd -> SCmd
scmdFrom SCmd_ShareDef16 = SCmd_ShareDef
scmdFrom SCmd_ShareRef16 = SCmd_ShareRef
scmdFrom SCmd_ShareDef8  = SCmd_ShareDef
scmdFrom SCmd_ShareRef8  = SCmd_ShareRef
scmdFrom c               = c

{-# LINE 140 "src/ehc/Base/Serialize.chs" #-}
data SerPutMp = forall x . (Typeable x, Ord x) => SerPutMp (Map.Map x Int)

data SPutS
  = SPutS
      { sputsInx :: Int
      , sputsSMp :: Map.Map String SerPutMp
      , sputsPut :: Bn.Put
      }

emptySPutS = SPutS 0 Map.empty (return ())

type SPut = St.State SPutS ()


{-# LINE 156 "src/ehc/Base/Serialize.chs" #-}
data SerGetMp = forall x . (Typeable x, Ord x) => SerGetMp (Map.Map Int x)

data SGetS
  = SGetS
      { sgetsSMp :: Map.Map String SerGetMp
      }

type SGet x = St.StateT SGetS Bn.Get x

{-# LINE 167 "src/ehc/Base/Serialize.chs" #-}
class Serialize x where
  sput :: x -> SPut
  sget :: SGet x

  sputNested :: x -> SPut
  sgetNested :: SGet x

  -- default just falls back on Binary, invoked by sputShared/sgetShared
  sputNested = panic "not implemented (must be done by instance): Serialize.sputNested"
  sgetNested = panic "not implemented (must be done by instance): Serialize.sgetNested"


{-# LINE 185 "src/ehc/Base/Serialize.chs" #-}
liftP :: Bn.Put -> SPut
liftP p
  = do { s <- St.get
       ; St.put (s { sputsPut = sputsPut s >> p
                   })
       }

liftG :: Bn.Get x -> SGet x
liftG g = lift g

{-# LINE 198 "src/ehc/Base/Serialize.chs" #-}
sputPlain :: (Bn.Binary x,Serialize x) => x -> SPut
sputPlain x = liftP (Bn.put x)
{-# INLINE sputPlain #-}

sgetPlain :: (Bn.Binary x,Serialize x) => SGet x
sgetPlain = lift Bn.get
{-# INLINE sgetPlain #-}


{-# LINE 209 "src/ehc/Base/Serialize.chs" #-}
sputUnshared :: (Bn.Binary x,Serialize x) => x -> SPut
sputUnshared x
  = do { s <- St.get
       ; St.put (s { sputsPut = sputsPut s >> Bn.put SCmd_Unshared >> Bn.put x
                   })
       }

putRef :: SCmd -> Int -> Bn.Put
putRef c i
  = if i < 256
    then do { Bn.put (scmdTo8 c)
            ; Bn.putWord8 (fromIntegral i)
            }
    else if i < 65000
    then do { Bn.put (scmdTo16 c)
            ; Bn.putWord16be (fromIntegral i)
            }
    else do { Bn.put c
            ; Bn.put i
            }

sputShared :: (Ord x, Serialize x, Typeable x) => x -> SPut
sputShared x
  = do { s <- St.get
       ; let tykey = tyConString $ typeRepTyCon $ typeOf x
       ; case Map.lookup tykey (sputsSMp s) of
           Just (SerPutMp m)
             -> case Map.lookup xcasted m of
                  Just i
                    -> useExisting i s
                  _ -> addNew tykey x xcasted m s
             where xcasted = panicJust "Serialize.sputShared A" $ cast x
           _ -> addNew tykey x x Map.empty s
       }
  where useExisting i s
          = St.put (s { sputsPut = sputsPut s >> putRef SCmd_ShareRef i })
        addNew tykey x xcasted m s
          = do { St.put (s { sputsInx = i+1
                           , sputsSMp = Map.insert tykey (SerPutMp (Map.insert xcasted i m)) (sputsSMp s)
                           , sputsPut = sputsPut s >> putRef SCmd_ShareDef i
                           })
               ; sputNested x
               }
          where i = sputsInx s

{-# LINE 256 "src/ehc/Base/Serialize.chs" #-}
getRef :: SCmd -> Bn.Get Int
getRef c
  = case scmdToNrBits c of
      8 -> do { i <- Bn.getWord8
              ; return (fromIntegral i :: Int)
              }
      16-> do { i <- Bn.getWord16be
              ; return (fromIntegral i :: Int)
              }
      _ -> Bn.get

sgetShared :: forall x. (Ord x, Serialize x, Typeable x) => SGet x
sgetShared
  = do { cmd <- lift Bn.get
       ; case scmdFrom cmd of
           SCmd_Unshared
             -> sgetNested
           SCmd_ShareDef
             -> do { i <- lift (getRef cmd)
                   ; x <- sgetNested
                   ; s <- St.get
                   ; let tykey = tyConString $ typeRepTyCon $ typeOf (undefined :: x)
                   ; case Map.lookup tykey (sgetsSMp s) of
                       Just (SerGetMp m)
                         -> St.put (s { sgetsSMp = Map.insert tykey (SerGetMp (Map.insert i xcasted m)) (sgetsSMp s)
                                      })
                         where xcasted = panicJust "Serialize.sgetShared A" $ cast x
                       _ -> St.put (s { sgetsSMp = Map.insert tykey (SerGetMp (Map.singleton i x)) (sgetsSMp s)
                                      })
                   ; return x
                   }
           SCmd_ShareRef
             -> do { i <- lift (getRef cmd)
                   ; s <- St.get
                   ; let tykey = tyConString $ typeRepTyCon $ typeOf (undefined :: x)
                   ; case Map.lookup tykey (sgetsSMp s) of
                       Just (SerGetMp m)
                         -> return $ panicJust "Serialize.sgetShared C" $ cast $ panicJust "Serialize.sgetShared B" $ Map.lookup i m
                       _ -> panic "Serialize.sgetShared D"
                   }
       }


{-# LINE 305 "src/ehc/Base/Serialize.chs" #-}
sputWord8            :: Word8 -> SPut
sputWord8 x          = liftP (Bn.putWord8 x)
{-# INLINE sputWord8 #-}

sgetWord8 :: SGet Word8
sgetWord8 = liftG Bn.getWord8
{-# INLINE sgetWord8 #-}


{-# LINE 316 "src/ehc/Base/Serialize.chs" #-}
sputWord16           :: Word16 -> SPut
sputWord16 x         = liftP (Bn.putWord16be x)
{-# INLINE sputWord16 #-}

sgetWord16 :: SGet Word16
sgetWord16 = liftG Bn.getWord16be
{-# INLINE sgetWord16 #-}


{-# LINE 327 "src/ehc/Base/Serialize.chs" #-}
sputEnum8 :: Enum x => x -> SPut
sputEnum8 x = liftP (Bn.putEnum8 x)
{-# INLINE sputEnum8 #-}

sgetEnum8 :: Enum x => SGet x
sgetEnum8 = liftG Bn.getEnum8
{-# INLINE sgetEnum8 #-}


{-# LINE 342 "src/ehc/Base/Serialize.chs" #-}
instance Serialize () where
  sput _ = return ()
  sget   = return ()

instance Serialize Int where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Char where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Bool where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Integer where
  sput = sputPlain
  sget = sgetPlain

{-
instance Serialize String where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain
-}

instance (Serialize a, Serialize b) => Serialize (a,b) where
    sput (a,b)           = sput a >> sput b
    sget                 = liftM2 (,) sget sget

instance (Serialize a, Serialize b, Serialize c) => Serialize (a,b,c) where
    sput (a,b,c)         = sput a >> sput b >> sput c
    sget                 = liftM3 (,,) sget sget sget

instance Serialize a => Serialize [a] where
    sput l  = sput (length l) >> mapM_ sput l
    sget    = do n <- sget :: SGet Int
                 replicateM n sget

instance (Serialize a) => Serialize (Maybe a) where
    sput Nothing  = sputWord8 0
    sput (Just x) = sputWord8 1 >> sput x
    sget = do
        w <- sgetWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just sget

instance (Ord a, Serialize a) => Serialize (Set.Set a) where
    sput = sput . Set.toAscList
    sget = liftM Set.fromDistinctAscList sget

instance (Ord k, Serialize k, Serialize e) => Serialize (Map.Map k e) where
    sput = sput . Map.toAscList
    sget = liftM Map.fromDistinctAscList sget

{-# LINE 406 "src/ehc/Base/Serialize.chs" #-}
runSPut :: SPut -> Bn.Put
runSPut x = sputsPut $ St.execState x emptySPutS

runSGet :: SGet x -> Bn.Get x
runSGet x = St.evalStateT x (SGetS Map.empty)


{-# LINE 415 "src/ehc/Base/Serialize.chs" #-}
serialize :: Serialize x => x -> Bn.Put
serialize x = runSPut (sput x)

unserialize :: Serialize x => Bn.Get x
unserialize = runSGet sget


{-# LINE 424 "src/ehc/Base/Serialize.chs" #-}
-- | SPut to FilePath
putSPutFile :: FilePath -> SPut -> IO ()
putSPutFile fn x
  = do { h <- openBinaryFile fn WriteMode
       ; L.hPut h (Bn.runPut $ runSPut x)
       ; hClose h
       }

-- | SGet from FilePath
getSGetFile :: FilePath -> SGet a -> IO a
getSGetFile fn x
  = do { h <- openBinaryFile fn ReadMode
       ; b <- liftM (Bn.runGet $ runSGet x) (L.hGetContents h)
       -- ; hClose h
       ; return b ;
       }

{-# LINE 443 "src/ehc/Base/Serialize.chs" #-}
-- | Serialize to FilePath
putSerializeFile :: Serialize a => FilePath -> a -> IO ()
putSerializeFile fn x
  = do { h <- openBinaryFile fn WriteMode
       ; L.hPut h (Bn.runPut $ serialize x)
       ; hClose h
       }

-- | Unserialize from FilePath
getSerializeFile :: Serialize a => FilePath -> IO a
getSerializeFile fn
  = do { h <- openBinaryFile fn ReadMode
       ; b <- liftM (Bn.runGet unserialize) (L.hGetContents h)
       -- ; hClose h
       ; return b ;
       }

