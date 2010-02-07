%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
%%]

%%[doesWhat doclatex
Serialization is built on top of Binary.
Throughout put/get a map is maintained which remembers previously put values.
When such a value is put a 2nd time, a reference to it is put instead.
%%]

%%[20 module {%{EH}Base.Serialize}
%%]

%%[20 import(qualified {%{EH}Base.Binary} as Bn)
%%]
%%[20 import(qualified Data.ByteString.Lazy as L, IO)
%%]

%%[20 import(EH.Util.Utils)
%%]

%%[20 import(Data.Typeable)
%%]
%%[20 import(qualified Data.Map as Map, qualified Data.Set as Set, Data.Maybe, Data.Word, Data.Array)
%%]
%%[20 import(Control.Monad, qualified Control.Monad.State as St, Control.Monad.Trans)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization with state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100206 AD, code is still in modeling stage, by default falls back on Binary's put/get.

%%[20
newtype SerKey
  = SerKey Int
  deriving (Eq,Ord)

instance Bn.Binary (SerKey) where
  put (SerKey x) = Bn.put x
  get = liftM SerKey Bn.get

%%]

%%[20
data SCmd
  = SCmd_Unshared | SCmd_ShareDef | SCmd_ShareRef
  deriving (Enum)

instance Bn.Binary SCmd where
  put = Bn.putEnum8
  get = Bn.getEnum8
%%]

%%[20
data SerPutMp = forall x . (Typeable x, Ord x) => SerPutMp (Map.Map x Int)

data SPutS
  = SPutS
      { sputsInx :: Int
      , sputsMp  :: Map.Map SerKey Int
      , sputsSMp :: Map.Map String SerPutMp
      , sputsPut :: Bn.Put
      }

emptySPutS = SPutS 0 Map.empty Map.empty (return ())

type SPut = St.State SPutS ()

%%]  

%%[20
data SerGetMp = forall x . (Typeable x, Ord x) => SerGetMp (Map.Map Int x)

data SGetS
  = SGetS
      { sgetsMp  :: Map.Map Int SerKey
      , sgetsSMp :: Map.Map String SerGetMp
      }

type SGet x = St.StateT SGetS Bn.Get x
%%]

%%[20 export(Serialize(..))
class Serialize x where
  share :: x -> Maybe SerKey
  unshare :: SerKey -> x
  sput :: x -> SPut
  sget :: SGet x
  {-
  shareFind :: x -> Maybe Int
  -}
  -- initSerPutMp :: (Typeable x, Ord x) => x -> SerPutMp
    
  share   _ = Nothing
  unshare _ = panic "Serialize.unshare"		-- unshare is only called when a corresponding share has returned a Just
  
  -- initSerPutMp _ = SerPutMp (Map.empty :: Map.Map x Int)

-- default just falls back on Binary
%%]
instance Bn.Binary x => Serialize x where
  sput = sputPlain
  sget = sgetPlain


%%[20
liftP :: Bn.Put -> SPut
liftP p
  = do { s <- St.get
       ; St.put (s { sputsPut = sputsPut s >> p
                   })
       }

liftG :: Bn.Get x -> SGet x
liftG g = lift g
%%]


%%[20 export(sputPlain,sgetPlain)
sputPlain :: (Bn.Binary x,Serialize x) => x -> SPut
sputPlain x = liftP (Bn.put x)

sgetPlain :: (Bn.Binary x,Serialize x) => SGet x
sgetPlain = lift Bn.get

%%]

%%[20 export(sputUnshared,sputShared)
sputUnshared :: (Bn.Binary x,Serialize x) => x -> SPut
sputUnshared x
  = do { s <- St.get
       ; St.put (s { sputsPut = sputsPut s >> Bn.put SCmd_Unshared >> Bn.put x
                   })
       }

sputShared3 :: (Bn.Binary x, Serialize x) => x -> SPut
sputShared3 x
  = case share x of
      Just k
        -> do { s <- St.get
              ; let m = sputsMp s
              ; case Map.lookup k m of
                  Just inx
                    -> -- if already put before, put the index referring to it
                       St.put (s { sputsPut = sputsPut s >> Bn.put SCmd_ShareRef >> Bn.put inx
                                 })
                  _ -> -- if not yet shared, allocate new index and put it together with the value
                       St.put (s { sputsInx = i+1
                                 , sputsMp  = Map.insert k i m
                                 , sputsPut = sputsPut s >> Bn.put SCmd_ShareDef >> Bn.put i >> Bn.put x
                                 })
                    where i = sputsInx s
              }
      _ -> sputUnshared x

-- 20100207 AD: this should work... not yet tested though
sputShared :: (Ord x, Bn.Binary x, Serialize x, Typeable x) => x -> SPut
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
          = St.put (s { sputsPut = sputsPut s >> Bn.put SCmd_ShareRef >> Bn.put i })
        addNew tykey x xcasted m s
          = St.put (s { sputsInx = i+1
                      , sputsSMp = Map.insert tykey (SerPutMp (Map.insert xcasted i m)) (sputsSMp s)
                      , sputsPut = sputsPut s >> Bn.put SCmd_ShareDef >> Bn.put i >> Bn.put x
                      })
          where i = sputsInx s
%%]

%%[20 export(sgetShared)
sgetShared3 :: (Bn.Binary x,Serialize x) => SGet x
sgetShared3
  = do { s <- St.get
       ; let m = sgetsMp s
       ; cmd <- lift Bn.get
       ; case cmd of
           SCmd_Unshared
             -> lift Bn.get
           SCmd_ShareDef
             -> do { inx <- lift Bn.get
                   ; k <- lift Bn.get
                   ; let m' = Map.insert inx k m
                   ; St.put (s {sgetsMp = m'})
                   ; return (unshare k)
                   }
           SCmd_ShareRef
             -> do { inx <- lift Bn.get
                   ; let x = unshare $ fromJust $ Map.lookup inx m
                   ; return x
                   }
       }

sgetShared :: forall x. (Ord x, Bn.Binary x, Serialize x, Typeable x) => SGet x
sgetShared
  = do { s <- St.get
       ; cmd <- lift Bn.get
       ; case cmd of
           SCmd_Unshared
             -> lift Bn.get
           SCmd_ShareDef
             -> do { i <- lift Bn.get
                   ; x <- lift Bn.get
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
             -> do { i <- lift Bn.get
                   ; let tykey = tyConString $ typeRepTyCon $ typeOf (undefined :: x)
                   ; case Map.lookup tykey (sgetsSMp s) of
                       Just (SerGetMp m)
                         -> return $ panicJust "Serialize.sgetShared C" $ cast $ panicJust "Serialize.sgetShared B" $ Map.lookup i m
                       _ -> panic "Serialize.sgetShared D"
                   }
       }

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(sputWord8)
sputWord8            :: Word8 -> SPut
sputWord8 x          = liftP (Bn.putWord8 x)
%%]

%%[20 export(sgetWord8)
sgetWord8 :: SGet Word8
sgetWord8 = liftG Bn.getWord8
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
instance Serialize Int where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Char where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Bool where
  sput = sputPlain
  sget = sgetPlain

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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The actual IO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(serialize,unserialize)
serialize :: Serialize x => x -> Bn.Put
serialize x = sputsPut $ snd $ St.runState (sput x) emptySPutS
 
unserialize :: Serialize x => Bn.Get x
unserialize = St.evalStateT sget (SGetS Map.empty Map.empty)

%%]

%%[20 export(putSerializeFile,getSerializeFile)
-- | Serialize to FilePath
putSerializeFile :: Serialize a => FilePath -> a -> IO ()
putSerializeFile fn x
  = do { h <- openFile fn WriteMode
       ; L.hPut h (Bn.runPut $ serialize x)
       ; hClose h
       }

-- | Unserialize from FilePath
getSerializeFile :: Serialize a => FilePath -> IO a
getSerializeFile fn
  = do { h <- openFile fn ReadMode
       ; b <- liftM (Bn.runGet unserialize) (L.hGetContents h)
       -- ; hClose h
       ; return b ;
       }
%%]

