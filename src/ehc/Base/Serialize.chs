%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
%%]
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

%%[doesWhat doclatex
Serialization is built on top of Binary, adding sharing, additionally
requiring Typeable and Ord instances for maintaining maps. Whilst
putting/getting a map is maintained which remembers previously put
values. When such a value is put a 2nd time, a reference to it is put
instead.

Values which are shared, put share commands (SCmd) onto the Binary
serialization. There are SCmds for defining a shared value and referring
to it, coming in 8/16/.. bitsized reference flavors (this saves space).

Turning on serialization means defining an instance for the type of the
value, similar to instances for Binary:

\begin{code}
data Foo = Bar1 | Bar2 Int Int

instance Serialize Foo where
  sput (Bar1    ) = sputWord8 0
  sput (Bar2 a b) = sputWord8 1 >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
           0 -> return Bar1
           1 -> liftM2 Bar2 sget sget
\end{code}

When a Binary instance already is defined, the definition can be more simple:
\begin{code}
instance Serialize Foo where
  sput = sputPlain
  sget = sgetPlain
\end{code}

The plain variants delegate the work to their Binary equivalents.

By default no sharing is done, it can be enabled by:

\begin{code}
instance Serialize Foo where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain
\end{code}

When shared the internals are not shared, as above. If the internals
(i.e. the fields) also must be shared the following instance is
required, using the original code for sput/sget for the fields of a
value:

\begin{code}
instance Serialize Foo where
  sput = sputShared
  sget = sgetShared
  sputNested (Bar1    ) = sputWord8 0
  sputNested (Bar2 a b) = sputWord8 1 >> sput a >> sput b
  sgetNested
    = do t <- sgetWord8
         case t of
           0 -> return Bar1
           1 -> liftM2 Bar2 sget sget
\end{code}

%%]

%%[20 module {%{EH}Base.Serialize}
%%]

%%[20 import(qualified {%{EH}Base.Binary} as Bn)
%%]
%%[20 import(qualified Data.ByteString.Lazy as L, IO, System.IO(openBinaryFile))
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

Shared values are stored in a per type map, to keep all type correct. To
make this work a double mapping is maintained, keyed by the type
descriptor string of a type (obtained via Typeable) and keyed by the Int
reference to it.

%%[20
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
%%]

%%[20 export(SPut)
data SerPutMp = forall x . (Typeable x, Ord x) => SerPutMp (Map.Map x Int)

data SPutS
  = SPutS
      { sputsInx :: Int
      , sputsSMp :: Map.Map String SerPutMp
      , sputsPut :: Bn.Put
      }

emptySPutS = SPutS 0 Map.empty (return ())

type SPut = St.State SPutS ()

%%]  

%%[20 export(SGet)
data SerGetMp = forall x . (Typeable x, Ord x) => SerGetMp (Map.Map Int x)

data SGetS
  = SGetS
      { sgetsSMp :: Map.Map String SerGetMp
      }

type SGet x = St.StateT SGetS Bn.Get x
%%]

%%[20 export(Serialize(..))
class Serialize x where
  sput :: x -> SPut
  sget :: SGet x
  
  sputNested :: x -> SPut
  sgetNested :: SGet x
    
  -- default just falls back on Binary, invoked by sputShared/sgetShared
  sputNested = panic "not implemented (must be done by instance): Serialize.sputNested"
  sgetNested = panic "not implemented (must be done by instance): Serialize.sgetNested"

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
{-# INLINE sputPlain #-}

sgetPlain :: (Bn.Binary x,Serialize x) => SGet x
sgetPlain = lift Bn.get
{-# INLINE sgetPlain #-}

%%]

%%[20 export(sputUnshared,sputShared)
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
%%]

%%[20 export(sgetShared)
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

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Low level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(sputWord8,sgetWord8)
sputWord8            :: Word8 -> SPut
sputWord8 x          = liftP (Bn.putWord8 x)
{-# INLINE sputWord8 #-}

sgetWord8 :: SGet Word8
sgetWord8 = liftG Bn.getWord8
{-# INLINE sgetWord8 #-}

%%]

%%[20 export(sputWord16,sgetWord16)
sputWord16           :: Word16 -> SPut
sputWord16 x         = liftP (Bn.putWord16be x)
{-# INLINE sputWord16 #-}

sgetWord16 :: SGet Word16
sgetWord16 = liftG Bn.getWord16be
{-# INLINE sgetWord16 #-}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default instances, copied & modified from Binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The actual IO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(runSPut,runSGet)
runSPut :: SPut -> Bn.Put
runSPut x = sputsPut $ snd $ St.runState x emptySPutS
 
runSGet :: SGet x -> Bn.Get x
runSGet x = St.evalStateT x (SGetS Map.empty)

%%]

%%[20 export(serialize,unserialize)
serialize :: Serialize x => x -> Bn.Put
serialize x = runSPut (sput x)
 
unserialize :: Serialize x => Bn.Get x
unserialize = runSGet sget

%%]

%%[20 export(putSPutFile,getSGetFile)
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
%%]

%%[20 export(putSerializeFile,getSerializeFile)
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
%%]

