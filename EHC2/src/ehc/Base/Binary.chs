%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional stuff for use of Binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 hs module {%{EH}Base.Binary}
%%]

%%[20 import(EH.Util.Binary) export (module EH.Util.Binary)
%%]
%%[20 import(Data.Typeable(Typeable,Typeable1), Data.Generics.Aliases) export(module Data.Typeable)
%%]
%%[2020 import(Data.Generics(Data, constrIndex, toConstr, gmapQ, indexConstr, dataTypeOf, fromConstrM))
%%]
%%[20 import(Data.Generics(Data)) export(module Data.Generics)
%%]
%%[20 import(Data.Word, Data.Array, Control.Monad)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Enum
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(putEnum, getEnum)
putEnum :: Enum x => x -> Put
putEnum x = put (fromEnum x)

getEnum :: Enum x => Get x
getEnum = do n <- get
             return (toEnum n)

%%]

%%[20 export(putEnum8, getEnum8)
putEnum8 :: Enum x => x -> Put
putEnum8 x = putWord8 (fromIntegral $ fromEnum x)

getEnum8 :: Enum x => Get x
getEnum8 = do n <- getWord8
              return (toEnum $ fromIntegral n)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% List abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(putList, getList)
putList :: (Binary a, Binary b) => (x -> Bool) -> (x -> (a,b)) -> x -> Put
putList isNil getCons x | isNil x   = putWord8 0
                        | otherwise = let (a,b) = getCons x in putWord8 1 >> put a >> put b

getList :: (Binary a, Binary b) => x -> (a -> b -> x) -> Get x
getList nil cons
  = do tag <- getWord8
       case tag of
         0 -> return nil
         1 -> liftM2 cons get get
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(liftM6,liftM7)
liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6
       ; return (f x1 x2 x3 x4 x5 x6)
       }

liftM7  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 f m1 m2 m3 m4 m5 m6 m7
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7
       ; return (f x1 x2 x3 x4 x5 x6 x7)
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic put & get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
%%]
instance (Data a, Typeable a) => Binary a where
  put = gput
  get = gget

%%[20
%%]
-- | Generic put.
gput :: Data a => a -> Put
gput = general 
      `extQ` char 
      `extQ` int
      `extQ` integer
      `extQ` float 
      `extQ` double
      `extQ` string
      -- `extQ` arr
      -- `extQ` array
  where
  -- Generic case
  general :: Data a => a -> Put
  general x = let n :: Word8
                  n = fromIntegral (constrIndex (toConstr x))
              in foldr1 (>>) (putWord8 n : gmapQ gput x)
  
  -- Base cases
  char :: Char -> Put
  char    = put

  int :: Int -> Put
  int     = put

  integer :: Integer -> Put
  integer = put

  float :: Float -> Put
  float   = put

  double :: Double -> Put
  double  = put

  string :: String -> Put
  string  = put

  -- array :: (Ix a, Data a, Data b) => Array a b -> Put
  -- array a = foldr1 (>>) (gmapQ gput (bounds a)) >> foldr1 (>>) (gmapQ gput (elems a))
  
  -- arr :: (Data e) => Array Int e -> Put
  -- arr a = gput (bounds a) >> gput (elems a)

-- | Generic get.
gget :: forall a. Data a => Get a
gget = general 
      `extR` char 
      `extR` int
      `extR` integer
      `extR` float 
      `extR` double 
      `extR` string where
  -- Generic case
  general :: forall a. Data a => Get a
  general = do  n <- getWord8
                let f :: Get a -> a -- just to get the type
                    f = error "impossible"
                    i = indexConstr (dataTypeOf (f general)) (fromIntegral n)
                fromConstrM gget i
  
  -- Base cases
  char :: Get Char
  char    = get

  int :: Get Int
  int     = get

  integer :: Get Integer
  integer = get

  float :: Get Float
  float   = get

  double :: Get Double
  double  = get

  string :: Get String
  string  = get

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
%%]
instance Enum a => Binary a where
  put e = put (fromEnum e)
  get   = do n <- get
             return (toEnum n)

