%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run length encoded list, to be used as an identification of scope
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Base.RLList}
%%]

%%[9 import(Data.Maybe, Data.List)
%%]

%%[9 import(Control.Monad)
%%]

%%[9 import(UHC.Util.Utils)
%%]

%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run length encoded list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(RLList(..))
newtype RLList a
  = RLList [(a,Int)]
  deriving (Eq)

instance Ord a => Ord (RLList a) where
  (RLList [])           `compare` (RLList [])           = EQ
  (RLList [])           `compare` (RLList _ )           = LT
  (RLList _ )           `compare` (RLList [])           = GT
  (RLList ((x1,c1):l1)) `compare` (RLList ((x2,c2):l2)) | x1 == x2 = if c1 == c2
                                                                     then RLList l1 `compare` RLList l2
                                                                     else c1 `compare` c2
                                                        | x1 <  x2 = LT
                                                        | x1 >  x2 = GT
%%]

%%[9 export(rllConcat,rllSingleton,rllEmpty,rllToList,rllFromList)
rllConcat :: Eq a => RLList a -> RLList a -> RLList a
rllConcat (RLList []) rll2  = rll2
rllConcat rll1 (RLList [])  = rll1
rllConcat (RLList l1) (RLList l2@(h2@(x2,c2):t2))
                            | x1 == x2  = RLList (h1 ++ [(x1,c1+c2)] ++ t2)
                            | otherwise = RLList (l1 ++ l2)
                            where (h1,t1@(x1,c1)) = fromJust (initlast l1)

rllEmpty :: RLList a
rllEmpty = RLList []

rllSingleton :: a -> RLList a
rllSingleton x = RLList [(x,1)]

rllToList :: RLList a -> [a]
rllToList (RLList l) = concatMap (\(x,c) -> replicate c x) l

rllFromList :: Eq a => [a] -> RLList a
rllFromList l = RLList [ (x,length g) | g@(x:_) <- group l ]
%%]

%%[9 export(rllLength,rllNull)
rllLength :: RLList a -> Int
rllLength (RLList l) = sum $ map snd l

rllNull :: RLList a -> Bool
rllNull (RLList []) = True
rllNull (RLList _ ) = False
%%]

%%[9 export(rllIsPrefixOf)
rllIsPrefixOf :: Eq a => RLList a -> RLList a -> Bool
rllIsPrefixOf (RLList []) _ = True
rllIsPrefixOf _ (RLList []) = False
rllIsPrefixOf (RLList ((x1,c1):l1)) (RLList ((x2,c2):l2))
                            | x1 == x2  = if c1 < c2
                                          then True
                                          else if c1 > c2
                                          then False
                                          else rllIsPrefixOf (RLList l1) (RLList l2)
                            | otherwise = False
%%]

%%[9 export(rllInits,rllInit,rllInitLast)
rllInitLast :: Eq a => RLList a -> Maybe (RLList a,a)
rllInitLast (RLList l ) = il [] l
                        where il acc [(x,1)]    = Just (RLList (reverse acc),x)
                              il acc [(x,c)]    = Just (RLList (reverse ((x,c-1):acc)),x)
                              il acc (a:as)     = il (a:acc) as
                              il _   _          = Nothing

rllInit :: Eq a => RLList a -> RLList a
rllInit = fst . fromJust . rllInitLast

rllInits :: Eq a => RLList a -> [RLList a]
rllInits = map rllFromList . inits . rllToList
%%]

%%[9 export(rllHeadTail)
rllHeadTail :: RLList a -> Maybe (a,RLList a)
rllHeadTail (RLList [])        = Nothing
rllHeadTail (RLList ((x,1):t)) = Just (x,RLList t)
rllHeadTail (RLList ((x,c):t)) = Just (x,RLList ((x,c-1):t))
%%]

%%[9
instance Show a => Show (RLList a) where
  show = show . rllToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Typeable1 RLList
deriving instance Data x => Data (RLList x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Binary a => Binary (RLList a) where
  put (RLList a) = put a
  get = liftM RLList get

%%]
