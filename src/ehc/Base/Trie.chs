%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie, variation which allows matching on subtrees marked as a variable (kind of unification)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This module should -someday- move to a general purpose library.

A Trie is a search structure where the key actually consists of a sequence of keys,
which are iteratively used for searching.
The search structure for common prefixes is shared.

The Trie structure implemented in this module deviates from the usual Trie implementations in
that it allows partial matches besides the normal full match. The objective is to also be able to
retrieve values for which (at insertion time) it has been indicated that part does not need full matching.
This intentionally is similar to unification, where matching on a variable will succeed for arbitrary values.
Unification is not the job of this Trie implementation, but by returning partial matches as well, a list of
possible match candidates is returned.

Partial matching is implemented by administering two deeper nested search trees instead of the usual one.
Normally the default tree is filled when constructing a Trie, the partial tree is filled when the key is flagged
with TKK_Partial. Only at insertion time the proper search structure is setup.

%%[9 module {%{EH}Base.Trie} import(qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import(Prelude hiding (lookup,null))
%%]

%%[9 import(qualified EH.Util.FastSeq as Seq)
%%]

%%[9 import(UU.Pretty hiding (empty),EH.Util.PPUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key into Trie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A [TrieKey a] is used as a key into a trie, keying into successive mappings.
A TK_One is just a single key.
TK_Partial is a TK_One, but makes looking up also return the corresponding value when partial matched.
TK_Sub gives structure to a [TrieKey a] by partioning corresponding to substructure.

%%[9 export(TrieKey(..),TrieKeyKind(..))
data TrieKeyKind = TKK_Partial | TKK_Normal
  deriving (Eq, Ord)

data TrieKey k
  = TK_One      { tkKind :: TrieKeyKind, tkKey :: k }
  deriving (Eq, Ord)

mkTrieKeys :: [k] -> [TrieKey k]
mkTrieKeys = Prelude.map (TK_One TKK_Normal)
%%]

%%[9
instance Show TrieKeyKind where
  show TKK_Partial = "P"
  show TKK_Normal  = "N"

instance PP TrieKeyKind where
  pp = pp . show

instance Show k => Show (TrieKey k) where
  show k = show (tkKind k) ++ ":" ++ show (tkKey k)

instance PP k => PP (TrieKey k) where
  pp k = tkKind k >|< ":" >|< tkKey k
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TrieKeyable(..))
class TrieKeyable x k where
  toTrieKey :: x -> [TrieKey k]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Trie,emptyTrie,empty)
data Trie k v
  = Trie
      { trieMbVal       :: Maybe v                  -- value
      , trieMbPartial   :: Maybe (k,Trie k v)       -- partial match continuation
      , trieCont        :: Map.Map k (Trie k v)     -- normal search continuation
      }

emptyTrie, empty :: Trie k v
emptyTrie = Trie Nothing Nothing Map.empty

empty = emptyTrie
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
%%]
instance PP a => PP (TrieKey a) where
  pp = pp . show

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Reconstruction of original key-value pairs.

%%[9 export(toListByKey,toList)
toFastSeq :: Bool -> Trie k v -> Seq.FastSeq ([TrieKey k],v)
toFastSeq inclEmpty trie
  =          (case trieMbVal trie of
                Just v | inclEmpty -> Seq.singleton ([],v)
                _                  -> Seq.empty
             )
    Seq.:++: maybe Seq.empty (\(k,t) -> Seq.map (\(ks,v) -> (TK_One TKK_Partial k:ks,v)) $ toFastSeq True t) (trieMbPartial trie)
    Seq.:++: Seq.unions [ Seq.map (\(ks,v) -> (TK_One TKK_Normal k:ks,v)) $ toFastSeq True t | (k,t) <- Map.toList $ trieCont trie ]

toListByKey :: Trie k v -> [([TrieKey k],v)]
toListByKey = Seq.toList . toFastSeq True

toList :: Trie k v -> [([k],v)]
toList = map (\(k,v) -> (map tkKey k,v)) . toListByKey
%%]

%%[9 export(fromListByKey,fromListByKeyWith,fromListWith,fromList)
fromListByKeyWith :: Ord k => (v -> v -> v) -> [([TrieKey k],v)] -> Trie k v
fromListByKeyWith cmb = unionsWith cmb . map (uncurry singletonByKey)

fromListByKey :: Ord k => [([TrieKey k],v)] -> Trie k v
fromListByKey = unions . map (uncurry singletonByKey)

fromListWith :: Ord k => (v -> v -> v) -> [([k],v)] -> Trie k v
fromListWith cmb = fromListByKeyWith cmb . map (\(k,v) -> (mkTrieKeys k,v))

fromList :: Ord k => [([k],v)] -> Trie k v
fromList = fromListByKey . map (\(k,v) -> (mkTrieKeys k,v))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Showing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance (Show k, Show v) => Show (Trie k v) where
  showsPrec d trie = showList $ toList trie
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Normal lookup for exact match + partial matches (which require some sort of further unification)

%%[9 export(lookupPartialByKey,lookupPartialByKey',lookupByKey,lookup)
lookupPartialByKey' :: Ord k => ([TrieKey k] -> v -> v') -> Bool -> [TrieKey k] -> Trie k v -> ([v'],Maybe v')
lookupPartialByKey' mkRes stopAtPartialKey keys trie
  = l keys
  where l  []                = ([],fmap (mkRes keys) $ trieMbVal trie)
        l  (TK_One TKK_Partial k : ks) | stopAtPartialKey
                             = (map (\(ks,v) -> mkRes ks v) $ Seq.toList $ toFastSeq False trie,Nothing)
        l  (TK_One knd k : ks)
                             = case Map.lookup k $ trieCont trie of
                                 Just trie'
                                   -> (lp mkRes' ks ++ p, m)
                                   where (p,m) = lookupPartialByKey' mkRes' stopAtPartialKey ks trie'
                                 _ -> (lp mkRes' ks, Nothing)
                             where mkRes' ks v = mkRes (TK_One knd k : ks) v
        lp mkRes ks          = case (trieMbPartial trie,ks) of
                                 (Just (_,pt),[]) -> lp' [[]] pt
                                 (Just (_,pt),_ ) -> lp' [[],ks] pt
                                 _ -> []
                             where lp' kss pt = concat [ lookupResultToList r | ks <- kss, let r = lookupPartialByKey' mkRes stopAtPartialKey ks pt ]

lookupPartialByKey :: Ord k => Bool -> [TrieKey k] -> Trie k v -> ([v],Maybe v)
lookupPartialByKey = lookupPartialByKey' (\_ v -> v)

lookupByKey :: Ord k => [TrieKey k] -> Trie k v -> Maybe v
lookupByKey keys trie = snd $ lookupPartialByKey False keys trie

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup keys = lookupByKey $ mkTrieKeys keys
%%]

%%[9 export(lookupResultToList)
lookupResultToList :: ([v],Maybe v) -> [v]
lookupResultToList (vs,mv) = maybeToList mv ++ vs
%%]
lookupPartialByKey :: Ord k => Bool -> [TrieKey k] -> Trie k v -> ([v],Maybe v)
lookupPartialByKey stopAtPartialKey keys trie
  = l keys
  where l  []                = ([],trieMbVal trie)
        l  (TK_One TKK_Partial k : ks) | stopAtPartialKey
                             = (map snd $ Seq.toList $ toFastSeq False trie,Nothing)
        l  (TK_One _ k : ks) = case Map.lookup k $ trieCont trie of
                                 Just trie'
                                   -> (lp ks ++ p, m)
                                   where (p,m) = lookupPartialByKey stopAtPartialKey ks trie'
                                 _ -> (lp ks, Nothing)
        lp ks                = case (trieMbPartial trie,ks) of
                                 (Just (_,pt),[]) -> lp' [[]] pt
                                 (Just (_,pt),_ ) -> lp' [[],ks] pt
                                 _ -> []
                             where lp' kss pt = concat [ maybeToList m ++ p | ks <- kss, let (p,m) = lookupPartialByKey stopAtPartialKey ks pt ]

lookupPartialByKey :: Ord k => [TrieKey k] -> Trie k v -> ([v],Maybe v)
lookupPartialByKey keys trie
  = l keys
  where l  []                = ([],trieMbVal trie)
        l  (TK_One _ k : ks) = case Map.lookup k $ trieCont trie of
                                 Just trie'
                                   -> (lp ks ++ p, m)
                                   where (p,m) = lookupPartialByKey ks trie'
                                 _ -> (lp ks, Nothing)
        lp ks                = case (trieMbPartial trie,ks) of
                                 (Just (_,pt),[]) -> lp' [[]] pt
                                 (Just (_,pt),_ ) -> lp' [[],ks] pt
                                 _ -> []
                             where lp' kss pt = concat [ maybeToList m ++ p | ks <- kss, let (p,m) = lookupPartialByKey ks pt ]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(isEmpty,null)
isEmpty :: Trie k v -> Bool
isEmpty trie
  =  isNothing (trieMbVal trie)
  && isNothing (trieMbPartial trie)
  && Map.null  (trieCont trie)

null :: Trie k v -> Bool
null = isEmpty
%%]

%%[9 export(elems)
elems :: Trie k v -> [v]
elems = map snd . toListByKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(singletonByKey,singletonKeyable)
singletonByKey :: Ord k => [TrieKey k] -> v -> Trie k v
singletonByKey keys val
  = s keys
  where s []                          = Trie (Just val) Nothing Map.empty
        s (TK_One TKK_Normal  k : ks) = Trie Nothing Nothing (Map.singleton k $ singletonByKey ks val)
        s (TK_One TKK_Partial k : ks) = Trie Nothing (Just (k,singletonByKey ks val)) Map.empty       
        -- s (TK_Sub kss           : ks) = singletonByKey (concat kss ++ ks) val      

singleton :: Ord k => [k] -> v -> Trie k v
singleton keys val = singletonByKey (mkTrieKeys keys) val

singletonKeyable :: (Ord k,TrieKeyable v k) => v -> Trie (TrieKey k) v
singletonKeyable val = singleton (toTrieKey val) val
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Union, insert, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(unionWith,union,unionsWith,unions)
unionWith :: Ord k => (v -> v -> v) -> Trie k v -> Trie k v -> Trie k v
unionWith cmb t1 t2
  = Trie
      { trieMbVal       = mkMb          cmb             (trieMbVal t1)     (trieMbVal t2)
      , trieMbPartial   = mkMb          (\(k,t1) (_,t2) -> (k,unionWith cmb t1 t2))
                                                        (trieMbPartial t1) (trieMbPartial t2)
      , trieCont        = Map.unionWith (unionWith cmb) (trieCont t1)      (trieCont t2)
      }
  where mkMb _   j         Nothing   = j
        mkMb _   Nothing   j         = j
        mkMb cmb (Just x1) (Just x2) = Just $ cmb x1 x2

union :: Ord k => Trie k v -> Trie k v -> Trie k v
union = unionWith const

unionsWith :: Ord k => (v -> v -> v) -> [Trie k v] -> Trie k v
unionsWith cmb [] = emptyTrie
unionsWith cmb ts = foldr1 (unionWith cmb) ts

unions :: Ord k => [Trie k v] -> Trie k v
unions = unionsWith const
%%]

%%[9 export(insertByKeyWith,insertByKey)
insertByKeyWith :: Ord k => (v -> v -> v) -> [TrieKey k] -> v -> Trie k v -> Trie k v
insertByKeyWith cmb keys val trie = unionsWith cmb [singletonByKey keys val,trie]

insertByKey :: Ord k => [TrieKey k] -> v -> Trie k v -> Trie k v
insertByKey = insertByKeyWith const

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert keys = insertByKey $ mkTrieKeys keys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Delete, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(deleteByKey,deleteListByKey,delete)
deleteByKey :: Ord k => [TrieKey k] -> Trie k v -> Trie k v
deleteByKey keys trie
  = d keys trie
  where d [] t
          = t {trieMbVal = Nothing}
        d (TK_One TKK_Normal k : ks) t
          = case fmap (d ks) $ Map.lookup k $ trieCont t of
              Just c | isEmpty c -> t {trieCont = k `Map.delete` trieCont t}
                     | otherwise -> t {trieCont = Map.insert k c $ trieCont t}
              _                  -> t
        d [TK_One TKK_Partial k] t@(Trie {trieMbPartial = Just (k',t')}) | k == k'
          = t {trieMbPartial = Nothing}
        d (TK_One TKK_Partial k : ks) t@(Trie {trieMbPartial = Just (k',t')}) | k == k'
          = t {trieMbPartial = p}
          where t'' = d ks t'
                p   = if null t'' then Nothing else Just (k,t'')
        d _ t = t

deleteListByKey :: Ord k => [[TrieKey k]] -> Trie k v -> Trie k v
deleteListByKey keys trie = foldl (\t k -> deleteByKey k t) trie keys

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete keys = deleteByKey $ mkTrieKeys keys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
%%]
l1 = lookupPartialByKey False [TK_One TKK_Normal 1]
l1pn = lookupPartialByKey True [TK_One TKK_Normal 1]
l1pp = lookupPartialByKey True [TK_One TKK_Partial 1]
l2 = lookupPartialByKey False [TK_One TKK_Normal 1,TK_One TKK_Normal 2]
l3 = lookupPartialByKey False [TK_One TKK_Normal 4]
l4 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 2]
l5 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 5]
l6 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 6]

test1
  = fromListByKey
      [ ([TK_One TKK_Partial 1],"a")
      , ([TK_One TKK_Normal 1,TK_One TKK_Normal 3],"ac")
      , ([TK_One TKK_Normal 1,TK_One TKK_Partial 2],"ab")
      , ([TK_One TKK_Normal 1,TK_One TKK_Partial 2,TK_One TKK_Normal 3],"abc")
      , ([TK_One TKK_Normal 2],"b")
      ]

test2
  = fromListByKey
      [ ([TK_One TKK_Normal 1],"a")
      , ([TK_One TKK_Partial 4],"d")
      , ([TK_One TKK_Partial 4,TK_One TKK_Partial 5],"de")
      , ([TK_One TKK_Normal 4,TK_One TKK_Normal 6],"df")
      ]

test3
  = fromListByKey
      [ ([TK_One TKK_Normal 1,TK_One TKK_Normal 2],"ab")
      ]

