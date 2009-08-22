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

%%[9 import(qualified EH.Util.FastSeq as Seq,qualified Data.List as List)
%%]

%%[9 import(EH.Util.Pretty hiding (empty))
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%[9999 import({%{EH}Base.Hashable})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key into Trie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A [TrieKey a] is used as a key into a trie, keying into successive mappings.
A TK_One is just a single key.
TK_Partial is a TK_One, but makes looking up also return the corresponding value when partial matched.
TK_Sub gives structure to a [TrieKey a] by partioning corresponding to substructure.

%%[9 export(TrieKey(..),TrieKeyKind(..),mkTrieKeys)
data TrieKeyKind = TKK_Partial | TKK_Normal
  deriving (Eq, Ord)

data TrieKey k
  = TK_One      { tkKind :: !TrieKeyKind, tkKey :: !k }
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

%%[9999
instance Hashable k => Hashable (TrieKey k) where
  hash = hash . tkKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Keyable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TrieKeyable(..))
class TrieKeyable x k where
  toTrieKey :: x -> [TrieKey k]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie lookup/insertion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TrieLookup(..))
data TrieLookup
  = TrieLookup_Normal
  | TrieLookup_Partial
  | TrieLookup_StopAtPartial
  deriving (Eq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Trie,emptyTrie,empty)
type SubTrie k v = Map.Map k (Trie k v)

data Trie k v
  = Trie
      { trieMbVal       :: Maybe v                  -- value
      , triePartSubs    :: SubTrie k v              -- partial search continuation
      , trieSubs        :: SubTrie k v              -- normal search continuation
      }

emptyTrie, empty :: Trie k v
emptyTrie = Trie Nothing Map.empty Map.empty

empty = emptyTrie
%%]

%%[9
trieUpdSubs :: SubTrie k v -> Trie k v -> Trie k v
trieUpdSubs s t = t {trieSubs = s}

trieUpdPartSubs :: SubTrie k v -> Trie k v -> Trie k v
trieUpdPartSubs s t = t {triePartSubs = s}
%%]

%%[9
instance (Show k, Show v) => Show (Trie k v) where
  showsPrec _ t = showList $ toListByKey t

instance (PP k, PP v) => PP (Trie k v) where
  pp t = ppBracketsCommasV $ map (\(a,b) -> ppTrieKey a >#< ":" >#< b) $ toListByKey t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ppTrieKey)
ppTrieKey :: PP k => [TrieKey k] -> PP_Doc
ppTrieKey k = ppBracketsCommas k
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Reconstruction of original key-value pairs.

%%[9 export(toListByKey,toList)
toFastSeqSubs :: TrieKeyKind -> Map.Map k (Trie k v) -> Seq.FastSeq ([TrieKey k],v)
toFastSeqSubs knd tries
  = Seq.unions [ Seq.map (\(ks,v) -> (TK_One knd k:ks,v)) $ toFastSeq True t | (k,t) <- Map.toList tries ]

toFastSeq :: Bool -> Trie k v -> Seq.FastSeq ([TrieKey k],v)
toFastSeq inclEmpty trie
  =          (case trieMbVal trie of
                Just v | inclEmpty -> Seq.singleton ([],v)
                _                  -> Seq.empty
             )
    Seq.:++: toFastSeqSubs TKK_Partial (triePartSubs trie)
    Seq.:++: toFastSeqSubs TKK_Normal (trieSubs trie)

toListByKey :: Trie k v -> [([TrieKey k],v)]
toListByKey = Seq.toList . toFastSeq True

toList :: Trie k v -> [([k],v)]
toList = map (\(k,v) -> (map tkKey k,v)) . toListByKey
%%]

%%[9 export(fromListPartialByKeyWith,fromListByKey,fromListByKeyWith,fromListWith,fromList)
fromListPartialByKeyWith :: Ord k => TrieLookup -> (v -> v -> v) -> [([TrieKey k],v)] -> Trie k v
fromListPartialByKeyWith trieLookup cmb = unionsWith cmb . map (uncurry (singletonPartialByKey trieLookup))

fromListByKeyWith :: Ord k => (v -> v -> v) -> [([TrieKey k],v)] -> Trie k v
fromListByKeyWith = fromListPartialByKeyWith TrieLookup_Partial

fromListByKey :: Ord k => [([TrieKey k],v)] -> Trie k v
fromListByKey = unions . map (uncurry singletonByKey)

fromListWith :: Ord k => (v -> v -> v) -> [([k],v)] -> Trie k v
fromListWith cmb = fromListByKeyWith cmb . map (\(k,v) -> (mkTrieKeys k,v))

fromList :: Ord k => [([k],v)] -> Trie k v
fromList = fromListByKey . map (\(k,v) -> (mkTrieKeys k,v))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Normal lookup for exact match + partial matches (which require some sort of further unification, determining whether it was found)

%%[9 export(lookupPartialByKey,lookupPartialByKey',lookupByKey,lookup)
lookupPartialByKey' :: Ord k => ([TrieKey k] -> v -> v') -> TrieLookup -> [TrieKey k] -> Trie k v -> ([v'],Maybe v')
lookupPartialByKey' mkRes trieLookup keys trie
  = l keys
  where l  []                = ([],fmap (mkRes []) $ trieMbVal trie)
        l  (kk@(TK_One TKK_Partial k) : ks) | trieLookup == TrieLookup_StopAtPartial
                             = (map (\(ks,v) -> mkRes ks v) $ Seq.toList $ toFastSeq True trie,Nothing)
        l  (kk@(TK_One TKK_Partial k) : ks) | trieLookup == TrieLookup_Partial
                             = case Map.lookup k $ triePartSubs trie of
                                 Just trie'
                                   -> (lp (k `Map.delete` triePartSubs trie) ks ++ p, m)
                                   where (p,m) = lookupPartialByKey' (mkRes' kk) trieLookup ks trie'
                                 _ -> (lp (triePartSubs trie) ks, Nothing)
        l  (kk@(TK_One TKK_Normal k) : ks) | trieLookup == TrieLookup_Partial
                             = case Map.lookup k $ trieSubs trie of
                                 Just trie'
                                   -> (lp (triePartSubs trie) ks ++ p, m)
                                   where (p,m) = lookupPartialByKey' (mkRes' kk) trieLookup ks trie'
                                 _ -> (lp (triePartSubs trie) ks, Nothing)
        l  (kk@(TK_One TKK_Normal k) : ks) | trieLookup == TrieLookup_StopAtPartial
                             = case Map.lookup k $ trieSubs trie of
                                 Just trie'
                                   -> lookupPartialByKey' (mkRes' kk) trieLookup ks trie'
                                 _ -> ([], Nothing)
        l  (kk@(TK_One knd k) : ks) | trieLookup /= TrieLookup_Partial
                             = case Map.lookup k $ subsOf knd trie of
                                 Just trie'
                                   -> ([], m)
                                   where (_,m) = lookupPartialByKey' (mkRes' kk) trieLookup ks trie'
                                 _ -> ([], Nothing)
        lp subs ks           = concat [ lookupResultToList $ lookupPartialByKey' (mkResP k) trieLookup ks psub | (k,psub) <- psubs, ks <- kss ]
                             where kss = if List.null ks then [[]] else [[],ks]
                                   psubs = Map.toList subs
        mkRes' kk ks v       = mkRes (kk : ks) v
        mkResN k             = mkRes' (TK_One TKK_Normal k)
        mkResP k             = mkRes' (TK_One TKK_Partial k)
        subsOf knd t         = case knd of
                                 TKK_Normal  -> trieSubs t
                                 TKK_Partial -> triePartSubs t

lookupPartialByKey :: Ord k => TrieLookup -> [TrieKey k] -> Trie k v -> ([v],Maybe v)
lookupPartialByKey = lookupPartialByKey' (\_ v -> v)

lookupByKey :: Ord k => [TrieKey k] -> Trie k v -> Maybe v
lookupByKey keys trie = snd $ lookupPartialByKey TrieLookup_Partial keys trie

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup keys = lookupByKey $ mkTrieKeys keys
%%]

%%[9 export(lookupResultToList)
lookupResultToList :: ([v],Maybe v) -> [v]
lookupResultToList (vs,mv) = maybeToList mv ++ vs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(isEmpty,null)
isEmpty :: Trie k v -> Bool
isEmpty trie
  =  isNothing (trieMbVal trie)
  && Map.null  (triePartSubs trie)
  && Map.null  (trieSubs trie)

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

%%[9 export(singletonPartialByKey,singletonByKey,singletonKeyable)
singletonPartialByKey :: Ord k => TrieLookup -> [TrieKey k] -> v -> Trie k v
singletonPartialByKey trieLookup keys val
  = s keys
  where s []                          = Trie (Just val) Map.empty Map.empty
        s (TK_One TKK_Partial k : ks) = Trie Nothing (Map.singleton k $ singletonPartialByKey trieLookup ks val) Map.empty       
        s (TK_One _           k : ks) = Trie Nothing Map.empty (Map.singleton k $ singletonPartialByKey trieLookup ks val)

singletonByKey :: Ord k => [TrieKey k] -> v -> Trie k v
singletonByKey = singletonPartialByKey TrieLookup_Partial

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
      , triePartSubs    = Map.unionWith (unionWith cmb) (triePartSubs t1)  (triePartSubs t2)
      , trieSubs        = Map.unionWith (unionWith cmb) (trieSubs t1)      (trieSubs t2)
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
        d (TK_One knd k : ks) t
          = case fmap (d ks) $ Map.lookup k $ subs of
              Just c | isEmpty c -> upd (k `Map.delete` subs) t
                     | otherwise -> upd (Map.insert k c $ subs) t
              _                  -> t
          where (subs,upd) = case knd of
                    TKK_Normal  -> (trieSubs t,trieUpdSubs)
                    TKK_Partial -> (triePartSubs t,trieUpdPartSubs)
%%]

%%[9
deleteListByKey :: Ord k => [[TrieKey k]] -> Trie k v -> Trie k v
deleteListByKey keys trie = foldl (\t k -> deleteByKey k t) trie keys

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete keys = deleteByKey $ mkTrieKeys keys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance (ForceEval k, ForceEval v) => ForceEval (Trie k v) where
  forceEval x | forceEval (trieMbVal x) `seq` forceEval (triePartSubs x) `seq` forceEval (trieSubs x) `seq` True = x
%%[[102
  fevCount x = cm1 "Trie" `cmUnion` fevCount (trieMbVal x) `cmUnion` fevCount (triePartSubs x) `cmUnion` fevCount (trieSubs x)
%%]]

instance ForceEval k => ForceEval (TrieKey k) where
  forceEval x | forceEval (tkKey x) `seq` True = x
%%[[102
  fevCount x = cm1 "TrieKey" `cmUnion` fevCount (tkKey x)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
test1
  = fromListByKey
      [ ([TK_One TKK_Partial 1],"a")
      , ([TK_One TKK_Normal 1,TK_One TKK_Normal 3],"ac")
      , ([TK_One TKK_Normal 1,TK_One TKK_Partial 2],"ab")
      , ([TK_One TKK_Normal 1,TK_One TKK_Partial 2,TK_One TKK_Normal 3],"abc")
      , ([TK_One TKK_Normal 2],"b")
      ]

l1t1 = lookupPartialByKey' (,) TrieLookup_Partial [TK_One TKK_Normal 1]
l2t1 = lookupPartialByKey' (,) TrieLookup_Partial [TK_One TKK_Normal 1,TK_One TKK_Partial 2]
l3t1 = lookupPartialByKey' (,) TrieLookup_Partial [TK_One TKK_Normal 1,TK_One TKK_Normal 2]

test4
  = fromListPartialByKeyWith TrieLookup_Normal const
      [ ([TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Partial 4],"ab_")
      , ([TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Normal 3,TK_One TKK_Partial 4],"ab_d")
      ]

l1t4 = lookupPartialByKey' (,) TrieLookup_Partial [TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Normal 3,TK_One TKK_Partial 4]
l2t4 = lookupPartialByKey' (,) TrieLookup_Normal [TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Normal 3,TK_One TKK_Partial 4]
l3t4 = lookupPartialByKey' (,) TrieLookup_StopAtPartial [TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Normal 3,TK_One TKK_Partial 4]
l4t4 = lookupPartialByKey' (,) TrieLookup_StopAtPartial [TK_One TKK_Normal 1,TK_One TKK_Normal 2,TK_One TKK_Partial 4]
%%]
l1 = lookupPartialByKey False [TK_One TKK_Normal 1]
l1pn = lookupPartialByKey True [TK_One TKK_Normal 1]
l1pp = lookupPartialByKey True [TK_One TKK_Partial 1]
l2 = lookupPartialByKey False [TK_One TKK_Normal 1,TK_One TKK_Normal 2]
l3 = lookupPartialByKey False [TK_One TKK_Normal 4]
l4 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 2]
l5 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 5]
l6 = lookupPartialByKey False [TK_One TKK_Normal 4,TK_One TKK_Normal 6]

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

