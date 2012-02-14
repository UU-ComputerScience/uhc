%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TreeTrie, variation which allows matching on subtrees marked as a variable (kind of unification)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
This module should -someday- move to a general purpose library.

A TreeTrie is a search structure where the key actually consists of a
tree of keys, represented as a list of layers in the tree, 1 for every
depth, starting at the top, which are iteratively used for searching.
The search structure for common path/prefixes is shared, the trie
branches to multiple corresponding to available children, length
equality of children is used in searching (should match)

The TreeTrie structure implemented in this module deviates from the
usual TreeTrie implementations in that it allows wildcard matches
besides the normal full match. The objective is to also be able to
retrieve values for which (at insertion time) it has been indicated that
part does not need full matching. This intentionally is similar to
unification, where matching on a variable will succeed for arbitrary
values. Unification is not the job of this TreeTrie implementation, but
by returning partial matches as well, a list of possible match
candidates is returned.
%%]

%%[9 module {%{EH}Base.TreeTrie} import(qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import(Prelude hiding (lookup,null))
%%]

%%[9 import(qualified EH.Util.FastSeq as Seq,qualified Data.List as List)
%%]

%%[9 import(EH.Util.Utils)
%%]

%%[9 import(EH.Util.Pretty hiding (empty), qualified EH.Util.Pretty as PP)
%%]

%%[50 import(Data.Typeable(Typeable,Typeable1), Data.Generics(Data))
%%]
%%[50 hs import(Control.Monad)
%%]
%%[50 hs import({%{EH}Base.Serialize})
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

-- Debug
%%[9 import({%{EH}Base.Debug})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key into TreeTrie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TreeTrie1Key(..),TreeTrieMp1Key(..),TreeTrieMpKey,TreeTrieKey)
-- | Both key and trie can allow partial matching, indicated by TreeTrie1Key
data TreeTrie1Key k
  = TT1K_One    !k
  | TT1K_Any                            -- used to wildcard match a single node in a tree
  deriving (Eq, Ord)

-- | A key in a layer of TreeTrieMpKey
data TreeTrieMp1Key k
  = TTM1K       [TreeTrie1Key k]
  | TTM1K_Any                           -- used to wildcard match multiple children, internal only
  deriving (Eq, Ord)

-- | The key into a map used internally by the trie
type TreeTrieMpKey k
  = [TreeTrieMp1Key k]

-- | The key used externally to index into a trie
type TreeTrieKey k
  = [TreeTrieMpKey k]
%%]

%%[9999 export(mkTreeTrieKeys)
-- | Basic construction
mkTreeTrieKeys :: [k] -> TreeTrieKey k
mkTreeTrieKeys = Prelude.map (\k -> TTK (TT1K_One k) [])
%%]

%%[50
deriving instance Typeable1 TreeTrie1Key
deriving instance Typeable1 TreeTrieMp1Key
deriving instance Data x => Data (TreeTrie1Key x) 
deriving instance Data x => Data (TreeTrieMp1Key x) 
%%]

%%[9
instance Show k => Show (TreeTrie1Key k) where
  show  TT1K_Any    = "*"
  show (TT1K_One k) = "1:" ++ show k

instance Show k => Show (TreeTrieMp1Key k) where
  show (TTM1K_Any )  = "**" -- ++ show i
  show (TTM1K k)     = show k

instance PP k => PP (TreeTrie1Key k) where
  pp  TT1K_Any    = pp "*"
  pp (TT1K_One k) = "1:" >|< k

instance PP k => PP (TreeTrieMp1Key k) where
  pp = ppTreeTrieMp1Key
%%]

%%[9 export(ppTreeTrieKey)
ppTreeTrieMp1Key :: PP k => TreeTrieMp1Key k -> PP_Doc
ppTreeTrieMp1Key (TTM1K l) = ppBracketsCommas l
ppTreeTrieMp1Key (TTM1K_Any ) = pp "**" -- >|< i

ppTreeTrieMpKey :: PP k => TreeTrieMpKey k -> PP_Doc
ppTreeTrieMpKey = ppListSep "<" ">" "," . map ppTreeTrieMp1Key

ppTreeTrieKey :: PP k => TreeTrieKey k -> PP_Doc
ppTreeTrieKey = ppBracketsCommas . map ppTreeTrieMpKey
%%]

%%[9999
instance Hashable k => Hashable (TreeTrieKey k) where
  hash = hash . ttkKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TreeTrieMpKey inductive construction from new node and children keys
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ttkSingleton,ttkAdd,ttkFixate)
-- | Make singleton, which should at end be stripped from bottom layer of empty TTM1K []
ttkSingleton :: TreeTrie1Key k -> TreeTrieKey k
ttkSingleton k = [[TTM1K [k]],[TTM1K []]]

-- | Add a new layer with single node on top, combining the rest.
--   length ks >= 2
ttkAdd :: TreeTrie1Key k -> [TreeTrieKey k] -> TreeTrieKey k
ttkAdd k ks
  = [TTM1K [k]]                                         -- new top node
    : [TTM1K $ concat [k | TTM1K k <- concat hs]]       -- first level children are put together in singleton list of list with all children
    : merge (split tls)                                 -- and the rest is just concatenated
  where (hs,tls) = split ks
        split = unzip . map hdAndTl
        merge (hs,[]) = [concat hs]
        merge (hs,tls) = concat hs : merge (split $ filter (not . List.null) tls)

-- | Fixate by removing lowest layer empty children
ttkFixate :: TreeTrieKey k -> TreeTrieKey k
ttkFixate (kk:kks) | all (\(TTM1K k) -> List.null k) kk = []
                   | otherwise                          = kk : ttkFixate kks
ttkFixate _                                             = []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TreeTrieMpKey matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
-- | Match 1st arg with wildcards to second, returning the to be propagated key to next layer in tree
matchTreeTrieMpKeyTo :: Eq k => TreeTrieMpKey k -> TreeTrieMpKey k -> Maybe (TreeTrieMpKey k -> TreeTrieMpKey k)
matchTreeTrieMpKeyTo l r
  | all isJust llrr = Just (\k -> concat $ zipWith ($) (concatMap fromJust llrr) k)
  | otherwise       = Nothing
  where llrr = zipWith m l r
        m (TTM1K     l) (TTM1K r) | length l == length r && all isJust lr
                                                  = Just (concatMap fromJust lr)
                                  | otherwise     = Nothing
                                  where lr = zipWith m1 l r
        m (TTM1K_Any  ) (TTM1K r) = Just [const $ replicate (length r) TTM1K_Any]
        m1  TT1K_Any     _                    = Just [const [TTM1K_Any]]
        m1 (TT1K_One l) (TT1K_One r) | l == r = Just [(:[])]
        m1  _            _                    = Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Keyable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TreeTrieKeyable(..))
class TreeTrieKeyable x k where
  toTreeTrieKey :: x -> TreeTrieKey k
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TreeTrie lookup/insertion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TreeTrieLookup(..))
data TreeTrieLookup
  = TTL_Exact                           -- lookup with exact match
  | TTL_WildInTrie                      -- lookup with wildcard matching in trie
  | TTL_WildInKey                       -- lookup with wildcard matching in key
  | TTL_ExpandAtPartialTreeTrie     -- expand subtries at partial trie
  deriving (Eq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TreeTrie structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(TreeTrie,emptyTreeTrie,empty)
-- | Child structure
type TreeTrieChildren k v
  = Map.Map (TreeTrieMpKey k) (TreeTrie k v)

{-
type TreeTrieChildren k v
  = Map.Map                                                 -- search continuations
        [TreeTrie1Key k]                                    --  for each sequence of TreeTrie1Key
        [TreeTrie k v]
-}

-- | The trie structure, branching out on (1) kind, (2) nr of children, (3) actual key
data TreeTrie k v
  = TreeTrie
      { ttrieMbVal       :: Maybe v                                                 -- value
      , ttrieSubs        :: TreeTrieChildren k v                                    -- children
      }
%%[[50
 deriving (Typeable, Data)
%%]]

emptyTreeTrie, empty :: TreeTrie k v
emptyTreeTrie = TreeTrie Nothing Map.empty

empty = emptyTreeTrie
%%]

%%[9999 export(ppTreeTrieAsIs)
-- | PP a TreeTrie as is, directly corresponding to original structure
ppTreeTrieAsIs :: (PP k, PP v) => TreeTrie k v -> PP_Doc
ppTreeTrieAsIs t
  =     "V:" >#< (maybe PP.empty pp $ ttrieMbVal t)
    >-< "P:" >#< (ppSub $ ttriePartSubs t)
    >-< "N:" >#< (ppSub $ ttrieSubs t)
  where ppKV (k,v) = k >-< indent 2 (":" >#< ppTreeTrieAsIs v)
        ppSub = ppBracketsCommasV . map ppKV . Map.toList

instance (Show k, Show v) => Show (TreeTrie k v) where
  showsPrec _ t = showList $ toListByKey t

instance (PP k, PP v) => PP (TreeTrie k v) where
  pp t = ppBracketsCommasV $ map (\(a,b) -> ppTreeTrieKey a >#< ":" >#< b) $ toListByKey t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Reconstruction of original key-value pairs.

  = Seq.unions [ Seq.map (\(ks,v) -> (TK_One knd k:ks,v)) $ toFastSeq True t | (k,t) <- Map.toList tries ]

%%[9 export(toListByKey,toList)
toFastSeqSubs :: TreeTrieChildren k v -> Seq.FastSeq (TreeTrieKey k,v)
toFastSeqSubs ttries
  = Seq.unions
      [ Seq.map (\(ks,v) -> (k:ks,v)) $ toFastSeq True t
      | (k,t) <- Map.toList ttries
      ]

toFastSeq :: Bool -> TreeTrie k v -> Seq.FastSeq (TreeTrieKey k,v)
toFastSeq inclEmpty ttrie
  =          (case ttrieMbVal ttrie of
                Just v | inclEmpty -> Seq.singleton ([],v)
                _                  -> Seq.empty
             )
    Seq.:++: toFastSeqSubs (ttrieSubs ttrie)

toListByKey, toList :: TreeTrie k v -> [(TreeTrieKey k,v)]
toListByKey = Seq.toList . toFastSeq True

toList = toListByKey
%%]

%%[9 export(fromListByKey,fromListByKeyWith,fromListWith,fromList)
fromListByKeyWith :: Ord k => (v -> v -> v) -> [(TreeTrieKey k,v)] -> TreeTrie k v
fromListByKeyWith cmb = unionsWith cmb . map (uncurry singleton)

fromListByKey :: Ord k => [(TreeTrieKey k,v)] -> TreeTrie k v
fromListByKey = unions . map (uncurry singleton)

fromListWith :: Ord k => (v -> v -> v) -> [(TreeTrieKey k,v)] -> TreeTrie k v
fromListWith cmb = fromListByKeyWith cmb

fromList :: Ord k => [(TreeTrieKey k,v)] -> TreeTrie k v
fromList = fromListByKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Normal lookup for exact match + partial matches (which require some sort of further unification, determining whether it was found)

%%[9 export(lookupPartialByKey,lookupPartialByKey',lookupByKey,lookup)
lookupPartialByKey' :: (PP k,Ord k) => (TreeTrieKey k -> v -> v') -> TreeTrieLookup -> TreeTrieKey k -> TreeTrie k v -> ([v'],Maybe v')
lookupPartialByKey' mkRes ttrieLookup keys ttrie
  = l keys
  where l  []                = ([],fmap (mkRes []) $ ttrieMbVal ttrie)
        l  (k : ks) | ttrieLookup == TTL_Exact
                             = case Map.lookup k $ ttrieSubs ttrie of
                                 Just ttrie'
                                   -> ([], m)
                                   where (_,m) = lookupPartialByKey' (mkRes' k) ttrieLookup ks ttrie'
                                 _ -> ([], Nothing)
        l  (k : ks) | ttrieLookup == TTL_WildInTrie
                             = (catMaybes mbs ++ concat subs, Nothing)
                             where (subs,mbs)
                                     = unzip
                                         [ lookupPartialByKey' (mkRes' k') ttrieLookup ks t
                                         | (k',t) <- Map.toList $ ttrieSubs ttrie, isJust (matchTreeTrieMpKeyTo k' k)
                                         ]
        l  (k : ks) | ttrieLookup == TTL_WildInKey
                             = (catMaybes mbs ++ concat subs, Nothing)
                             where (subs,mbs)
                                     = unzip
                                         [ case ks of
                                             (k'':ks'')                  -> l k' (fromJust m k'' : ks'') t
                                             [] | Map.null (ttrieSubs t) -> l k' [] t
                                                | otherwise              -> l k' [fromJust m $ repeat (TTM1K [])] t
                                         | (k',t) <- Map.toList $ ttrieSubs ttrie
                                         , let m = -- (\v -> tr "XX" (ppTreeTrieMpKey k >#< ppTreeTrieMpKey k' >#< maybe (pp "--") (\f -> ppTreeTrieMpKey $ f $ repeat (TTM1K [])) v) v) $ 
                                                   matchTreeTrieMpKeyTo k k'
                                         , isJust m
                                         ]
                                     where l k' k t = lookupPartialByKey' (mkRes' k') ttrieLookup k t
{-
        l  (kk@(TTK_Cons TT1K_One k) : ks) | ttrieLookup == TTL_WildInTrie
                             = case Map.lookup k $ ttrieSubs ttrie of
                                 Just ttrie'
                                   -> (subsPart ++ p, m)
                                   where (p,m) = lookupPartialByKey' (mkRes' kk) ttrieLookup ks ttrie'
                                 _ -> (subsPart, Nothing)
                             where subsPart = lp (ttriePartSubs ttrie) ks
        l  (kk@(TTK_Cons TT1K_One k) : ks) | ttrieLookup == TTL_WildInKey
                             = case Map.lookup k $ ttrieSubs ttrie of
                                 Just ttrie'
                                   -> lookupPartialByKey' (mkRes' kk) ttrieLookup ks ttrie'
                                 _ -> ([], Nothing)
        l  (kk@(TTK_Cons TT1K_One k) : ks) | ttrieLookup == TTL_ExpandAtPartialTreeTrie -- && not (Map.null $ ttriePartSubs ttrie)
                             = case Map.lookup k $ ttrieSubs ttrie of
                                 Just ttrie'
                                   -> (p ++ subsPart, m)
                                   where (p,m) = lookupPartialByKey' (mkRes' kk) ttrieLookup ks ttrie'
                                 _ -> (subsPart, Nothing)
                             where subsPart = lf mkRes (ttrie {ttrieSubs = Map.empty})
        l  (kk@(TTK_Cons TT1K_Any k) : ks) | ttrieLookup == TTL_WildInKey || ttrieLookup == TTL_ExpandAtPartialTreeTrie
                             = (lf mkRes ttrie, Nothing)
        l  (kk@(TTK_Cons TT1K_Any k) : ks) | ttrieLookup == TTL_WildInTrie
                             = case Map.lookup k $ ttriePartSubs ttrie of
                                 Just ttrie'
                                   -> (lp (k `Map.delete` ttriePartSubs ttrie) ks ++ p, m)
                                   where (p,m) = lookupPartialByKey' (mkRes' kk) ttrieLookup ks ttrie'
                                 _ -> (lp (ttriePartSubs ttrie) ks, Nothing)
        l  (kk@(TTK_Cons knd k) : ks) | ttrieLookup /= TTL_WildInTrie && ttrieLookup /= TTL_ExpandAtPartialTreeTrie
                             = case Map.lookup k $ subsOf knd ttrie of
                                 Just ttrie'
                                   -> ([], m)
                                   where (_,m) = lookupPartialByKey' (mkRes' kk) ttrieLookup ks ttrie'
                                 _ -> ([], Nothing)
        -- lookup sub ttries
        lp subs ks           = concat [ lookupResultToList $ lookupPartialByKey' (mkResP k) ttrieLookup ks psub | (k,psub) <- psubs, ks <- kss ]
                             where kss = if List.null ks then [[]] else [[],ks]
                                   psubs = Map.toList subs
        -- lookup full ttrie
        lf mkRes ttrie        = map (\(ks,v) -> mkRes ks v) $ Seq.toList $ toFastSeq True ttrie
        mkResN k             = mkRes' (TTK_Cons TT1K_One k)
        mkResP k             = mkRes' (TTK_Cons TT1K_Any k)
        subsOf knd t         = case knd of
                                 TT1K_One  -> ttrieSubs t
                                 TT1K_Any -> ttriePartSubs t
-}
        mkRes' k ks v        = mkRes (k : ks) v

lookupPartialByKey :: (PP k,Ord k) => TreeTrieLookup -> TreeTrieKey k -> TreeTrie k v -> ([v],Maybe v)
lookupPartialByKey = lookupPartialByKey' (\_ v -> v)

lookupByKey, lookup :: (PP k,Ord k) => TreeTrieKey k -> TreeTrie k v -> Maybe v
lookupByKey keys ttrie = snd $ lookupPartialByKey TTL_WildInTrie keys ttrie

lookup = lookupByKey
%%]

%%[9999 export(lookupResultToList)
lookupResultToList :: ([v],Maybe v) -> [v]
lookupResultToList (vs,mv) = maybeToList mv ++ vs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(isEmpty,null)
isEmpty :: TreeTrie k v -> Bool
isEmpty ttrie
  =  isNothing (ttrieMbVal ttrie)
  && Map.null  (ttrieSubs ttrie)

null :: TreeTrie k v -> Bool
null = isEmpty
%%]

%%[9 export(elems)
elems :: TreeTrie k v -> [v]
elems = map snd . toListByKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(singleton,singletonKeyable)
singleton :: Ord k => TreeTrieKey k -> v -> TreeTrie k v
singleton keys val
  = s keys
  where s []       = TreeTrie (Just val) Map.empty
        s (k : ks) = TreeTrie Nothing (Map.singleton k $ singleton ks val) 

singletonKeyable :: (Ord k,TreeTrieKeyable v k) => v -> TreeTrie k v
singletonKeyable val = singleton (toTreeTrieKey val) val
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Union, insert, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(unionWith,union,unionsWith,unions)
unionWith :: Ord k => (v -> v -> v) -> TreeTrie k v -> TreeTrie k v -> TreeTrie k v
unionWith cmb t1 t2
  = TreeTrie
      { ttrieMbVal       = mkMb          cmb             (ttrieMbVal t1) (ttrieMbVal t2)
      , ttrieSubs        = Map.unionWith (unionWith cmb) (ttrieSubs  t1) (ttrieSubs  t2)
      }
  where mkMb _   j         Nothing   = j
        mkMb _   Nothing   j         = j
        mkMb cmb (Just x1) (Just x2) = Just $ cmb x1 x2

union :: Ord k => TreeTrie k v -> TreeTrie k v -> TreeTrie k v
union = unionWith const

unionsWith :: Ord k => (v -> v -> v) -> [TreeTrie k v] -> TreeTrie k v
unionsWith cmb [] = emptyTreeTrie
unionsWith cmb ts = foldr1 (unionWith cmb) ts

unions :: Ord k => [TreeTrie k v] -> TreeTrie k v
unions = unionsWith const
%%]

%%[9 export(insertByKeyWith,insertByKey)
insertByKeyWith :: Ord k => (v -> v -> v) -> TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insertByKeyWith cmb keys val ttrie = unionsWith cmb [singleton keys val,ttrie]

insertByKey :: Ord k => TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insertByKey = insertByKeyWith const

insert :: Ord k => TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insert = insertByKey

insertKeyable :: (Ord k,TreeTrieKeyable v k) => v -> TreeTrie k v -> TreeTrie k v
insertKeyable val = insertByKey (toTreeTrieKey val) val
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Delete, ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(deleteByKey, delete)
deleteByKey, delete :: Ord k => TreeTrieKey k -> TreeTrie k v -> TreeTrie k v
deleteByKey keys ttrie
  = d keys ttrie
  where d [] t
          = t {ttrieMbVal = Nothing}
        d (k : ks) t
          = case fmap (d ks) $ Map.lookup k $ ttrieSubs t of
              Just c | isEmpty c -> t { ttrieSubs = k `Map.delete` ttrieSubs t }
                     | otherwise -> t { ttrieSubs = Map.insert k c $ ttrieSubs t }
              _                  -> t

delete = deleteByKey
%%]

%%[9 export(deleteListByKey)
deleteListByKey :: Ord k => [TreeTrieKey k] -> TreeTrie k v -> TreeTrie k v
deleteListByKey keys ttrie = foldl (\t k -> deleteByKey k t) ttrie keys
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Serialize, ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Serialize k => Serialize (TreeTrie1Key k) where
  sput (TT1K_Any            ) = sputWord8 0
  sput (TT1K_One   a        ) = sputWord8 1 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> return TT1K_Any
            1 -> liftM  TT1K_One         sget

instance Serialize k => Serialize (TreeTrieMp1Key k) where
  sput (TTM1K_Any            ) = sputWord8 0 -- >> sput a
  sput (TTM1K       a        ) = sputWord8 1 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> return TTM1K_Any     -- sget
            1 -> liftM  TTM1K         sget

instance (Ord k, Serialize k, Serialize v) => Serialize (TreeTrie k v) where
  sput (TreeTrie a b) = sput a >> sput b
  sget = liftM2 TreeTrie sget sget
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
test1
  = fromListByKey
      [ ( [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_Any, TT1K_One "P"]]
          , [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K []]
          ]
        , "C (* D F) P"
        )
      , ( [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_One "B", TT1K_One "P"]]
          , [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K []]
          ]
        , "C (B D F) P"
        )
      , ( [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_One "B", TT1K_One "P"]]
          , [TTM1K [], TTM1K [TT1K_One "Q", TT1K_One "R"]]
          ]
        , "C B (P Q R)"
        )
      , ( [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_One "B", TT1K_Any]]
          ]
        , "C B *"
        )
      ]

m1 = fromJust 
     $ fmap (\f -> f $ [TTM1K [], TTM1K [TT1K_One "Z"]])
     $ matchTreeTrieMpKeyTo
        [TTM1K [TT1K_Any, TT1K_One "P"]]
        [TTM1K [TT1K_One "B", TT1K_One "P"]]

m2 = fmap (\f -> f $ repeat (TTM1K []))
     $ matchTreeTrieMpKeyTo
        m1
        [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K [TT1K_One "Z"]]

m3 = fromJust 
     $ fmap (\f -> f $ [TTM1K [TT1K_Any, TT1K_One "P"]])
     $ matchTreeTrieMpKeyTo
        [TTM1K [TT1K_One "C"]]
        [TTM1K [TT1K_One "C"]]

m4 = fmap (\f -> f $ repeat (TTM1K []))
     $ matchTreeTrieMpKeyTo
        m3
        [TTM1K [TT1K_One "B", TT1K_One "P"]]

m5 = fmap (\f -> f $ repeat (TTM1K []))
     $ matchTreeTrieMpKeyTo
        (fromJust m4)
        [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K []]

l1t1 = lookupPartialByKey' (,) TTL_Exact
          [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_Any, TT1K_One "P"]]
          ]

l2t1 = lookupPartialByKey' (,) TTL_WildInTrie
          [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_One "B", TT1K_One "P"]]
          ]

l3t1 = lookupPartialByKey' (,) TTL_WildInKey
          [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_Any, TT1K_One "P"]]
          ]

l4t1 = lookupPartialByKey' (,) TTL_WildInKey
          [ [TTM1K [TT1K_Any :: TreeTrie1Key String]]
          ]

l5t1 = lookupPartialByKey' (,) TTL_WildInTrie
          [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_One "B", TT1K_One "P"]]
          , [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K []]
          ]

l6t1 = lookupPartialByKey' (,) TTL_WildInKey
          [ [TTM1K [TT1K_One "C"]]
          , [TTM1K [TT1K_Any, TT1K_One "P"]]
          , [TTM1K [TT1K_One "D", TT1K_One "F"], TTM1K []]
          ]

%%]
