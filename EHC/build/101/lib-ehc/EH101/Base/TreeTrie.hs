module EH101.Base.TreeTrie
( TreeTrie1Key (..), TreeTrieMp1Key (..), TreeTrieMpKey, TreeTrieKey
, ppTreeTrieKey
, ttkSingleton, ttkAdd', ttkAdd, ttkChildren, ttkFixate
, ttkParentChildren
, TreeTrieKeyable (..)
, TreeTrieLookup (..)
, TreeTrie, emptyTreeTrie, empty
, toListByKey, toList
, fromListByKeyWith, fromList
, lookupPartialByKey, lookupPartialByKey', lookupByKey, lookup
, lookupResultToList
, isEmpty, null
, elems
, singleton, singletonKeyable
, unionWith, union, unionsWith, unions
, insertByKeyWith, insertByKey
, deleteByKey, delete
, deleteListByKey )
where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup,null)
import qualified EH.Util.FastSeq as Seq
import qualified Data.List as List
import EH.Util.Utils
import EH.Util.Pretty hiding (empty)
import qualified EH.Util.Pretty as PP
import EH101.Base.Debug
import Data.Typeable (Typeable,Typeable1)
import Data.Generics (Data)
import Control.Monad
import EH101.Base.Serialize



{-# LINE 60 "src/ehc/Base/TreeTrie.chs" #-}
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

{-# LINE 88 "src/ehc/Base/TreeTrie.chs" #-}
deriving instance Typeable1 TreeTrie1Key
deriving instance Typeable1 TreeTrieMp1Key
deriving instance Data x => Data (TreeTrie1Key x)
deriving instance Data x => Data (TreeTrieMp1Key x)

{-# LINE 95 "src/ehc/Base/TreeTrie.chs" #-}
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

{-# LINE 112 "src/ehc/Base/TreeTrie.chs" #-}
ppTreeTrieMp1Key :: PP k => TreeTrieMp1Key k -> PP_Doc
ppTreeTrieMp1Key (TTM1K l) = ppBracketsCommas l
ppTreeTrieMp1Key (TTM1K_Any ) = pp "**" -- >|< i

ppTreeTrieMpKey :: PP k => TreeTrieMpKey k -> PP_Doc
ppTreeTrieMpKey = ppListSep "<" ">" "," . map ppTreeTrieMp1Key

ppTreeTrieKey :: PP k => TreeTrieKey k -> PP_Doc
ppTreeTrieKey = ppBracketsCommas . map ppTreeTrieMpKey

{-# LINE 133 "src/ehc/Base/TreeTrie.chs" #-}
-- | Make singleton, which should at end be stripped from bottom layer of empty TTM1K []
ttkSingleton :: TreeTrie1Key k -> TreeTrieKey k
ttkSingleton k = [TTM1K [k]] : ttkEmpty

-- | empty key
ttkEmpty :: TreeTrieKey k
ttkEmpty = [[TTM1K []]]

-- | Construct intermediate structure for children for a new Key
--   length ks >= 2
ttkChildren :: [TreeTrieKey k] -> [TreeTrieMpKey k]
ttkChildren ks
  =   [TTM1K $ concat [k | TTM1K k <- concat hs]]       -- first level children are put together in singleton list of list with all children
    : merge (split tls)                                 -- and the rest is just concatenated
  where (hs,tls) = split ks
        split = unzip . map hdAndTl
        merge (hs,[]) = [concat hs]
        merge (hs,tls) = concat hs : merge (split $ filter (not . List.null) tls)

-- | Add a new layer with single node on top, combining the rest.
ttkAdd' :: TreeTrie1Key k -> [TreeTrieMpKey k] -> TreeTrieKey k
ttkAdd' k ks = [TTM1K [k]] : ks

-- | Add a new layer with single node on top, combining the rest.
--   length ks >= 2
ttkAdd :: TreeTrie1Key k -> [TreeTrieKey k] -> TreeTrieKey k
ttkAdd k ks = ttkAdd' k (ttkChildren ks)

-- | Fixate by removing lowest layer empty children
ttkFixate :: TreeTrieKey k -> TreeTrieKey k
ttkFixate (kk:kks) | all (\(TTM1K k) -> List.null k) kk = []
                   | otherwise                          = kk : ttkFixate kks
ttkFixate _                                             = []

{-# LINE 173 "src/ehc/Base/TreeTrie.chs" #-}
-- | Split key into parent and children components, inverse of ttkAdd'
ttkParentChildren :: TreeTrieKey k -> ( TreeTrie1Key k, [TreeTrieMpKey k] )
ttkParentChildren k
  = case k of
      ([TTM1K [h]] : t) -> (h,t)

{-# LINE 185 "src/ehc/Base/TreeTrie.chs" #-}
-- | Match 1st arg with wildcards to second, returning the to be propagated key to next layer in tree
matchTreeTrieMpKeyTo :: Eq k => TreeTrieMpKey k -> TreeTrieMpKey k -> Maybe (TreeTrieMpKey k -> TreeTrieMpKey k)
matchTreeTrieMpKeyTo l r
  | all isJust llrr = Just (\k -> concat $ zipWith ($) (concatMap (fromJust) llrr) k)
  | otherwise       = Nothing
  where llrr = zipWith m l r
        m (TTM1K     l) (TTM1K r) | length l == length r && all isJust lr
                                                  = Just (concatMap fromJust lr)
                                  | otherwise     = Nothing
                                  where lr = zipWith m1 l r
        m (TTM1K_Any  ) (TTM1K []) = Just []
        m (TTM1K_Any  ) (TTM1K r ) = Just [const $ replicate (length r) TTM1K_Any]
        m1  TT1K_Any     _                    = Just [const [TTM1K_Any]]
        m1 (TT1K_One l) (TT1K_One r) | l == r = Just [\x -> [x]]
        m1  _            _                    = Nothing

{-# LINE 207 "src/ehc/Base/TreeTrie.chs" #-}
-- | Keyable values, i.e. capable of yielding a TreeTrieKey for retrieval from a trie
class TreeTrieKeyable x k where
  toTreeTrieKey :: x -> TreeTrieKey k

{-# LINE 217 "src/ehc/Base/TreeTrie.chs" #-}
data TreeTrieLookup
  = TTL_Exact                           -- lookup with exact match
  | TTL_WildInTrie                      -- lookup with wildcard matching in trie
  | TTL_WildInKey                       -- lookup with wildcard matching in key
  deriving (Eq)

{-# LINE 229 "src/ehc/Base/TreeTrie.chs" #-}
-- | Child structure
type TreeTrieChildren k v
  = Map.Map (TreeTrieMpKey k) (TreeTrie k v)

-- | The trie structure, branching out on (1) kind, (2) nr of children, (3) actual key
data TreeTrie k v
  = TreeTrie
      { ttrieMbVal       :: Maybe v                                                 -- value
      , ttrieSubs        :: TreeTrieChildren k v                                    -- children
      }
 deriving (Typeable, Data)

emptyTreeTrie, empty :: TreeTrie k v
emptyTreeTrie = TreeTrie Nothing Map.empty

empty = emptyTreeTrie

{-# LINE 261 "src/ehc/Base/TreeTrie.chs" #-}
instance (Show k, Show v) => Show (TreeTrie k v) where
  showsPrec _ t = showList $ toListByKey t

instance (PP k, PP v) => PP (TreeTrie k v) where
  pp t = ppBracketsCommasV $ map (\(a,b) -> ppTreeTrieKey a >#< ":" >#< b) $ toListByKey t

{-# LINE 275 "src/ehc/Base/TreeTrie.chs" #-}
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

{-# LINE 297 "src/ehc/Base/TreeTrie.chs" #-}
fromListByKeyWith :: Ord k => (v -> v -> v) -> [(TreeTrieKey k,v)] -> TreeTrie k v
fromListByKeyWith cmb = unionsWith cmb . map (uncurry singleton)

fromListByKey :: Ord k => [(TreeTrieKey k,v)] -> TreeTrie k v
fromListByKey = unions . map (uncurry singleton)

fromListWith :: Ord k => (v -> v -> v) -> [(TreeTrieKey k,v)] -> TreeTrie k v
fromListWith cmb = fromListByKeyWith cmb

fromList :: Ord k => [(TreeTrieKey k,v)] -> TreeTrie k v
fromList = fromListByKey

{-# LINE 317 "src/ehc/Base/TreeTrie.chs" #-}
lookupPartialByKey' :: forall k v v' . (PP k,Ord k) => (TreeTrieKey k -> v -> v') -> TreeTrieLookup -> TreeTrieKey k -> TreeTrie k v -> ([v'],Maybe v')
lookupPartialByKey' mkRes ttrieLookup keys ttrie
  = l id mkRes keys ttrie
  where l :: (TreeTrieMpKey k -> TreeTrieMpKey k) -> (TreeTrieKey k -> v -> v') -> TreeTrieKey k -> TreeTrie k v -> ([v'],Maybe v')
        l = case ttrieLookup of
              -- Exact match
              TTL_Exact -> \updTKey mkRes keys ttrie ->
                case keys of
                  [] -> dflt mkRes ttrie
                  (k : ks)
                     -> case Map.lookup k $ ttrieSubs ttrie of
                          Just ttrie'
                            -> ([], m)
                            where (_,m) = l id (res mkRes k) ks ttrie'
                          _ -> ([], Nothing)

              -- Match with possible wildcard in Trie
              TTL_WildInTrie -> \updTKey mkRes keys ttrie ->
                -- tr "TTL_WildInTrie" (ppTreeTrieKey keys >#< (ppTreeTrieMpKey $ updTKey $ replicate (5) (TTM1K []))) $
                case keys of
                  [] -> dflt mkRes ttrie
                  (k : ks)
                     -> (catMaybes mbs ++ concat subs, Nothing)
                     where (subs,mbs)
                             = unzip
                                 [ case ks of
                                     []                                  -> l id (res mkRes k) [] t
                                     (ksk:ksks) | Map.null (ttrieSubs t) -> match (res mkRes k) (fromJust mbm) ks
                                                | otherwise              -> l (fromJust mbm) (res mkRes k) ks t
                                        where match mkRes m (km:kms)
                                                = case matchTreeTrieMpKeyTo kt' km of
                                                    Just m -> match (res mkRes k) m kms
                                                    _      -> ([], Nothing)
                                                where kt' = m $ repeat (TTM1K [])
                                              match mkRes _ []
                                                = l id (res mkRes k) [] t
                                 | (kt,t) <- Map.toList $ ttrieSubs ttrie
                                 , let kt' = updTKey kt
                                       mbm = -- (\v -> tr "XX" (ppTreeTrieMpKey kt >#< (ppTreeTrieMpKey $ updTKey $ replicate (5) (TTM1K [])) >#< ppTreeTrieMpKey kt' >#< ppTreeTrieMpKey k >#< maybe (pp "--") (\f -> ppTreeTrieMpKey $ f $ repeat (TTM1K [])) v) v) $
                                             matchTreeTrieMpKeyTo kt' k
                                 , isJust mbm
                                 ]

              -- Match with possible wildcard in Key
              TTL_WildInKey -> \updTKey mkRes keys ttrie ->
                case keys of
                  [] -> dflt mkRes ttrie
                  (k : ks)
                     -> (catMaybes mbs ++ concat subs, Nothing)
                     where (subs,mbs)
                             = unzip
                                 [ case ks of
                                     (ksk:ksks)                  -> l id (res mkRes kt) (fromJust m ksk : ksks) t
                                     [] | Map.null (ttrieSubs t) -> l id (res mkRes kt) [] t
                                        | otherwise              -> l id (res mkRes kt) [fromJust m $ repeat (TTM1K [])] t
                                 | (kt,t) <- Map.toList $ ttrieSubs ttrie
                                 , let m = -- (\v -> tr "YY" (ppTreeTrieMpKey k >#< ppTreeTrieMpKey kt >#< maybe (pp "--") (\f -> ppTreeTrieMpKey $ f $ repeat (TTM1K [])) v) v) $
                                           matchTreeTrieMpKeyTo k kt
                                 , isJust m
                                 ]

          -- Utils
          where dflt mkRes ttrie = ([],fmap (mkRes []) $ ttrieMbVal ttrie)
                res mkRes k = \ks v -> mkRes (k : ks) v

lookupPartialByKey :: (PP k,Ord k) => TreeTrieLookup -> TreeTrieKey k -> TreeTrie k v -> ([v],Maybe v)
lookupPartialByKey = lookupPartialByKey' (\_ v -> v)

lookupByKey, lookup :: (PP k,Ord k) => TreeTrieKey k -> TreeTrie k v -> Maybe v
lookupByKey keys ttrie = snd $ lookupPartialByKey TTL_WildInTrie keys ttrie

lookup = lookupByKey

{-# LINE 392 "src/ehc/Base/TreeTrie.chs" #-}
lookupResultToList :: ([v],Maybe v) -> [v]
lookupResultToList (vs,mv) = maybeToList mv ++ vs

{-# LINE 401 "src/ehc/Base/TreeTrie.chs" #-}
isEmpty :: TreeTrie k v -> Bool
isEmpty ttrie
  =  isNothing (ttrieMbVal ttrie)
  && Map.null  (ttrieSubs ttrie)

null :: TreeTrie k v -> Bool
null = isEmpty

{-# LINE 411 "src/ehc/Base/TreeTrie.chs" #-}
elems :: TreeTrie k v -> [v]
elems = map snd . toListByKey

{-# LINE 420 "src/ehc/Base/TreeTrie.chs" #-}
singleton :: Ord k => TreeTrieKey k -> v -> TreeTrie k v
singleton keys val
  = s keys
  where s []       = TreeTrie (Just val) Map.empty
        s (k : ks) = TreeTrie Nothing (Map.singleton k $ singleton ks val)

singletonKeyable :: (Ord k,TreeTrieKeyable v k) => v -> TreeTrie k v
singletonKeyable val = singleton (toTreeTrieKey val) val

{-# LINE 435 "src/ehc/Base/TreeTrie.chs" #-}
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

{-# LINE 457 "src/ehc/Base/TreeTrie.chs" #-}
insertByKeyWith :: Ord k => (v -> v -> v) -> TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insertByKeyWith cmb keys val ttrie = unionsWith cmb [singleton keys val,ttrie]

insertByKey :: Ord k => TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insertByKey = insertByKeyWith const

insert :: Ord k => TreeTrieKey k -> v -> TreeTrie k v -> TreeTrie k v
insert = insertByKey

insertKeyable :: (Ord k,TreeTrieKeyable v k) => v -> TreeTrie k v -> TreeTrie k v
insertKeyable val = insertByKey (toTreeTrieKey val) val

{-# LINE 475 "src/ehc/Base/TreeTrie.chs" #-}
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

{-# LINE 490 "src/ehc/Base/TreeTrie.chs" #-}
deleteListByKey :: Ord k => [TreeTrieKey k] -> TreeTrie k v -> TreeTrie k v
deleteListByKey keys ttrie = foldl (\t k -> deleteByKey k t) ttrie keys

{-# LINE 499 "src/ehc/Base/TreeTrie.chs" #-}
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


