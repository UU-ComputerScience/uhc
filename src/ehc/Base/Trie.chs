%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie, variation which allows matching on subtrees marked as a variable (kind of unification)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This module should -someday- move to a general purpose library.



%%[9 module {%{EH}Base.Trie} import(qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import(Prelude hiding (lookup))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key into Trie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A [TrieKey a] is used as a key into a trie, keying into successive mappings.
A TK_One is just a single key.
TK_Partial is a TK_One, but makes looking up also return the corresponding value when partial matched.
TK_Sub gives structure to a [TrieKey a] by partioning corresponding to substructure.

%%[9 export(TrieKey(..))
data TrieKeyKind = TKK_Partial | TKK_Normal
  deriving (Eq, Ord, Show)

data TrieKey k
  = TK_One      TrieKeyKind k
  | TK_Sub      [[TrieKey k]]
  deriving (Eq, Ord, Show)

mkTrieKeys :: [k] -> [TrieKey k]
mkTrieKeys = map (TK_One TKK_Normal)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trie structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Trie,emptyTrie,empty)
data Trie k v
  = Trie
      { trieMbVal       :: Maybe v                  -- value
      , trieMbPartial   :: Maybe (Trie k v)         -- partial match continuation
      , trieCont        :: Map.Map k (Trie k v)     -- search continuation
      }

emptyTrie, empty :: Trie k v
emptyTrie = Trie Nothing Nothing Map.empty

empty = emptyTrie
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Normal lookup for exact match + partial matches (which require some sort of further unification)

%%[9 export(lookupPartialByKey,lookupByKey,lookup)
lookupPartialByKey :: Ord k => [TrieKey k] -> Trie k v -> ([v],Maybe v)
lookupPartialByKey keys trie
  = l keys
  where l  []                = ([],trieMbVal trie)
        l  (TK_One _ k : ks) = case Map.lookup k $ trieCont trie of
                                 Just trie'
                                   -> (lp ks ++ p, m)
                                   where (p,m) = lookupPartialByKey ks trie'
                                 _ -> (lp ks, Nothing)
        l  (TK_Sub kss : ks) = l (concat kss ++ ks)
        lp ks                = case trieMbPartial trie of
                                 Just pt
                                   -> maybeToList m ++ p
                                   where (p,m) = lookupPartialByKey ks pt
                                 _ -> []

lookupByKey :: Ord k => [TrieKey k] -> Trie k v -> Maybe v
lookupByKey keys trie = snd $ lookupPartialByKey keys trie

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup keys = lookupByKey $ mkTrieKeys keys

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(singletonByKey)
singletonByKey :: Ord k => [TrieKey k] -> v -> Trie k v
singletonByKey keys val
  = s keys
  where s []                          = Trie (Just val) Nothing Map.empty
        s (TK_One TKK_Normal  k : ks) = Trie Nothing Nothing (Map.singleton k $ singletonByKey ks val)
        s (TK_One TKK_Partial _ : ks) = Trie Nothing (Just $ singletonByKey ks val) Map.empty       
        s (TK_Sub kss           : ks) = singletonByKey (concat kss ++ ks) val      

singleton :: Ord k => [k] -> v -> Trie k v
singleton keys val = singletonByKey (mkTrieKeys keys) val
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Union, insert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(unionWith,union,unions)
unionWith :: Ord k => (v -> v -> v) -> Trie k v -> Trie k v -> Trie k v
unionWith cmb t1 t2
  = Trie
      { trieMbVal       = mkMb          cmb             (trieMbVal t1)     (trieMbVal t2)
      , trieMbPartial   = mkMb          (unionWith cmb) (trieMbPartial t1) (trieMbPartial t2)
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


