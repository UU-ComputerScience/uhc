%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

Assumptions (to be documented further)
- The key [Trie.TrieKey Key] used to lookup a constraint in a CHR should be distinguishing enough to be used for the prevention
  of the application of a propagation rule for a 2nd time.

%%[9 module {%{EH}CHR.Solve} import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Key})
%%]

%%[9 import({%{EH}Base.Trie} as Trie)
%%]

%%[9 import(qualified Data.Set as Set,Data.List as List)
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR store, with fast search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRStore,emptyCHRStore)
type CHRKey = [Trie.TrieKey Key]

data StoredCHR p i g s
  = StoredCHR
      { storedChr       :: CHR (Constraint p i) g s     -- the CHR
      , storedKeyedInx  :: Int                          -- index of constraint for which is keyed into store
      , storedSimpSz    :: Int                          -- nr of simpl CHR
      , storedKeys      :: [Maybe CHRKey]               -- keys of all constraints; at storedKeyedInx: Nothing
      , storedIdent     :: CHRKey                       -- the identification of a CHR, used for propagation rules (see remark at begin)
      }

data CHRStore pred info guard subst
  = CHRStore
      { chrstoreTrie    :: Trie.Trie Key [StoredCHR pred info guard subst]
      }

mkCHRStore trie = CHRStore trie

emptyCHRStore :: CHRStore pred info guard subst
emptyCHRStore = mkCHRStore Trie.empty
%%]

%%[9 export(chrStoreFromElems,chrStoreUnion,chrStoreUnions,chrStoreSingletonElem)
chrStoreFromElems :: Keyable p => [CHR (Constraint p i) g s] -> CHRStore p i g s
chrStoreFromElems chrs
  = mkCHRStore
    $ Trie.fromListByKeyWith (++)
        [ (k,[StoredCHR chr i simpSz ks' (concat ks)])
        | chr <- chrs
        , let cs = chrSimpHead chr ++ chrPropHead chr
              simpSz = length $ chrSimpHead chr
              ks = map toKey cs
        , (c,k,i) <- zip3 cs ks [0..]
        , let (ks1,(_:ks2)) = splitAt i ks
              ks' = map Just ks1 ++ [Nothing] ++ map Just ks2
        ]

chrStoreSingletonElem :: Keyable p => CHR (Constraint p i) g s -> CHRStore p i g s
chrStoreSingletonElem x = chrStoreFromElems [x]

chrStoreUnion :: CHRStore p i g s -> CHRStore p i g s -> CHRStore p i g s
chrStoreUnion cs1 cs2 = mkCHRStore $ Trie.unionWith (++) (chrstoreTrie cs1) (chrstoreTrie cs2)

chrStoreUnions :: [CHRStore p i g s] -> CHRStore p i g s
chrStoreUnions []  = emptyCHRStore
chrStoreUnions [s] = s
chrStoreUnions ss  = foldr1 chrStoreUnion ss
%%]

%%[9 export(chrStoreToList,chrStoreElems)
chrStoreToList :: CHRStore p i g s -> [(CHRKey,[CHR (Constraint p i) g s])]
chrStoreToList cs
  = [ (k,chrs)
    | (k,e) <- Trie.toListByKey $ chrstoreTrie cs
    , let chrs = [chr | (StoredCHR {storedChr = chr, storedKeyedInx = 0}) <- e]
    , not $ Prelude.null chrs
    ]

chrStoreElems :: CHRStore p i g s -> [CHR (Constraint p i) g s]
chrStoreElems = concatMap snd . chrStoreToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ppCHRStore)
ppCHRStore :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> ppBracketsCommas k >-< indent 2 (":" >#< ppBracketsCommasV v)) . chrStoreToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type WorkKey = CHRKey

data Work p i
  = Work
      { workCnstr   :: Constraint p i           -- the constraint to be reduced
      , workUsedIn  :: Set.Set WorkKey          -- marked with the propagation rules already applied to it
      }

data WorkList p i
  = WorkList
      { wlTrie      :: Trie.Trie Key (Work p i)
      , wlQueue     :: [CHRKey]
      }

mkWorkList :: Keyable p => [Constraint p i] -> WorkList p i
mkWorkList cs
  = WorkList (Trie.fromListByKeyWith (const) work) (map fst work)
  where work = [ (toKey c,Work c Set.empty) | c <- cs ]
%%]

%%[9
data SolveStep p i g s
  = SolveStep
      { stepChr     :: CHR (Constraint p i) g s
      , stepSimps   :: [Constraint p i]
      , stepProps   :: [Constraint p i]
      , stepBody    :: [Constraint p i]
      }

type SolveTrace p i g s = [SolveStep p i g s]

data SolveState p i g s
  = SolveState
      { stWorkList      :: WorkList p i
      , stScannedWork   :: [WorkKey]
      , stDoneCnstrs    :: [Constraint p i]
      , stTrace         :: SolveTrace p i g s
      }
%%]

%%[9 export(chrSolve)
chrSolve :: ( CHRMatchable env p s, CHRCheckable g s
            , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable p tvar s
            , CHREmptySubstitution s
            ) => env -> CHRStore p i g s -> [Constraint p i] -> [Constraint p i]
chrSolve env chrStore cnstrs
  = stDoneCnstrs $ iter initState
  where iter st
          = case firstMatch st of
              Just m
                -> st
              _ -> st
        firstMatch st@(SolveState {stWorkList = WorkList {wlQueue = (workHd:workTl), wlTrie = wlTrie}})
          = foldr first Nothing
            $ concatMap (\c -> zip (repeat c) (foldr combine [] $ candidate c))
            $ concat $ lookupResultToList $ lookupPartialByKey False workHd
            $ chrstoreTrie chrStore
          where candidate (StoredCHR {storedKeys = ks})
                  = map (maybe (lkup workHd) lkup) ks
                  where lkup k = lookupResultToList $ lookupPartialByKey' (,) True k wlTrie
                combine l ls = concatMap (\e -> map (e:) ls) l
                match chr cnstrs
                  = foldl cmb (Just chrEmptySubst) $ matches chr cnstrs ++ checks chr
                  where matches (StoredCHR {storedChr = CHR {chrSimpHead = sc, chrPropHead = pc}}) cnstrs
                          = zipWith mt (sc ++ pc) cnstrs
                          where mt cFr cTo subst = chrMatchTo env (subst `chrAppSubst` cFr) cTo
                        checks (StoredCHR {storedChr = CHR {chrGuard = gd}})
                          = map chk gd
                          where chk g subst = chrCheck (subst `chrAppSubst` g)
                        cmb (Just s) next = fmap (`chrAppSubst` s) $ next s
                        cmb _        _    = Nothing
                first (chr,keysWorks) cont
                  = case match chr (map (workCnstr . snd) keysWorks) of
                      r@(Just s) -> Just (chr,keysWorks,s)
                      _          -> cont
        initState = SolveState (mkWorkList wl) [] done []
                  where (wl,done) = splitDone cnstrs
        splitDone = partition (\x -> cnstrRequiresSolve x)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gerrit's stuff:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{-# OPTIONS -fglasgow-exts #-}
module CHRSolver
 ( (<=>)
 , (==>)
 , (|>)
 , CHR (..)
 , Matchable (..)
 , chrSolve'
 , chrSolve
 , chrSolveWl
 , Subst
 , Pattern (..)
 , propMatch
 )
where

-- 
-- TODO:: - Solver can be made faster in the case: ftv constraint == []
--        - Use of monoid (`mappend`) for substitutions is wrong, substitutions must agree

import Data.List ((\\))
import qualified Data.Set as Set
import Data.Maybe
import Data.Monoid
import Test.QuickCheck (property, Property)

infix   1 <=>, ==>
infixr  0 |>

class (Ord c, Monoid s) => Matchable c s | c -> s where
  match :: c -> c -> Maybe s
  subst :: s -> c -> c

  -- default case: syntactic equality
  match x y | x == y    = Just mempty
            | otherwise = Nothing
  subst = flip const

data CHR c s = CHR { head    :: [c]
                   , guard   :: (s -> Maybe s)
                   , body    :: [c]
                   }

emptyGuard :: (Monoid a) => t -> Maybe a
emptyGuard = const $ Just mempty

instance (Show c, Eq c) => Show (CHR c s) where
  show (CHR  hs _ cs) = if all (`elem` cs) hs
                        then  sepWithComma hs ++ " ==> " ++ isEmpty (sepWithComma (cs \\ hs))
                        else  sepWithComma hs ++ " <=> " ++ isEmpty (sepWithComma cs)

(|>) :: CHR p s -> (s -> Maybe s) -> CHR p s
(|>) (CHR hs _ bs) g = CHR hs g bs

(<=>), (==>) :: Monoid s => [p] -> [p] -> CHR p s
hs <=>  bs = CHR hs emptyGuard bs
hs ==>  bs = hs <=> (hs ++ bs)

type Constraints c = Set.Set c

chrSolve' :: Matchable c s => [CHR c s] -> [c] -> [c]
chrSolve' chrs cs = Set.toList $ chrSolveWl chrs (Set.fromList cs) cs

chrSolve :: Matchable c s => [CHR c s] ->  Constraints c -> Constraints c
chrSolve chrs cs = chrSolveWl chrs cs (Set.toList cs)

chrSolveWl :: Matchable c s => [CHR c s] -> Constraints c -> [c] -> Constraints c
chrSolveWl chrs = foldr (solveConstraint chrs)

solveConstraint :: Matchable c s => [CHR c s] -> c -> Constraints c -> Constraints c
solveConstraint chrs c cs = foldr (applyCHR chrs [] mempty c) cs chrs

applyCHR :: Matchable c s => [CHR c s] -> [c] -> s -> c -> CHR c s -> Constraints c -> Constraints c
applyCHR chrs ms s' c (CHR hs g bs) constraints
  = foldr f constraints (matches c hs)
  where f (s, m, mhs) cs  = let sbs = map (subst s) bs                           
                                sms = map (subst s) (m:ms)
                                shs = map (subst s) mhs
                                s'' = s `mappend` s'  
                                (cs', wl) = simplify (g s'') sms sbs cs
                            in if null mhs
                               then chrSolveWl chrs cs' wl
                               else Set.fold (\d-> applyCHR chrs sms s'' d (CHR shs g sbs)) 
                                             cs 
                                             (Set.delete c cs) 

matches :: Matchable a s => a -> [a] -> [(s, a, [a])]
matches h' = rec []
  where rec _  []     = []
        rec rs (h:hs) = case match h' h of
                     Nothing -> rec (h:rs) hs
                     Just s  -> (s, h, rs ++ hs) : rec (h:rs) hs

simplify :: Matchable c s => Maybe s -> [c] -> [c] -> Constraints c -> (Constraints c, [c])
simplify Nothing   _   _    st = (st, [])
simplify (Just s)  hd  bd   st
 =  if all (`Set.member` st) hd
    then (foldr Set.insert st' sbd, new)
    else (st, [])
    where new = filter (\c -> not $ Set.member c st) sbd
          st' = foldr Set.delete st hd
          sbd = map (subst s) bd
     
-- Substitution + variable + instance Matchable
data Pattern a = Var Int
               | Val a
               deriving (Eq, Ord)

type Subst a = [(Int, a)]

instance Matchable a s => Matchable (Pattern a) (Subst a, s) where
  match (Val x) (Var i) = return ([(i, x)], mempty)
  match (Val x) (Val y) = do s <- match x y 
                             return ([], s)
  match _       _       = Nothing 

  subst (_, s)   (Val x) = Val (subst s x)
  subst (s, _) v@(Var i) = case lookup i s of
                                Nothing -> v
                                Just x  -> Val x
                                
instance Functor Pattern where
  fmap f (Val x) = Val (f x)
  fmap _ (Var i) = Var i

instance Show a => Show (Pattern a) where
  show (Val a) = show a
  show (Var i) = "v" ++ show i

--- Tests
propMatch :: Pattern Int -> Pattern Int -> Property
propMatch c h =
  case match c h of 
    Just s   -> property (c == subst s h)
    Nothing  -> property ()

instance Matchable Int ()

-- Some helper function for pp:
sepWithComma :: Show a => [a] -> String
sepWithComma = sepWith ", "

sepWith :: Show a => String -> [a] -> String
sepWith _   [] = ""
sepWith _   [x]    = show x
sepWith sep (x:xs) = show x ++ sep ++ sepWith sep xs

isEmpty :: String -> String
isEmpty "" = "true"
isEmpty s  = s
