module EH101.CHR.Solve
( CHRStore, emptyCHRStore
, chrStoreFromElems, chrStoreUnion, chrStoreUnions, chrStoreSingletonElem
, chrStoreToList, chrStoreElems
, ppCHRStore, ppCHRStore'
, SolveStep (..), SolveTrace
, ppSolveTrace
, SolveState, emptySolveState
, solveStateResetDone
, chrSolveStateDoneConstraints, chrSolveStateTrace
, chrSolve, chrSolve'
, chrSolve'' )
where
import EH101.CHR
import EH101.CHR.Constraint
import EH101.CHR.Key
import EH101.Substitutable
import EH101.VarLookup
import EH101.Base.Common
import EH101.Base.TreeTrie as TreeTrie
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List as List
import Data.Maybe
import EH.Util.Pretty as Pretty
import EH.Util.Utils
import Data.Typeable (Typeable,Typeable1)
import Data.Generics (Data)
import EH101.Base.Serialize
import Control.Monad




{-# LINE 93 "src/ehc/CHR/Solve.chs" #-}
type CHRTrie k v = TreeTrie.TreeTrie k v
type CHRTrieKey = TreeTrie.TreeTrieKey Key
type CHRLookupHow = TreeTrieLookup

chrLookupHowExact      = TTL_Exact
chrLookupHowWildAtTrie = TTL_WildInTrie
chrLookupHowWildAtKey  = TTL_WildInKey

emptyCHRTrie = TreeTrie.empty

ppCHRTrieKey :: CHRTrieKey -> PP_Doc
ppCHRTrieKey = ppTreeTrieKey

chrTrieFromListByKeyWith :: (v -> v -> v) -> [(CHRTrieKey,v)] -> CHRTrie Key v
chrTrieFromListByKeyWith = TreeTrie.fromListByKeyWith

chrTrieToListByKey :: CHRTrie Key v -> [(CHRTrieKey,v)]
chrTrieToListByKey = TreeTrie.toListByKey

chrTrieUnionWith :: (v -> v -> v) -> CHRTrie Key v -> CHRTrie Key v -> CHRTrie Key v
chrTrieUnionWith = TreeTrie.unionWith

chrTrieUnion :: CHRTrie Key v -> CHRTrie Key v -> CHRTrie Key v
chrTrieUnion = TreeTrie.union

chrTrieElems :: CHRTrie Key v -> [v]
chrTrieElems = TreeTrie.elems

chrTrieDeleteListByKey :: [CHRTrieKey] -> CHRTrie Key v -> CHRTrie Key v
chrTrieDeleteListByKey = TreeTrie.deleteListByKey

chrTrieFromListPartialExactWith :: (v -> v -> v) -> [(CHRTrieKey,v)] -> CHRTrie Key v
chrTrieFromListPartialExactWith = TreeTrie.fromListByKeyWith

chrTrieLookup' :: (CHRTrieKey -> v -> v') -> CHRLookupHow -> CHRTrieKey -> CHRTrie Key v -> ([v'],Maybe v')
chrTrieLookup' = TreeTrie.lookupPartialByKey'

chrTrieLookup :: CHRLookupHow -> CHRTrieKey -> CHRTrie Key v -> ([v],Maybe v)
chrTrieLookup = TreeTrie.lookupPartialByKey

chrToKey :: TTKeyable x => x -> CHRTrieKey
chrToKey = ttkFixate . toTTKey

chrToWorkKey :: TTKeyable x => x -> CHRTrieKey
chrToWorkKey = ttkFixate . toTTKey' (defaultTTKeyableOpts {ttkoptsVarsAsWild = False})

{-# LINE 145 "src/ehc/CHR/Solve.chs" #-}
type CHRKey = CHRTrieKey
type UsedByKey = (CHRKey,Int)

{-# LINE 150 "src/ehc/CHR/Solve.chs" #-}
-- | A CHR as stored in a CHRStore, requiring additional info for efficiency
data StoredCHR p i g s
  = StoredCHR
      { storedChr       :: !(CHR (Constraint p i) g s)   	-- the CHR
      , storedKeyedInx  :: !Int                          	-- index of constraint for which is keyed into store
      , storedKeys      :: ![Maybe CHRKey]               	-- keys of all constraints; at storedKeyedInx: Nothing
      , storedIdent     :: !UsedByKey                    	-- the identification of a CHR, used for propagation rules (see remark at begin)
      }
  deriving (Typeable, Data)

-- | The size of the simplification part of a CHR
storedSimpSz :: StoredCHR p i g s -> Int
storedSimpSz = chrSimpSz . storedChr

-- | A CHR store is a trie structure
newtype CHRStore pred info guard subst
  = CHRStore
      { chrstoreTrie    :: CHRTrie Key [StoredCHR pred info guard subst]
      }
  deriving (Typeable, Data)

mkCHRStore trie = CHRStore trie

emptyCHRStore :: CHRStore pred info guard subst
emptyCHRStore = mkCHRStore emptyCHRTrie

{-# LINE 182 "src/ehc/CHR/Solve.chs" #-}
-- | Combine lists of stored CHRs by concat, adapting their identification nr to be unique
cmbStoredCHRs :: [StoredCHR p i g s] -> [StoredCHR p i g s] -> [StoredCHR p i g s]
cmbStoredCHRs s1 s2
  = map (\s@(StoredCHR {storedIdent=(k,nr)}) -> s {storedIdent = (k,nr+l)}) s1 ++ s2
  where l = length s2

{-# LINE 190 "src/ehc/CHR/Solve.chs" #-}
instance Show (StoredCHR p i g s) where
  show _ = "StoredCHR"

ppStoredCHR :: (PP p, PP i, PP g) => StoredCHR p i g s -> PP_Doc
ppStoredCHR c@(StoredCHR {storedIdent=(idKey,idSeqNr)})
  = storedChr c
    >-< indent 2
          (ppParensCommas
            [ pp $ storedKeyedInx c
            , pp $ storedSimpSz c
            , "keys" >#< (ppBracketsCommas $ map (maybe (pp "?") ppCHRTrieKey) $ storedKeys c)
            , "ident" >#< ppParensCommas [ppCHRTrieKey idKey,pp idSeqNr]
            ])

instance (PP p, PP i, PP g) => PP (StoredCHR p i g s) where
  pp = ppStoredCHR

{-# LINE 209 "src/ehc/CHR/Solve.chs" #-}
-- | Convert from list to store
chrStoreFromElems :: (TTKeyable p) => [CHR (Constraint p i) g s] -> CHRStore p i g s
chrStoreFromElems chrs
  = mkCHRStore
    $ chrTrieFromListByKeyWith cmbStoredCHRs
        [ (k,[StoredCHR chr i ks' (concat ks,0)])
        | chr <- chrs
        , let cs = chrHead chr
              simpSz = chrSimpSz chr
              ks = map chrToKey cs
        , (c,k,i) <- zip3 cs ks [0..]
        , let (ks1,(_:ks2)) = splitAt i ks
              ks' = map Just ks1 ++ [Nothing] ++ map Just ks2
        ]

chrStoreSingletonElem :: (TTKeyable p) => CHR (Constraint p i) g s -> CHRStore p i g s
chrStoreSingletonElem x = chrStoreFromElems [x]

chrStoreUnion :: CHRStore p i g s -> CHRStore p i g s -> CHRStore p i g s
chrStoreUnion cs1 cs2 = mkCHRStore $ chrTrieUnionWith cmbStoredCHRs (chrstoreTrie cs1) (chrstoreTrie cs2)

chrStoreUnions :: [CHRStore p i g s] -> CHRStore p i g s
chrStoreUnions []  = emptyCHRStore
chrStoreUnions [s] = s
chrStoreUnions ss  = foldr1 chrStoreUnion ss

{-# LINE 237 "src/ehc/CHR/Solve.chs" #-}
chrStoreToList :: CHRStore p i g s -> [(CHRKey,[CHR (Constraint p i) g s])]
chrStoreToList cs
  = [ (k,chrs)
    | (k,e) <- chrTrieToListByKey $ chrstoreTrie cs
    , let chrs = [chr | (StoredCHR {storedChr = chr, storedKeyedInx = 0}) <- e]
    , not $ Prelude.null chrs
    ]

chrStoreElems :: CHRStore p i g s -> [CHR (Constraint p i) g s]
chrStoreElems = concatMap snd . chrStoreToList

{-# LINE 250 "src/ehc/CHR/Solve.chs" #-}
ppCHRStore :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> ppCHRTrieKey k >-< indent 2 (":" >#< ppBracketsCommasV v)) . chrStoreToList

ppCHRStore' :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore' = ppCurlysCommasBlock . map (\(k,v) -> ppCHRTrieKey k >-< indent 2 (":" >#< ppBracketsCommasV v)) . chrTrieToListByKey . chrstoreTrie

{-# LINE 267 "src/ehc/CHR/Solve.chs" #-}
type WorkTime = Int

initWorkTime :: WorkTime
initWorkTime = 0

{-# LINE 278 "src/ehc/CHR/Solve.chs" #-}
type WorkKey = CHRKey
type WorkUsedInMap = Map.Map (Set.Set CHRKey) (Set.Set UsedByKey)
type WorkTrie p i = CHRTrie Key (Work p i)

-- | A chunk of work to do when solving, a constraint + sequence nr
data Work p i
  = Work
      { workKey     :: WorkKey
      , workCnstr   :: !(Constraint p i)            -- the constraint to be reduced
      , workTime	:: WorkTime						-- the history count at which the work was added
      -- , workUsedIn  :: Set.Set CHRKey              -- marked with the propagation rules already applied to it
      }

-- | The work to be done (wlQueue), also represented as a trie (wlTrie) because efficient check on already worked on is needed.
--   A done set (wlDoneSet) remembers which CHRs (itself a list of constraints) have been solved.
--   To prevent duplicate propagation a mapping from CHRs to a map (wlUsedIn) to the CHRs it is used in is maintained.
data WorkList p i
  = WorkList
      { wlTrie      :: !(WorkTrie p i)
      , wlDoneSet   :: !(Set.Set WorkKey)                  	-- accumulative store of all keys added, set semantics, thereby avoiding double entry
      , wlQueue     :: !(AssocL WorkKey (Work p i))
      , wlScanned   :: !(AssocL WorkKey (Work p i))     	-- tried but could not solve, so retry when other succeeds
      , wlUsedIn    :: !WorkUsedInMap                    	-- which work items are used in which propagation constraints
      }

emptyWorkList = WorkList emptyCHRTrie Set.empty [] {- Set.empty -} [] Map.empty

{-# LINE 307 "src/ehc/CHR/Solve.chs" #-}
wlUsedInUnion :: WorkUsedInMap -> WorkUsedInMap -> WorkUsedInMap
wlUsedInUnion = Map.unionWith Set.union

{-# LINE 312 "src/ehc/CHR/Solve.chs" #-}
instance Show (Work p i) where
  show _ = "SolveWork"

instance (PP p,PP i) => PP (Work p i) where
  pp w = pp $ workCnstr w

ppUsedByKey :: UsedByKey -> PP_Doc
ppUsedByKey (k,i) = ppCHRTrieKey k >|< "/" >|< i

{-# LINE 323 "src/ehc/CHR/Solve.chs" #-}
mkWorkList :: (TTKeyable p) => WorkTime -> [Constraint p i] -> WorkList p i
mkWorkList wtm = flip (wlInsert wtm) emptyWorkList

wlToList :: {- (PP p, PP i) => -} WorkList p i -> [Constraint p i]
wlToList wl = map workCnstr $ chrTrieElems $ wlTrie wl

wlCnstrToIns :: (TTKeyable p) => WorkList p i -> [Constraint p i] -> AssocL WorkKey (Constraint p i)
wlCnstrToIns wl@(WorkList {wlDoneSet = ds}) inscs
  = [(chrToWorkKey c,c) | c <- inscs, let k = chrToKey c, not (k `Set.member` ds)]

wlDeleteByKeyAndInsert' :: WorkTime -> [WorkKey] -> AssocL WorkKey (Constraint p i) -> WorkList p i -> WorkList p i
wlDeleteByKeyAndInsert' wtm delkeys inskeycs wl@(WorkList {wlQueue = wlq, wlTrie = wlt, wlDoneSet = ds})
  = wl { wlQueue   = Map.toList inswork ++ [ w | w@(k,_) <- wlq, not (k `elem` delkeys) ]
       , wlTrie    = instrie `chrTrieUnion` chrTrieDeleteListByKey delkeys wlt
       , wlDoneSet = Map.keysSet inswork `Set.union` ds
       }
  where inswork = Map.fromList [ (k,Work k c wtm) | (k,c) <- inskeycs ]
        instrie = chrTrieFromListPartialExactWith const $ Map.toList inswork

wlDeleteByKeyAndInsert :: (TTKeyable p) => WorkTime -> [WorkKey] -> [Constraint p i] -> WorkList p i -> WorkList p i
wlDeleteByKeyAndInsert wtm delkeys inscs wl
  = wlDeleteByKeyAndInsert' wtm delkeys (wlCnstrToIns wl inscs) wl

wlInsert :: (TTKeyable p) => WorkTime -> [Constraint p i] -> WorkList p i -> WorkList p i
wlInsert wtm = wlDeleteByKeyAndInsert wtm []

{-# LINE 355 "src/ehc/CHR/Solve.chs" #-}
data SolveStep p i g s
  = SolveStep
      { stepChr     	:: CHR (Constraint p i) g s
      , stepSubst   	:: s
      , stepNewTodo 	:: [Constraint p i]
      , stepNewDone 	:: [Constraint p i]
      }
  | SolveStats
      { stepStats    	:: Map.Map String PP_Doc
      }
  | SolveDbg
      { stepPP      	:: PP_Doc
      }

type SolveTrace p i g s = [SolveStep p i g s]

{-# LINE 373 "src/ehc/CHR/Solve.chs" #-}
instance Show (SolveStep p i g s) where
  show _ = "SolveStep"

instance (PP p, PP i, PP g) => PP (SolveStep p i g s) where
  pp (SolveStep   step _ todo done) = "STEP" >#< (step >-< "new todo:" >#< ppBracketsCommas todo >-< "new done:" >#< ppBracketsCommas done)
  pp (SolveStats  stats           ) = "STATS"  >#< (ppAssocLV (Map.toList stats))
  pp (SolveDbg    p               ) = "DBG"  >#< p

{-# LINE 383 "src/ehc/CHR/Solve.chs" #-}
ppSolveTrace :: (PP s, PP p, PP i, PP g) => SolveTrace p i g s -> PP_Doc
ppSolveTrace tr = ppBracketsCommasV [ pp st | st <- tr ]

{-# LINE 392 "src/ehc/CHR/Solve.chs" #-}
type SolveCount a b = Map.Map a (Map.Map b Int)

scntUnion :: (Ord a,Ord b) => SolveCount a b -> SolveCount a b -> SolveCount a b
scntUnion = Map.unionWith (Map.unionWith (+))

scntInc :: (Ord a,Ord b) => a -> b -> SolveCount a b -> SolveCount a b
scntInc a b c1
  = Map.singleton a (Map.singleton b 1) `scntUnion` c1

{-# LINE 407 "src/ehc/CHR/Solve.chs" #-}
-- type SolveMatchCache p i g s = Map.Map CHRKey [((StoredCHR p i g s,([WorkKey],[Work p i])),s)]
type SolveMatchCache p i g s = Map.Map WorkKey [((StoredCHR p i g s,([WorkKey],[Work p i])),s)]

{-# LINE 416 "src/ehc/CHR/Solve.chs" #-}
type LastQueryW = Map.Map WorkKey WorkTime
type LastQuery = Map.Map CHRKey LastQueryW

emptyLastQuery :: LastQuery
emptyLastQuery = Map.empty

{-# LINE 424 "src/ehc/CHR/Solve.chs" #-}
lqSingleton :: CHRKey -> Set.Set WorkKey -> WorkTime -> LastQuery
lqSingleton ck wks wtm = Map.singleton ck $ Map.fromList [ (w,wtm) | w <- Set.toList wks ]

lqUnion :: LastQuery -> LastQuery -> LastQuery
lqUnion = Map.unionWith Map.union

lqLookupC :: CHRKey -> LastQuery -> LastQueryW
lqLookupC = Map.findWithDefault Map.empty

lqLookupW :: WorkKey -> LastQueryW -> WorkTime
lqLookupW = Map.findWithDefault initWorkTime

{-# LINE 452 "src/ehc/CHR/Solve.chs" #-}
data SolveState p i g s
  = SolveState
      { stWorkList      :: !(WorkList p i)
      , stDoneCnstrSet  :: !(Set.Set (Constraint p i))
      , stTrace         :: SolveTrace p i g s
      , stCountCnstr	:: SolveCount WorkKey String
      , stMatchCache	:: !(SolveMatchCache p i g s)
      , stHistoryCount  :: WorkTime
      , stLastQuery		:: LastQuery
      }

stDoneCnstrs :: SolveState p i g s -> [Constraint p i]
stDoneCnstrs = Set.toList . stDoneCnstrSet

emptySolveState :: SolveState p i g s
emptySolveState = SolveState emptyWorkList Set.empty [] Map.empty Map.empty initWorkTime emptyLastQuery

{-# LINE 471 "src/ehc/CHR/Solve.chs" #-}
solveStateResetDone :: SolveState p i g s -> SolveState p i g s
solveStateResetDone s = s {stDoneCnstrSet = Set.empty}

{-# LINE 476 "src/ehc/CHR/Solve.chs" #-}
chrSolveStateDoneConstraints :: SolveState p i g s -> [Constraint p i]
chrSolveStateDoneConstraints = stDoneCnstrs

chrSolveStateTrace :: SolveState p i g s -> SolveTrace p i g s
chrSolveStateTrace = stTrace

{-# LINE 488 "src/ehc/CHR/Solve.chs" #-}
chrSolve
  :: ( CHRMatchable env p s, CHRCheckable env g s
     -- , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , VarLookupCmb s s
     , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     )
     => env
     -> CHRStore p i g s
     -> [Constraint p i]
     -> [Constraint p i]
chrSolve env chrStore cnstrs
  = work ++ done
  where (work,done,_) = chrSolve' env chrStore cnstrs

chrSolve'
  :: ( CHRMatchable env p s, CHRCheckable env g s
     -- , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , VarLookupCmb s s
     , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     )
     => env
     -> CHRStore p i g s
     -> [Constraint p i]
     -> ([Constraint p i],[Constraint p i],SolveTrace p i g s)
chrSolve' env chrStore cnstrs
  = (wlToList (stWorkList finalState), stDoneCnstrs finalState, stTrace finalState)
  where finalState = chrSolve'' env chrStore cnstrs emptySolveState

{-# LINE 530 "src/ehc/CHR/Solve.chs" #-}
chrSolve''
  :: forall env p i g s .
     ( CHRMatchable env p s, CHRCheckable env g s
     -- , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , VarLookupCmb s s
     , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     )
     => env
     -> CHRStore p i g s
     -> [Constraint p i]
     -> SolveState p i g s
     -> SolveState p i g s
chrSolve'' env chrStore cnstrs prevState
  = postState {stMatchCache = Map.empty}
  where postState
          = st
          where st = iter $ initState prevState
        iter st@(SolveState {stWorkList = wl@(WorkList {wlQueue = (workHd@(workHdKey,_) : workTl)})})
          = case matches of
              (_:_)
                -> expandMatch stmatch matches
                where -- expandMatch :: SolveState p i g s -> [((StoredCHR p i g s, ([WorkKey], [Work p i])), s)] -> SolveState p i g s
                      expandMatch st@(SolveState {stWorkList = wl, stHistoryCount = histCount})
                                  ( ( ( schr@(StoredCHR {storedIdent = chrId, storedChr = chr@(CHR {chrBody = b, chrSimpSz = simpSz})})
                                      , (keys,works)
                                      )
                                    , subst
                                    ) : tlMatch
                                  )
                        = expandMatch st' tlMatchY
                        where (tlMatchY,tlMatchN) = partition (\(r@(_,(ks,_)),_) -> not (any (`elem` keysSimp) ks || slvIsUsedByPropPart (wlUsedIn wl') r)) tlMatch
                              (keysSimp,keysProp) = splitAt simpSz keys
                              usedIn              = Map.singleton (Set.fromList keysProp) (Set.singleton chrId)
                              (bTodo,bDone)       = splitDone $ map (varUpd subst) b
                              bTodo'              = wlCnstrToIns wl bTodo
                              wl' = wlDeleteByKeyAndInsert' histCount keysSimp bTodo'
                                    $ wl { wlUsedIn  = usedIn `wlUsedInUnion` wlUsedIn wl
                                         , wlScanned = []
                                         , wlQueue   = wlQueue wl ++ wlScanned wl
                                         }
                              st' = st { stWorkList       = wl'
                                       , stDoneCnstrSet   = Set.unions [Set.fromList bDone, Set.fromList $ map workCnstr $ take simpSz works, stDoneCnstrSet st]
                                       , stMatchCache     = if List.null bTodo' then stMatchCache st else Map.empty
                                       , stHistoryCount   = histCount + 1
                                       }
                      expandMatch st _
                        = iter st

              _ -> iter st'
                where wl' = wl { wlScanned = workHd : wlScanned wl, wlQueue = workTl }
                      st' = stmatch { stWorkList = wl', stTrace = SolveDbg (ppdbg) : {- -} stTrace stmatch }
          where (matches,lastQuery,ppdbg,stats) = workMatches st
                stmatch =
                            (st { stCountCnstr = scntInc workHdKey "workMatched" $ stCountCnstr st
                                , stMatchCache = Map.insert workHdKey [] (stMatchCache st)
                                , stLastQuery  = lastQuery
                                })
        iter st
          = st
        mkStats  stats new    = stats `Map.union` Map.fromList (assocLMapKey showPP new)
        addStats _     _   st = st
        workMatches st@(SolveState {stWorkList = WorkList {wlQueue = (workHd@(workHdKey,Work {workTime = workHdTm}) : _), wlTrie = wlTrie, wlUsedIn = wlUsedIn}, stHistoryCount = histCount, stLastQuery = lastQuery})
          | isJust mbInCache  = ( fromJust mbInCache
                                , lastQuery
                                , Pretty.empty, mkStats Map.empty [("cache sz",pp (Map.size (stMatchCache st)))]
                                )
          | otherwise         = ( r5
                                , foldr lqUnion lastQuery [ lqSingleton ck wks histCount | (_,(_,(ck,wks))) <- r23 ]
                                , Pretty.empty
                                , Map.empty
                                )
          where -- cache result, if present use that, otherwise the below computation
                mbInCache = Map.lookup workHdKey (stMatchCache st)

                -- results, stepwise computed for later reference in debugging output
                -- basic search result
                r2 :: [StoredCHR p i g s]										-- CHRs matching workHdKey
                r2  = concat													-- flatten
                		$ TreeTrie.lookupResultToList									-- convert to list
                		$ chrTrieLookup chrLookupHowWildAtTrie workHdKey		-- lookup the store, allowing too many results
                		$ chrstoreTrie chrStore

                -- lookup further info in wlTrie, in particular to find out what has been done already
                r23 :: [( StoredCHR p i g s										-- the CHR
                        , ( [( [(CHRKey, Work p i)]								-- for each CHR the list of constraints, all possible work matches
                             , [(CHRKey, Work p i)]
                             )]
                          , (CHRKey, Set.Set CHRKey)
                        ) )]
                r23 = map (\c -> (c, slvCandidate workHdKey lastQuery wlTrie c)) r2

                -- possible matches
                r3, r4
                    :: [( StoredCHR p i g s										-- the matched CHR
                        , ( [CHRKey]											-- possible matching constraints (matching with the CHR constraints), as Keys, as Works
                          , [Work p i]
                        ) )]
                r3  = concatMap (\(c,cands) -> zip (repeat c) (map unzip $ slvCombine cands)) $ r23

                -- same, but now restricted to not used earlier as indicated by the worklist
                r4  = filter (not . slvIsUsedByPropPart wlUsedIn) r3

                -- finally, the 'real' match of the 'real' constraint, yielding (by tupling) substitutions instantiating the found trie matches
                r5  :: [( ( StoredCHR p i g s
                          , ( [CHRKey]
                            , [Work p i]
                          ) )
                        , s
                        )]
                r5  = mapMaybe (\r@(chr,kw@(_,works)) -> fmap (\s -> (r,s)) $ slvMatch env chr (map workCnstr works)) r4
        initState st = st { stWorkList = wlInsert (stHistoryCount st) wlnew $ stWorkList st, stDoneCnstrSet = Set.unions [Set.fromList done, stDoneCnstrSet st] }
                     where (wlnew,done) = splitDone cnstrs
        splitDone  = partition cnstrRequiresSolve

{-# LINE 723 "src/ehc/CHR/Solve.chs" #-}
-- | Extract candidates matching a CHRKey.
--   Return a list of CHR matches,
--     each match expressed as the list of constraints (in the form of Work + Key) found in the workList wlTrie, thus giving all combis with constraints as part of a CHR,
--     partititioned on before or after last query time (to avoid work duplication later)
slvCandidate
  :: CHRKey
     -> LastQuery
     -> WorkTrie p i
     -> StoredCHR p i g s
     -> ( [( [(CHRKey, Work p i)]
           , [(CHRKey, Work p i)]
           )]
        , (CHRKey, Set.Set CHRKey)
        )
slvCandidate workHdKey lastQuery wlTrie (StoredCHR {storedIdent = (ck,_), storedKeys = ks, storedChr = chr})
  = ( map (maybe (lkup chrLookupHowExact workHdKey) (lkup chrLookupHowWildAtKey)) ks
    , ( ck
      , Set.fromList $ map (maybe workHdKey id) ks
    ) )
  where lkup how k = partition (\(_,w) -> workTime w < lastQueryTm) $ map (\w -> (workKey w,w)) $ TreeTrie.lookupResultToList $ chrTrieLookup how k wlTrie
                   where lastQueryTm = lqLookupW k $ lqLookupC ck lastQuery

{-# LINE 754 "src/ehc/CHR/Solve.chs" #-}
slvCombine :: Eq k => ([([Assoc k v], [Assoc k v])], t) -> [AssocL k v]
slvCombine ([],_) = []
slvCombine ((lh:lt),_)
  = concatMap combineToDistinguishedElts l2
  where l2 = g2 [] lh lt
           where g2 ll l []           = [mk ll l []]
                 g2 ll l lr@(lrh:lrt) = mk ll l lr : g2 (ll ++ [l]) lrh lrt
                 mk ll (bef,aft) lr   = map fst ll ++ [aft] ++ map cmb lr
                                      where cmb (a,b) = a++b

{-# LINE 766 "src/ehc/CHR/Solve.chs" #-}
-- | Check whether the CHR propagation part of a match already has been used (i.e. propagated) earlier,
--   this to avoid duplicate propagation.
slvIsUsedByPropPart
  :: Ord k
     => Map.Map (Set.Set k) (Set.Set UsedByKey)
     -> (StoredCHR p i g s, ([k], t))
     -> Bool
slvIsUsedByPropPart wlUsedIn (chr,(keys,_))
  = fnd $ drop (storedSimpSz chr) keys
  where fnd k = maybe False (storedIdent chr `Set.member`) $ Map.lookup (Set.fromList k) wlUsedIn

{-# LINE 779 "src/ehc/CHR/Solve.chs" #-}
-- | Match the stored CHR with a set of possible constraints, giving a substitution on success
slvMatch
  :: ( CHREmptySubstitution s
     , CHRMatchable env p s
     , CHRCheckable env g s
     , VarLookupCmb s s
     )
     => env -> StoredCHR p i g s -> [Constraint p i] -> Maybe s
slvMatch env chr cnstrs
  = foldl cmb (Just chrEmptySubst) $ matches chr cnstrs ++ checks chr
  where matches (StoredCHR {storedChr = CHR {chrHead = hc}}) cnstrs
          = zipWith mt hc cnstrs
          where mt cFr cTo subst = chrMatchTo env subst cFr cTo
        checks (StoredCHR {storedChr = CHR {chrGuard = gd}})
          = map chk gd
          where chk g subst = chrCheck env subst g
        cmb (Just s) next = fmap (|+> s) $ next s
        cmb _        _    = Nothing

{-# LINE 804 "src/ehc/CHR/Solve.chs" #-}
instance (Serialize p, Serialize i, Serialize g, Serialize s) => Serialize (CHRStore p i g s) where
  sput (CHRStore a) = sput a
  sget = liftM CHRStore sget

instance (Serialize p, Serialize i, Serialize g, Serialize s) => Serialize (StoredCHR p i g s) where
  sput (StoredCHR a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 StoredCHR sget sget sget sget

