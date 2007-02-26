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

%%[9 import(qualified Data.Set as Set,qualified Data.Map as Map,Data.List as List,Data.Maybe)
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

-- For debug
%%[9 import(EH.Util.Utils)
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
      , storedKeys      :: [Maybe CHRKey]               -- keys of all constraints; at storedKeyedInx: Nothing
      , storedIdent     :: CHRKey                       -- the identification of a CHR, used for propagation rules (see remark at begin)
      }

storedSimpSz :: StoredCHR p i g s -> Int
storedSimpSz = chrSimpSz . storedChr

data CHRStore pred info guard subst
  = CHRStore
      { chrstoreTrie    :: Trie.Trie Key [StoredCHR pred info guard subst]
      }

mkCHRStore trie = CHRStore trie

emptyCHRStore :: CHRStore pred info guard subst
emptyCHRStore = mkCHRStore Trie.empty
%%]

%%[9
instance Show (StoredCHR p i g s) where
  show _ = "StoredCHR"

instance (PP p, PP i, PP g) => PP (StoredCHR p i g s) where
  pp c = storedChr c
         >-< indent 2
               (ppParensCommas
                 [ pp $ storedKeyedInx c
                 , pp $ storedSimpSz c
                 , "keys" >#< (ppBracketsCommas $ map (maybe (pp "?") ppTrieKey) $ storedKeys c)
                 , "ident" >#< (ppTrieKey $ storedIdent c)
                 ])
%%]

%%[9 export(chrStoreFromElems,chrStoreUnion,chrStoreUnions,chrStoreSingletonElem)
chrStoreFromElems :: Keyable p => [CHR (Constraint p i) g s] -> CHRStore p i g s
chrStoreFromElems chrs
  = mkCHRStore
    $ Trie.fromListByKeyWith (++)
        [ (k,[StoredCHR chr i ks' (concat ks)])
        | chr <- chrs
        , let cs = chrHead chr
              simpSz = chrSimpSz chr
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

%%[9 export(ppCHRStore)
ppCHRStore :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> ppBracketsCommas k >-< indent 2 (":" >#< ppBracketsCommasV v)) . chrStoreToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver worklist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type WorkKey = CHRKey
type WorkUsedInMap = Map.Map CHRKey (Set.Set CHRKey)

data Work p i
  = Work
      { workCnstr   :: Constraint p i           -- the constraint to be reduced
      -- , workUsedIn  :: Set.Set WorkKey          -- marked with the propagation rules already applied to it
      }

data WorkList p i
  = WorkList
      { wlTrie      :: Trie.Trie Key (Work p i)
      , wlDoneSet   :: Set.Set WorkKey					-- accumulative store of all keys added, set semantics, thereby avoiding double entry
      , wlQueue     :: [CHRKey]
      -- , wlQueueSet  :: Set.Set CHRKey					-- for fast membership test
      , wlScanned   :: [CHRKey]							-- tried but could not solve, so retry when other succeeds
      , wlUsedIn	:: WorkUsedInMap					-- which work items are used in which propagation constraints
      }

emptyWorkList = WorkList Trie.empty Set.empty [] {- Set.empty -} [] Map.empty
%%]

%%[9
wlUsedInUnion :: WorkUsedInMap -> WorkUsedInMap -> WorkUsedInMap
wlUsedInUnion = Map.unionWith Set.union
%%]

%%[9
instance Show (Work p i) where
  show _ = "SolveWork"

instance (PP p,PP i) => PP (Work p i) where
  pp w = pp $ workCnstr w
%%]

%%[9
mkWorkList :: Keyable p => [Constraint p i] -> WorkList p i
mkWorkList = flip wlInsert emptyWorkList

wlToList :: {- (PP p, PP i) => -} WorkList p i -> [Constraint p i]
wlToList wl@(WorkList {wlQueue = q, wlScanned = s, wlTrie = t})
  = map workCnstr $ Trie.elems t
  -- = mapMaybe (\k -> fmap workCnstr $ lookupByKey k $ t) $ q ++ s

wlInsert :: Keyable p => [Constraint p i] -> WorkList p i -> WorkList p i
wlInsert cs wl@(WorkList {wlQueue = q, wlTrie = t, wlDoneSet = ds})
  = wl {wlDoneSet = Map.keysSet work `Set.union` ds, wlTrie = trie `Trie.union` t, wlQueue = Map.keys work ++ q }
  where work = Map.fromList [ (k,Work c) | c <- cs, let k = toKey c, not (k `Set.member` ds) ]
        trie = Trie.fromListPartialByKeyWith TrieLookup_Normal const $ Map.toList work

wlDeleteByKey :: [[Trie.TrieKey Key]] -> WorkList p i -> WorkList p i
wlDeleteByKey keys wl@(WorkList {wlQueue = wlq, wlTrie = wlt})
  = wl { wlQueue = wlq \\ keys
       , wlTrie = Trie.deleteListByKey keys wlt
       }
%%]
wlInsert :: Keyable p => [Constraint p i] -> WorkList p i -> WorkList p i
wlInsert cs wl@(WorkList {wlQueue = q, wlTrie = t, wlQueueSet = qs})
  = wl {wlQueueSet = Map.keysSet work `Set.union` qs, wlTrie = trie `Trie.union` t, wlQueue = Map.keys work ++ q }
  where work = Map.fromList [ (toKey c,Work c) | c <- cs, let k = toKey c, not (k `Set.member` qs) ]
        trie = Trie.fromListPartialByKeyWith TrieLookup_Normal const $ Map.toList work

wlDeleteByKey :: [[Trie.TrieKey Key]] -> WorkList p i -> WorkList p i
wlDeleteByKey keys wl@(WorkList {wlQueue = wlq, wlTrie = wlt, wlQueueSet = wlqs})
  = wl { wlQueue = wlq \\ keys, wlQueueSet = wlqs `Set.difference` Set.fromList keys
       , wlTrie = Trie.deleteListByKey keys wlt
       }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(SolveStep(..),SolveState,SolveTrace)
data SolveStep p i g s
  = SolveStep
      { stepChr     :: CHR (Constraint p i) g s
      , stepSubst   :: s
      }
  | SolveDbg
      { stepPP      :: PP_Doc
      }

type SolveTrace p i g s = [SolveStep p i g s]

data SolveState p i g s
  = SolveState
      { stWorkList      :: WorkList p i
      , stDoneCnstrs    :: [Constraint p i]
      , stTrace         :: SolveTrace p i g s
      }
%%]

%%[9
instance Show (SolveStep p i g s) where
  show _ = "SolveStep"

instance (PP p, PP i, PP g) => PP (SolveStep p i g s) where
  pp (SolveStep step _) = "STEP" >#< step
  pp (SolveDbg  p     ) = "DBG"  >#< p
%%]

%%[9 export(chrSolveStateDoneConstraints,chrSolveStateTrace)
chrSolveStateDoneConstraints :: SolveState p i g s -> [Constraint p i]
chrSolveStateDoneConstraints = stDoneCnstrs

chrSolveStateTrace :: SolveState p i g s -> SolveTrace p i g s
chrSolveStateTrace = stTrace
%%]

%%[9 export(ppSolveTrace)
ppSolveTrace :: (PP s, PP p, PP i, PP g) => SolveTrace p i g s -> PP_Doc
ppSolveTrace tr = ppBracketsCommasV [ pp st | st <- tr ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(chrSolve,chrSolve',chrSolve'')
chrSolve
  :: ( CHRMatchable env p s, CHRCheckable g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> [Constraint p i]
chrSolve env chrStore cnstrs
  = work ++ done
  where (work,done,_) = chrSolve' env chrStore cnstrs

chrSolve'
  :: ( CHRMatchable env p s, CHRCheckable g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> ([Constraint p i],[Constraint p i],SolveTrace p i g s)
chrSolve' env chrStore cnstrs
  = (wlToList (stWorkList finalState), stDoneCnstrs finalState, stTrace finalState)
  where finalState = chrSolve'' env chrStore cnstrs

chrSolve''
  :: ( CHRMatchable env p s, CHRCheckable g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> SolveState p i g s
chrSolve'' env chrStore cnstrs
  = iter initState
  where iter st@(SolveState {stWorkList = wl@(WorkList {wlQueue = wlQueue@(workHd:workTl), wlScanned = wlScanned})})
          = case firstMatch st of
              (Just (schr@(StoredCHR {storedIdent = chrId, storedChr = chr@(CHR {chrBody = b, chrSimpSz = simpSz})}),(keys,works),subst),ppdbg)
                -> iter {- $ trp "YY" ("chr" >#< schr >-< ppwork) $ -} st'
                where (keysSimp,keysProp) = splitAt simpSz keys
                      usedIn = Map.fromListWith Set.union $ zip keysProp (repeat $ Set.singleton chrId)
                      (bTodo,bDone) = splitDone $ map (chrAppSubst subst) b
                      wl' = wlInsert bTodo
                            $ wlDeleteByKey keysSimp
                            $ wl { wlUsedIn = usedIn `wlUsedInUnion` wlUsedIn wl
                                 , wlScanned = [], wlQueue = wlQueue ++ wlScanned
                                 }
                      st' = st { stWorkList = wl'
                               , stTrace = SolveStep (subst `chrAppSubst` chr) subst : {- SolveDbg (ppwork >-< ppdbg) : -} stTrace st
                               , stDoneCnstrs = bDone ++ (map workCnstr $ take simpSz works) ++ stDoneCnstrs st
                               }
              (_,ppdbg)
                -> iter {- $ trp "XX" ppwork $ -} st'
                where wl' = wl { wlScanned = workHd : wlScanned, wlQueue = workTl
                               }
                      st' = st { stWorkList = wl'
                               , stTrace = {- SolveDbg (ppwork >-< ppdbg) : -} stTrace st
                               }
          where ppwork = "workkey" >#< ppTrieKey workHd >#< ":" >#< (ppBracketsCommas (map ppTrieKey workTl) >-< ppBracketsCommas (map ppTrieKey wlScanned))
                         >-< "worktrie" >#< wlTrie wl
                         >-< "usedin" >#< (ppBracketsCommasV $ map (\(k,s) -> ppTrieKey k >#< ppBracketsCommas (map ppTrieKey $ Set.toList s)) $ Map.toList $ wlUsedIn wl)
        iter st
          = st
        firstMatch st@(SolveState {stWorkList = WorkList {wlQueue = (workHd:_), wlTrie = wlTrie, wlUsedIn = wlUsedIn}})
          = (r4, pp2 >-< pp3)
          where -- results
                r2 = concat $ lookupResultToList $ lookupPartialByKey TrieLookup_Partial workHd $ chrstoreTrie chrStore
                r3 = concatMap (\c -> zip (repeat c) (map unzip $ combine' $ candidate c)) $ r2
                r4 = foldr first Nothing $ filter (not . isUsedByPropPart) $ r3
                pp2 = "lookups" >#< ppBracketsCommasV r2
                pp3 = "candidates" >#< (ppBracketsCommasV $ map (\(chr,(ks,ws)) -> "chr" >#< chr >-< "keys" >#< ppBracketsCommas (map ppTrieKey ks) >-< "works" >#< ppBracketsCommasV ws) $ r3)
                -- util functions
                candidate (StoredCHR {storedIdent = chrId, storedKeys = ks, storedChr = chr@(CHR {chrSimpSz = simpSz})})
                  = cand lkup sks ++ cand (\h k -> {- notPropUsed $ -} lkup h k) pks
                  where (sks,pks)   = splitAt simpSz ks
                        lkup how k  = lookupResultToList $ lookupPartialByKey' (,) how k wlTrie
                        -- notPropUsed = filter (\(k,_) -> maybe True (not . (chrId `Set.member`)) $ Map.lookup k wlUsedIn)
                        cand lkup   = map (maybe (lkup TrieLookup_Normal workHd) (lkup TrieLookup_StopAtPartial))
                combine' [] = []
                combine' [[]] = []
                combine' [x] = [x]
                combine' (l:ls) = combine l $ combine' ls
                                where combine l ls = concatMap (\e@(k,_) -> mapMaybe (\ll -> maybe (Just (e:ll)) (const Nothing) $ List.lookup k ll) ls) l
                match chr cnstrs
                  = foldl cmb (Just chrEmptySubst) $ matches chr cnstrs ++ checks chr
                  where matches (StoredCHR {storedChr = CHR {chrHead = hc}}) cnstrs
                          = zipWith mt hc cnstrs
                          where mt cFr cTo subst = chrMatchTo env (subst `chrAppSubst` cFr) cTo
                        checks (StoredCHR {storedChr = CHR {chrGuard = gd}})
                          = map chk gd
                          where chk g subst = chrCheck (subst `chrAppSubst` g)
                        cmb (Just s) next = fmap (`chrAppSubst` s) $ next s
                        cmb _        _    = Nothing
                isUsedByPropPart (chr,(keys,_))
                  = all fnd $ drop (storedSimpSz chr) keys
                  where fnd k = maybe False (storedIdent chr `Set.member`) $ Map.lookup k wlUsedIn
                first (chr,kw@(_,works)) cont
                  = case match chr (map workCnstr works) of
                      r@(Just s) -> Just (chr,kw,s)
                      _          -> cont
        initState  = SolveState (mkWorkList wl) done []
                   where (wl,done) = splitDone cnstrs
        splitDone  = partition (\x -> cnstrRequiresSolve x)
%%]
                r4 = foldr first Nothing $ r3
                -> iter $ trp "PICK" (schr >-< "HD:" >#< ppTrieKey workHd >-< "TL:" >#< ppBracketsCommas (map ppTrieKey workTl) >-< "KEYS:" >#< ppBracketsCommas (map ppTrieKey keys) >-< "WORKS:" >#< ppBracketsCommasV works) $ st'
              _ -> iter $ trp "NOT MATCHED" (ppTrieKey workHd) $ st'
                  = (\v -> trp "ZZ" ("workHd" >#< ppBracketsCommas workHd >#< "ks" >#< (ppBracketsCommas $ map (fmap ppBracketsCommas) ks) >#< (ppBracketsCommasV $ map (map (\(a,b) -> ppParensCommas [ppBracketsCommas a,pp b])) $ v)) v)
            -- $ filter (not . isUsedByPropPart)
            -- $ (\v -> trp "XX" (ppBracketsCommasV v) v)

                isUsedByPropPart (chr,(keys,_))
                  = any fnd $ drop (storedSimpSz chr) keys
                  where fnd k = maybe False (storedIdent chr `Set.member`) $ Map.lookup k wlUsedIn

