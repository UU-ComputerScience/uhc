%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

Assumptions (to be documented further)
- The key [Trie.TrieKey Key] used to lookup a constraint in a CHR should be distinguishing enough to be used for the prevention
  of the application of a propagation rule for a 2nd time.

%%[(9 hmtyinfer || hmtyast) module {%{EH}CHR.Solve} import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Key})
%%]

%%[(9 hmtyinfer || hmtyast) import({%{EH}Base.Common},{%{EH}Base.Trie} as Trie)
%%]

%%[(9 hmtyinfer || hmtyast) import(qualified Data.Set as Set,qualified Data.Map as Map,Data.List as List,Data.Maybe)
%%]

%%[(9 hmtyinfer || hmtyast) import(EH.Util.Pretty as Pretty)
%%]

%%[(2020 hmtyinfer || hmtyast) import({%{EH}Base.Binary})
%%]

%%[(9999 hmtyinfer || hmtyast) import({%{EH}Base.ForceEval})
%%]

-- For debug
%%[(9 hmtyinfer || hmtyast) import(EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR store, with fast search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type CHRKey = [Trie.TrieKey Key]
type UsedByKey = (CHRKey,Int)
%%]

%%[(9 hmtyinfer || hmtyast) export(CHRStore,emptyCHRStore)
data StoredCHR p i g s
  = StoredCHR
      { storedChr       :: !(CHR (Constraint p i) g s)   	-- the CHR
      , storedKeyedInx  :: !Int                          	-- index of constraint for which is keyed into store
      , storedKeys      :: ![Maybe CHRKey]               	-- keys of all constraints; at storedKeyedInx: Nothing
      , storedIdent     :: !UsedByKey                    	-- the identification of a CHR, used for propagation rules (see remark at begin)
      }
%%[[20
  -- deriving (Typeable, Data)
%%]]

storedSimpSz :: StoredCHR p i g s -> Int
storedSimpSz = chrSimpSz . storedChr

data CHRStore pred info guard subst
  = CHRStore
      { chrstoreTrie    :: Trie.Trie Key [StoredCHR pred info guard subst]
      }
%%[[20
  -- deriving (Typeable, Data)
%%]]

mkCHRStore trie = CHRStore trie

emptyCHRStore :: CHRStore pred info guard subst
emptyCHRStore = mkCHRStore Trie.empty
%%]

%%[(9 hmtyinfer || hmtyast)
cmbStoredCHRs :: [StoredCHR p i g s] -> [StoredCHR p i g s] -> [StoredCHR p i g s]
cmbStoredCHRs s1 s2
  = map (\s@(StoredCHR {storedIdent=(k,nr)}) -> s {storedIdent = (k,nr+l)}) s1 ++ s2
  where l = length s2
%%]

%%[(9 hmtyinfer || hmtyast)
instance Show (StoredCHR p i g s) where
  show _ = "StoredCHR"

ppStoredCHR :: (PP p, PP i, PP g) => StoredCHR p i g s -> PP_Doc
ppStoredCHR c@(StoredCHR {storedIdent=(idKey,idSeqNr)})
  = storedChr c
    >-< indent 2
          (ppParensCommas
            [ pp $ storedKeyedInx c
            , pp $ storedSimpSz c
            , "keys" >#< (ppBracketsCommas $ map (maybe (pp "?") ppTrieKey) $ storedKeys c)
            , "ident" >#< ppParensCommas [ppTrieKey idKey,pp idSeqNr]
            ])

instance (PP p, PP i, PP g) => PP (StoredCHR p i g s) where
  pp = ppStoredCHR
%%]

%%[(9 hmtyinfer || hmtyast) export(chrStoreFromElems,chrStoreUnion,chrStoreUnions,chrStoreSingletonElem)
chrStoreFromElems :: Keyable p => [CHR (Constraint p i) g s] -> CHRStore p i g s
chrStoreFromElems chrs
  = mkCHRStore
    $ Trie.fromListByKeyWith cmbStoredCHRs
        [ (k,[StoredCHR chr i ks' (concat ks,0)])
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
chrStoreUnion cs1 cs2 = mkCHRStore $ Trie.unionWith cmbStoredCHRs (chrstoreTrie cs1) (chrstoreTrie cs2)

chrStoreUnions :: [CHRStore p i g s] -> CHRStore p i g s
chrStoreUnions []  = emptyCHRStore
chrStoreUnions [s] = s
chrStoreUnions ss  = foldr1 chrStoreUnion ss
%%]

%%[(9 hmtyinfer || hmtyast) export(chrStoreToList,chrStoreElems)
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

%%[(9 hmtyinfer || hmtyast) export(ppCHRStore,ppCHRStore')
ppCHRStore :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> ppTrieKey k >-< indent 2 (":" >#< ppBracketsCommasV v)) . chrStoreToList

ppCHRStore' :: (PP p,PP g,PP i) => CHRStore p i g s -> PP_Doc
ppCHRStore' = ppCurlysCommasBlock . map (\(k,v) -> ppTrieKey k >-< indent 2 (":" >#< ppBracketsCommasV v)) . Trie.toListByKey . chrstoreTrie
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WorkTime, the time/history counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type WorkTime = Int

initWorkTime :: WorkTime
initWorkTime = 0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver worklist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type WorkKey = CHRKey
-- type WorkUsedInMap = Map.Map CHRKey (Set.Set UsedByKey)
type WorkUsedInMap = Map.Map (Set.Set CHRKey) (Set.Set UsedByKey)

data Work p i
  = Work
      { workCnstr   :: !(Constraint p i)            -- the constraint to be reduced
      , workTime	:: WorkTime						-- the history count at which the work was added
      -- , workUsedIn  :: Set.Set CHRKey              -- marked with the propagation rules already applied to it
      }

data WorkList p i
  = WorkList
      { wlTrie      :: !(Trie.Trie Key (Work p i))
      , wlDoneSet   :: !(Set.Set WorkKey)                  	-- accumulative store of all keys added, set semantics, thereby avoiding double entry
      , wlQueue     :: !(AssocL WorkKey (Work p i))
      , wlScanned   :: !(AssocL WorkKey (Work p i))     	-- tried but could not solve, so retry when other succeeds
      , wlUsedIn    :: !WorkUsedInMap                    	-- which work items are used in which propagation constraints
      }

emptyWorkList = WorkList Trie.empty Set.empty [] {- Set.empty -} [] Map.empty
%%]

%%[(9 hmtyinfer || hmtyast)
wlUsedInUnion :: WorkUsedInMap -> WorkUsedInMap -> WorkUsedInMap
wlUsedInUnion = Map.unionWith Set.union
%%]

%%[(9 hmtyinfer || hmtyast)
instance Show (Work p i) where
  show _ = "SolveWork"

instance (PP p,PP i) => PP (Work p i) where
  pp w = pp $ workCnstr w

ppUsedByKey :: UsedByKey -> PP_Doc
ppUsedByKey (k,i) = ppTrieKey k >|< "/" >|< i
%%]

%%[(9 hmtyinfer || hmtyast)
mkWorkList :: Keyable p => WorkTime -> [Constraint p i] -> WorkList p i
mkWorkList wtm = flip (wlInsert wtm) emptyWorkList

wlToList :: {- (PP p, PP i) => -} WorkList p i -> [Constraint p i]
wlToList wl = map workCnstr $ Trie.elems $ wlTrie wl

wlCnstrToIns :: Keyable p => WorkList p i -> [Constraint p i] -> AssocL WorkKey (Constraint p i)
wlCnstrToIns wl@(WorkList {wlDoneSet = ds}) inscs
  = [(toKey c,c) | c <- inscs, let k = toKey c, not (k `Set.member` ds)]

wlDeleteByKeyAndInsert' :: WorkTime -> [WorkKey] -> AssocL WorkKey (Constraint p i) -> WorkList p i -> WorkList p i
wlDeleteByKeyAndInsert' wtm delkeys inskeycs wl@(WorkList {wlQueue = wlq, wlTrie = wlt, wlDoneSet = ds})
  = wl { wlQueue   = Map.toList inswork ++ [ w | w@(k,_) <- wlq, not (k `elem` delkeys) ]
       , wlTrie    = instrie `Trie.union` Trie.deleteListByKey delkeys wlt
       , wlDoneSet = Map.keysSet inswork `Set.union` ds
       }
  where inswork = Map.fromList [ (k,Work c wtm) | (k,c) <- inskeycs ]
        instrie = Trie.fromListPartialByKeyWith TrieLookup_Normal const $ Map.toList inswork

wlDeleteByKeyAndInsert :: Keyable p => WorkTime -> [WorkKey] -> [Constraint p i] -> WorkList p i -> WorkList p i
wlDeleteByKeyAndInsert wtm delkeys inscs wl
  = wlDeleteByKeyAndInsert' wtm delkeys (wlCnstrToIns wl inscs) wl

wlInsert :: Keyable p => WorkTime -> [Constraint p i] -> WorkList p i -> WorkList p i
wlInsert wtm = wlDeleteByKeyAndInsert wtm []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver trace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(SolveStep(..),SolveTrace)
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
%%]

%%[(9 hmtyinfer || hmtyast)
instance Show (SolveStep p i g s) where
  show _ = "SolveStep"

instance (PP p, PP i, PP g) => PP (SolveStep p i g s) where
  pp (SolveStep   step _ todo done) = "STEP" >#< (step >-< "new todo:" >#< ppBracketsCommas todo >-< "new done:" >#< ppBracketsCommas done)
  pp (SolveStats  stats           ) = "STATS"  >#< (ppAssocLV (Map.toList stats))
  pp (SolveDbg    p               ) = "DBG"  >#< p
%%]

%%[(9 hmtyinfer || hmtyast) export(ppSolveTrace)
ppSolveTrace :: (PP s, PP p, PP i, PP g) => SolveTrace p i g s -> PP_Doc
ppSolveTrace tr = ppBracketsCommasV [ pp st | st <- tr ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver counting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type SolveCount a b = Map.Map a (Map.Map b Int)

scntUnion :: (Ord a,Ord b) => SolveCount a b -> SolveCount a b -> SolveCount a b
scntUnion = Map.unionWith (Map.unionWith (+))

scntInc :: (Ord a,Ord b) => a -> b -> SolveCount a b -> SolveCount a b
scntInc a b c1
  = Map.singleton a (Map.singleton b 1) `scntUnion` c1
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cache for maintaining which WorkKey has already had a match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
-- type SolveMatchCache p i g s = Map.Map CHRKey [((StoredCHR p i g s,([WorkKey],[Work p i])),s)]
type SolveMatchCache p i g s = Map.Map WorkKey [((StoredCHR p i g s,([WorkKey],[Work p i])),s)]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WorkTime of last search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type LastQueryW = Map.Map WorkKey WorkTime
type LastQuery = Map.Map CHRKey LastQueryW

emptyLastQuery :: LastQuery
emptyLastQuery = Map.empty
%%]

%%[(9 hmtyinfer || hmtyast)
lqSingleton :: CHRKey -> Set.Set WorkKey -> WorkTime -> LastQuery
lqSingleton ck wks wtm = Map.singleton ck $ Map.fromList [ (w,wtm) | w <- Set.toList wks ]

lqUnion :: LastQuery -> LastQuery -> LastQuery
lqUnion = Map.unionWith Map.union

lqLookupC :: CHRKey -> LastQuery -> LastQueryW
lqLookupC = Map.findWithDefault Map.empty

lqLookupW :: WorkKey -> LastQueryW -> WorkTime
lqLookupW = Map.findWithDefault initWorkTime
%%]
lqAdd :: CHRKey -> WorkKey -> WorkTime -> LastQuery -> LastQuery
lqAdd ck wk wtm lq = Map.insertWithKey (\_ _ m -> Map.insert wk wtm m) ck (Map.singleton wk wtm) lq

lqLookup :: CHRKey -> WorkKey -> LastQuery -> WorkTime
lqLookup ck wk lq
  = maybe initWorkTime id
      (do m   <- Map.lookup ck lq
          wtm <- Map.lookup wk m
          return wtm
      )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solve state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(SolveState,emptySolveState)
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
%%]

%%[(9 hmtyinfer || hmtyast) export(solveStateResetDone)
solveStateResetDone :: SolveState p i g s -> SolveState p i g s
solveStateResetDone s = s {stDoneCnstrSet = Set.empty}
%%]

%%[(9 hmtyinfer || hmtyast) export(chrSolveStateDoneConstraints,chrSolveStateTrace)
chrSolveStateDoneConstraints :: SolveState p i g s -> [Constraint p i]
chrSolveStateDoneConstraints = stDoneCnstrs

chrSolveStateTrace :: SolveState p i g s -> SolveTrace p i g s
chrSolveStateTrace = stTrace
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(chrSolve,chrSolve')
chrSolve
  :: ( CHRMatchable env p s, CHRCheckable env g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable i tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> [Constraint p i]
chrSolve env chrStore cnstrs
  = work ++ done
  where (work,done,_) = chrSolve' env chrStore cnstrs

chrSolve'
  :: ( CHRMatchable env p s, CHRCheckable env g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable i tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> ([Constraint p i],[Constraint p i],SolveTrace p i g s)
chrSolve' env chrStore cnstrs
  = (wlToList (stWorkList finalState), stDoneCnstrs finalState, stTrace finalState)
  where finalState = chrSolve'' env chrStore cnstrs emptySolveState
%%]

%%[(9 hmtyinfer || hmtyast) export(chrSolve'')
chrSolve''
  :: ( CHRMatchable env p s, CHRCheckable env g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable i tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , Ord (Constraint p i)
     , PP g, PP i, PP p -- for debugging
     ) => env -> CHRStore p i g s -> [Constraint p i] -> SolveState p i g s -> SolveState p i g s
chrSolve'' env chrStore cnstrs prevState
  = postState {stMatchCache = Map.empty}
  where postState
%%[[9
          = addStats Map.empty [("workMatches",ppAssocLV [(ppTrieKey k,pp (fromJust l)) | (k,c) <- Map.toList $ stCountCnstr st, let l = Map.lookup "workMatched" c, isJust l])] st
%%][100
          = st
%%]]
          where st = iter $ initState prevState
        iter st@(SolveState {stWorkList = wl@(WorkList {wlQueue = (workHd@(workHdKey,_) : workTl)})})
          = case matches of
              (_:_)
%%[[9
                -> expandMatch
                       (addStats Map.empty
                            [ ("(0) yes work", ppTrieKey workHdKey)
                            ] stmatch)
                       matches
%%][100
                -> expandMatch stmatch matches
%%]]
                where expandMatch st@(SolveState {stWorkList = wl, stHistoryCount = histCount})
                                  ( ( ( schr@(StoredCHR {storedIdent = chrId, storedChr = chr@(CHR {chrBody = b, chrSimpSz = simpSz})})
                                      , (keys,works)
                                      )
                                    , subst
                                    ) : tlMatch
                                  )
%%[[9
                        = expandMatch (addStats Map.empty [ ("chr",pp chr')
                                                          , ("leftover sz", pp (length tlMatchY))
                                                          , ("filtered out sz", pp (length tlMatchN))
                                                          , ("new done sz", pp (length bDone))
                                                          , ("new todo sz", pp (length bTodo))
                                                          , ("wl queue sz", pp (length (wlQueue wl')))
                                                          , ("wl usedin sz", pp (Map.size (wlUsedIn wl')))
                                                          , ("done sz", pp (Set.size (stDoneCnstrSet st')))
                                                          , ("hist cnt", pp histCount)
                                                          ]
                                                st')
                                      tlMatchY
%%][100
                        = expandMatch st' tlMatchY
%%]]
                        where (tlMatchY,tlMatchN) = partition (\(r@(_,(ks,_)),_) -> not (any (`elem` keysSimp) ks || isUsedByPropPart (wlUsedIn wl') r)) tlMatch
                              (keysSimp,keysProp) = splitAt simpSz keys
                              usedIn              = Map.singleton (Set.fromList keysProp) (Set.singleton chrId)
                              (bTodo,bDone)       = splitDone $ map (chrAppSubst subst) b
                              bTodo'              = wlCnstrToIns wl bTodo
                              wl' = wlDeleteByKeyAndInsert' histCount keysSimp bTodo'
                                    $ wl { wlUsedIn  = usedIn `wlUsedInUnion` wlUsedIn wl
                                         , wlScanned = []
                                         , wlQueue   = wlQueue wl ++ wlScanned wl
                                         }
                              chr'= subst `chrAppSubst` chr
                              st' = st { stWorkList       = wl'
%%[[9
                                       , stTrace          = SolveStep chr' subst (assocLElts bTodo') bDone : {- SolveDbg (ppwork >-< ppdbg) : -} stTrace st
%%][100
%%]]
                                       , stDoneCnstrSet   = Set.unions [Set.fromList bDone, Set.fromList $ map workCnstr $ take simpSz works, stDoneCnstrSet st]
                                       , stMatchCache     = if List.null bTodo' then stMatchCache st else Map.empty
                                       , stHistoryCount   = histCount + 1
                                       }
%%[[9
                              ppwork = "workkey" >#< ppTrieKey workHdKey >#< ":" >#< (ppBracketsCommas (map (ppTrieKey . fst) workTl) >-< ppBracketsCommas (map (ppTrieKey . fst) $ wlScanned wl))
                                         >-< "workkeys" >#< ppBracketsCommas (map ppTrieKey keys)
                                         >-< "worktrie" >#< wlTrie wl
                                         >-< "schr" >#< schr
                                         >-< "usedin" >#< (ppBracketsCommasV $ map (\(k,s) -> ppKs k >#< ppBracketsCommas (map ppUsedByKey $ Set.toList s)) $ Map.toList $ wlUsedIn wl)
                                         >-< "usedin'" >#< (ppBracketsCommasV $ map (\(k,s) -> ppKs k >#< ppBracketsCommas (map ppUsedByKey $ Set.toList s)) $ Map.toList $ wlUsedIn wl')
                                     where ppKs ks = ppBracketsCommas $ map ppTrieKey $ Set.toList ks
%%][100
%%]]
                      expandMatch st _ 
                        = iter st
                      
%%[[9
              _ -> iter (addStats Map.empty
                             [ ("no match work", ppTrieKey workHdKey)
                             , ("wl queue sz", pp (length (wlQueue wl')))
                             ] st')
%%][100
              _ -> iter st'
%%]]
                where wl' = wl { wlScanned = workHd : wlScanned wl, wlQueue = workTl }
                      st' = stmatch { stWorkList = wl', stTrace = SolveDbg (ppdbg) : {- -} stTrace stmatch }
          where (matches,lastQuery,ppdbg,stats) = workMatches st
%%[[9
                stmatch = addStats stats [("(a) workHd", ppTrieKey workHdKey), ("(b) matches", ppBracketsCommasV [ s `chrAppSubst` storedChr schr | ((schr,_),s) <- matches ])]
%%][100
                stmatch =
%%]]
                            (st { stCountCnstr = scntInc workHdKey "workMatched" $ stCountCnstr st
                                , stMatchCache = Map.insert workHdKey [] (stMatchCache st)
                                , stLastQuery  = lastQuery
                                })
        iter st
          = st
        mkStats  stats new    = stats `Map.union` Map.fromList (assocLMapKey showPP new)
%%[[9
        addStats stats new st = st { stTrace = SolveStats (mkStats stats new) : stTrace st }
%%][100
        addStats _     _   st = st
%%]]
        workMatches st@(SolveState {stWorkList = WorkList {wlQueue = (workHd@(workHdKey,Work {workTime = workHdTm}) : _), wlTrie = wlTrie, wlUsedIn = wlUsedIn}, stHistoryCount = histCount, stLastQuery = lastQuery})
          | isJust mbInCache  = ( fromJust mbInCache
                                , lastQuery
                                , Pretty.empty, mkStats Map.empty [("cache sz",pp (Map.size (stMatchCache st)))]
                                )
          | otherwise         = ( r5
                                , foldr lqUnion lastQuery [ lqSingleton ck wks histCount | (_,(_,(ck,wks))) <- r23 ]
%%[[9
                                -- , Pretty.empty
                                , pp2 >-< {- pp2b >-< pp2c >-< -} pp3
                                , mkStats Map.empty [("(1) lookup sz",pp (length r2)), ("(2) cand sz",pp (length r3)), ("(3) unused cand sz",pp (length r4)), ("(4) final cand sz",pp (length r5))]
%%][100
                                , Pretty.empty
                                , Map.empty
%%]]
                                )
          where -- cache result
                mbInCache = Map.lookup workHdKey (stMatchCache st)
                -- results
                r2  = concat $ lookupResultToList $ lookupPartialByKey TrieLookup_Partial workHdKey $ chrstoreTrie chrStore
                r23 = map (\c -> (c,candidate c)) r2
                r3  = concatMap (\(c,cands) -> zip (repeat c) (map unzip $ combine cands)) $ r23
                r4  = filter (not . isUsedByPropPart wlUsedIn) r3
                r5  = mapMaybe (\r@(chr,kw@(_,works)) -> fmap (\s -> (r,s)) $ match chr (map workCnstr works)) r4
%%[[9
                pp2  = "lookups"    >#< ("for" >#< ppTrieKey workHdKey >-< ppBracketsCommasV r2)
                -- pp2b = "cand1"      >#< (ppBracketsCommasV $ map (ppBracketsCommasV . map (ppBracketsCommasV . map (\(k,w) -> ppTrieKey k >#< w)) . fst . candidate) r2)
                -- pp2c = "cand2"      >#< (ppBracketsCommasV $ map (ppBracketsCommasV . map (ppBracketsCommasV) . combineToDistinguishedElts . fst . candidate) r2)
                pp3  = "candidates" >#< (ppBracketsCommasV $ map (\(chr,(ks,ws)) -> "chr" >#< chr >-< "keys" >#< ppBracketsCommas (map ppTrieKey ks) >-< "works" >#< ppBracketsCommasV ws) $ r3)
%%][100
%%]]
                -- util functions
                candidate (StoredCHR {storedIdent = (ck,_), storedKeys = ks, storedChr = chr@(CHR {chrSimpSz = simpSz})})
                  = (cand lkup sks ++ cand (\h k -> lkup h k) pks, (ck,queriedWorkS))
                  where (sks,pks)     = splitAt simpSz ks
                        lkup how k    = partition (\(_,w) -> workTime w < lastQueryTm) $ lookupResultToList $ lookupPartialByKey' (,) how k wlTrie
                                      where lastQueryTm = lqLookupW k lastQueryW
                        cand lkup     = map (maybe (lkup TrieLookup_Normal workHdKey) (lkup TrieLookup_StopAtPartial))
                        lastQueryW    = lqLookupC ck lastQuery
                        queriedWorkS  = Set.fromList $ map (maybe workHdKey id) ks
                combine ([],_) = []
                combine ((lh:lt),_)
                  = concatMap combineToDistinguishedElts l2
                  -- = combineToDistinguishedElts $ map (\(bef,aft) -> bef++aft) l
                  where l2 = g2 [] lh lt
                           where g2 ll l []           = [mk ll l []]
                                 g2 ll l lr@(lrh:lrt) = mk ll l lr : g2 (ll ++ [l]) lrh lrt
                                 mk ll (bef,aft) lr   = map fst ll ++ [aft] ++ map cmb lr
                                                      where cmb (a,b) = a++b
                match chr cnstrs
                  = foldl cmb (Just chrEmptySubst) $ matches chr cnstrs ++ checks chr
                  where matches (StoredCHR {storedChr = CHR {chrHead = hc}}) cnstrs
                          = zipWith mt hc cnstrs
                          where mt cFr cTo subst = chrMatchTo env subst cFr cTo
                        checks (StoredCHR {storedChr = CHR {chrGuard = gd}})
                          = map chk gd
                          where chk g subst = chrCheck env subst g
                        cmb (Just s) next = fmap (`chrAppSubst` s) $ next s
                        cmb _        _    = Nothing
        isUsedByPropPart wlUsedIn (chr,(keys,_))
          = fnd $ drop (storedSimpSz chr) keys
          where fnd k = maybe False (storedIdent chr `Set.member`) $ Map.lookup (Set.fromList k) wlUsedIn
        initState st = st { stWorkList = wlInsert (stHistoryCount st) wlnew $ stWorkList st, stDoneCnstrSet = Set.unions [Set.fromList done, stDoneCnstrSet st] }
                     where (wlnew,done) = splitDone cnstrs
        splitDone  = partition cnstrRequiresSolve
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer || hmtyast)
instance ForceEval (CHR (Constraint p i) g s) => ForceEval (StoredCHR p i g s) where
  forceEval x@(StoredCHR c i ks id) | forceEval c `seq` forceEval ks `seq` forceEval id `seq` True = x
%%[[102
  fevCount (StoredCHR c i ks id) = cm1 "StoredCHR" `cmUnion` fevCount c `cmUnion` fevCount i `cmUnion` fevCount ks `cmUnion` fevCount id
%%]]

instance ForceEval (StoredCHR p i g s) => ForceEval (CHRStore p i g s) where
  forceEval x@(CHRStore t) | forceEval t `seq` True = x
%%[[102
  fevCount (CHRStore t) = cm1 "CHRStore" `cmUnion` fevCount t
%%]]
%%]

