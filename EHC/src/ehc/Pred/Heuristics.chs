%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Heuristics for evidence computation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[(9 hmtyinfer) module {%{EH}Pred.Heuristics} import({%{EH}Ty},{%{EH}Ty.FitsInCommon2}, {%{EH}CHR},{%{EH}VarMp},{%{EH}Pred.CHR},{%{EH}Pred.Evidence},{%{EH}CHR.Constraint})
%%]

%%[(9 hmtyinfer) import(Data.List(nub, partition),Data.Maybe)
%%]

%%[(9 hmtyinfer) import(EH.Util.Pretty,EH.Util.AGraph,EH.Util.Utils)
%%]

%%[(9 hmtyinfer) export(Heuristic,SHeuristic)
type Heuristic p info = [info] -> HeurAlts p info -> [(info, Evidence p info)]

type SHeuristic p info = HeurAlts p info -> Evidence p info
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alternatives: Each alternative for reducing a predicate is
%%% represented in the datatype HeurAlts, because Haskell is lazy this
%%% tree is only evaluated when needed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(HeurAlts(..),HeurRed(..))
data HeurAlts  p  info
  = HeurAlts
     { redaltsPredicate  	:: p
     , redaltsAlts  		:: [HeurRed p info]
     }

data HeurRed   p  info
  = HeurRed
     { redInfo           	:: info
     , redContext   		:: [HeurAlts p info]
     }
  | HeurRed_Rec
     { redRecPred           :: p
     }
%%]

%%[(9 hmtyinfer)
instance Show (HeurAlts  p  info) where
  show _ = "HeurAlts"

instance Show (HeurRed  p  info) where
  show _ = "HeurRed"
%%]
 
%%[(9 hmtyinfer)
instance (PP p, PP info) => PP (HeurAlts  p  info) where
  pp x = "HeurAlts" >#< redaltsPredicate x >#< ppBracketsCommasV (redaltsAlts x)

instance (PP p, PP info) => PP (HeurRed  p  info) where
  pp (HeurRed     i subs) = "HeurRed" >#< i >#< ppBracketsCommasV subs
  pp (HeurRed_Rec p     ) = "HeurRec" >#< p
%%]
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Making a Heuristic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(toHeuristic)
toHeuristic :: SHeuristic p info -> Heuristic p info
toHeuristic h infos alts
  = zip infos (repeat ev)
  where ev = h alts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Try combinator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(heurTry)
heurTry :: Eq p => SHeuristic p info -> SHeuristic p info -> SHeuristic p info
heurTry f g a  | null (evidUnresolved ev) = ev
               | otherwise                = g a
               where ev = f a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion to evidence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer) export(toEvidence)
toEvidence :: (HeurAlts p info -> HeurAlts p info) -> SHeuristic p info
toEvidence f a = evd (f a)
  where  evd (HeurAlts p [])                   =  Evid_Unresolved p
         evd (HeurAlts p [r@(HeurRed_Rec p)])  =  Evid_Recurse p
         evd (HeurAlts p [r@(HeurRed i   _)])  =  Evid_Proof   p i (snd $ red r)
         evd (HeurAlts p rs)                   =  reallyAmbigEvid p (reds rs)
         red (HeurRed i alts)                  =  (i,map evd alts)
         reds rs                               =  map red rs
%%]
toEvidence :: (HeurAlts p info -> HeurAlts p info) -> SHeuristic p info
toEvidence f a = evd (f a)
  where  evd (HeurAlts p [])                 =  Evid_Unresolved p
         evd (HeurAlts p [r@(HeurRed i _)])  =  Evid_Proof p i (snd $ red r)
         evd (HeurAlts p rs)                 =  Evid_Ambig p (reds rs)
         red (HeurRed i alts)                =  (i,map evd alts)
         reds rs                             =  map red rs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local / Binary choice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(localChoice,binChoice)
localChoice :: Eq info => (p -> [info] -> [info]) -> SHeuristic p info  
localChoice choose (HeurAlts p reds) = 
  case filter ((`elem` redinfos) . redInfo) reds of
    []                    -> Evid_Unresolved p
    [r@(HeurRed_Rec p)]   -> Evid_Recurse p
    [r@(HeurRed i   _)]   -> Evid_Proof p i (snd $ ch r)
    rs                    -> reallyAmbigEvid p (chs rs)
  where redinfos          = choose p (map redInfo reds)
        ch (HeurRed i rs) = (i,map (localChoice choose) rs)
        chs rs            = map ch rs

binChoice :: Eq info => (info -> info -> PartialOrdering) -> SHeuristic p info
binChoice order = localChoice (const local)
  where  local []  = []
         local is  = [mx]
                   where (mx,eqPairs) = heurMaximumBy order is			-- do something with equal pairs, construct Evid_Ambig perhaps?
%%]

%%[(9 hmtyinfer)
heurChoose :: (x -> x -> PartialOrdering) -> (x,[(x,x)]) -> x -> (x,[(x,x)])
heurChoose cmp (x,eqPairs) y
  = case cmp x y of
      P_LT -> (y,[])
      P_GT -> (x,eqPairs)
      P_EQ -> (x,[(x,y)]++eqPairs)

heurMaximumBy :: (x -> x -> PartialOrdering) -> [x] -> (x,[(x,x)])
heurMaximumBy cmp (x:xs)
  = foldl (heurChoose cmp) (x,[]) xs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Local / Binary choice with reduction context
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
contextChoice :: (p -> [HeurRed p info] -> [HeurRed p info]) -> SHeuristic p info
contextChoice choose (HeurAlts p reds) = 
  case choose p reds of
         []                   -> Evid_Unresolved p
         [r@(HeurRed_Rec p)]  -> Evid_Recurse p
         [r@(HeurRed i   _)]  -> Evid_Proof p i (snd $ ch r)
         rs                   -> reallyAmbigEvid p (chs rs)
  where ch (HeurRed i rs) = (i,map (contextChoice choose) rs)
        chs rs            = map ch rs
         
contextBinChoice :: (HeurRed p info -> HeurRed p info -> PartialOrdering) -> SHeuristic p info
contextBinChoice order = contextChoice (const local)
  where  local []                = []
         local is | null eqPairs = [mx]
                  | otherwise    = concatMap (\(x,y) -> [x,y]) eqPairs
                   where (mx,eqPairs) = heurMaximumBy order is			-- do something with equal pairs, construct Evid_Ambig perhaps?
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Determine whether ambiguous really is ambiguous
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be merged with similar choices made at callsites.

%%[(9 hmtyinfer)
reallyAmbigEvid :: p -> [(info,[Evidence p info])] -> Evidence p info
reallyAmbigEvid p evs
  = case filter (not . null . snd) evs of
      []       -> Evid_Unresolved p
      [(i,ev)] -> Evid_Proof p i ev
      _        -> Evid_Ambig p evs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Heuristic that only selects solvable alternatives (using backtracking)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(solvable)
solvable :: HeurAlts p info -> HeurAlts p info
solvable (HeurAlts p rs) = HeurAlts p (catMaybes (map heu rs))
   where heu h@(HeurRed info reds)  | all hasAlts reds'  = Just (HeurRed info  reds') 
                                    | otherwise          = Nothing
                                    where reds' = map solvable reds
         heu h@(HeurRed_Rec p    )                       = Just h

hasAlts :: HeurAlts p info -> Bool
hasAlts (HeurAlts _ [])  = False
hasAlts _                = True
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predefined heuristics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should not depend on emptyVarMp, but abstract away from it. Perhaps use chrEmptySubst

%%[(9 hmtyinfer)
cmpSpecificness :: FIIn -> Pred -> Pred -> PartialOrdering
cmpSpecificness env p q = 
  case  chrMatchTo env emptyVarMp p q of 
    Nothing  -> P_GT
    Just _   -> case  chrMatchTo env emptyVarMp q p of
                  Nothing  -> P_LT
                  Just _   -> P_EQ
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Haskell98 heuristics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(heurHaskell98)
anncmpHaskell98 :: FIIn -> RedHowAnnotation -> RedHowAnnotation -> PartialOrdering
anncmpHaskell98 env ann1 ann2
  = case (ann1,ann2) of
      (RedHow_ByInstance   _ p s, RedHow_ByInstance   _ q t)  ->  case pscpCmpByLen s t of
                                                                    EQ   -> cmpSpecificness env p q
                                                                    ord  -> toPartialOrdering ord
      (RedHow_ByInstance   _ _ _, _                        )  ->  P_GT
      (_                        , RedHow_ByInstance   _ _ _)  ->  P_LT
      (RedHow_BySuperClass _ _ _, _                        )  ->  P_GT
      (_                        , RedHow_BySuperClass _ _ _)  ->  P_LT
      (RedHow_Assumption     _ _, _                        )  ->  P_GT
      (_                        , RedHow_Assumption     _ _)  ->  P_LT
      (RedHow_ByScope _         , _                        )  ->  P_GT
      (_                        , RedHow_ByScope _         )  ->  P_LT
      (RedHow_ProveObl       _ _, _                        )  ->  P_GT
--      (_                        , RedHow_ProveObl       _ _)  ->  P_LT

heurHaskell98 :: FIIn -> Heuristic p RedHowAnnotation
heurHaskell98 env = toHeuristic $ binChoice (anncmpHaskell98 env)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GHC heuristics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(heurGHC)
anncmpGHCBinSolve :: FIIn -> RedHowAnnotation -> RedHowAnnotation -> PartialOrdering
anncmpGHCBinSolve env ann1 ann2
  = case (ann1,ann2) of
      (RedHow_Assumption     _ _, _                        )  ->  P_GT
      (_                        , RedHow_Assumption     _ _)  ->  P_LT
      (RedHow_BySuperClass _ _ _, _                        )  ->  P_GT
      (_                        , RedHow_BySuperClass _ _ _)  ->  P_LT
      (RedHow_ByInstance   _ _ _, _                        )  ->  P_GT
      (_                        , RedHow_ByInstance   _ _ _)  ->  P_LT
      (RedHow_ByScope _         , _                        )  ->  P_GT
      (_                        , RedHow_ByScope _         )  ->  P_LT
      (RedHow_ProveObl       _ _, _                        )  ->  P_GT
--      (_                        , RedHow_ProveObl       _ _)  ->  P_LT

ghcSolve :: Eq p => FIIn -> SHeuristic p RedHowAnnotation
ghcSolve env = binChoice (anncmpGHCBinSolve env)

ghcLocalReduce :: a -> [RedHowAnnotation] -> [RedHowAnnotation]
ghcLocalReduce _  reds =  let  p (RedHow_BySuperClass _ _ _)  = True
                               p _                            = False
                          in   filter p reds

ghcReduce :: Eq p => SHeuristic p RedHowAnnotation
ghcReduce = localChoice ghcLocalReduce

heurGHC :: Eq p => FIIn -> Heuristic p RedHowAnnotation
heurGHC env
  = toHeuristic
    $ heurTry (ghcSolve env)
              ghcReduce
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC heuristics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
cmpEqReds :: RedHowAnnotation -> RedHowAnnotation -> PartialOrdering
%%[[16
cmpEqReds RedHow_ByEqIdentity           _                               = P_GT
cmpEqReds _                             RedHow_ByEqIdentity             = P_LT
cmpEqReds RedHow_ByPredSeqUnpack        _                               = P_GT
cmpEqReds _                             RedHow_ByPredSeqUnpack          = P_LT
cmpEqReds RedHow_ByEqFromAssume         _                               = P_GT
cmpEqReds _                             RedHow_ByEqFromAssume           = P_LT
cmpEqReds (RedHow_Assumption _ _)       _                               = P_GT
cmpEqReds _                             (RedHow_Assumption _ _)         = P_LT
cmpEqReds (RedHow_ByEqTyReduction _ _)  _                               = P_GT
cmpEqReds _                             (RedHow_ByEqTyReduction _ _)    = P_LT
cmpEqReds RedHow_ByEqCongr              _                               = P_GT
cmpEqReds _                             RedHow_ByEqCongr                = P_LT
cmpEqReds RedHow_ByEqTrans              _                               = P_GT
cmpEqReds _                             RedHow_ByEqTrans                = P_LT
cmpEqReds RedHow_ByEqSymmetry           _                               = P_GT
cmpEqReds _                             RedHow_ByEqSymmetry             = P_LT
%%]]
cmpEqReds r1                            r2                              = panic ("cmpEqReds: don't know how to deal with: " ++ show (pp r1) ++ " and " ++ show (pp r2))
%%]

%%[(9 hmtyinfer)
anncmpEHCScoped :: Bool -> FIIn -> HeurRed CHRPredOcc RedHowAnnotation -> HeurRed CHRPredOcc RedHowAnnotation -> PartialOrdering
anncmpEHCScoped preferInst env ann1 ann2
  = case (ann1,ann2) of
      (HeurRed (RedHow_Assumption     _ _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_Assumption     _ _) _    )              ->  P_LT
      (HeurRed (RedHow_ByScope ByScopeRedHow_Assume) _
                                                , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByScope ByScopeRedHow_Assume) _)        ->  P_LT
      (HeurRed_Rec _                            , _                                        )              ->  P_GT
      (_                                        , HeurRed_Rec _                            )              ->  P_LT
      (HeurRed (RedHow_ByInstance  _ p  s) _    , HeurRed (RedHow_ByInstance  _ q  t) _    )              ->  case pscpCmpByLen s t of
                                                                                                                EQ   -> cmpSpecificness env p q
                                                                                                                ord  -> toPartialOrdering ord
      (HeurRed (RedHow_ByInstance  _ _  s) _    , HeurRed (RedHow_ByScope _) [HeurAlts q _])              ->  toPartialOrdering $ pscpCmpByLen s (cpoScope q)
      (HeurRed (RedHow_ByScope _) [HeurAlts p _], HeurRed (RedHow_ByInstance  _ _  t) _    )              ->  toPartialOrdering $ pscpCmpByLen (cpoScope p) t
      (HeurRed (RedHow_ByInstance  _ _  _) _    , _                                        ) | preferInst ->  P_GT
      (_                                        , HeurRed (RedHow_ByInstance  _ _  _) _    ) | preferInst ->  P_LT
%%[[10
      (HeurRed (RedHow_ByLabel     _ _  s) _    , HeurRed (RedHow_ByLabel     _ _  t) _    )              ->  toPartialOrdering $ pscpCmpByLen s t
      (HeurRed (RedHow_ByLabel     _ _  _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByLabel     _ _  _) _    )              ->  P_LT
%%]]
      (HeurRed (RedHow_BySuperClass _ _ _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_BySuperClass _ _ _) _    )              ->  P_LT
      (HeurRed (RedHow_ByScope _) [HeurAlts p _], HeurRed (RedHow_ByScope _) [HeurAlts q _])              ->  toPartialOrdering $ pscpCmpByLen (cpoScope p) (cpoScope q)
      (HeurRed (RedHow_ByScope _) _             , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByScope _) _             )              ->  P_LT
      (HeurRed (RedHow_ByInstance  _ _  _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByInstance  _ _  _) _    )              ->  P_LT
      _                                                                                                   ->  panic ("anncmpEHCScoped: don't know how to deal with:\n  " ++ show (pp ann1) ++ "\n  " ++ show (pp ann2))
%%]

If no full solution is possible, we just use the superclass relationship.
- This relationship is fixed, so closed world choice works here.
- We also allow for ambiguity here, randomly picking an alternative, the first. This is not good, but will work for now...

%%[(9999 hmtyinfer)
ehcAllowForGeneralization :: HeurRed CHRPredOcc RedHowAnnotation -> Bool
ehcAllowForGeneralization (HeurRed (RedHow_BySuperClass _ _ _) _) = True
ehcAllowForGeneralization _                                       = False

ehcOnlySuperReduce :: a -> [HeurRed CHRPredOcc RedHowAnnotation] -> [HeurRed CHRPredOcc RedHowAnnotation]
ehcOnlySuperReduce _  reds = take 1 $ filter ehcAllowForGeneralization reds
%%]

%%[(9 hmtyinfer) export(heurScopedEHC)
heurScopedEHC :: FIIn -> Heuristic CHRPredOcc RedHowAnnotation
heurScopedEHC env
  = toHeuristic
    $ ifthenelseSHeuristic isEqHeuristic
        eqHeuristic
{-
        defaultHeuristic
-}
{-
        $ heurTry (contextBinChoice (anncmpEHCScoped True  env))
                  -- (contextBinChoice (anncmpEHCScoped False env))
                  (contextChoice ehcOnlySuperReduce)
-}
        $ contextBinChoice (anncmpEHCScoped True  env)
  where
%%[[16
    isEqHeuristic (CHRPredOcc (Pred_Eq _ _) _) = True
%%]]
    isEqHeuristic _                            = False
    eqHeuristic = binChoice cmpEqReds . solvable
{-
    defaultHeuristic
      = contextBinChoice (anncmpEHCScoped env)
-}

ifthenelseSHeuristic :: (p -> Bool) -> SHeuristic p info -> SHeuristic p info -> SHeuristic p info
ifthenelseSHeuristic g t e alts
  | g (redaltsPredicate alts) = t alts
  | otherwise = e alts
%%]

Previous heuristic did not behave ghc alike in that instances were eagerly used, also when it still would lead to unresolved predicates.
These would then end up in the context of a function, the early decision inhibiting the use of other instances at a later moment:

heurScopedEHC :: FIIn -> Heuristic CHRPredOcc RedHowAnnotation
heurScopedEHC env
  = toHeuristic
    $ ifthenelseSHeuristic isEqHeuristic
        eqHeuristic
        defaultHeuristic


%%[(9999 hmtyinfer)
btHeuristic :: Heuristic p RedHowAnnotation
btHeuristic = toHeuristic $ toEvidence solvable
%%]

