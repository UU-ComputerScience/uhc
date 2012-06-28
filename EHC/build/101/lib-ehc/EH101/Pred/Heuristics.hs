module EH101.Pred.Heuristics
( Heuristic, SHeuristic
, HeurAlts (..), HeurRed (..)
, toHeuristic
, heurTry
, localChoice
, binChoice
, solvable
, heurScopedEHC )
where
import EH101.Ty
import EH101.Ty.FitsInCommon2
import EH101.CHR
import EH101.VarMp
import EH101.Pred.CHR
import EH101.Pred.Evidence
import EH101.CHR.Constraint
import Data.List (nub,partition)
import Data.Maybe
import EH.Util.Pretty
import EH.Util.AGraph
import EH.Util.Utils

{-# LINE 17 "src/ehc/Pred/Heuristics.chs" #-}
type Heuristic p info = [info] -> HeurAlts p info -> [(info, Evidence p info)]

type SHeuristic p info = HeurAlts p info -> Evidence p info

{-# LINE 45 "src/ehc/Pred/Heuristics.chs" #-}
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

{-# LINE 62 "src/ehc/Pred/Heuristics.chs" #-}
instance Show (HeurAlts  p  info) where
  show _ = "HeurAlts"

instance Show (HeurRed  p  info) where
  show _ = "HeurRed"

{-# LINE 70 "src/ehc/Pred/Heuristics.chs" #-}
instance (PP p, PP info) => PP (HeurAlts  p  info) where
  pp x = "HeurAlts" >#< redaltsPredicate x >#< ppBracketsCommasV (redaltsAlts x)

instance (PP p, PP info) => PP (HeurRed  p  info) where
  pp (HeurRed     i subs) = "HeurRed" >#< i >#< ppBracketsCommasV subs
  pp (HeurRed_Rec p     ) = "HeurRec" >#< p

{-# LINE 83 "src/ehc/Pred/Heuristics.chs" #-}
toHeuristic :: SHeuristic p info -> Heuristic p info
toHeuristic h infos alts
  = zip infos (repeat ev)
  where ev = h alts

{-# LINE 94 "src/ehc/Pred/Heuristics.chs" #-}
heurTry :: Eq p => SHeuristic p info -> SHeuristic p info -> SHeuristic p info
heurTry f g a  | null (evidUnresolved ev) = ev
               | otherwise                = g a
               where ev = f a

{-# LINE 105 "src/ehc/Pred/Heuristics.chs" #-}
localChoice :: (Eq p, Eq info) => (p -> [info] -> [info]) -> SHeuristic p info
localChoice choose (HeurAlts p reds) =
  case filter ((`elem` redinfos) . redInfo) reds of
    []                    -> Evid_Unresolved p (concatMap evidUnresolved [ Evid_Proof p i evs | (i,evs) <- chs reds])
    [r@(HeurRed_Rec p)]   -> Evid_Recurse p
    [r@(HeurRed i   _)]   -> Evid_Proof p i (snd $ ch r)
    rs                    -> reallyOverlapEvid p (chs rs)
  where redinfos          = choose p (map redInfo reds)
        ch (HeurRed i rs) = (i,map (localChoice choose) rs)
        chs rs            = map ch rs

{-# LINE 118 "src/ehc/Pred/Heuristics.chs" #-}
binChoice :: (Eq p, Eq info) => (info -> info -> PartialOrdering) -> SHeuristic p info
binChoice order = localChoice (const local)
  where  local []  = []
         local is  = [mx]
                   where (mx,eqPairs) = heurMaximumBy order is

{-# LINE 126 "src/ehc/Pred/Heuristics.chs" #-}
-- | Choose maximum, also giving list of equals of there are more maximum x-es
heurChoose :: (x -> x -> PartialOrdering) -> (x,[x]) -> x -> (x,[x])
heurChoose cmp (x,eqPairs) y
  = case cmp x y of
      P_LT -> (y,[])
      P_GT -> (x,eqPairs)
      P_EQ -> (x,if null eqPairs then [x,y] else y:eqPairs)

heurMaximumBy :: (x -> x -> PartialOrdering) -> [x] -> (x,[x])
heurMaximumBy cmp (x:xs)
  = foldl (heurChoose cmp) (x,[]) xs

{-# LINE 144 "src/ehc/Pred/Heuristics.chs" #-}
contextChoice :: Eq p => (p -> [HeurRed p info] -> [HeurRed p info]) -> SHeuristic p info
contextChoice choose (HeurAlts p reds) =
  case choose p reds of
         []                   -> Evid_Unresolved p [UnresolvedTrace_Fail p []]
         [r@(HeurRed_Rec p)]  -> Evid_Recurse p
         [r@(HeurRed i   _)]  -> Evid_Proof p i (snd $ ch r)
         rs                   -> reallyOverlapEvid p (chs rs)
  where ch (HeurRed i rs) = (i,map (contextChoice choose) rs)
        chs rs            = map ch rs

contextBinChoice :: Eq p => (HeurRed p info -> HeurRed p info -> PartialOrdering) -> SHeuristic p info
contextBinChoice order = contextChoice (const local)
  where  local []                = []
         local is | null eqPairs = [mx]
                  | otherwise    = eqPairs
                   where (mx,eqPairs) = heurMaximumBy order is			-- do something with equal pairs, construct Evid_Ambig perhaps?

{-# LINE 169 "src/ehc/Pred/Heuristics.chs" #-}
reallyOverlapEvid :: p -> [(info,[Evidence p info])] -> Evidence p info
reallyOverlapEvid p evs
  = case filter (not . null . snd) evs of
      []       -> Evid_Ambig p evs
      [(i,ev)] -> Evid_Proof p i ev
      _        -> Evid_Ambig p evs

{-# LINE 188 "src/ehc/Pred/Heuristics.chs" #-}
solvable :: HeurAlts p info -> HeurAlts p info
solvable (HeurAlts p rs) = HeurAlts p (catMaybes (map heu rs))
   where heu h@(HeurRed info reds)  | all hasAlts reds'  = Just (HeurRed info  reds')
                                    | otherwise          = Nothing
                                    where reds' = map solvable reds
         heu h@(HeurRed_Rec p    )                       = Just h

hasAlts :: HeurAlts p info -> Bool
hasAlts (HeurAlts _ [])  = False
hasAlts _                = True

{-# LINE 207 "src/ehc/Pred/Heuristics.chs" #-}
cmpSpecificness :: CHRMatchable (FIIn' gm) Pred VarMp => FIIn' gm -> Pred -> Pred -> PartialOrdering
cmpSpecificness env p q =
  case  chrMatchTo env (emptyVarMp :: VarMp) p q of
    Nothing  -> P_GT
    Just _   -> case  chrMatchTo env (emptyVarMp :: VarMp) q p of
                  Nothing  -> P_LT
                  Just _   -> P_EQ

{-# LINE 284 "src/ehc/Pred/Heuristics.chs" #-}
cmpEqReds :: RedHowAnnotation -> RedHowAnnotation -> PartialOrdering
cmpEqReds r1                            r2                              = panic ("cmpEqReds: don't know how to deal with: " ++ show (pp r1) ++ " and " ++ show (pp r2))

{-# LINE 307 "src/ehc/Pred/Heuristics.chs" #-}
anncmpEHCScoped :: CHRMatchable (FIIn' gm) Pred VarMp => Bool -> FIIn' gm -> HeurRed CHRPredOcc RedHowAnnotation -> HeurRed CHRPredOcc RedHowAnnotation -> PartialOrdering
anncmpEHCScoped preferInst env ann1 ann2
  = case (ann1,ann2) of
      (HeurRed (RedHow_Assumption     _ _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_Assumption     _ _) _    )              ->  P_LT
      (HeurRed (RedHow_ByScope ByScopeRedHow_Assume) _
                                                , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByScope ByScopeRedHow_Assume) _)        ->  P_LT
      (HeurRed_Rec _                            , _                                        )              ->  P_GT
      (_                                        , HeurRed_Rec _                            )              ->  P_LT
      (HeurRed (RedHow_ByInstance _ p   s) _    , HeurRed (RedHow_ByInstance _ q   t) _    )              ->  case pscpCmpByLen s t of
                                                                                                                EQ   -> cmpSpecificness env p q
                                                                                                                ord  -> toPartialOrdering ord
      (HeurRed (RedHow_ByInstance _ _   s) _    , HeurRed (RedHow_ByScope _) [HeurAlts q _])              ->  toPartialOrdering $ pscpCmpByLen s (cpoScope q)
      (HeurRed (RedHow_ByScope _) [HeurAlts p _], HeurRed (RedHow_ByInstance _ _   t) _    )              ->  toPartialOrdering $ pscpCmpByLen (cpoScope p) t
      (HeurRed (RedHow_ByInstance _ _   _) _    , _                                        ) | preferInst ->  P_GT
      (_                                        , HeurRed (RedHow_ByInstance _ _   _) _    ) | preferInst ->  P_LT
      (HeurRed (RedHow_ByLabel     _ _  s) _    , HeurRed (RedHow_ByLabel     _ _  t) _    )              ->  toPartialOrdering $ pscpCmpByLen s t
      (HeurRed (RedHow_ByLabel     _ _  _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByLabel     _ _  _) _    )              ->  P_LT
      (HeurRed (RedHow_BySuperClass _ _ _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_BySuperClass _ _ _) _    )              ->  P_LT
      (HeurRed (RedHow_ByScope _) [HeurAlts p _], HeurRed (RedHow_ByScope _) [HeurAlts q _])              ->  toPartialOrdering $ pscpCmpByLen (cpoScope p) (cpoScope q)
      (HeurRed (RedHow_ByScope _) _             , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByScope _) _             )              ->  P_LT
      (HeurRed (RedHow_ByInstance _ _   _) _    , _                                        )              ->  P_GT
      (_                                        , HeurRed (RedHow_ByInstance _ _   _) _    )              ->  P_LT
      _                                                                                                   ->  panic ("anncmpEHCScoped: don't know how to deal with:\n  " ++ show (pp ann1) ++ "\n  " ++ show (pp ann2))

{-# LINE 353 "src/ehc/Pred/Heuristics.chs" #-}
heurScopedEHC :: CHRMatchable (FIIn' gm) Pred VarMp => FIIn' gm -> Heuristic CHRPredOcc RedHowAnnotation
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

