{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances  #-}
module Heuristics
(  Heuristic    
,  toHeuristic
,  SHeuristic
,  Evidence (..)
,  Alts (Alts)
,  Red  (Red)
,  unresolved
,  updateUnresolved
,  toEvidence
,  solvable
,  localChoice
,  binChoice
,  localChoice
,  contextBinChoice
,  try
)
where

import Constraints

import Data.Maybe
import Data.List(nub, maximumBy)

type Heuristic p info = [info] -> Alts p info -> [(info, Evidence p info)]

type SHeuristic p info = Alts p info -> Evidence p info

------------------------------------------------------------------
-- Alternatives: Each alternative for reducing a predicate is
-- represented in the datatype Alts, because Haskell is lazy this
-- tree is only evaluated when needed.
------------------------------------------------------------------
data Alts  p  info = Alts  { predicate  :: p,       alts     :: [Red p info]    }
data Red   p  info = Red   { info       :: info,    context  :: [Alts p info]   }
 
------------------------------------------------------------------
-- Representation of evidence
------------------------------------------------------------------
data Evidence  p info  =  Unresolved p
                       |  Proof p  info [Evidence p info]

instance (Show info, Show p) => Show (Evidence p info) where
  show (Proof _ info []) = show info
  show (Proof _ info es) = show info ++ " " ++ show es
  show (Unresolved p)    = "unresolved: " ++ show p
  
instance Substitutable p v s => Substitutable (Evidence  p info) v s where
  ftv (Unresolved  p)        = ftv p
  ftv (Proof p _ es)         = ftv p ++ concatMap ftv es
 
  substitute s (Unresolved  p)  = Unresolved   (substitute s p)
  substitute s (Proof  p i es)  = Proof (substitute s p) i (map (substitute s) es)  

unresolved :: Eq p => Evidence p info -> [p]
unresolved (Unresolved p)  = [p]
unresolved (Proof _ _ ps)  = nub $ concatMap unresolved ps

updateUnresolved :: Eq p => Evidence p info -> Evidence p info -> Evidence p info
updateUnresolved e                 (Unresolved _)  = e
updateUnresolved (Proof p i qs)    e               = Proof p i [updateUnresolved q e | q <- qs] 
updateUnresolved u@(Unresolved q)  e@(Proof p _ _)
           | q == p     = e
           | otherwise  = u

toHeuristic :: SHeuristic p info -> Heuristic p info
toHeuristic h infos alts = zip infos (repeat $ h alts)

-----------------------------------------------------------------------------
-- Try combinator
-----------------------------------------------------------------------------
try :: Eq p => SHeuristic p info -> SHeuristic p info -> SHeuristic p info
try f g a  | null (unresolved e)    = e
           | otherwise              = g a
           where e = f a

toEvidence :: (Alts p info -> Alts p info) -> SHeuristic p info
toEvidence f a = rec (f a)
  where  rec (Alts p [])            =  Unresolved p
         rec (Alts p [Red i alts])  =  Proof p i (map rec alts)
         rec _                      =  error "Alternatives left"

-----------------------------------------------------------------------------
-- Local / Binary choice
-----------------------------------------------------------------------------
localChoice :: Eq info => (p -> [info] -> [info]) -> SHeuristic p info  
localChoice choose (Alts p reds) = 
  let  redinfos  = choose p (map info reds)
  in   case filter ((`elem` redinfos) . info) reds of
         []            -> Unresolved p
         [(Red i rs)]  -> Proof p i (map (localChoice choose) rs)
         _             -> error "Alternatives left"

binChoice :: Eq info => (info -> info -> Ordering) -> SHeuristic p info
binChoice order = localChoice (const local)
  where  local []  = []
         local is  = [maximumBy order is]
         
-----------------------------------------------------------------------------
-- Local / Binary choice
-----------------------------------------------------------------------------
contextChoice :: (p -> [Red p info] -> [Red p info]) -> Alts p info -> Evidence p info  
contextChoice choose (Alts p reds) = 
  case choose p reds of
         []            -> Unresolved p
         [(Red i rs)]  -> Proof p i (map (contextChoice choose) rs)
         _             -> error "Alternatives left"         
         
contextBinChoice :: (Red p info -> Red p info -> Ordering) -> SHeuristic p info
contextBinChoice order = contextChoice (const local)
  where  local []  = []
         local is  = [maximumBy order is]         
         

-----------------------------------------------------------------------------
-- Heuristic that only selects solvable alternatives (using backtracking)
-----------------------------------------------------------------------------
solvable :: Alts p info -> Alts p info
solvable (Alts p rs) = Alts p (catMaybes (map rec rs))
   where rec (Red info reds)  | all hasAlts reds'  = Just (Red info  reds') 
                              | otherwise          = Nothing
                              where reds' = map solvable reds

hasAlts :: Alts p info -> Bool
hasAlts (Alts _ [])  = False
hasAlts _            = True
