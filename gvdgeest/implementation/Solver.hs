{-# OPTIONS -fglasgow-exts #-}
module Solver
     ( SolveState ()
     , runSolver
     , solve
     , solveImpr
     , mkState
     , mkImprState
     , fixImprove
     , improve
     , simplify
     , solveConstraints
     , getProveObligations
     , Matchable (..)
     , Substitutable (..)
     , Constraint (..)
     , Rule
     , (<=>)
     , (==>)
     , (|>)
)
where

import Text.PrettyPrint 
import Control.Monad.State
import Data.List (nub, intersect)
import Data.Monoid
import qualified Data.Map as Map 

import Constraints
import Simplification
import Heuristics
import CHRSolver

type Constraints  p info = Map.Map (Constraint p info) [info]
type EvidenceMap  p info = Map.Map info (Evidence p info)

data SolveState p s info = 
  SolveSt  { rules        :: [Rule p s info]
           , imprRules    :: [Rule p s info]
           , heuristic    :: Heuristic    p info
           , constraints  :: Constraints  p info
           , evidence     :: EvidenceMap  p info
           }

instance (Show p, Ord p, Show info, Eq info) => Show (SolveState p s info) where
  show = ppState
         
mkState :: Ord p => [Rule p s info] -> Heuristic p info -> SolveState p s info
mkState rls h = SolveSt rls [] h Map.empty Map.empty

mkImprState :: Ord p => [Rule p s info] -> [Rule p s info] -> Heuristic p info -> SolveState p s info
mkImprState rls imprRls h = SolveSt rls imprRls h Map.empty Map.empty

solve :: (Matchable p s, Ord info) => [(Constraint p info, info)] -> SolveState p s info -> SolveState p s info
solve cs s = runSolver (do solveConstraints cs
                           simplify
                       ) s
                       
solveImpr ::  (Matchable p s, Ord info, Substitutable p v a)                                                                    
              => (p -> Maybe a) -> [(Constraint p info, info)] -> SolveState p s info -> (a, SolveState p s info)
solveImpr f cs s = runState (do solveConstraints cs
                                a <- fixImprove f mempty
                                return a
                             ) s                       

runSolver :: (Ord p) => State (SolveState p s b) a -> SolveState p s b -> SolveState p s b
runSolver m s = snd (runState m s)

solveConstraints ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info) 
                     => [(Constraint p info, info)] -> m ()
solveConstraints = mapM_ solveConstraint

solveConstraint ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info)
                    => (Constraint p info, info) -> m ()
solveConstraint (c, i) =
  modifyConstraints  (Map.insertWith (++) c [i])
  
fixImprove :: ( MonadState (SolveState p s info) m, Matchable p s, Ord info, Substitutable p v a)
              => (p -> Maybe a) -> a -> m a
fixImprove mgu s = 
  do  simplify
      s' <- improve mgu
      if s' == mempty
        then return s
        else do  applySubstitution s'
                 fixImprove mgu (s `mappend` s')  
                
improve :: (MonadState (SolveState p s info) m, Matchable p s, Ord info, Substitutable p v a) =>
           (p -> Maybe a) -> m a  
improve mgu =
  do  rls       <- gets imprRules
      cnstrs    <- gets constraints  
      let  equalities    = chrSolveList rls (Map.keys cnstrs)
           (s, cnstrs')  = foldr (imprSubst mgu) (mempty, cnstrs) equalities 
      modifyConstraints (const cnstrs')
      return s  
     
imprSubst ::  (Ord p, Ord info, Substitutable p v a) =>
              (p -> Maybe a) -> Constraint p info -> (a, Constraints  p info) -> (a, Constraints  p info)
imprSubst mgu  c@(Prove p)  (s, cs) = 
  case mgu (substitute s p) of 
    Nothing  -> (s,               Map.insertWith (++) c [] cs)
    Just s'  -> (s' `mappend` s,  Map.delete c cs)

imprSubst _    c            (s, cs) = 
  (s, Map.insertWith (++) c [] cs)

simplify ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info) => m ()
simplify =
  do  chrs      <- gets rules
      cnstrs    <- gets constraints  
      let  initGraph   = Map.foldWithKey addAssumption emptyAGraph cnstrs
           reductions  = chrSolveList chrs (Map.keys cnstrs)
           graph       = foldr addReduction initGraph reductions
      modifyConstraints (const Map.empty)                             
      mapM_ (constructEvidence graph) (Map.assocs cnstrs)

constructEvidence ::   (MonadState (SolveState p s info) m, Matchable p s, Ord info)
                       => Graph p info -> (Constraint p info, [info]) -> m ()
constructEvidence graph  (Prove p, infos)  =
  do  hrstc <- gets heuristic
      let  trees  = hrstc infos (alternatives graph p)
      modifyEvidence    (\em -> foldr insertEvidence em trees)      
      solveConstraints  (concatMap remaining trees)

constructEvidence _      (c, infos)        =
      solveConstraints  (zip (repeat c) infos)

insertEvidence ::  (Eq p, Ord info) 
                   => (info, Evidence p info) -> EvidenceMap p info -> EvidenceMap p info
insertEvidence = uncurry (Map.insertWith updateUnresolved)

remaining :: Eq p => (info, Evidence p info) -> [(Constraint p info, info)]
remaining (i, tree) = zip (map Prove (unresolved tree)) (repeat i)


applySubstitution :: (MonadState (SolveState p s info) m, Ord p, Ord info, Substitutable p v a)  =>  a -> m ()
applySubstitution s = 
  do  modifyConstraints  (Map.mapKeysWith (++)  (substitute s))
      modifyEvidence     (Map.map               (substitute s))

getProveObligations :: (MonadState (SolveState p s info) m, Substitutable p v a) => [v] -> m [p]
getProveObligations tps =
 do  prvs <- gets constraints
     let  g = not . null . intersect tps . ftv

          isProve (Prove _)  = True
          isProve _          = False

          unProve (Prove p)  = p
     return [ unProve p | p <- (Map.keys prvs), isProve p, g p ]

modifyConstraints  f = modify (\s -> s { constraints  = f (constraints  s)})
modifyEvidence     f = modify (\s -> s { evidence     = f (evidence     s)})

-----------------------------------------------------------------------------
-- Som ugly pretty print code:
-----------------------------------------------------------------------------
ppState :: (Show info, Show p, Eq info, Ord p) => SolveState p s info -> String
ppState st  =  render $
               text "rules      " <+> text "=" <+> ppRules (rules st)
            $$ text "constraints" <+> text "=" <+> ppShow  (constraints st)
            $$ text "evidence   " <+> text "=" <+> ppAL    (Map.assocs $ evidence st)

ppAL :: (Show a, Show b) => [(a, b)] -> Doc
ppAL []      = braces empty
ppAL [d]     = braces (ppTuple d)
ppAL (d:ds)  = cat [char '{', 
	             vcat (nest 2 (ppTuple d) : [text ", " <> (ppTuple d) | d <- ds]),
		     char '}']

ppTuple :: (Show a, Show b) => (a, b) -> Doc
ppTuple (a, b) = ppShow a <> text " -> " <> ppShow b

ppRules :: Show a => [a] -> Doc
ppRules []      = brackets empty
ppRules [d]     = brackets (ppShow d)
ppRules (d:ds)  = cat [char '[', 
	             vcat (nest 2 (ppShow d) : [text ", " <> (ppShow d) | d <- ds]),
		     char ']']

ppShow :: Show a => a -> Doc
ppShow = text . show
