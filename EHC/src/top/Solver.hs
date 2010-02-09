{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Solver 
   ( module Top.Solver
   , module Control.Monad.Writer
   ) where

import Top.Types
import Top.Interface.Basic
import Top.Interface.TypeInference
import Top.Interface.Substitution
import Top.Interface.Qualification
import Top.Implementation.General
import Top.Util.Option
import Top.Monad.StateFix
import Top.Constraint
import qualified Data.Map as M
import Top.Constraint.Information
import Control.Monad.Writer

data ConstraintSolver constraint info = ConstraintSolver (SolveOptions -> [constraint] -> (SolveResult info, LogEntries))

makeConstraintSolver :: (Empty (f () (BasicMonad f))) =>
                           (SolveOptions -> [constraint] -> BasicMonad f (SolveResult info))
                           -> ConstraintSolver constraint info
makeConstraintSolver f = ConstraintSolver (\options -> evalBasicMonad . f options)

solve :: SolveOptions -> [constraint] -> ConstraintSolver constraint info -> (SolveResult info, LogEntries)
solve options constraints (ConstraintSolver f) = f options constraints

---

onlySolveConstraints :: 
   ( HasTI m info
   , HasBasic m info
   , HasSubst m info
   , HasQual m info
   , TypeConstraintInfo info
   , Solvable constraint m
   , MonadState s m
   , SolveState s
   , MonadWriter LogEntries m
   ) =>
     [constraint] -> m ()

onlySolveConstraints cs = 
   do pushConstraints (liftConstraints cs)
      logState
      startSolving
      makeConsistent
      checkSkolems
      ambiguities
      logState

solveConstraints :: 
   ( HasTI m info
   , HasBasic m info
   , HasSubst m info
   , HasQual m info
   , TypeConstraintInfo info
   , Solvable constraint m
   , MonadState s m
   , SolveState s
   , MonadWriter LogEntries m
   ) =>
     SolveOptions ->
     [constraint] -> 
     m (SolveResult info)

solveConstraints options cs = 
   do initialize cs options
      onlySolveConstraints cs
      solveResult
 
solveResult :: 
   ( HasBasic m info
   , HasTI m info
   , HasSubst m info
   , HasQual m info
   , TypeConstraintInfo info
   ) => 
     m (SolveResult info)            
solveResult = 
   do uniqueAtEnd <- getUnique
      errs        <- getLabeledErrors
      qs          <- allQualifiers
      sub         <- fixpointSubst
      ts          <- allTypeSchemes        
      return (SolveResult uniqueAtEnd sub ts qs errs)

----------------------------------------------------------------------
-- Solve type constraints

data SolveResult info =  
   SolveResult { uniqueFromResult       :: Int
               , substitutionFromResult :: FixpointSubstitution
               , typeschemesFromResult  :: M.Map Int (Scheme Predicates)
               , qualifiersFromResult   :: Predicates
               , errorsFromResult       :: [(info, ErrorLabel)]
               }

instance Empty (SolveResult info) where 
   empty = emptyResult 0

emptyResult :: Int -> SolveResult info
emptyResult unique = SolveResult unique emptyFPS M.empty empty []

combineResults :: SolveResult info -> SolveResult info -> SolveResult info
combineResults (SolveResult _ s1 ts1 qs1 er1) (SolveResult unique s2 ts2 qs2 er2) = 
   SolveResult unique (disjointFPS s1 s2) (ts1 `M.union` ts2) (qs1 ++ qs2) (er1++er2)

--------------------------------------------------------------------------------  

data SolveOptions = SolveOptions_ 
   { 
     -- initial values
     uniqueCounter    :: Int
   , typeSynonyms     :: OrderedTypeSynonyms
   , classEnvironment :: ClassEnvironment
   
   -- optional settings
   , setStopAfterFirstError :: Bool -- see Basic
   , setCheckConditions     :: Bool -- see Basic
   }

solveOptions :: SolveOptions
solveOptions = SolveOptions_
   { uniqueCounter          = -1
   , typeSynonyms           = noOrderedTypeSynonyms
   , classEnvironment       = standardClasses
   , setStopAfterFirstError = currentValue stopOption
   , setCheckConditions     = currentValue checkOption
   } 

initialize :: (HasBasic m info, HasQual m info, HasTI m info, Substitutable a) => a -> SolveOptions -> m ()
initialize cs options = 
   do setUnique           unique
      setTypeSynonyms     (typeSynonyms options)
      setClassEnvironment (classEnvironment options)
      setOption stopAfterFirstError (setStopAfterFirstError options)
      setOption checkConditions     (setCheckConditions options)
 where
   unique
      | uniqueCounter options < 0 = 1 + maximum (-1 : ftv cs) 
      | otherwise                 = uniqueCounter options

----------------------
-- Basic Monad

type BasicMonad f = StateFixT (f ()) (Writer LogEntries)

newtype LogEntries = LogEntries ([LogEntry] -> [LogEntry])
data    LogEntry   = LogEntry { priority :: Int, msg :: String }

noLogEntries :: LogEntries
noLogEntries = LogEntries id

singleEntry :: Int -> String -> LogEntries
singleEntry i s = LogEntries (LogEntry i s:)

evalBasicMonad :: Empty (f () (BasicMonad f)) => BasicMonad f a -> (a, LogEntries)
evalBasicMonad = runWriter . flip evalStateFixT empty

instance Monoid LogEntries where
   mempty = LogEntries id
   mappend (LogEntries f) (LogEntries g) = LogEntries (f . g)

instance Show LogEntry where
   show = msg

instance Show LogEntries where
   show (LogEntries f) = unlines (map show (f [])) 

logMsg :: MonadWriter LogEntries m => String -> m ()
logMsg = logMsgPrio 5

logMsgPrio :: MonadWriter LogEntries m => Int -> String -> m ()
logMsgPrio i s =
   let entry = LogEntry { priority = i, msg = s }
   in tell (LogEntries (entry:))

-- |Print the current state and add this as a debug message. 
logState :: (MonadState s m, SolveState s, MonadWriter LogEntries m) => m ()
logState = 
   do xs <- allStates
      ys <- allOptions
      let hline        = replicate 80 '-'
          options      = "Solver options:\n" ++ indent (unlines ys)
          f i (name,s) = show i ++ ". " ++ name ++ "\n" ++ indent s
          indent       = unlines . map ("      "++) . lines
      logMsg (unlines $ hline : options : zipWith f [1::Int ..] xs ++ [hline])