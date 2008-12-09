{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.General 
   ( module Top.Implementation.General
   , module Top.Util.Empty
   ) where

import Top.Util.Embedding
import Top.Util.Empty
import Top.Monad.Select

class (Show s, Empty s) => SolveState s where
   showState     :: s -> String
   stateName     :: s -> String
   stateOptions  :: s -> [String]
   collectStates :: s -> [(String, String)]
   
   showState       = show
   stateOptions _  = []
   collectStates s = [(stateName s, showState s)]

instance SolveState () where
   stateName     _ = "EmptyState" 
   collectStates _ = []

allStates :: (MonadState s m, SolveState s) => m [(String, String)]
allStates = gets collectStates

allOptions :: (MonadState s m, SolveState s) => m [String]
allOptions = gets stateOptions

----------------------
-- New

-- ToDo: replace And by infix type constructor (:^:)
-- ToDo: kind annotations for And, Simple, Fix
-- infixr 7 :^:

data And f g   x m = Compose (f (g x m) m)      | AndDummy (m ())
data Simple a  x m = Simple a x                 | SimpleDummy (m ())
data Fix g     x m = Fix (g m) x                | FixDummy (m ())

--- Empty
instance Empty (f (g x m) m) => Empty (And f g x m) where 
   empty = Compose empty
   
instance (Empty a, Empty x) => Empty (Simple a x m) where
   empty = Simple empty empty
   
instance (Empty (g m), Empty x) => Empty (Fix g x m) where
   empty = Fix empty empty

-- Show
instance Show (f (g x m) m) => Show (And f g x m) where
   show (Compose a) = show a
   
instance (Show a, Show x) => Show (Simple a x m) where
   show (Simple a x) = show (a, x)
   
instance (Show (f m), Show x) => Show (Fix f x m) where
   show (Fix a x) = show (a, x)

-- SolveState
instance SolveState (f (g x m) m) => SolveState (And f g x m) where
   showState     (Compose a) = showState a
   stateName     (Compose a) = stateName a
   stateOptions  (Compose a) = stateOptions a
   collectStates (Compose a) = collectStates a

instance (SolveState a, SolveState x) => SolveState (Simple a x m) where
   showState     (Simple a x) = show (a, x)
   stateName     (Simple a x) = concat ["(", stateName a, ",", stateName x, ")"]
   stateOptions  (Simple a x) = stateOptions  a ++ stateOptions  x
   collectStates (Simple a x) = collectStates a ++ collectStates x

instance (SolveState (f m), SolveState x) => SolveState (Fix f x m) where
   showState     (Fix a x) = show (a, x)
   stateName     (Fix a x) = concat ["(", stateName a, ",", stateName x, ")"]
   stateOptions  (Fix a x) = stateOptions  a ++ stateOptions  x
   collectStates (Fix a x) = collectStates a ++ collectStates x

-- Embedded
instance Embedded c (f (g x m) m) s => Embedded c (And f g x m) s  where
   embedding = composeE (Embedding { getE = \(Compose a) -> a, changeE = \f (Compose a) -> Compose (f a) }) embedding 

instance Embedded c x s => Embedded c (Simple a x m) s where
   embedding = composeE (Embedding { getE = \(Simple _ b) -> b, changeE = \f (Simple a b) -> Simple a (f b) }) embedding
   
instance Embedded c x s => Embedded c (Fix a x m) s where
   embedding = composeE (Embedding { getE = \(Fix _ b) -> b, changeE = \f (Fix a b) -> Fix a (f b) }) embedding

fromFstFixE :: Embedding (g m) c -> Embedding (Fix g x m) c
fromFstFixE = composeE (Embedding { getE = \(Fix a _) -> a, changeE = \f (Fix a b) -> Fix (f a) b })

fromFstSimpleE :: Embedding a c -> Embedding (Simple a x m) c
fromFstSimpleE = composeE fstSimpleE

fstSimpleE :: Embedding (Simple a x m) a
fstSimpleE = Embedding { getE = \(Simple a _) -> a, changeE = \f (Simple a b) -> Simple (f a) b }