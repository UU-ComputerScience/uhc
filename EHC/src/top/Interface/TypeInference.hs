{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Interface.TypeInference where

import Top.Types
import Top.Monad.Select
import Top.Monad.StateFix
import Top.Interface.Basic
import Top.Interface.Substitution
import Top.Constraint.Information
import Data.List (intersect, sortBy, partition, groupBy)
import qualified Data.Map as M

------------------------------------------------------------------------
-- (I)  Class name and (dedicated) deselect function

data ClassTI = ClassTI

deTI :: (Embedded ClassTI (s (StateFixT s m)) t, Monad m) => Select t (StateFixT s m) a -> StateFixT s m a
deTI = deselectFor ClassTI

------------------------------------------------------------------------
-- (II)  Type class declaration

class Monad m => HasTI m info | m -> info where  

   -- unique counter
   getUnique           :: m Int
   setUnique           :: Int -> m ()  
   -- type synonyms
   setTypeSynonyms     :: OrderedTypeSynonyms -> m ()
   getTypeSynonyms     :: m OrderedTypeSynonyms 
   -- skolem variables
   getSkolems          :: m [([Int], info, Tps)] 
   setSkolems          :: [([Int], info, Tps)] -> m ()  
   -- type scheme map
   allTypeSchemes   :: m (M.Map Int (Scheme Predicates))
   getTypeScheme    :: Int -> m (Scheme Predicates)
   storeTypeScheme  :: Int -> Scheme Predicates -> m ()  

------------------------------------------------------------------------
-- (III)  Instance for solver monad

instance ( Monad m
         , Embedded ClassTI (s (StateFixT s m)) t
         , HasTI (Select t (StateFixT s m)) info
         ) => 
           HasTI (StateFixT s m) info where

   getUnique           = deTI $ getUnique
   setUnique           = deTI . setUnique
   -- type synonym
   setTypeSynonyms     = deTI . setTypeSynonyms
   getTypeSynonyms     = deTI $ getTypeSynonyms
   -- skolem variables
   getSkolems          = deTI $ getSkolems
   setSkolems          = deTI . setSkolems
   -- type scheme map
   allTypeSchemes      = deTI $ allTypeSchemes
   getTypeScheme       = deTI . getTypeScheme
   storeTypeScheme i   = deTI . storeTypeScheme i
   
------------------------------------------------------------------------
-- (IV)  Additional functions

nextUnique :: HasTI m info => m Int
nextUnique = 
   do i <- getUnique
      setUnique (i+1)
      return i

zipWithUniques :: HasTI m info => (Int -> a -> b) -> [a] -> m [b]
zipWithUniques f as = 
   do i <- getUnique
      setUnique (i+length as)
      return (zipWith f [i..] as) 
{-
addToProve :: HasTI m info => Predicate -> info -> m ()
addToProve p info = 
   do qm <- getQM
      putQM (qm { globalQualifiers = (p, info) : globalQualifiers qm })

addToAssume :: HasTI m info => Predicate -> info -> m ()
addToAssume p info = 
   do qm <- getQM
      putQM (qm { globalAssumptions = (p, info) : globalAssumptions qm })

generalizeWithPreds :: HasTI m info => Tps -> Tp -> m (Scheme Predicates)
generalizeWithPreds monos tp =
   do qm <- getQM
      let as = ftv tp \\ ftv monos
          ps = [ p | (p, _) <- globalQualifiers qm, any (`elem` as) (ftv p) ]
      return (generalize monos (ps .=>. tp))
      
type NeverDirective    info = (Predicate, info)
type CloseDirective    info = (String, info)
type DisjointDirective info = ([String], info)
type DefaultDirective  info = (String, (Tps, info))

data TypeClassDirectives info = TypeClassDirectives 
   { neverDirectives    :: [NeverDirective info]
   , closeDirectives    :: [CloseDirective info]
   , disjointDirectives :: [DisjointDirective info]
   , defaultDirectives  :: [DefaultDirective info]
   }

instance Show info => Show (TypeClassDirectives info) where
   show tcd = 
      let f title pf xs
             | null xs   = ""
             | otherwise = "\n   "++title++": "++concat (intersperse "; " (map pf xs))
          p1 (x, _) = show x
          p2 (x, _) = x
          p3 (x, _) = concat (intersperse "," x)
          p4 (cn, (tps, _)) = cn ++ " ("++concat (intersperse "," (map show tps)) ++ ")"
      in f "never"    p1 (neverDirectives tcd)    ++
         f "close"    p2 (closeDirectives tcd)    ++
         f "disjoint" p3 (disjointDirectives tcd) ++
         f "default"  p4 (defaultDirectives tcd) 
         
instance Empty (TypeClassDirectives info) where
   empty = TypeClassDirectives { neverDirectives = [], closeDirectives = [], disjointDirectives = [], defaultDirectives = [] }

addNeverDirective :: HasTI m info => NeverDirective info -> m ()
addNeverDirective x = 
   changeTCD (\s -> s { neverDirectives = x : neverDirectives s })
  
addCloseDirective :: HasTI m info => CloseDirective info -> m ()
addCloseDirective x =
   changeTCD (\s -> s { closeDirectives = x : closeDirectives s })

addDisjointDirective :: HasTI m info => DisjointDirective info -> m ()
addDisjointDirective x =
   changeTCD (\s -> s { disjointDirectives = x : disjointDirectives s })

addDefaultDirective :: HasTI m info => DefaultDirective info -> m ()
addDefaultDirective x =
   changeTCD (\s -> s { defaultDirectives = x : defaultDirectives s }) -}
      
-- * Instantiation and skolemization

addSkolem  :: HasTI m info => ([Int], info, Tps) -> m ()
addSkolem x = 
   do xs <- getSkolems
      setSkolems (x:xs)
      
instantiateM :: (HasTI m info, Substitutable a) => Forall a -> m a
instantiateM fa =
   do unique <- getUnique
      let (newUnique, a) = instantiate unique fa
      setUnique newUnique
      return a
      
skolemizeTruly :: (HasTI m info, Substitutable a) => Forall a -> m a
skolemizeTruly fa =
   do unique <- getUnique
      let (newUnique, a) = skolemize unique fa
      setUnique newUnique
      return a
      
skolemizeFaked :: (HasTI m info, Substitutable a) => info -> Tps -> Forall a -> m a
skolemizeFaked info monos fa =
   do unique <- getUnique
      let (newUnique, a) = instantiate unique fa
          new = ([ unique .. newUnique-1 ], info, monos)
      addSkolem new
      setUnique newUnique
      return a

getSkolemSubstitution :: HasTI m info => m MapSubstitution
getSkolemSubstitution =
   do skcs <- getSkolems
      return $ listToSubstitution [ (i, makeSkolemConstant i) | (is, _, _) <- skcs, i <- is ]
  
-- |First, make the substitution consistent. Then check the skolem constants(?)
makeConsistent :: (HasTI m info, HasBasic m info, HasSubst m info) => m ()
makeConsistent = makeSubstConsistent -- >> checkSkolems

checkSkolems :: (HasTI m info, HasSubst m info, HasBasic m info, TypeConstraintInfo info) => m ()
checkSkolems = 
   do xs    <- getSkolems
      list1 <- let f (is, info, monos) = 
                      do tps <- mapM findSubstForVar is
                         return (zip is tps, (info, monos))
               in mapM f xs
      
      -- skolem constant versus type constant
      let (list2, errs) = partition (all (isTVar . snd) . fst) list1
      mapM_ (addLabeledError skolemVersusConstantLabel . fst . snd) errs
      
      -- skolem constant versus a different skolem constant
      let problems = filter ((>1) . length)
                   . groupBy (\x y -> fst x == fst y)
                   . sortBy  (\x y -> fst x `compare` fst y)
                   $ [ (i, info) | (pairs, (info, _)) <- list2, (_, TVar i) <- pairs ]
          list3 = let is = concatMap (map fst) problems
                      p (pairs, _) = null (ftv (map snd pairs) `intersect` is)
                  in filter p list2
      mapM_  (addLabeledError skolemVersusSkolemLabel . snd . head) problems

      -- escaping skolem constants
      list4 <- let op rest this@(pairs, (info, monos)) =
                      do monos' <- applySubst monos
                         case ftv monos' `intersect` ftv (map snd pairs) of
                            []  -> return (this:rest)
                            esc -> do addLabeledError escapingSkolemLabel (escapedSkolems esc info)
                                      return rest
               in foldM op [] list3

      -- store the remaining skolem constants (that are consistent with the current substitution).
      let new = [ (concatMap (ftv . snd) pairs, info, monos) | (pairs, (info, monos)) <- list4 ]
      setSkolems new

skolemVersusConstantLabel :: ErrorLabel
skolemVersusConstantLabel = ErrorLabel "skolem versus constant" 

skolemVersusSkolemLabel :: ErrorLabel
skolemVersusSkolemLabel = ErrorLabel "skolem versus skolem" 

escapingSkolemLabel :: ErrorLabel
escapingSkolemLabel = ErrorLabel "escaping skolem"

replaceSchemeVar :: HasTI m info => Sigma Predicates -> m (Scheme Predicates)
replaceSchemeVar (SigmaVar i)    = getTypeScheme i
replaceSchemeVar (SigmaScheme s) = return s

findScheme :: HasTI m info => Sigma Predicates -> m (Scheme Predicates)
findScheme = replaceSchemeVar

---------------------------------------------------------------------
-- Global qualifier map
{-
data GlobalQM q info = 
   GlobalQM
      { globalQualifiers    :: [(q, info)]
      , globalGeneralizedQs :: [(q, info)]
      , globalAssumptions   :: [(q, info)]
      }
     
instance (Show qs, Show info) => Show (GlobalQM qs info) where
   show qm = 
      let f (s, sf)
             | null ps   = []
             | otherwise = ["   " ++ s ++ ": " ++ foldr1 (\x y -> x++", "++y) (map g ps)]
            where ps = sf qm 
          g (p, info) = show p ++ "{" ++ show info ++ "}"
      in unlines $ concatMap f 
            [ ("qualifiers"            , globalQualifiers)
            , ("generalized qualifiers", globalGeneralizedQs)
            , ("assumptions"           , globalAssumptions)
            ]
 
instance Empty (GlobalQM qs info) where
   empty = GlobalQM { globalQualifiers = [], globalGeneralizedQs = [], globalAssumptions = [] }
   
instance Substitutable qs => Substitutable (GlobalQM qs info) where
   sub |-> (GlobalQM as bs cs) = 
      let as' = [ (sub |-> a, info) | (a, info) <- as ]
          bs' = [ (sub |-> b, info) | (b, info) <- bs ]
          cs' = [ (sub |-> c, info) | (c, info) <- cs ]
      in GlobalQM as' bs' cs'
   ftv (GlobalQM as bs cs) = ftv (map fst $ as ++ bs ++ cs) -}