{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.Overloading where

import Top.Types hiding (contextReduction)
import qualified Top.Types (contextReduction)
import Top.Constraint.Information
import Top.Implementation.General
import Top.Interface.TypeInference (getTypeSynonyms, HasTI, getSkolems)
import Top.Interface.Basic
import Top.Interface.Substitution
import Top.Interface.Qualification
import Top.Monad.Select
import Top.Util.Embedding
import qualified Data.Map as M
import Data.Maybe
import Data.List (intersperse, (\\), partition)

------------------------------------------------------------------------
-- (I)  Algebraic data type

data OverloadingState info = OverloadingState 
   { classEnvironment    :: ClassEnvironment            -- ^ All known type classes and instances
   , predicateMap        :: PredicateMap info           -- ^ Type class assertions
   , typeClassDirectives :: TypeClassDirectives info    -- ^ Directives for type class assertions
   }
   
------------------------------------------------------------------------
-- (II)  Instance of SolveState (Empty, Show)

instance Empty (OverloadingState info) where
   empty = OverloadingState 
      { classEnvironment    = emptyClassEnvironment
      , predicateMap        = empty
      , typeClassDirectives = []
      }

instance Show (OverloadingState info) where
   show s = unlines [ "class environment: " ++ concat (intersperse "," (M.keys (classEnvironment s)))
                    , "directives: " ++ show (typeClassDirectives s)
                    , "predicates: " ++ show (predicateMap s)
                    ] 

instance Show info => SolveState (OverloadingState info) where 
   stateName _ = "OverloadingState State"
   
------------------------------------------------------------------------
-- (III)  Embeddings

instance Embedded ClassQual (OverloadingState info) (OverloadingState info)                where embedding = idE
instance Embedded ClassQual (Simple (OverloadingState info) x m) (OverloadingState info)   where embedding = fromFstSimpleE embedding 

------------------------------------------------------------------------
-- (IV)  Instance declaration

instance ( MonadState s m
         , HasBasic m info
         , HasTI    m info
         , TypeConstraintInfo info
         , Embedded ClassQual s (OverloadingState info)
         ) =>
           HasQual (Select (OverloadingState info) m) info where

   setClassEnvironment env =
      modify (\s -> s { classEnvironment = env })
      
   getClassEnvironment =
      gets classEnvironment
      
   proveQualifier info p =
      modifyPredicateMap (\qm -> qm { globalQualifiers = (p, info) : globalQualifiers qm })

   assumeQualifier info p =
      modifyPredicateMap (\qm -> qm { globalAssumptions = (p, info) : globalAssumptions qm })

   changeQualifiers f =
      do let g = mapM (\(p, info) -> f p >>= \new -> return (new, info) )
         as <- gets (globalQualifiers    . predicateMap) >>= g
         bs <- gets (globalGeneralizedQs . predicateMap) >>= g
         cs <- gets (globalAssumptions   . predicateMap) >>= g
         modifyPredicateMap (\qm -> qm { globalQualifiers    = as 
                                       , globalGeneralizedQs = bs
                                       , globalAssumptions   = cs })

   allQualifiers = 
      do syns     <- select getTypeSynonyms
         classEnv <- getClassEnvironment
         qmap     <- gets predicateMap
         let ps = globalQualifiers qmap ++ globalGeneralizedQs qmap ++ globalAssumptions qmap
         return (fst (Top.Types.contextReduction syns classEnv (map fst ps)))
         
   generalizeWithQualifiers monos tp =
      do preds1 <- proveQsSubst
         preds2 <- generalizedQsSubst
         let is       = ftv tp \\ ftv monos
             p        = any (`elem` is) . ftv . fst
             (as, bs) = partition p preds1
             cs       = filter    p preds2
         modifyPredicateMap (\qm -> qm { globalQualifiers = bs, globalGeneralizedQs = as ++ globalGeneralizedQs qm })
         return (generalize monos (map fst (as ++ cs) .=>. tp))
         

   -- improveQualifiersFinal -- use Default directives
   
   simplifyQualifiers =
      do preds       <- proveQsSubst
         assumptions <- assumeQsSubst
         syns        <- select getTypeSynonyms
         classEnv    <- getClassEnvironment
         directives  <- gets typeClassDirectives
         new         <- select (simplify syns classEnv directives preds)
         let final = filter (not . entail syns classEnv (map fst assumptions) . fst) new
         modifyPredicateMap (\qm -> qm { globalQualifiers = final })
 
   ambiguousQualifiers =
      do ps <- proveQsSubst
         select (ambiguous ps)
         
------------------------------------------------------------------------
-- (IV)  Helper-functions

simplify :: (HasTI m info, TypeConstraintInfo info, HasBasic m info)
               => OrderedTypeSynonyms -> ClassEnvironment -> TypeClassDirectives info -> [(Predicate, info)] -> m [(Predicate, info)]
simplify syns classEnv directives psNew = 
   do let loopIn t@(p@(Predicate className _), info)
             | inHeadNormalForm p = return [t]
             | otherwise =                         
                  case byInstance syns classEnv p of
                     Just ps -> 
                        loopInList [ (q, parentPredicate p info) | q <- ps ]
                     Nothing ->
                        let nevers  = [ (q, i) | NeverDirective q i <- directives, isJust (matchPredicates syns p q) ]
                            newInfo = 
                               case nevers of 
                                  tuple:_ -> neverDirective tuple info
                                  [] -> case [ i | CloseDirective s i <- directives, s == className ] of
                                           [i] -> closeDirective (className, i) info
                                           _   -> unresolvedPredicate p info
                        in addLabeledError unresolvedLabel newInfo >> return []
                
          loopInList ts = 
             do psList <- mapM loopIn ts
                return (concat psList)
                
          loopSc rs [] = rs
          loopSc rs (x:xs) 
             | scEntail classEnv (map fst (rs++xs)) (fst x)
                  = loopSc rs xs
             | otherwise                    
                  = loopSc (x:rs) xs
                
          testDisjoints [] = return []
          testDisjoints (t@(Predicate className tp, info):ts) =
             let f t'@(Predicate className' tp', info') = 
                    case [ i | tp == tp', DisjointDirective ss i <- directives, className `elem` ss, className' `elem` ss ] of
                       [] -> return ([t'], True)
                       infodir : _ ->
                          do addLabeledError disjointLabel (disjointDirective (className, info) (className', info') infodir)
                             return ([], False)
                             
             in do result <- mapM f ts
                   let (list, bs) = unzip result
                   rest <- testDisjoints (concat list)
                   return $ if and bs then t : rest else rest
                
      hnf <- loopInList psNew
      testDisjoints (loopSc [] hnf)
      
ambiguous :: (HasBasic m info, HasTI m info, TypeConstraintInfo info) 
                => [(Predicate, info)] -> m ()
ambiguous listStart =
   do skolems <- getSkolems
      let skolemPairs = [ (is, info) | (is, info, _) <- skolems ]
      
          reportAmbiguous (p, info) = 
             addLabeledError ambiguousLabel (ambiguousPredicate p info)
             
          reportMissing pair info2 =
             addLabeledError missingInSignatureLabel (predicateArisingFrom pair info2)
          
          f pair@(Predicate _ (TVar i), _) = 
             case [ info2 | (is, info2) <- skolemPairs, i `elem` is ] of
                info2:_ -> reportMissing pair info2
                _       -> reportAmbiguous pair
          f pair = reportAmbiguous pair

      mapM_ f listStart

{-
   -- try to use a defaulting directive before reporting an error message
   tryToDefault (i, ts) =
      do candidates <- 
            let f (Predicate cn _) = 
                   case [ (tps, info) | DefaultDirective s tps info <- directives, s == cn ] of 
                      [(tps, info)] ->
                         let op result tp = 
                                do let sub = singleSubstitution i tp
                                   let b = entailList syns classEnv [] [ sub |-> x | (x, _) <- ts ]
                                   return $ if b then (tp, info) : result else result
                         in foldM op [] (reverse tps)
                      _ -> return []
             in mapM (f . fst) ts
                    
         case [ x | x:_ <- candidates ] of
            (tp, info) : rest | all (tp ==) (map fst rest) -> 
               do solveConstraint ( TVar i .==. tp $ info )
                  makeSubstConsistent -- ??
                  return []
                  
            _ -> return ts -}
      
modifyPredicateMap :: MonadState (OverloadingState info) m => (PredicateMap info -> PredicateMap info) -> m ()
modifyPredicateMap f = 
   modify (\s -> s { predicateMap = f (predicateMap s) })

proveQsSubst, assumeQsSubst, generalizedQsSubst :: 
   (MonadState s m, Embedded ClassQual s (OverloadingState info) {-, MonadState s m, HasSubst m info -}) 
      => Select (OverloadingState info) m [(Predicate, info)]

proveQsSubst       = gets (globalQualifiers    . predicateMap) -- >>= select . mapM substPredicate
assumeQsSubst      = gets (globalAssumptions   . predicateMap) -- >>= select . mapM substPredicate
generalizedQsSubst = gets (globalGeneralizedQs . predicateMap) -- >>= select . mapM substPredicate

substPredicate :: HasSubst m info => (Predicate, info) -> m (Predicate, info)
substPredicate (p, info) = 
   do new <- applySubst p
      return (new, info)

-- Type class directives
type TypeClassDirectives info = [TypeClassDirective info]

data TypeClassDirective info 
   = NeverDirective     Predicate  info
   | CloseDirective     String     info
   | DisjointDirective  [String]   info
   | DefaultDirective   String Tps info

instance Show (TypeClassDirective info) where
   show _ = "<<type class directive>>"
   
-- Predicate map
data PredicateMap info = 
   PredicateMap
      { globalQualifiers    :: [(Predicate, info)]
      , globalGeneralizedQs :: [(Predicate, info)]
      , globalAssumptions   :: [(Predicate, info)]
      }
     
instance Show (PredicateMap info) where
   show qm = 
      let f (s, sf)
             | null ps   = []
             | otherwise = ["   " ++ s ++ ": " ++ concat (intersperse "," (map (show . fst) ps))]
            where ps = sf qm 
      in unlines $ concatMap f 
            [ ("qualifiers"            , globalQualifiers)
            , ("generalized qualifiers", globalGeneralizedQs)
            , ("assumptions"           , globalAssumptions)
            ]
 
instance Empty (PredicateMap info) where
   empty = PredicateMap { globalQualifiers = [], globalGeneralizedQs = [], globalAssumptions = [] }
   
instance Substitutable (PredicateMap info) where
   sub |-> (PredicateMap as bs cs) = 
      let as' = [ (sub |-> a, info) | (a, info) <- as ]
          bs' = [ (sub |-> b, info) | (b, info) <- bs ]
          cs' = [ (sub |-> c, info) | (c, info) <- cs ]
      in PredicateMap as' bs' cs'
   ftv (PredicateMap as bs cs) = ftv (map fst $ as ++ bs ++ cs)

unresolvedLabel :: ErrorLabel
unresolvedLabel = ErrorLabel "unresolved predicate"

disjointLabel :: ErrorLabel
disjointLabel = ErrorLabel "disjoint predicates"

ambiguousLabel :: ErrorLabel
ambiguousLabel = ErrorLabel "ambiguous predicate" 

missingInSignatureLabel :: ErrorLabel
missingInSignatureLabel = ErrorLabel "predicate missing in signature" 