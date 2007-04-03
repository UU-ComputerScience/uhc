-----------------------------------------------------------------------------
-- Pred:		Predicates
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------

module Pred
  ( Qual(..)
  , Pred(..)
  , ClassEnv(..)
  , Inst
  , EnvTransformer
  , (<:>)
  , addInst
  , addPreludeClasses
  , addClass
  , reduce
  , initialEnv
  )

 where
import List(union,(\\),concatMap)
import Monad(msum)
import Debug.Trace
import Maybe(catMaybes, fromJust)
import Id
import Kind
import Type
import Subst
import Unify
import PPrint

import GraphSolver
import Data.Graph.Inductive.Graph(nodes,edges, outdeg, lab, Graph)
import Data.Graph.Inductive.Graphviz(graphviz')

data Qual t = [Pred] :=> t
              deriving Eq

instance PPrint t => PPrint (Qual t) where
  pprint (ps  :=> t) = (pprint ps <+> text ":=>") $$ nest 2 (parPprint t)

data Pred   = IsIn Id Type  
            | And [Pred]
            | Prove Pred Int
            | Assume Pred Int
            deriving (Eq, Ord)

truePred = And []

instance PPrint Pred where
  pprint (IsIn i t)  = text "isIn1" <+> text ("c" ++ i) <+> parPprint t 
  pprint (And [])    = text "True"
  pprint (And ps)    = text "And"   <+> pplist ps
  pprint (Prove p i) = text "Prove (" <> pprint p <> text ")" <+> text (show i)
  pprint (Assume p i)= text "Assume (" <> pprint p <> text ")" <+> text (show i)

instance Show Pred where
  show = render . pprint

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (Prove p i) = Prove (apply s p) i
  apply s (Assume p i) = Assume (apply s p) i
  apply s (IsIn i t) = IsIn i (apply s t)
  apply s (And ps)   = And (map (apply s) ps)
  tv (Prove p _)     = tv p
  tv (Assume p _)     = tv p
  tv (IsIn i t)      = tv t
  tv (And ps)        = concatMap tv ps

mguPred, matchPred :: Pred -> Pred -> Maybe Subst
mguPred             = lift mgu
matchPred           = lift match

lift m (IsIn i t) (IsIn i' t')
         | i == i'   = m t t'
         | otherwise = fail "classes differ"
lift m (And ps) (And ps')
  = do subst <- msum (zipWith (lift m) ps ps') 
       return subst

-----------------------------------------------------------------------------

type Inst     = Qual Pred

data ClassEnv = ClassEnv { classes  :: RuleEnv Pred String,
                           defaults :: [Type] }

initialEnv :: ClassEnv
initialEnv  = ClassEnv { classes  = \c -> [],
                         defaults = [tInteger, tDouble] }

modify       :: ClassEnv -> RuleEnv Pred String -> ClassEnv
modify ce f = ce{classes = \c -> (f c) ++ (classes ce c)}

type EnvTransformer = ClassEnv -> ClassEnv

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = g . f $ ce

addClass :: Id -> [Id] -> EnvTransformer
addClass id is ce = 
  modify ce add
  where add p@(IsIn id' t) = if id == id' 
                             then zip3 (map (\c -> IsIn c t) is) (repeat p) (repeat "sup")
                             else []
        add _              = []

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn id _) ce = 
  modify ce add
  where add q@(IsIn id' _) = if id == id'
                             then case tryInst ps p q of
                                    Nothing     -> []
                                    (Just [nd]) -> [(q, nd, "inst")]
                                    (Just nds)  -> (q, And nds, "inst") : zip3 (repeat (And nds)) nds (repeat "inst")
                             else []
        add _              = []

        tryInst ps p q = do u <- matchPred p q
                            return (map (apply u) ps)
addPreludeClasses :: EnvTransformer
addPreludeClasses  = addCoreClasses
                     <:> addNumClasses
                     <:> addProveRule
                     <:> addAssumeRule

addProveRule :: EnvTransformer
addProveRule ce = modify ce proveRule
  where proveRule p@(Prove q _) = [(p, q, "prv")]
        proveRule _             = []

addAssumeRule :: EnvTransformer
addAssumeRule ce = modify ce assumeRule
  where assumeRule p@(Assume q _) = [(q, p, "ass")]
        assumeRule _             = []

addCoreClasses ::   EnvTransformer
addCoreClasses  =   addClass "Eq" []
                <:> addClass "Ord" ["Eq"]
                <:> addClass "Show" []
                <:> addClass "Read" []
                <:> addClass "Bounded" []
                <:> addClass "Enum" []
                <:> addClass "Functor" []
                <:> addClass "Monad" []

addNumClasses  ::   EnvTransformer
addNumClasses   =   addClass "Num" ["Eq", "Show"]
                <:> addClass "Real" ["Num", "Ord"]
                <:> addClass "Fractional" ["Num"]
                <:> addClass "Integral" ["Real", "Enum"]
                <:> addClass "RealFrac" ["Real", "Fractional"]
                <:> addClass "Floating" ["Fractional"]
                <:> addClass "RealFloat" ["RealFrac", "Floating"]

exampleInsts ::  EnvTransformer
exampleInsts =   addPreludeClasses
             <:> addInst [] (IsIn "Ord" tUnit)
             <:> addInst [] (IsIn "Ord" tChar)
             <:> addInst [] (IsIn "Ord" tInt)
             <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star)),
                          IsIn "Ord" (TVar (Tyvar "b" Star))]
                         (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
                                           (TVar (Tyvar "b" Star))))

-----------------------------------------------------------------------------
-- GvdG: Alternative context reduction, using context graphs
-----------------------------------------------------------------------------

reduce :: ClassEnv -> [Pred] -> [Pred]
-- reduce ce ps = trace (show ps) (remaining gr)
reduce ce ps = trace (graphviz' gr ++ "\n\n") (remaining gr)
--reduce ce ps = trace (show (remaining gr)) (remaining gr)
--reduce ce ps = remaining gr
 where (gr, nm) = constructGraphT (classes ce) ps

remaining :: Graph gr => gr Pred b -> [Pred]
remaining tree
  = filter (\p -> p /= truePred && not (isAssumePred p)) preds
  where nds = filter (\n -> (outdeg tree n) == 0) (nodes tree)
        preds = map (fromJust . lab tree) nds

isAssumePred (Assume  _ _) = True
isAssumePred _             = False
