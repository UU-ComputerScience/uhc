{-| Module      :  BindingGroupAnalysis
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    Binding groups (mutually recursive function definitions)
-}

-- To do: clean up this module. Also see BGA for kind inferencing

module Helium.StaticAnalysis.Inferencers.BindingGroupAnalysis where

import Helium.Syntax.UHA
import Helium.StaticAnalysis.Miscellaneous.TypeConstraints
import Helium.StaticAnalysis.Miscellaneous.ConstraintInfo
import Lvm.Common.TopSort (topSort)
import Top.Types
import Top.Ordering.Tree
import qualified Data.Map as M
import Data.List

type Assumptions        = M.Map Name [(Name,Tp)]
type PatternAssumptions = M.Map Name Tp
type Monos              = Tps

noAssumptions :: M.Map Name a
noAssumptions = M.empty

listToAssumptions :: [(Name, Tp)] -> Assumptions
listToAssumptions list =
   foldr combine noAssumptions [ M.fromList [(n, [tuple])] | tuple@(n, _) <- list ]

combine :: Assumptions -> Assumptions -> Assumptions
combine = M.unionWith (++)

single :: Name -> Tp -> Assumptions
single n t = M.singleton n [(n,t)]

type BindingGroups = [BindingGroup]
type BindingGroup  = (PatternAssumptions, Assumptions, ConstraintSets)
type InheritedBDG  = [(Names, (Monos, Int))]

emptyBindingGroup :: BindingGroup
emptyBindingGroup = 
   (noAssumptions, noAssumptions, [])

combineBindingGroup :: BindingGroup -> BindingGroup -> BindingGroup
combineBindingGroup (e1,a1,c1) (e2,a2,c2) = 
   (e1 `M.union` e2, a1 `combine` a2, c1++c2)

concatBindingGroups :: BindingGroups -> BindingGroup
concatBindingGroups = foldr combineBindingGroup emptyBindingGroup

-- |Input for binding group analysis
type InputBDG     = (Bool, Int, Int, Monos, M.Map Name TpScheme, Maybe (Assumptions, ConstraintSets), Int)
type OutputBDG    = (Assumptions, ConstraintSet, InheritedBDG, Int, Int, M.Map Name (Sigma Predicates))

performBindingGroup :: InputBDG -> BindingGroups -> OutputBDG
performBindingGroup (topLevel, currentChunk, uniqueChunk, monos, typeSignatures, chunkContext, unique) groups = 
   variableDependencies 

   where   
        bindingGroupAnalysis :: BindingGroups -> BindingGroups
        bindingGroupAnalysis cs =
           let explicits = M.keys typeSignatures
               indexMap = concat (zipWith f cs [0..])
               f (env,_,_) i = [ (n,i) | n <- M.keys env, n `notElem` explicits ]
               edges    = concat (zipWith f' cs [0..])
               f' (_,ass,_) i = [ (i,j)| n <- M.keys ass, (n',j) <- indexMap, n==n' ]
               list = topSort (length cs-1) edges
           in map (concatBindingGroups . map (cs !!)) list

        chunkedBindingGroups  :: [(Int, BindingGroup)]
        chunkedBindingGroups = 
           zip [uniqueChunk..] (bindingGroupAnalysis groups) ++ 
           case chunkContext of 
              Nothing     -> []
              Just (a, c) -> [(currentChunk, (M.empty, a, c))]
        
        monomorphicNames :: [Name]
        monomorphicNames = 
           let initial = let f (e, a, _) = if any (`elem` ftv monos) (ftv $ map snd $ concat $ M.elems a)
                                             then M.keys e
                                             else []
                         in concatMap f groups
               expand [] _       = []
               expand (n:ns) gps = let (xs, ys)  = partition p gps
                                       p (_,a,_) = n `elem` M.keys a
                                       f (e,_,_) = M.keys e
                                   in n : expand (concatMap f xs ++ ns) ys
           in expand initial groups
                          
        variableDependencies :: OutputBDG
        variableDependencies = 
           let (aset, cset, mt, newUnique, fm) = foldr op initial chunkedBindingGroups
           in (aset, cset, mt, uniqueChunk + length groups, newUnique, fm)

          where        
            initial = (noAssumptions, emptyTree, [], unique, M.empty)
          
            op (cnr, (e, a, c)) (aset, cset, mt, un, fm) =
               let (cset1,e'   )  = (typeSignatures !:::! e) monos cinfoBindingGroupExplicitTypedBinding                
                   (cset2,a'   )  = (typeSignatures .:::. a) (cinfoBindingGroupExplicit monos (M.keys e))
                   (cset3,a''  )  = (e' .===. a')            cinfoSameBindingGroup
                   
                   implicits      = zip [un..] (M.assocs e')
                   implicitsFM    = M.fromList [ (name, SigmaVar sv) | (sv, (name, _)) <- implicits ]
                   cset4          = genConstraints monos cinfoGeneralize implicits                   
                   (cset5, aset') = (implicitsFM .<==. aset) cinfoBindingGroupImplicit
                                    
                   monomorphic    = not topLevel -- simplification: was 
                                                 -- any (`elem` monomorphicNames) (keysFM e) || cnr == currentChunk

                   constraintTree =
                      StrictOrder 
                         ( (if monomorphic then id else Chunk cnr)
                         $ StrictOrder
                              ( (cset1 ++ cset2 ++ cset3) .>>. Node (reverse c) )
                              (listTree cset4))
                         (cset5 .>>. cset)
               in
                  ( a'' `combine` aset'
                  , constraintTree
                  , (M.keys e, (M.elems e', if monomorphic then currentChunk else cnr)) : mt
                  , un + M.size e'
                  , implicitsFM `M.union` fm
                  ) 

findMono :: Name -> InheritedBDG -> Monos
findMono n = let p = elem n . fst
             in fst . snd . head . filter p                  

getMonos :: TypeConstraints info -> Monos
getMonos tcs = [ TVar i | tc <- tcs, i <- ftv tc ]

findCurrentChunk :: Name -> InheritedBDG -> Int
findCurrentChunk n = let p = elem n . fst
                     in snd . snd . head . filter p