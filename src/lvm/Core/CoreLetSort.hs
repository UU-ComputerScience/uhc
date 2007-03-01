{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreLetSort.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- Determine which bindings are really recursive and which are not.
-- maintains free variable information & normalised structure
----------------------------------------------------------------
module Lvm.Core.CoreLetSort( coreLetSort ) where

import Lvm.Common.TopSort( topSort )
import Lvm.Common.Id     ( Id )
import Lvm.Common.IdSet  ( IdSet, elemSet, foldSet )
import Lvm.Core.Core

----------------------------------------------------------------
-- coreLetSort
-- pre: [coreFreeVar] all let bindings are annotated with their free variables
--
-- transform a @Rec@ bindings into the smallest @NonRec@ and @Rec@ bindings.
----------------------------------------------------------------
coreLetSort :: CoreModule -> CoreModule
coreLetSort mod
  = mapExpr lsExpr mod

lsExpr :: Expr -> Expr
lsExpr expr
  = case expr of
      Let (Strict (Bind id rhs)) expr
        -> Let (Strict (Bind id (lsExpr rhs))) (lsExpr expr)
      Let binds expr
        -> let bindss = sortBinds binds
           in foldr Let (lsExpr expr) bindss
      Match id alts
        -> Match id (lsAlts alts)
      Lam id expr
        -> Lam id (lsExpr expr)
      Ap expr1 expr2
        -> Ap (lsExpr expr1) (lsExpr expr2)
      Con (ConTag tag arity)
        -> Con (ConTag (lsExpr tag) arity)
      Note n expr
        -> Note n (lsExpr expr)
      other
        -> other

lsAlts alts
  = mapAlts (\pat expr -> Alt pat (lsExpr expr)) alts

----------------------------------------------------------------
-- topological sort let bindings
----------------------------------------------------------------
sortBinds :: Binds -> [Binds]
sortBinds (Rec bindsrec)
  = let binds  = map (\(Bind id rhs) -> (id,rhs)) bindsrec
        names  = zip (map fst binds) [0..]
        edges  = concat (map (depends names) binds)
        sorted = topSort (length names-1) edges
        binds'  = map (map (binds!!)) sorted
        binds'' = map (map (\(id,expr) -> (id,lsExpr expr))) binds'
    in  map toBinding binds'' -- foldr sortLets (lsExpr expr) binds''

sortBinds binds
  = [mapBinds (\id expr -> Bind id (lsExpr expr)) binds]

toBinding [(id,rhs)]
  | not (elemSet id (freeVar rhs)) = NonRec (Bind id rhs)
toBinding binds
  = Rec (map (uncurry Bind) binds)


type Vertex = Int

depends :: [(Id,Vertex)] -> (Id,Expr) -> [(Vertex,Vertex)]
depends names (v,expr)
  = foldSet depend [] (freeVar expr)
  where
    index     = case lookup v names of
                  Just i  -> i
                  Nothing -> error "CoreLetSort.depends: id not in let group??"

    depend x ds   = case lookup x names of
                      Just i  -> (index,i):ds
                      Nothing -> ds

freeVar expr
  = case expr of
      Note (FreeVar fv) expr  -> fv
      other                   -> error "CoreLetSort.freeVar: no annotation. Do coreFreeVar first?"
