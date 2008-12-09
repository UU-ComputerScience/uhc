{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreFreeVar.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- Annotate let bound expression with their free variables
----------------------------------------------------------------
module Lvm.Core.CoreFreeVar( coreFreeVar ) where

import Lvm.Common.Standard( warning )
import Lvm.Common.Id   ( Id )
import Lvm.Common.IdSet( IdSet, emptySet, isEmptySet
            , setFromMap, listFromSet, setFromList
            , elemSet, insertSet, unionSets, unionSet, deleteSet, diffSet )
import Lvm.Core.Core

----------------------------------------------------------------
-- coreFreeVar
-- Annotate let bound expression with their free variables
----------------------------------------------------------------
coreFreeVar :: CoreModule -> CoreModule
coreFreeVar mod
  = mapExpr (fvDeclExpr (globalNames mod)) mod

fvDeclExpr globals expr
  = let (expr',fv) = fvExpr globals expr
    in if (isEmptySet fv)
        then Note (FreeVar fv) expr'
        else warning ("CoreFreeVar.fvDeclExpr: top-level binding with free variables: "
                      ++ show (listFromSet fv)) (Note (FreeVar fv) expr')



fvBindExpr globals expr
  = let (expr',fv) = fvExpr globals expr
    in  (Note (FreeVar fv) expr',fv)

fvExpr :: IdSet -> Expr -> (Expr,IdSet)
fvExpr globals expr
  = case expr of
      Let binds expr
        -> let (expr',fv)       = fvExpr globals expr
               (binds',fvbinds) = fvBinds globals binds
           in (Let binds' expr', diffSet (unionSet fvbinds fv) (setFromList (binders (listFromBinds binds))))
      Lam id expr
        -> let (expr',fv) = fvExpr globals expr
           in  (Lam id expr',deleteSet id fv)
      Match id alts
        -> let (alts',fvalts) = fvAlts globals alts
           in  (Match id alts',insertSet id fvalts)
      Ap expr1 expr2
        -> let (expr1',fv1)   = fvExpr globals expr1
               (expr2',fv2)   = fvExpr globals expr2
           in  (Ap expr1' expr2', unionSet fv1 fv2)
      Var id
        -> if (elemSet id globals)
            then (expr,emptySet)
            else (expr,insertSet id emptySet)
      Con (ConTag tag arity)
        -> let (tag',fv) = fvExpr globals tag
           in (Con (ConTag tag' arity),fv)
      Note n expr
        -> let (expr',fv) = fvExpr globals expr
           in  (Note n expr',fv)
      other
        -> (other,emptySet)


fvAlts :: IdSet -> Alts -> (Alts,IdSet)
fvAlts globals alts
  = let alts' = mapAlts (\pat expr -> let (expr',fv)   = fvExpr globals expr                                          
                                      in  Alt pat (Note (FreeVar fv) expr')) alts
        fvs   = unionSets (map (\(Alt pat expr) -> diffSet (freeVar expr) (patBinders pat)) (alts'))
    in  (alts',fvs)

fvBinds :: IdSet -> Binds -> (Binds,IdSet)
fvBinds globals binds
  = case binds of
      NonRec (Bind id expr)
        -> nonrec NonRec id expr
      Strict (Bind id expr)
        -> nonrec Strict id expr
      other 
        -> let binds' = mapBinds (\id rhs -> Bind id (fst (fvBindExpr globals rhs))) binds
               fvs    = unionSets (map (\(Bind id rhs) -> freeVar rhs) (listFromBinds binds'))
           in  (binds',fvs)
  where
    nonrec make id expr
      = let (expr',fv) = fvBindExpr globals expr
        in if (elemSet id fv)
            then error "CoreFreeVar.fvBinds: non-recursive binding refers to itself? (do CoreNoShadow first?)"
            else (make (Bind id expr'),fv)


freeVar expr
  = case expr of
      Note (FreeVar fv) expr  -> fv
      other                   -> error "CoreFreeVar.freeVar: no free variable annotation"
