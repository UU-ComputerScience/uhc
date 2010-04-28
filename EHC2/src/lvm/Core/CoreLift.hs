{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreLift.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- Do "johnson" style lambda lifting
-- After this pass, each binding has either no free variables or no arguments.
-- maintains free variable information & normalised structure
----------------------------------------------------------------
module Lvm.Core.CoreLift ( coreLift ) where

import List    ( mapAccumL )
import Lvm.Common.Standard( foldlStrict )

import Lvm.Common.Id      ( Id )
import Lvm.Common.IdMap   ( IdMap, elemMap, extendMap, lookupMap, emptyMap )
import Lvm.Common.IdSet   ( IdSet, elemSet, listFromSet, emptySet, foldSet
               , unionSet, sizeSet, setFromList )
import Lvm.Core.Core

----------------------------------------------------------------
-- The environment maps variables to variables that should
-- be supplied as arguments at each call site
----------------------------------------------------------------
data Env  = Env IdSet (IdMap [Id])     -- primitives && the free variables to be passed as arguments

elemFree (Env prim env) id
  = elemMap id env

lookupFree :: Env -> Id -> [Id]
lookupFree (Env prim env) id
  = case lookupMap id env of
      Nothing -> []
      Just fv -> fv

isPrimitive :: Env -> Id -> Bool
isPrimitive (Env prim _) id
  = elemSet id prim

extendFree (Env prim env) id fv
  = Env prim (extendMap id fv env)

----------------------------------------------------------------
-- coreLift
-- pre: [coreFreeVar]  each binding is annotated with free variables
--      [coreNoShadow] there is no shadowing
----------------------------------------------------------------
coreLift :: CoreModule -> CoreModule
coreLift mod
  = mapExpr (liftExpr (Env primitives emptyMap)) mod
  where
    primitives  = externNames mod

liftExpr :: Env -> Expr -> Expr
liftExpr env expr
  = case expr of
      Let binds expr
        -> let (binds',env') = liftBinds env binds
           in Let binds' (liftExpr env' expr)
      Match id alts
        -> Match id (liftAlts env alts)
      Lam id expr
        -> Lam id (liftExpr env expr)
      Ap expr1 expr2
        -> Ap (liftExpr env expr1) (liftExpr env expr2)
      Var id
        -> foldlStrict (\e v -> Ap e (Var v)) expr (lookupFree env id)
      Con (ConTag tag arity)
        -> Con (ConTag (liftExpr env tag) arity)
      Note n e
        -> Note n (liftExpr env e)
      other
        -> other

liftAlts env alts
  = mapAlts (\pat expr -> Alt pat (liftExpr env expr)) alts


----------------------------------------------------------------
-- Lift binding groups
----------------------------------------------------------------
liftBinds env binds
  = case binds of
      NonRec bind -> let ([bind'],env') = liftBindsRec env [bind]
                     in  (NonRec bind',env')      
      Rec recs    -> let (recs',env') = liftBindsRec env recs
                     in (Rec recs',env')
      Strict (Bind id rhs)
                  -> (Strict (Bind id (liftExpr env rhs)),env)
  where
    nonrec make bind = let ([bind'],env') = liftBindsRec env [bind]
                       in  (make bind',env')
      


liftBindsRec :: Env -> [Bind] -> ([Bind],Env)
liftBindsRec env recs
  = let (ids,exprs)  = unzipBinds recs
        -- calculate the mutual free variables
        fvmap   = fixMutual (zip ids (map (liftedFreeVar env . freeVarSet) exprs))
        -- note these recursive equations :-)
        fvs     = map  (removeLifted env' .  listFromSet . snd) fvmap
        env'    = foldl insertLifted env (zip recs fvs)

        -- put the computed free variables back into the bindings as lambdas
        recs'  = zipWith (addLambdas env) fvs (zipWith Bind ids (map (liftExpr env') exprs))
    in (recs', env')


addLambdas env fv bind@(Bind id (Note (FreeVar _) expr))  
  | isAtomExpr env expr = bind
--   | isValueExpr expr    = Bind id (Note (FreeVar fvset) (Let (NonRec (Bind id (foldlStrict (\e v -> Ap e (Var v)) (Var id) fv))) (Var id)))
  | otherwise           = Bind id (Note (FreeVar emptySet) (foldr Lam expr fv))
  where
    fvset = setFromList fv

addLambdas env fv bind
  = error "CoreLift.addLambdas: no free variable annotation. Do coreFreeVar first?"

insertLifted env ((Bind id expr),fv)
  = if (isAtomExpr env expr) --  || isValueExpr expr)
     then env
     else extendFree env id fv

removeLifted env fv
  = filter (\id -> not (elemFree env id)) fv


fixMutual :: [(Id,IdSet)] -> [(Id,IdSet)]
fixMutual fvmap
  = let fvmap' = map addMutual fvmap
    in  if (size fvmap' == size fvmap)
         then fvmap
         else fixMutual fvmap'
  where
    addMutual (id,fv)
      = (id, foldSet addLocalFree fv fv)

    addLocalFree id fv0
      = case lookup id fvmap of
          Just fv1  -> unionSet fv0 fv1
          Nothing   -> fv0

    size xs
      = sum (map (sizeSet . snd) xs)


liftedFreeVar :: Env -> IdSet -> IdSet
liftedFreeVar env fv
  = unionSet fv (setFromList (concat (map (lookupFree env) (listFromSet fv))))


freeVar expr
  = listFromSet (freeVarSet expr)

freeVarSet (Note (FreeVar fv) expr)
  = fv
freeVarSet expr
  = error "CoreLetSort.freeVar: no annotation. Do coreFreeVar first?"

----------------------------------------------------------------
-- is an expression atomic: i.e. can we generate code inplace
----------------------------------------------------------------
isAtomExpr :: Env -> Expr -> Bool
isAtomExpr env expr
  = case expr of
      Ap e1 e2  -> isAtomExpr env e1 && isAtomExpr env e2
      Note n e  -> isAtomExpr env e
      Var id    -> not (isPrimitive env id)
      Con con   -> True
      Lit lit   -> True
      Let binds expr  
                -> isAtomBinds env binds && isAtomExpr env expr
      other     -> False

isAtomBinds env binds
  = case binds of
      Strict bind           -> False
      NonRec (Bind id expr) -> isAtomExpr env expr
      Rec bindings          -> all (isAtomExpr env) (snd (unzipBinds bindings))

isValueExpr expr
  = case expr of
      Note n e  -> isValueExpr e
      Lam id e  -> False
      other     -> True