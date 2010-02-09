{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreNoShadow.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- Make all local bindings locally unique.
-- and all local let-bindings globally unique.
--
-- After this pass, no variables shadow each other and let-bound variables
-- are globally unique.
----------------------------------------------------------------
module Lvm.Core.CoreNoShadow( coreNoShadow, coreRename ) where

import Lvm.Common.Id     ( Id, freshIdFromId, NameSupply, splitNameSupply, splitNameSupplies )
import Lvm.Common.IdMap  ( IdMap, emptyMap, lookupMap, extendMap )
import Lvm.Common.IdSet  ( IdSet, emptySet, elemSet, insertSet, setFromMap, setFromList, unionSets  )
import Lvm.Core.Core

----------------------------------------------------------------
-- Environment: name supply, id's in scope & renamed identifiers
----------------------------------------------------------------
data Env  = Env NameSupply IdSet (IdMap Id)

renameBinders env bs
  = let (env',bs') = foldl (\(env,ids) id -> renameBinder env id $ \env' id' -> (env',id':ids)) (env,[]) bs
    in  (env',reverse bs')

renameLetBinder :: Env -> Id -> (Env -> Id -> a) -> a
renameLetBinder env@(Env supply inscope renaming) id cont
    = let (id',supply') = freshIdFromId id supply
          inscope'      = insertSet id inscope
          renaming'     = extendMap id id' renaming
      in cont (Env supply' inscope' renaming') id'

renameBinder :: Env -> Id -> (Env -> Id -> a) -> a
renameBinder env@(Env supply set map) id cont
  | elemSet id set
      = renameLetBinder env id cont
  | otherwise
      = cont (Env supply (insertSet id set) map) id

renameVar :: Env -> Id -> Id
renameVar (Env supply set map) id
  = case lookupMap id map of
      Nothing  -> id
      Just id' -> id'

splitEnv :: Env -> (Env,Env)
splitEnv env@(Env supply set map)
  = let (s0,s1) = splitNameSupply supply
    in  (Env s0 set map,Env s1 set map)

splitEnvs :: Env -> [Env]
splitEnvs env@(Env supply set idmap)
  = map (\s -> Env s set idmap) (splitNameSupplies supply)


----------------------------------------------------------------
-- coreNoShadow: make all local variables locally unique
-- ie. no local variable shadows another variable
----------------------------------------------------------------
coreNoShadow :: NameSupply -> CoreModule -> CoreModule
coreNoShadow supply mod
  = mapExprWithSupply (nsDeclExpr emptySet) supply mod

coreRename :: NameSupply -> CoreModule -> CoreModule
coreRename supply mod
  = mapExprWithSupply (nsDeclExpr (globalNames mod)) supply mod

nsDeclExpr inscope supply expr
  = nsExpr (Env supply inscope emptyMap) expr


nsExpr :: Env -> Expr -> Expr
nsExpr env expr
  = case expr of
      Note n expr       -> Note n (nsExpr env expr)
      Let binds expr    -> nsBinds env binds $ \env' binds' ->
                           Let binds' (nsExpr env' expr)
      Match id alts     -> Match (renameVar env id) (nsAlts env alts)
      Lam id expr       -> renameBinder env id $ \env' id' ->
                           Lam id' (nsExpr env' expr)
      Ap expr1 expr2    -> let (env1,env2) = splitEnv env
                           in  Ap (nsExpr env1 expr1) (nsExpr env2 expr2)
      Var id            -> Var (renameVar env id)
      Con (ConTag e a)  -> Con (ConTag (nsExpr env e) a)
      other             -> expr


nsBinds env binds cont
  = case binds of
      Strict (Bind id rhs)  -> nonrec Strict id rhs
      NonRec (Bind id rhs)  -> nonrec NonRec id rhs
      Rec recs              -> rec 
  where
    nonrec make id rhs
      = renameLetBinder env id $ \env' id' ->
        cont env' (make (Bind id' (nsExpr env rhs)))
      
    rec 
      = let (binds',env') = mapAccumBinds (\env id rhs -> renameLetBinder env id $ \env' id' -> (Bind id' rhs,env'))
                                           env binds
        in cont env' (zipBindsWith (\env id rhs -> Bind id (nsExpr env rhs)) (splitEnvs env') binds')


nsAlts env alts
  = zipAltsWith nsAlt (splitEnvs env) alts

nsAlt env pat expr
  = let (pat',env') = nsPat env pat
    in Alt pat' (nsExpr env' expr)

nsPat env pat
  = case pat of
      PatCon con ids -> let (env',ids') = renameBinders env ids
                        in (PatCon con ids',env')
      other          -> (other,env)
