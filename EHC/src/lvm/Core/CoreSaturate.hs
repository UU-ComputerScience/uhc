{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreSaturate.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- saturate all calls to externals, instructions and constructors.
-- pre: [coreNoShadow]
----------------------------------------------------------------
module Lvm.Core.CoreSaturate( coreSaturate ) where

import List   ( mapAccumR )
import Lvm.Common.Id     ( Id, NameSupply, freshId, splitNameSupply, splitNameSupplies )
import Lvm.Common.IdMap  ( IdMap, emptyMap, lookupMap, filterMap, mapFromList )
import Lvm.Core.Core

----------------------------------------------------------------
-- Environment: a name supply and a map from id to its arity
----------------------------------------------------------------
data Env    = Env NameSupply (IdMap Int)

uniqueId (Env supply arities)
  = let (id,supply') = freshId supply
    in  (id,Env supply' arities)

findArity id (Env supply arities)
  = case lookupMap id arities of
      Nothing -> 0
      Just n  -> n

splitEnv (Env supply arities)
  = let (s0,s1) = splitNameSupply supply
    in  (Env s0 arities, Env s1 arities)

splitEnvs (Env supply arities)
  = map (\s -> Env s arities) (splitNameSupplies supply)

----------------------------------------------------------------
-- coreSaturate
----------------------------------------------------------------
coreSaturate :: NameSupply -> CoreModule -> CoreModule
coreSaturate supply mod
  = mapExprWithSupply (satDeclExpr arities) supply mod
  where
    arities = mapFromList [(declName d,declArity d) | d <- moduleDecls mod, isDeclCon d || isDeclExtern d]


satDeclExpr :: IdMap Int -> NameSupply -> Expr -> Expr
satDeclExpr arities supply expr
  = satExpr (Env supply arities) expr

----------------------------------------------------------------
-- saturate expressions
----------------------------------------------------------------
satExpr :: Env -> Expr -> Expr
satExpr env expr
  = case expr of
      Let binds expr
        -> let (env0,env1) = splitEnv env
           in  Let (satBinds env0 binds) (satExpr env1 expr)
      Match id alts
        -> Match id (satAlts env alts)
      Lam id expr
        -> Lam id (satExpr env expr)
      Note n expr
        -> Note n (satExpr env expr)
      other
        -> let expr'  = satExprSimple env expr
           in addLam env  (requiredArgs env expr') expr'

satBinds env binds
  = zipBindsWith (\env id expr -> Bind id (satExpr env expr)) (splitEnvs env) binds

satAlts env alts
  = zipAltsWith (\env pat expr -> Alt pat (satExpr env expr)) (splitEnvs env) alts

-- don't saturate Ap, Var and Con here
satExprSimple env expr
  = case expr of
      Let _ _     -> satExpr env expr
      Match _ _   -> satExpr env expr
      Lam _ _     -> satExpr env expr
      Ap e1 e2    -> let (env1,env2) = splitEnv env
                     in  Ap (satExprSimple env1 e1) (satExpr env2 e2)
      Note n e    -> Note n (satExprSimple env e)
      other       -> expr

----------------------------------------------------------------
-- Add lambda's
----------------------------------------------------------------
addLam env n expr
  = let (env',ids) = mapAccumR (\env i -> let (id,env') = uniqueId env in (env',id)) env [1..n]
    in  foldr Lam (foldl Ap expr (map Var ids)) ids

requiredArgs env expr
  = case expr of
      Let binds expr        -> 0
      Match id alts         -> 0
      Lam id expr           -> 0
      Ap expr1 expr2        -> requiredArgs env expr1 - 1
      Var id                -> findArity id env
      Con (ConId id)        -> findArity id env
      Con (ConTag e arity)  -> arity
      Note n expr           -> requiredArgs env expr
      other                 -> 0
