{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreNormalize.hs 222 2004-02-14 16:33:04Z uust $

----------------------------------------------------------------
-- Normalises Core:
--  * no lambda's, except directly at let-bindings
--  * each Ap argument is atomic & not a call to an instruction or external function
--  * each Ap target is atomic
--
-- an atomic expression is
--  * a Var
--  * a Lit
--  * a Con
--  * a normalised Ap
--  * a normalised Let(Rec) expression 
--
-- pre: [coreNoShadow, coreSaturate]
----------------------------------------------------------------
module Lvm.Core.CoreNormalize ( coreNormalize ) where

import Lvm.Common.Id     ( Id, NameSupply, splitNameSupply, splitNameSupplies, freshId )
import Lvm.Common.IdSet  ( IdSet, elemSet )
import Lvm.Core.Core

----------------------------------------------------------------
-- Environment: the name supply
----------------------------------------------------------------
data Env   = Env NameSupply !IdSet {- instructions + externs -}

uniqueId (Env supply directs)
  = fst (freshId supply)

splitEnv (Env s d)
  = let (s0,s1) = splitNameSupply s in (Env s0 d, Env s1 d)

splitEnvs (Env s d)
  = map (\s -> Env s d) (splitNameSupplies s)

isDirect (Env s d) id
  = elemSet id d

----------------------------------------------------------------
-- coreNormalise
----------------------------------------------------------------
coreNormalize :: NameSupply -> CoreModule -> CoreModule
coreNormalize supply mod
  = mapExprWithSupply (normDeclExpr primitives) supply mod
  where
    primitives  = externNames mod

normDeclExpr directs supply expr
  = normBind (Env supply directs) expr


----------------------------------------------------------------
-- Expression & bindings
----------------------------------------------------------------
normExpr env expr
  = let (env1,env2) = splitEnv env
        expr'       = normBind env1 expr
    in case expr' of
         Lam _ _  -> let id = uniqueId env2
                     in (Let (NonRec (Bind id expr')) (Var id))
         other    -> expr'

-- can return lambda's on top
normBind :: Env -> Expr -> Expr
normBind env expr
  = case expr of
      Let binds expr    -> let (env1,env2) = splitEnv env
                           in Let (normBinds env1 binds) (normExpr env2 expr)
      Match id alts     -> Match id (normAlts env alts)
      Lam id expr       -> Lam id (normBind env expr)
      Note n expr       -> normBind env expr  -- de-annotate
      Ap expr1 expr2    -> normAtomExpr env expr
      other             -> expr

normBinds env binds
  = zipBindsWith (\env id expr -> Bind id (normBind env expr)) (splitEnvs env) binds

normAlts env alts
  = zipAltsWith (\env pat expr -> Alt pat (normExpr env expr)) (splitEnvs env) alts

normAtomExpr env expr
  = let (atom,f) = normAtom env expr
    in  (f atom)

-- returns an atomic expression + a function that adds the right bindings
normAtom env expr
  = case expr of
      Match _ _         -> freshBinding
      Lam _ _           -> freshBinding
      Let (Strict _) _  -> freshBinding
      -- we could leave let bindings in place when they are fully
      -- atomic but otherwise the bindings get messed up (shadow7.core).
      -- we lift all bindings out and rely on asmInline to put them
      -- back again if possible.
      Let binds expr    -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 expr
                               -- (abinds,g)  = normAtomBinds env2 binds
                           in  (atom, Let (normBinds env2 binds) . f)
                               -- (abinds atom, f . g)
      Ap expr1 expr2    -> let (env1,env2) = splitEnv env
                               (atom,f)    = normAtom env1 expr1
                               (arg,g)     = normArg  env2 expr2
                           in (Ap atom arg, f . g)
      Note n expr       -> normAtom env expr  -- de-annotate
      other             -> (expr,id)
  where
    freshBinding         = let (env1,env2) = splitEnv env
                               expr'       = normBind env1 expr
                               id          = uniqueId env2
                           in  (Var id, Let (NonRec (Bind id expr')))

-- normAtomBinds returns two functions: one that adds atomic
-- let bindings and one that adds non-atomic bindings
normAtomBinds :: Env -> Binds -> (Expr -> Expr, Expr -> Expr)
normAtomBinds env binds
  = let (binds',(env',f)) = mapAccumBinds norm (env,id) binds 
    in (Let binds', f)
  where
    norm (env,f) id expr    = let (env1,env2) = splitEnv env
                                  (atom,g)    = normAtom env1 expr
                              in (Bind id atom, (env2, f . g))

-- just as an atomic expression but binds 'direct' applications (ie. externs & instructions)
normArg env expr
  = let (env1,env2) = splitEnv env
        (atom,f)    = normAtom env1 expr
    in  if (isDirectAp env atom)
         then let id = uniqueId env2
              in  (Var id, f . Let (NonRec (Bind id atom)))
         else (atom,f)

isDirectAp env expr
  = case expr of
      Ap e1 e2  -> isDirectAp env e1
      Note n e  -> isDirectAp env e
      Var id    -> isDirect env id
      other     -> False
