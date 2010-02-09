{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Core.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.Core ( module Lvm.Lvm.Module
            , CoreModule, CoreDecl
            , Expr(..), Note(..), Binds(..), Bind(..)
            , Alts, Alt(..), Pat(..), Literal(..), Con(..)

            , listFromBinds, unzipBinds, binders, mapBinds
            , mapAccumBinds, zipBindsWith
            
            , patBinders
            , mapAlts, zipAltsWith
            
            , mapExprWithSupply, mapExpr
            , mapAccum 
            ) where

import Lvm.Common.Byte   ( Bytes )
import Lvm.Common.Id     ( Id, NameSupply, mapWithSupply )
import Lvm.Lvm.Module
import Lvm.Common.IdMap  ( IdMap, filterMap )
import Lvm.Common.IdSet  ( IdSet, emptySet, setFromMap, setFromList )

----------------------------------------------------------------
-- Modules
----------------------------------------------------------------
type CoreModule = Module Expr
type CoreDecl   = Decl Expr

----------------------------------------------------------------
-- Core expressions:
----------------------------------------------------------------
data Expr       = Let       !Binds Expr       
                | Match     !Id Alts
                | Ap        Expr Expr
                | Lam       !Id Expr
                | Con       !(Con Expr)
                | Var       !Id
                | Lit       !Literal 
                | Note      !Note !Expr

data Note       = FreeVar   !IdSet

data Binds      = Rec       ![Bind]
                | Strict    !Bind
                | NonRec    !Bind

data Bind       = Bind      !Id Expr

type Alts       = [Alt]
data Alt        = Alt       !Pat Expr

data Pat        = PatCon    !(Con Tag) ![Id]
                | PatLit    !Literal
                | PatDefault

data Literal    = LitInt    !Int
                | LitDouble !Double
                | LitBytes  !Bytes

data Con tag    = ConId  !Id
                | ConTag tag !Arity


----------------------------------------------------------------
-- Binders functions
----------------------------------------------------------------
listFromBinds :: Binds -> [Bind]
listFromBinds binds
  = case binds of
      NonRec bind -> [bind]
      Strict bind -> [bind]
      Rec recs    -> recs
      
binders :: [Bind] -> [Id]
binders binds
  = map (\(Bind id rhs) -> id) (binds)

unzipBinds :: [Bind] -> ([Id],[Expr])
unzipBinds binds
  = unzip (map (\(Bind id rhs) -> (id,rhs)) (binds))

mapBinds :: (Id -> Expr -> Bind) -> Binds -> Binds
mapBinds f binds
  = case binds of
      NonRec (Bind id rhs)
        -> NonRec (f id rhs)
      Strict (Bind id rhs)
        -> Strict (f id rhs)
      Rec recs
        -> Rec (map (\(Bind id rhs) -> f id rhs) recs)

mapAccumBinds :: (a -> Id -> Expr -> (Bind,a)) -> a -> Binds -> (Binds,a)
mapAccumBinds f x binds
  = case binds of
      NonRec (Bind id rhs)
        -> let (bind,y) = f x id rhs
           in  (NonRec bind, y)
      Strict (Bind id rhs)
        -> let (bind,y) = f x id rhs
           in  (Strict bind, y)
      Rec recs
        -> let (recs',z) = mapAccum (\x (Bind id rhs) -> f x id rhs) x recs
           in  (Rec recs',z)

mapAccum               :: (a -> b -> (c,a)) -> a -> [b] -> ([c],a)
mapAccum f s []         = ([],s)
mapAccum f s (x:xs)     = (y:ys,s'')
                         where (y,s' )  = f s x
                               (ys,s'') = mapAccum f s' xs


zipBindsWith :: (a -> Id -> Expr -> Bind) -> [a] -> Binds -> Binds
zipBindsWith f (x:xs) (Strict (Bind id rhs))
  = Strict (f x id rhs)
zipBindsWith f (x:xs) (NonRec (Bind id rhs))
  = NonRec (f x id rhs)
zipBindsWith f xs (Rec recs)
  = Rec (zipWith (\x (Bind id rhs) -> f x id rhs) xs recs)

----------------------------------------------------------------
-- Alternatives functions
----------------------------------------------------------------
patBinders :: Pat -> IdSet
patBinders pat
  = case pat of
      PatCon id ids -> setFromList ids
      other         -> emptySet


mapAlts :: (Pat -> Expr -> Alt) -> Alts -> Alts
mapAlts f alts
  = map (\(Alt pat expr) -> f pat expr) alts

zipAltsWith :: (a -> Pat -> Expr -> Alt) -> [a] -> Alts -> Alts
zipAltsWith f xs alts
  = zipWith (\x (Alt pat expr) -> f x pat expr) xs alts


----------------------------------------------------------------
--
----------------------------------------------------------------
mapExprWithSupply :: (NameSupply -> Expr -> Expr) -> NameSupply -> CoreModule -> CoreModule
mapExprWithSupply f supply mod
  = mod{ moduleDecls = mapWithSupply fvalue supply (moduleDecls mod) }
  where
    fvalue supply decl@(DeclValue{}) = decl{ valueValue = f supply (valueValue decl)}
    fvalue supply decl               = decl

mapExpr :: (Expr -> Expr) -> CoreModule -> CoreModule
mapExpr f mod
  = mapValues f mod
