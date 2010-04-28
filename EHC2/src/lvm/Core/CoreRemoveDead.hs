{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreRemoveDead.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CoreRemoveDead( coreRemoveDead ) where

import qualified Lvm.Common.Set as Set
import Lvm.Common.Standard ( foldlStrict, trace )
import Lvm.Common.Id       ( Id, idFromString )
import Lvm.Common.IdSet    ( IdSet, emptySet, elemSet, insertSet, setFromList, unionSet )
import Lvm.Core.Core


----------------------------------------------------------------
-- The identity of a declaration is it's name *and* the kind.
-- i.e. we can have a kind Type and a type Type. Extern declarations
-- are identified as Value declarations since they are not
-- distinguished from normal values inside core expressions.
----------------------------------------------------------------
type Identity   = (DeclKind,Id)
type Used       = Set.Set Identity

declIdentity :: CoreDecl -> Identity
declIdentity decl@(DeclExtern {})
  = (DeclKindValue, declName decl)
declIdentity decl
  = (declKindFromDecl decl, declName decl)



----------------------------------------------------------------
-- Remove all dead declarations
-- TODO: at the moment, the analysis is too conservative and
-- only removes private declarations that are nowhere used.
-- A proper analysis would find all reachable declaratins.
----------------------------------------------------------------
coreRemoveDead :: CoreModule -> CoreModule
coreRemoveDead mod
  = mod{ moduleDecls = filter (isUsed used) (moduleDecls mod) }
  where
    -- Retain main$ even though it is private and not used
    -- It cannot be public because it would be imported and clash
    -- in other modules
    used  = foldlStrict usageDecl alwaysUsed (moduleDecls mod)

    alwaysUsed = Set.fromList $ map (\name -> (DeclKindValue,idFromString name)) $
                 ["main$","main"]
    
----------------------------------------------------------------
-- Is a declaration used?
----------------------------------------------------------------
isUsed :: Used -> CoreDecl -> Bool
isUsed used decl
  = (accessPublic (declAccess decl) || Set.member (declIdentity decl) used) 


----------------------------------------------------------------
-- Find used declarations
----------------------------------------------------------------
usageDecl :: Used -> CoreDecl -> Used
usageDecl used decl
  = let usedCustoms = usageCustoms used (declCustoms decl)
    in case decl of
         DeclValue{} -> let usedExpr = usageValue usedCustoms (valueValue decl)
                            usedEnc  = case (valueEnc decl) of
                                        Just id  -> Set.insert (DeclKindValue,id) usedExpr
                                        Nothing  -> usedExpr
                         in usedEnc
         other       -> usedCustoms

usageCustoms :: Used -> [Custom] -> Used
usageCustoms used customs
  = foldlStrict usageCustom used customs

usageCustom used custom
  = case custom of
      CustomLink  id kind       -> Set.insert (kind,id) used
      CustomDecl  kind customs  -> usageCustoms used customs
      other                     -> used

----------------------------------------------------------------
-- Find used declarations in expressions
----------------------------------------------------------------
usageValue used expr
  = usageExpr emptySet used expr


usageExprs locals used exprs
  = foldlStrict (usageExpr locals) used exprs

usageExpr :: IdSet -> Used -> Expr -> Used
usageExpr locals used expr
 = case expr of
      Let binds expr  -> let used'   = usageBinds locals used binds 
                             locals' = unionSet locals (setFromList (binders (listFromBinds binds)))
                         in usageExpr locals' used' expr
      Lam id expr     -> usageExpr (insertSet id locals) used expr
      Match id alts   -> usageAlts locals (usageVar locals used id) alts
      Ap expr1 expr2  -> usageExpr locals (usageExpr locals used expr1) expr2
      Var id          -> usageVar locals used id
      Con con         -> usageCon locals used con
      Note n expr     -> usageExpr locals used expr
      Lit lit         -> used

usageVar locals used id
  | elemSet id locals = used
  | otherwise         = Set.insert (DeclKindValue,id) used

usageCon locals used con
  = case con of
      ConId id          -> Set.insert (DeclKindCon,id) used
      ConTag tag arity  -> usageExpr locals used tag

usageBinds locals used binds 
  = case binds of
      NonRec (Bind id rhs)  -> usageExpr locals used rhs
      Strict (Bind id rhs)  -> usageExpr locals used rhs
      Rec binds             -> let (ids,rhss) = unzipBinds binds
                                   locals'    = unionSet locals (setFromList ids)
                               in usageExprs locals' used rhss
  
      
usageAlts locals used alts
  = foldlStrict (usageAlt locals) used alts

usageAlt locals used (Alt pat expr)
  = case pat of
      PatCon con ids  -> let locals' = unionSet locals (setFromList ids)
                             used'   = usageConPat locals used con
                         in usageExpr locals' used' expr
      other           -> usageExpr locals used expr

usageConPat locals used con
  = case con of
      ConId id          -> Set.insert (DeclKindCon,id) used
      ConTag tag arity  -> used
