{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreToAsm.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CoreToAsm( coreToAsm ) where

import Lvm.Common.Standard( assert, unsafeCoerce )
import Lvm.Common.Id      ( Id, idFromString, NameSupply, splitNameSupplies )
import Lvm.Common.IdMap   ( IdMap, listFromMap, mapFromList )
import Lvm.Common.IdSet   ( IdSet, elemSet )
import Lvm.Core.Core
import qualified Lvm.Asm.Asm as Asm

import Lvm.Core.CoreNoShadow   ( coreNoShadow, coreRename )    -- rename local variables
import Lvm.Core.CoreSaturate   ( coreSaturate )    -- saturate constructors, instructions and externs
import Lvm.Core.CoreNormalize  ( coreNormalize )   -- normalize core, ie. atomic arguments and lambda's at let bindings
import Lvm.Core.CoreFreeVar    ( coreFreeVar )     -- attach free variable information at let bindings
import Lvm.Core.CoreLetSort    ( coreLetSort )     -- find smallest recursive let binding groups
import Lvm.Core.CoreLift       ( coreLift )        -- lambda-lift, ie. make free variables arguments

{-
import Lvm.Common.Standard    ( trace )
import Lvm.Core.CorePretty  ( corePretty )

traceCore core  = trace (show (corePretty core)) core
-}

{---------------------------------------------------------------
  coreToAsm: translate Core expressions into Asm expressions
---------------------------------------------------------------}
coreToAsm :: NameSupply -> CoreModule -> Asm.AsmModule
coreToAsm supply mod
  = exprToTop 
  $ coreLift
  $ coreLetSort
  $ coreFreeVar
  $ coreNormalize supply2
  $ coreSaturate supply1
  $ coreRename supply0
  $ mod
  where        
    (supply0:supply1:supply2:supplies) = splitNameSupplies supply

exprToTop :: CoreModule -> Asm.AsmModule
exprToTop mod
  = mod{ moduleDecls = concatMap (asmDecl (externNames mod)) (moduleDecls mod) }

{---------------------------------------------------------------
  top-level bindings
---------------------------------------------------------------}

asmDecl prim (DeclValue id acc enc expr custom)
  = let (pars,(lifted,asmexpr)) = asmTop prim expr
    in (DeclValue id acc enc (Asm.Top pars asmexpr) custom) : concatMap (asmLifted prim id) lifted
asmDecl prim decl
  = [unsafeCoerce decl]


asmLifted prim enc (Bind id expr)
  = let (pars,(lifted,asmexpr)) = asmTop prim expr
    in  (DeclValue id (Defined False) (Just enc) (Asm.Top pars asmexpr) []) 
        : concatMap (asmLifted prim id) lifted


asmTop prim expr
  = let (pars,expr') = splitParams expr
    in (pars,asmExpr prim expr')

splitParams :: Expr -> ([Id],Expr)
splitParams expr
  = case expr of
      Note n e  -> splitParams e
      Lam x e   -> let (pars,e') = splitParams e in (x:pars,e')
      other     -> ([],expr)

{---------------------------------------------------------------
  expressions
---------------------------------------------------------------}
asmExpr :: IdSet -> Expr -> ([Bind],Asm.Expr)
asmExpr prim expr
  = case expr of
      Note n e        -> asmExpr prim e
      Lam x e         -> error "CoreToAsm.asmExpr: unexpected lambda expression (do 'coreNormalise' first?)"
      Let binds e     -> asmLet prim binds (asmExpr prim e)
      Match id alts   -> let (lifted,asmalts) = asmAlts prim alts
                         in (concat lifted, Asm.Match id asmalts)
      atom            -> let asmatom = asmAtom atom []  -- handles prim ap's too
                         in case asmatom of
                              Asm.Ap id args  | elemSet id prim
                                              -> ([],Asm.Prim id args)
                              other           -> ([],asmatom)

asmAlts prim alts
  = unzip (map (asmAlt prim) alts)

asmAlt prim (Alt pat expr)
  = let (lifted,asmexpr) = asmExpr prim expr
    in (lifted, Asm.Alt (asmPat pat) asmexpr)

asmPat pat
  = case pat of
      PatCon con params -> Asm.PatCon (asmPatCon con) params
      PatLit lit        -> Asm.PatLit (asmLit lit)
      PatDefault        -> Asm.PatVar (idFromString  ".def")

asmPatCon con
  = case con of
      ConId id         -> Asm.ConId id
      ConTag tag arity -> Asm.ConTag tag arity


asmLet prim binds (lifted,asmexpr)
  = case binds of
      NonRec bind@(Bind id expr)
                -> if (isAtomic prim expr)
                    then (lifted, Asm.Let id (asmAtom expr []) asmexpr)
                    else (Bind id expr:lifted,asmexpr)
      Strict bind@(Bind id rhs)
                -> let (liftedrhs,asmrhs) = asmExpr prim rhs
                   in  (lifted ++ liftedrhs,Asm.Eval id asmrhs asmexpr)
      Rec binds -> let (lifted',binds') = foldr asmRec (lifted,[]) binds
                   in if (null binds')
                       then (lifted',asmexpr)
                       else (lifted',Asm.LetRec binds' asmexpr)
  where
    asmRec bind@(Bind id expr) (lifted,binds)
      | isAtomic prim expr = (lifted,(id,asmAtom expr []):binds)
      | otherwise          = (bind:lifted,binds)


{---------------------------------------------------------------
 atomic expressions & primitive applications
---------------------------------------------------------------}
asmAtom atom args
  = case atom of
      Note n e  -> asmAtom e args
      Ap e1 e2  -> asmAtom e1 (asmAtom e2 []:args)
      Var id    -> Asm.Ap id args
      Con con   -> Asm.Con (asmCon con) args
      Lit lit   | null args -> Asm.Lit (asmLit lit)
      Let binds expr
                -> asmAtomBinds binds (asmAtom expr args)
      other     -> error "CoreToAsm.asmAtom: non atomic expression (do 'coreNormalise' first?)"

asmCon con 
  = case con of
      ConId id          -> Asm.ConId id 
      ConTag tag arity  -> assert (simpleTag tag) "CoreToAsm.asmCon: tag expression too complex (should be integer or (strict) variable" $
                           Asm.ConTag (asmAtom tag []) arity
  where
    simpleTag (Lit (LitInt i))  = True
    simpleTag (Var id)          = True
    simpleTag (Note n e)        = simpleTag e
    simpleTag other             = False

asmAtomBinds binds atom
  = case binds of
      NonRec (Bind id expr) -> Asm.Let id (asmAtom expr []) atom
      Rec binds             -> Asm.LetRec [(id,asmAtom expr []) | Bind id expr <- binds] atom
      other                 -> error "CoreToAsm.asmAtomBinds: strict binding as atomic expression (do 'coreNormalise first?)"


asmLit lit
  = case lit of
     LitInt i    -> Asm.LitInt i
     LitDouble d -> Asm.LitFloat d
     LitBytes s  -> Asm.LitBytes s

{---------------------------------------------------------------
  is an expression atomic ?
---------------------------------------------------------------}
isAtomic prim expr
  = case expr of
      Note n e  -> isAtomic prim e
      Ap e1 e2  -> isAtomic prim e1 && isAtomic prim e2
      Var id    -> not (elemSet id prim)
      Con (ConId id)   -> True
      Con (ConTag t a) -> isAtomic prim t
      Lit lit   -> True
      Let binds expr
                -> isAtomicBinds prim binds && isAtomic prim expr
      other     -> False

isAtomicBinds prim binds
  = case binds of
      Strict bind  -> False
      other        -> all (isAtomic prim) (snd (unzipBinds (listFromBinds binds)))