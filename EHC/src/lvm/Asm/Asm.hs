{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Asm.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Asm.Asm where

import Lvm.Common.Byte   ( Bytes )
import Lvm.Common.Id     ( Id )
import Lvm.Lvm.Module

{---------------------------------------------------------------
  Asm modules
---------------------------------------------------------------}
type AsmModule  = Module Top
type AsmDecl    = Decl Top

{---------------------------------------------------------------
  low level "assembly" language
---------------------------------------------------------------}
data Top    = Top ![Id] Expr      -- arguments expression

type Atom   = Expr
data Expr   = Eval   !Id Expr Expr
            | Match  !Id ![Alt]
            | Prim   !Id ![Atom]
            -- atomic
            | LetRec ![(Id,Atom)] Expr
            | Let    !Id Atom Expr
            | Ap     !Id ![Atom]
            | Con    !(Con Atom) ![Atom]
            | Lit    !Lit
            | Note   !Note !Expr

data Note   = Occur  !Occur
data Occur  = Never | Once | Many

data Lit    = LitInt   !Int
            | LitFloat !Double
            | LitBytes !Bytes

data Alt    = Alt !Pat Expr

data Pat    = PatVar !Id
            | PatCon !(Con Int) ![Id]
            | PatLit !Lit

data Con tag = ConId !Id
             | ConTag tag !Arity
