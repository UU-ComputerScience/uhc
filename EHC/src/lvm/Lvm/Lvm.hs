{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: Lvm.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.Lvm( module Lvm.Lvm.Module, LvmModule, LvmDecl

          -- constants
          , recHeader,recFooter           
          ) where

import Lvm.Common.Byte    ( Byte )
import Lvm.Common.Id      ( Id )
import Lvm.Lvm.Instr   ( Instr )
import Lvm.Lvm.Module

{--------------------------------------------------------------
  An LVM module
---------------------------------------------------------------}
type LvmModule  = Module [Instr]
type LvmDecl    = Decl [Instr]

{---------------------------------------------------------------
  Constants
---------------------------------------------------------------}
recHeader,recFooter :: Int
recHeader     = 0x1F4C564D
recFooter     = 0x1E4C564D
