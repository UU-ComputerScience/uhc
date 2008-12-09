{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: LvmPretty.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.LvmPretty( lvmPretty ) where

import Lvm.Common.PPrint
import Lvm.Lvm.Lvm
import Lvm.Lvm.InstrPretty  ( instrPretty )
import Lvm.Lvm.ModulePretty ( modulePretty )

lvmPretty :: LvmModule -> Doc
lvmPretty mod
  = modulePretty instrPretty mod
