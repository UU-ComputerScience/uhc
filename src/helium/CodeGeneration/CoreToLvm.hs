{-| Module      :  CoreToLvm
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module CoreToLvm ( coreToLvm ) where

import Id         ( newNameSupply )
import CoreToAsm  ( coreToAsm )         -- enriched lambda expressions (Core) to Asm
import AsmToLvm   ( asmToLvm )          -- translate Asm to instructions
import AsmOptimize( asmOptimize )       -- optimize Asm (ie. inlining)
import LvmWrite   ( lvmWriteFile )

import PPrint     ( putDoc )
import LvmPretty  ( lvmPretty )

coreToLvm source coremod = do
    nameSupply  <- newNameSupply

    -- coreRemoveDead gebeurt al in Compile.hs
    let asmmod  = coreToAsm nameSupply coremod
        asmopt  = asmOptimize asmmod
        lvmmod  = asmToLvm  asmopt
        target  = source ++ ".lvm"
   
    -- putDoc (lvmPretty lvmmod)
    lvmWriteFile target lvmmod
