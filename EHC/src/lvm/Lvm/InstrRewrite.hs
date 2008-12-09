{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: InstrRewrite.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.InstrRewrite( instrRewrite )  where

import Lvm.Common.Standard ( assert, trace )
import Lvm.Common.Id       ( Id, stringFromId )
import Lvm.Lvm.Instr    ( Instr(..), Alt(..), Pat(..), Var(..)
                , idFromVar, offsetFromVar, depthFromVar
                , arityFromGlobal, arityFromCon
                , instrHasStrictResult
                )
import Lvm.Lvm.InstrPretty

{---------------------------------------------------------------
  debugging
---------------------------------------------------------------}
showInstr instr
  = showInstrs [instr]

showInstrs instrs
  = show (instrPretty instrs)

traceInstrs instrs 
  = trace ("trace:\n" ++ showInstrs instrs ++ "\n\n") instrs

{---------------------------------------------------------------
  rewrite rules
---------------------------------------------------------------}
instrRewrite :: [Instr] -> [Instr]
instrRewrite instrs
  = peephole (rewrites (dummies (rewrites (rewrites instrs))))

rewrites instrs
  = case instrs of  
      -- TODO: the following three rules optimize things like (id x = x) but
      -- this can probably be better done on the code generation level
      PUSHVAR (Var id 0 depth) : SLIDE 1 m d : is
        | m >= 1
        -> rewrites (SLIDE 1 (m-1) (d-1) : is)

      PUSHVAR (Var id0 1 d0) : PUSHVAR (Var id1 1 d1) : SLIDE 2 m d : is
        | m >= 2
        -> rewrites (SLIDE 2 (m-2) (d-2) : is)

      PUSHVAR (Var id0 2 d0) : PUSHVAR (Var id1 2 d1) : PUSHVAR (Var id2 2 d2) : SLIDE 3 m d : is
        | m >= 3
        -> rewrites (SLIDE 3 (m-3) (d-3) : is)
           
      -- applications
      NEWAP i : SLIDE n m d: ENTER : is
        -> SLIDE (n+i-1) m (d+i-1): ENTER : rewrites is

      NEWNAP i : SLIDE n m d: ENTER : is
        -> SLIDE (n+i-1) m (d+i-1): ENTER : rewrites is

      CALL global : SLIDE 1 m d: ENTER : is
        -> SLIDE arity m (d+arity-1): CALL global : ENTER : rewrites is
        where
          arity = arityFromGlobal global

      -- returns
      NEWCON con : SLIDE 1 m d: ENTER : is
        -> SLIDE n m (d+n-1): RETURNCON con : rewrites is
        where n = arityFromCon con

      PUSHINT i : SLIDE 1 m d: ENTER : is
        -> SLIDE 0 m (d-1): RETURNINT i : rewrites is

      instr : SLIDE 1 m d: ENTER : is
        | instrHasStrictResult instr
        -> instr : SLIDE 1 m d: RETURN : is

      -- eval and pushcode
      PUSHCODE f : is
        -> rewritePushCode f (rewrites is)

      EVAL d is' : is
        -> rewriteEval d (rewrites is') is

      -- merge slides
      SLIDE n0 m0 d0 : SLIDE n1 m1 d1 : is
        | n1 <= n0  -> rewrites (SLIDE n1 (m0+m1-(n0-n1)) d0 : is)

      -- essential rewrites
      MATCH alts : is
        -> [rewriteMatch MATCH alts is]

      MATCHCON alts : is
        -> [rewriteMatch MATCHCON alts is]

      SWITCHCON alts : is
        -> [rewriteMatch SWITCHCON alts is]

      MATCHINT alts : is
        -> [rewriteMatch MATCHINT alts is]

      -- default
      instr:instrs  -> instr:rewrites instrs
      []            -> []


rewriteMatch match alts is
  = match (map (rewriteAlt is) alts)

rewriteAlt instrs (Alt pat [])
  = Alt pat []

rewriteAlt instrs (Alt pat is)
  = Alt pat (rewrites (is ++ instrs))


-- rewrite PUSHCODE
rewritePushCode f instrs
  = case instrs of
      NEWAP n : is
        | arity >= n -> PUSHCODE f : NEWNAP n : is
      PACKAP var n : is
        | arity >= n -> PUSHCODE f : PACKNAP var n : is
      SLIDE n m d: ENTER : is
        | arity == (n-1) && arity /= 0  -> SLIDE (n-1) m (d-1): ENTERCODE f : is
      other
        -> PUSHCODE f  : instrs
  where
    arity = arityFromGlobal f

-- rewrite EVAL
rewriteEval d evalis is
  =  case evalis of
       [PUSHVAR (Var id ofs d),SLIDE 1 0 d',ENTER]
          -> rewrites ([EVALVAR (Var id (ofs-3) d)] ++ is)
       [PUSHVAR (Var id ofs d),ENTER]
          -> rewrites ([EVALVAR (Var id (ofs-3) d)] ++ is)
       other
          -> [EVAL d evalis] ++ rewrites is

{---------------------------------------------------------------
  peephole optimization
---------------------------------------------------------------}
peephole instrs
  = simplify shorten instrs

dummies instrs
  = simplify id instrs

simplify single instrs
  = walk instrs
  where
    walk instrs 
      = case instrs of
          -- structured
          EVAL d is' : is           -> EVAL d (walk is') : walk is
          MATCH alts : is           -> MATCH (map walkAlt alts)  : walk is
          MATCHCON alts : is        -> MATCHCON (map walkAlt alts)  : walk is
          SWITCHCON alts : is       -> SWITCHCON (map walkAlt alts)  : walk is
          MATCHINT alts : is        -> MATCHINT (map walkAlt alts)  : walk is

          -- dummy
          NEWAP 1 : is              -> walk is
          NEWNAP 1 : is             -> walk is

          -- slide
          SLIDE n 0 d : is          -> walk is
          SLIDE 1 m d : RETURN : is -> walk (RETURN : is)
          SLIDE n m d : RETURNCON con : is
                                    | arityFromCon con == n -> walk (RETURNCON con : is)
          SLIDE 0 m d : RETURNINT i : is
                                    -> walk (RETURNINT i : is)

          -- shorten sequences
          PUSHVAR v : PUSHVAR w : is
                                    -> PUSHVARS2 v w : walk is

          -- default
          instr:is                  -> single instr : walk is
          []                        -> []

    walkAlt (Alt pat is)
      = Alt pat (walk is)

shorten instr
  = case instr of
      PUSHVAR var   -> case offsetFromVar var of
                         0     -> PUSHVAR0
                         1     -> PUSHVAR1
                         2     -> PUSHVAR2
                         3     -> PUSHVAR3
                         4     -> PUSHVAR4
                         other -> instr

      NEWAP n       -> case n of
                         2     -> NEWAP2
                         3     -> NEWAP3
                         4     -> NEWAP4
                         other -> instr

      NEWNAP n      -> case n of
                         2     -> NEWNAP2
                         3     -> NEWNAP3
                         4     -> NEWNAP4
                         other -> instr

      NEWCON con    -> case arityFromCon con of
                         0     -> NEWCON0 con
                         1     -> NEWCON1 con
                         2     -> NEWCON2 con
                         3     -> NEWCON3 con
                         other -> instr

      RETURNCON con -> case arityFromCon con of
                         0     -> RETURNCON0 con
                         other -> instr

      other         -> instr
