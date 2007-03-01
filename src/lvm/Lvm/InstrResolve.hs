{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: InstrResolve.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.InstrResolve( instrResolve ) where

import Lvm.Common.Standard( assert, trace )
import Lvm.Common.Id      ( Id )
import Lvm.Common.IdMap   ( IdMap, emptyMap, lookupMap, extendMap )
import Lvm.Lvm.Instr

{---------------------------------------------------------------
  resolve monad
---------------------------------------------------------------}
newtype Resolve a   = R ((Base,Env,Depth) -> (a,Depth))

type Env            = IdMap Depth
type Base           = Depth

find id env
  = case lookupMap id env of
     Nothing    -> error ("InstrResolve.find: unknown identifier " ++ show id)
     Just depth -> depth


instance Functor Resolve where
  fmap f (R r)      = R (\ctx -> case r ctx of (x,depth) -> (f x,depth))

instance Monad Resolve where
  return x          = R (\(base,env,depth) -> (x,depth))
  (R r) >>= f       = R (\ctx@(base,env,depth) ->
                            case r ctx of
                              (x,depth') -> case f x of
                                              R fr -> fr (base,env,depth'))

{---------------------------------------------------------------
  non-proper morphisms
---------------------------------------------------------------}
pop n
  = push (-n)

push n
  = R (\(base,env,depth) -> ((),depth+n))

base
  = R (\(base,env,depth) -> (base,depth))

depth
  = R (\(base,env,depth) -> (depth,depth))

bind x (R r)
  = R (\(base,env,depth) -> r (base,extendMap x depth env,depth))

based (R r)
  = R (\(base,env,depth) -> r (depth,env,depth))

resolveVar (Var id _ _)
  = R (\(base,env,depth) -> let xdepth = find id env in (Var id (depth - xdepth) xdepth,depth))

alternative depth (R r)
  = R (\(base,env,_) -> let (x,depth') = r (base,env,depth)
                        in --- assert (depth'==base) ("InstrResolve.alternative: still elements on the stack " ++ show depth' ++ ", " ++ show base) $
                           assert (depth'==depth+1) ("InstrResolve.alternative: invalid elements on the stack " ++ show depth' ++ ", " ++ show depth) $
                           (x,depth'))

runResolve (R r)
  = let (x,depth) = r (0,emptyMap,0)
    in  assert (depth==0) ("InstrResolve.runResolve: still elements on the stack (" ++ show depth ++ ")") $
        x

{---------------------------------------------------------------
  codeResolver
---------------------------------------------------------------}
instrResolve :: [Instr] -> [Instr]
instrResolve instrs
  = runResolve (resolves instrs)

resolves :: [Instr] -> Resolve [Instr]
resolves instrs
  = case instrs of
      (PARAM id : instrs)       -> do{ push 1; bind id (resolves instrs) }
      (VAR id : instrs)         -> bind id (resolves instrs)
      (instr : instrs)          -> do{ is <- resolve instr
                                     ; iss <- resolves instrs
                                     ; return (is ++ iss)
                                     }
      []                        -> return []

resolve :: Instr -> Resolve [Instr]
resolve (PUSHVAR v)
  = do{ var <- resolveVar v
      ; push 1
      ; return [PUSHVAR var]
      }

resolve (STUB v)
  = do{ var <- resolveVar v
      ; return [STUB var]
      }

resolve (PACKAP v n)
  = do{ var <- resolveVar v
      ; pop n
      ; return [PACKAP var n]
      }

resolve (PACKNAP v n)
  = do{ var <- resolveVar v
      ; pop n
      ; return [PACKNAP var n]
      }

resolve (PACKCON con v)
  = do{ var <- resolveVar v
      ; pop (arityFromCon con)
      ; return [PACKCON con var]
      }

resolve (PACK arity v)
  = do{ var <- resolveVar v
      ; pop arity
      ; return [PACK arity var]
      }

resolve (EVAL _ is)
  = do{ push 3
      ; d   <- depth
      ; is' <- based (resolves is)
      ; pop 3
      ; push 1
      ; return [EVAL d is']
      }

resolve (CATCH is)
  = do{ pop 1
      ; push 3
      ; is' <- resolves is
      ; return (PUSHCATCH : is')
      }
{-

  = do{ b   <- base
      ; pop 1
      ; push 3
      ; is' <- based (resolves is)
      ; d   <- depth
      ; pop (d-b)
      ; return (PUSHCATCH : is')
      }
-}
{-
resolve (RESULT is)
  = do{ b   <- base
      ; d   <- depth
      ; is' <- resolves is
      ; d'  <- depth
      ; pop (d' - b)
      ; if (d' <= d)
         then return (is' ++ [SLIDE 1 (d'-b-1) (d'-1), ENTER])
         else return (is' ++ [SLIDE (d'-d) (d-b) d,ENTER])
      }
-}
resolve (ATOM is)
  = resolveSlide 1 is

resolve (INIT is)
  = resolveSlide 0 is

resolve (MATCH alts)
  = resolveAlts MATCH alts

resolve (MATCHCON alts)
  = resolveAlts MATCHCON alts

resolve (SWITCHCON alts)
  = resolveAlts SWITCHCON alts

resolve (MATCHINT alts)
  = resolveAlts MATCHINT alts

resolve instr
  = do{ effect instr; return [instr] }

resolveSlide n is
  = do{ d0  <- depth
      ; is' <- resolves is
      ; d1  <- depth
      ; let m = d1-d0-n
      ; pop m
      ; return (is' ++ [SLIDE n m d1])
      }

resolveAlts match alts
  = do{ pop 1
      ; d     <- depth
      ; alts' <- sequence (map (alternative d . resolveAlt) alts)
      ; return [match alts']
      }

{-
resolveAlt (Alt pat [])
  = do{ b <- base
      ; d <- depth
      ; pop (d-b)
      ; return (Alt pat [])
      }
-}

resolveAlt (Alt pat [])
  = do{ push 1
      ; return (Alt pat [])
      }

resolveAlt (Alt pat is)
  = do{ is' <- resolves is
      ; return (Alt pat (is'))
      }


effect instr
  = case instr of
      ENTER            -> pop 1
      RAISE            -> pop 1

      CALL global      -> do{ pop (arityFromGlobal global); push 1 }

      ALLOCAP n        -> push 1
      NEWAP n          -> do{ pop n; push 1 }
      NEWNAP n         -> do{ pop n; push 1 }

      ALLOCCON con     -> push 1
      NEWCON con       -> do{ pop (arityFromCon con); push 1 }

      NEW arity        -> do{ pop 1; pop arity; push 1 }
      UNPACK arity     -> do{ pop 1; push arity }

      ALLOC            -> do{ pop 2; push 1 }
      GETFIELD         -> do{ pop 2; push 1 }
      SETFIELD         -> pop 3
      GETTAG           -> do{ pop 1; push 1 }
      GETSIZE          -> do{ pop 1; push 1 }
      UPDFIELD         -> do{ pop 3; push 1 }

      RETURNCON con    -> do{ pop (arityFromCon con) }          -- it is the last instruction!

      PUSHCODE global  -> push 1
      PUSHINT i        -> push 1
      PUSHFLOAT d      -> push 1
      PUSHBYTES s c    -> push 1

      PUSHCONT ofs     -> push 3
      PUSHCATCH        -> do{ pop 1; push 3 }

      ADDINT           -> pop 1
      SUBINT           -> pop 1
      MULINT           -> pop 1
      DIVINT           -> pop 1
      MODINT           -> pop 1
      QUOTINT          -> pop 1
      REMINT           -> pop 1

      ANDINT           -> pop 1
      XORINT           -> pop 1
      ORINT            -> pop 1
      SHRINT           -> pop 1
      SHLINT           -> pop 1
      SHRNAT           -> pop 1

      EQINT            -> pop 1
      NEINT            -> pop 1
      LTINT            -> pop 1
      GTINT            -> pop 1
      LEINT            -> pop 1
      GEINT            -> pop 1

      ADDFLOAT           -> pop 1
      SUBFLOAT           -> pop 1
      MULFLOAT           -> pop 1
      DIVFLOAT           -> pop 1

      EQFLOAT            -> pop 1
      NEFLOAT            -> pop 1
      LTFLOAT            -> pop 1
      GTFLOAT            -> pop 1
      LEFLOAT            -> pop 1
      GEFLOAT            -> pop 1

      other -> return ()
