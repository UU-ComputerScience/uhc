{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: AsmToLvm.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Asm.AsmToLvm( asmToLvm )  where

import Lvm.Common.Standard ( assert )
import List     ( partition)
import Lvm.Common.Id       ( Id, idFromString )
import Lvm.Common.IdMap    ( IdMap, emptyMap, lookupMap, extendMap, mapMapWithId, mapFromList )
import Lvm.Asm.Asm as Asm
import Lvm.Lvm.Lvm

import Lvm.Lvm.Instr hiding  ( Con(..), Alt(..), Pat(..) )
import qualified Lvm.Lvm.Instr as Instr

import Lvm.Lvm.InstrResolve   ( instrResolve )
import Lvm.Lvm.InstrRewrite   ( instrRewrite )

import Lvm.Asm.AsmOccur(asmOccur)

{---------------------------------------------------------------
  asmToLvm: generate instructions from Asm expressions
---------------------------------------------------------------}
asmToLvm :: AsmModule -> LvmModule
asmToLvm mod
  = mapValues (codegen env) mod
  where
    env = initialEnv mod

codegen :: Env -> Top -> [Instr]
codegen env top
  = instrRewrite (instrResolve (cgTop env top))

{---------------------------------------------------------------
  top-level declarations
---------------------------------------------------------------}
cgTop :: Env -> Top -> [Instr]
cgTop env (Top params expr)
  = [ARGCHK (length params)] ++ [ATOM (cgParams env params ++ cgExpr env expr)] ++ [ENTER]

cgParams env params
  = map PARAM (reverse params)

{---------------------------------------------------------------
 expressions
---------------------------------------------------------------}
cgExpr :: Env -> Expr -> [Instr]
cgExpr env expr
  = case expr of
      -- optimized schemes
      Eval id1 (Note (Occur Once) e1) (Match id2 alts)  | id1 == id2 && whnf env e1
                        -> [ATOM (cgExpr env e1)] ++ cgMatch env alts
      Eval id1 (Note (Occur Once) e1) (Match id2 alts)  | id1 == id2 
                        -> [EVAL 0 [ATOM (cgExpr env e1),ENTER]] ++ cgMatch env alts
      Eval id e1 e2     | whnf env e1
                        -> [ATOM (cgExpr env e1),VAR id] ++ cgExpr env e2
      Eval id1 e1 (Ap id2 []) | id1 == id2
                        -> cgExpr env e1

      -- basic cases
      LetRec binds expr -> cgLetRec env binds ++ cgExpr env expr
      Let id atom expr  -> cgLet env id atom ++ cgExpr env expr
      Eval id expr expr'-> [EVAL 0 [ATOM (cgExpr env expr),ENTER],VAR id] ++ cgExpr env expr'
      Match id alts     -> cgVar env id ++ cgMatch env alts
      Prim id args      -> cgPrim env id args
      Note note expr    -> cgExpr env expr
      atom              -> (cgAtom env atom)
--      other             -> error "AsmToCode.cgExpr: undefined case"


{---------------------------------------------------------------
 let bindings
---------------------------------------------------------------}
cgLet env id atom
  = cgAtom env atom ++ [VAR id]

cgLetRec env binds
  = concat (map (cgAlloc env) binds ++ map (cgInit env) binds)

cgAlloc env (id,atom)
  = [ATOM (cgAlloc' env atom),VAR id]

cgAlloc' env atom
  = case atom of
      Asm.Ap x args    -> [ALLOCAP (length args + 1)]
      Asm.Let id e1 e2 -> cgAlloc' env e2
      Asm.LetRec bs e2 -> cgAlloc' env e2
      Asm.Note note e  -> cgAlloc' env e
      Asm.Con (ConId x) args   
                       -> [ALLOCCON (conFromId x (length args) env)]
      Asm.Con (ConTag tag arity) args  -- TODO: tag may not be recursively bound!
                       -> assert (arity == length args) "AsmToCode.cgAlloc': constructor arity doesn't match arguments" $
                          [PUSHINT arity] ++ cgAtom env tag ++ [ALLOC]
      Asm.Lit lit      -> error "AsmToCode.cgAlloc': literal in recursive binding."
      other            -> error "AsmToCode.cgAlloc': non-atomic expression encountered."

cgInit env (id,atom)
  = [INIT (cgInit' env id atom)]

cgInit' env id atom
  = case atom of
      Asm.Ap x args    -> cgArgs env args ++ cgVar env x ++ [PACKAP (varFromId id) (length args + 1)]
      Asm.Let id e1 e2 -> cgLet env id e1 ++ cgInit' env id e2
      Asm.LetRec bs e2 -> cgLetRec env bs ++ cgInit' env id e2
      Asm.Note note e  -> cgInit' env id e
      Asm.Con (ConId x) args   
                       -> cgArgs env args ++ [PACKCON (conFromId x (length args) env) (varFromId id)]
      Asm.Con (ConTag tag arity) args
                       -> cgArgs env args ++ [PACK arity (varFromId id)]
      Asm.Lit lit      -> error "AsmToCode.cgInit: literal in recursive binding."
      other            -> error "AsmToCode.cgInit: non-atomic expression encountered."



{---------------------------------------------------------------
  alternatives
  result alternatives are 'normalized': the default alternative
  is always there and comes first.
---------------------------------------------------------------}
cgMatch env alts
  = case partition isVarAlt alts of
      ([],alts)    -> cgAlts env (Instr.Alt Instr.PatDefault []) alts
      ([alt],alts) -> cgAlts env (cgAlt env alt) alts
      (vars,alts)  -> error "AsmToCode.cgMatch: multiple default patterns"
  where
    isVarAlt (Alt (PatVar _) _)   = True
    isVarAlt other                = False

cgAlts env def alts
  | all isConIdAlt alts = [MATCHCON (def:map (cgAlt env) alts)]
  | all isConAlt alts   = [MATCH    (def:map (cgAltTag env) alts)]
  | all isIntAlt alts   = [MATCHINT (def:map (cgAlt env) alts)]
  | otherwise           = error "AsmToCode.cgMatch: unknown or mixed type patterns"
  where
    isConIdAlt (Alt (PatCon (ConId _) _) _) = True
    isConIdAlt other                        = False

    isConAlt (Alt (PatCon _ _) _) = True
    isConAlt other                = False

    isIntAlt (Alt (PatLit (LitInt _)) _)  = True
    isIntAlt other                        = False



cgAlt env (Alt pat expr)
  = case pat of
      PatCon (ConId id) params  
          -> Instr.Alt (Instr.PatCon (conFromId id (length params) env))
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]
      PatLit (LitInt i) 
          -> Instr.Alt (Instr.PatInt i) [ATOM (cgExpr env expr)]
      PatVar id         
          -> Instr.Alt (Instr.PatDefault) [ATOM ([PARAM id] ++ cgExpr env expr)]
      other             
          -> error "AsmToCode.cgAlt: unknown pattern"

cgAltTag env (Alt pat expr)
  = case pat of
      PatCon (ConTag tag arity) params  
          -> Instr.Alt (Instr.PatTag tag arity)
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]                      
      PatCon (ConId id) params  
          -> let (tag,arity) = tagArityFromId id (length params) env
             in Instr.Alt (Instr.PatTag tag arity)
                       [ATOM (map PARAM (reverse params) ++ cgExpr env expr)]
      PatVar id         
          -> Instr.Alt (Instr.PatDefault) [ATOM ([PARAM id] ++ cgExpr env expr)]
      other             
          -> error "AsmToCode.cgAltTag: invalid pattern"




{---------------------------------------------------------------
  primitives
---------------------------------------------------------------}
cgPrim env id args
  = case lookupInstr id env of
      Nothing    -> case lookupGlobal id env of
                      Nothing    -> error ("AsmToCode.cgPrim: unknown primitive " ++ show id)
                      Just arity -> if (arity /= length args)
                                     then error ("AsmToCode.cgPrim: unsaturated primitive " ++ show id)
                                     else result (CALL (Global id 0 arity))
      Just instr -> if (isCATCH instr)
                     then case args of
                            [handler,atom] -> let id = idFromString "@catch@" in
                                              cgAtom env handler ++ 
                                              [CATCH [EVAL 0 ((cgAtom env atom)++[ENTER]),VAR id]]
                            other          -> error ("AsmToCode.cgPrim: CATCH expects 2 arguments")
                     else result instr
  where
    result instr  = [ATOM (cgArgs env args ++ [instr])]

{---------------------------------------------------------------
  atomic expressions
---------------------------------------------------------------}
cgAtom env atom
  = [ATOM (cgAtom' env atom)]

cgAtom' env atom
  = case atom of
      Ap id args  -> cgArgs env args ++ cgVar env id ++
                      (if null args then [] else [NEWAP (length args + 1)])
      Lit lit     -> cgLit lit
      Let id e1 e2-> cgLet env id e1 ++ cgAtom' env e2
      LetRec bs e2-> cgLetRec env bs ++ cgAtom' env e2
      Note note e -> cgAtom' env e
      Con (ConId id) args 
                  -> cgArgs env args ++ [NEWCON (conFromId id (length args) env) ]
      Con (ConTag tag arity) args 
                  -> cgArgs env args ++ cgAtom env tag ++ [NEW arity]
            
      -- optimizer: inlined strict bindings 
      Eval id e1 e2  | whnf env e1
                  -> [ATOM (cgExpr env e1), VAR id] ++ cgAtom' env e2
      Eval id e1 e2
                  -> [EVAL 0 [ATOM (cgExpr env e1), ENTER], VAR id] ++ cgAtom' env e2


      other       -> error ("AsmToCode.cgAtom: non-atomic expression encountered")

cgArgs env args
  = concatMap (cgAtom env) (reverse args)


{---------------------------------------------------------------
  literals and variables
---------------------------------------------------------------}
cgLit lit
  = case lit of
      LitInt i    -> [PUSHINT i]
      LitFloat d  -> [PUSHFLOAT d]
      LitBytes b  -> [PUSHBYTES b 0]


cgVar env id
  = case lookupGlobal id env of
      Nothing     -> [PUSHVAR (varFromId id)]
      Just arity  -> [PUSHCODE (Global id 0 arity)]

{---------------------------------------------------------------
 whnf: returns True when the expression puts a weak head normal form
       value on the stack.
---------------------------------------------------------------}
whnf env expr
  = case expr of
      LetRec bs e   -> whnf env e
      Let id e1 e2  -> whnf env e2
      Eval id e1 e2 -> whnf env e2
      Match id alts -> all (whnfAlt env) alts
      Prim id args  -> whnfPrim env id
      Ap id args    -> False
      Con con args  -> True
      Lit lit       -> True
      Note note e   -> whnf env e

whnfAlt env (Alt pat e)
  = whnf env e

whnfPrim env id
  = case lookupInstr id env of
      Nothing    -> False -- TODO: look at the type of a primitive
      Just instr -> instrHasStrictResult instr


{---------------------------------------------------------------
  the code generation environment
---------------------------------------------------------------}
data Env  = Env{ arities :: IdMap Arity
               , instrs  :: IdMap Instr
               , cons    :: IdMap (Tag,Arity)
               }

lookupInstr id env
  = lookupMap id (instrs env)

lookupGlobal id env
  = lookupMap id (arities env)

varFromId id
  = Var id 0 0

conFromId id argcount env
  = let (tag,arity) = tagArityFromId id argcount env
    in  Instr.Con id 0 arity tag

tagArityFromId id argcount env
  = case lookupMap id (cons env) of
      Just (tag,arity)  -> if (arity /= argcount)
                            then error ("AsmToCode.conFromId: unsaturated constructor " ++ show id)
                            else (tag,arity)
      Nothing           -> error ("AsmToCode.conFromId: undeclared constructor " ++ show id)

-- create an initial environment from the declarations
initialEnv :: AsmModule -> Env
initialEnv mod
  = Env globals instrs cons
  where
    globals = mapFromList [(declName d,getArity d) | d <- moduleDecls mod
                                                    , isDeclValue d || isDeclAbstract d || isDeclExtern d ]

    instrs  = mapFromList [(declName d,instrFromEx d) | d <- moduleDecls mod
                                                      , isDeclExtern d, externCall d == CallInstr]

    cons    = mapFromList [(declName d,(conTag d,declArity d)) | d <- moduleDecls mod
                                                               , isDeclCon d]

    getArity (DeclValue{valueValue=Top args body})  = length args           
    getArity decl                                   = declArity decl

    instrFromEx x   = case externName x of
                        Plain s    -> instrFromName s
                        Decorate s -> instrFromName s
                        Ordinal i  -> instrFromOpcode i
