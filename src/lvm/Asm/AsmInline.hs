{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: AsmInline.hs 222 2004-02-14 16:33:04Z uust $
module Lvm.Asm.AsmInline (asmInline) where

import Lvm.Common.Standard ( assert )
import Lvm.Common.Id       ( Id )
import Lvm.Common.IdMap    ( IdMap, emptyMap, extendMap, deleteMap, elemMap, lookupMap )
import Lvm.Lvm.Module   ( mapValues )

import Lvm.Asm.Asm
import Lvm.Asm.AsmOccur ( asmOccur )

{---------------------------------------------------------------
  Inline environment maps identifiers to their definition
---------------------------------------------------------------}
type Env  = IdMap Expr

removeIds ids env
  = foldr deleteMap env ids

{---------------------------------------------------------------
  asmInline
---------------------------------------------------------------}
asmInline :: AsmModule -> AsmModule
asmInline mod
  = mapValues inlineTop (asmOccur mod)

inlineTop :: Top -> Top
inlineTop (Top params expr)
  = Top params (inlineExpr emptyMap expr)

inlineExpr :: Env -> Expr -> Expr
inlineExpr env expr
  = case expr of
      -- dead variable
      Let id (Note (Occur Never) e1) e2
                    -> inlineExpr env e2
      -- once
      Let id (Note (Occur Once) e1) e2
                    -> let e1' = inlineExpr env e1  -- de-annotate
                       in  inlineExpr (extendMap id e1' env) e2

      -- trivial, inline everywhere
      Let id e1 e2  | trivial e1
                    -> let e1' = inlineExpr env (deAnnotate e1)
                       in inlineExpr (extendMap id e1' env) e2
                       
      Eval id e1 e2 | whnfTrivial e1
                    -> let e1' = inlineExpr env (deAnnotate e1)
                       in inlineExpr (extendMap id e1' env) e2

      -- inline-able let! binding?
      Eval id (Note (Occur Once) e1) e2  
                    -> let e1' = inlineExpr env e1 -- de-annotate
                       in if (firstuse id e2)
                           -- firstuse is true, we can inline immediately
                           then let env' = extendMap id (Eval id e1' (Ap id [])) env  -- NOTE: should we use a fresh id?
                                in inlineExpr env' e2
                           else let e2'  = inlineExpr env e2
                                in if (firstuse id e2')
                                    -- firstuse became true after inlining! re-inline this definition again (is this too expensive?)
                                    then let env' = extendMap id (Eval id e1' (Ap id [])) emptyMap  -- NOTE: should we use a fresh id?
                                         in inlineExpr env' e2'
                                    -- otherwise, don't inline this definition
                                    else Eval id (Note (Occur Once) e1') e2'
      
      -- basic cases
      Let id e1 e2  -> let env' = deleteMap id env
                       in Let id (inlineExpr env e1) (inlineExpr env' e2)
      
      Eval id e1 e2 -> let env' = deleteMap id env 
                       in Eval id (inlineExpr env e1) (inlineExpr env' e2)

      LetRec bs e   -> let (bs',env') = inlineBinds env bs 
                       in LetRec bs' (inlineExpr env' e)

      Match id alts -> case lookupMap id env of
                         Just e  -> -- trivial inlining of a let! binding leads to this configuration.
                                    -- a case-of-known transformation would actually remove this match.
                                    Eval id (Note (Occur Once) e) (Match id (inlineAlts env alts))
                         Nothing -> Match id (inlineAlts env alts)

      Ap id []      -> case lookupMap id env of
                         Just e   -> e
                         Nothing  -> Ap id []
      Ap id args    -> let args0 = inlineExprs env args
                       in case lookupMap id env of
                            Just e   -> case e of
                                          Ap id1 args1 -> Ap id1 (args1 ++ args0)     -- flatten applications
                                          Eval id1 e1 (Ap id2 [])  | id1==id2         -- special case for the strict inliner
                                                       -> Eval id1 e1 (Ap id1 args0)   
                                          other        -> Let id e (Ap id args)       -- don't inline!
                            Nothing  -> Ap id args0
      Con con args  -> Con (inlineCon env con)  (inlineExprs env args)
      Prim id args  -> Prim id (inlineExprs env args)
      Lit lit       -> expr
      Note note e   -> Note note (inlineExpr env e)

inlineCon env con
  = case con of
      ConTag tag arity  -> ConTag (inlineExpr env tag) arity
      other             -> con

inlineExprs :: Env -> [Expr] -> [Expr]
inlineExprs env exprs
  = [inlineExpr env expr | expr <- exprs]

inlineBinds :: Env -> [(Id,Expr)] -> ([(Id,Expr)],Env)
inlineBinds env binds 
  = let env' = removeIds (map fst binds) env
    in ([(id,inlineExpr env' e) | (id,e) <- binds], env')

inlineAlts :: Env -> [Alt] -> [Alt]
inlineAlts env alts
  = [inlineAlt env alt | alt <- alts]

inlineAlt env (Alt pat expr)
  = Alt pat (inlineExpr (removeIds (patIds pat) env) expr)
  where
    patIds (PatVar id)      = [id]
    patIds (PatCon con ids) = ids
    patIds (PatLit lit)     = []


{---------------------------------------------------------------
  deAnnotate
---------------------------------------------------------------}
deAnnotate :: Expr -> Expr
deAnnotate expr
  = case expr of
      Note note e  -> deAnnotate e
      other        -> expr

{---------------------------------------------------------------
  trivial   
---------------------------------------------------------------}
trivial :: Expr -> Bool
trivial expr
  = case expr of
      Note note e         -> trivial e
      Ap id []            -> True
      Con (ConId id) []   -> True
      Lit lit             -> True
      other               -> False

whnfTrivial :: Expr -> Bool
whnfTrivial expr
  = case expr of
      Note note e         -> whnfTrivial e
      Con (ConId id) []   -> True
      Lit lit             -> True
      other               -> False

{---------------------------------------------------------------
  firstuse
---------------------------------------------------------------}
firstuse x expr
  = first x False expr

firsts x c exprs
  = foldl (first x) c exprs

first x c expr
  = case expr of
      LetRec bs e   -> firsts x c (map snd bs ++ [e])
      Let id e1 e2  -> firsts x c [e1,e2]
      Eval id e1 e2 -> first x False e1
      Match id alts -> False
      Prim id args  -> firsts x False args
      Ap id args    | null args && id == x -> True
                    | not (null args)      -> firsts x c ([Ap id []] ++ args)
      Con id args   -> firsts x c args
      Note note e   -> first x c e
      other         -> c