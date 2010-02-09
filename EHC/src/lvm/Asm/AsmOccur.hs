{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: AsmOccur.hs 222 2004-02-14 16:33:04Z uust $
module Lvm.Asm.AsmOccur( asmOccur ) where

import Lvm.Common.Id     ( Id )
import Lvm.Common.IdMap  ( IdMap, emptyMap, insertMapWith, unionMapWith, deleteMap, lookupMap )
import Lvm.Lvm.Module ( mapValues )
import Lvm.Asm.Asm

{---------------------------------------------------------------
  Occ: maps identifiers to the number of syntactic
  occurrences
---------------------------------------------------------------}
type Occ  = IdMap Int

addOcc :: Id -> Occ -> Occ
addOcc id occ 
  = insertMapWith id 1 (\n -> n+1) occ

delOcc id occ
  = deleteMap id occ

unionOcc occ1 occ2
  = unionMapWith (+) occ1 occ2

unionOccs occs
  = foldr unionOcc emptyMap occs

occur :: Id -> Occ -> Occur
occur id occ
  = case lookupMap id occ of
      Just 0  -> Never
      Just 1  -> Once
      Just n  -> Many
      Nothing -> Never

{---------------------------------------------------------------
  asmOccur: annotate every Eval and Let binding with
  the number of syntactic occurrences.
---------------------------------------------------------------}
asmOccur :: AsmModule -> AsmModule
asmOccur mod
  = mapValues occTop mod

occTop :: Top -> Top
occTop (Top id expr)
  = let (expr',occ) = occExpr expr
    in  Top id expr'

occExpr :: Expr -> (Expr,Occ)
occExpr expr
  = case expr of
      -- TODO: if we determine that a let bound variable is never used, 
      -- we can delete occurrences in its definition *if* an inliner 
      -- removes all dead let bindings. This would make the occurrence 
      -- analysis more precise
      Eval id e1 e2 -> let (e1',occ1)  = occExpr e1
                           (e2',occ2)  = occExpr e2
                           occ         = unionOcc occ1 occ2
                       in (Eval id (Note (Occur (occur id occ)) e1') e2', delOcc id occ)
      Let id e1 e2  -> let (e2',occ2) = occExpr e2  
                           (e1',occ1) = occExpr e1
                           occ        = unionOcc occ1 occ2
                       in (Let id (Note (Occur (occur id occ)) e1') e2', delOcc id occ)
      LetRec bs e   -> let (e',occ1)  = occExpr e
                           (ids,es)   = unzip bs
                           (es',occ2) = occExprs es
                           occ        = unionOcc occ1 occ2
                           bs'        = [(id, Note (Occur (occur id occ)) e) | (id,e) <- zip ids es']
                       in (LetRec bs' e', foldr delOcc occ ids)                       

      Match id alts -> let (alts',occ) = occAlts alts
                       in  (Match id alts',addOcc id occ)
      Ap id atoms   -> let (atoms',occ) = occExprs atoms
                       in (Ap id atoms',addOcc id occ)
      Con con atoms -> let (atoms',occ1) = occExprs atoms
                           (con',occ2)   = occCon con
                       in (Con con' atoms',unionOcc occ1 occ2)
      Prim id atoms -> let (atoms',occ) = occExprs atoms
                       in (Prim id atoms',occ)
      Lit lit       -> (expr,emptyMap)
      Note note e   -> let (expr',occ) = occExpr e 
                       in (Note note expr',occ)
        
occCon con
  = case con of
      ConTag tag arity  -> let (tag',occ) = occExpr tag in (ConTag tag' arity,occ)
      other             -> (con,emptyMap)

occExprs :: [Expr] -> ([Expr],Occ)
occExprs exprs
  = let (exprs',occs) = unzip (map occExpr exprs)
    in (exprs',unionOccs occs)

occAlts alts
  = let (alts',occs) = unzip (map occAlt alts)
    in  (alts',unionOccs occs)

occAlt (Alt pat expr)
  = let (expr',occ) = occExpr expr
    in (Alt pat expr',foldr delOcc occ (patIds pat))
  where
    patIds (PatVar id)      = [id]
    patIds (PatCon con ids) = ids
    patIds (PatLit lit)     = []


                         