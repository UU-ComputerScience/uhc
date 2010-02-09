{------------------------------------------------------------------------
  The Core Assembler.

  Daan Leijen.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: AsmPretty.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Asm.AsmPretty( asmPretty )  where

import Lvm.Common.PPrint
import Lvm.Common.Byte         ( stringFromBytes )
import Lvm.Common.Id           ( stringFromId )
import Lvm.Asm.Asm
import Lvm.Lvm.ModulePretty ( modulePretty )

{---------------------------------------------------------------
  pretty print Asm expressions
---------------------------------------------------------------}
asmPretty :: AsmModule -> Doc
asmPretty mod
  = modulePretty ppTop mod

ppTop (Top args expr)
  = nest 2 (text "\\" <> hsep (map ppId args) <+> text "->" <$> ppExpr expr)

{---------------------------------------------------------------
  expressions
---------------------------------------------------------------}
ppExpr expr 
  = ppExprEx id expr

ppArg expr
  = ppExprEx parens expr

ppExprEx pars expr
  = case expr of
      Let id atom e   -> pars $ align $ hang 3 (text "let" <+> ppBind (id,atom)) <$> (text "in" <+> ppExpr e)
      LetRec binds e  -> pars $ align $ hang 7 (text "letrec" <+> vcat (map ppBind binds)) <$> nest 3 (text "in" <+> ppExpr e)
      Eval id e e'    -> pars $ align $ hang 7 (text "let!" <+> ppId id <+> text "=" </> ppExpr e) 
                                        <$> nest 3 (text "in" <+> ppExpr e')
      Match id alts   -> pars $ align $ hang 2 (text "match" <+> ppId id <+> text "with" <$> vcat (map ppAlt alts))
      Prim id args    -> pars $ text "prim" <> char '[' <> (ppId id) <+> hsep (map ppArg args) <> char ']'
      Ap id []        -> ppId id
      Ap id args      -> pars $ ppId id <+> hsep (map ppArg args)
      Con con []      -> ppCon ppExpr con
      Con con args    -> pars $ ppCon ppExpr con <+> hsep (map ppArg args)
      Lit lit         -> ppLit lit
      Note note e     -> pars $ align $ ppNote note </> ppExpr e

ppCon ppTag con
  = case con of
      ConId id          -> ppId id
      ConTag tag arity  -> text "(@" <> ppTag tag <> char ',' <> pretty arity <> char ')'

ppNote (Occur occ)
  = angled (ppOccur occ)

ppOccur occ
  = case occ of
      Never -> text "never"
      Once  -> text "once"
      Many  -> text "many"


ppBind (id,atom)
  = ppId id <+> text "=" <+> ppExpr atom


{---------------------------------------------------------------
  alternatives
---------------------------------------------------------------}
ppAlt (Alt pat expr)
  = hang 2 (ppPat pat <+> text "->" </> ppExpr expr)

ppPat pat
  = case pat of
      PatCon con params -> ppCon pretty con <+> hsep (map ppId params)
      PatVar id         -> ppId id
      PatLit lit        -> ppLit lit

{---------------------------------------------------------------
  literals and variables
---------------------------------------------------------------}
ppLit lit
  = case lit of
      LitInt i      -> pretty i
      LitFloat d    -> pretty d
      LitBytes b    -> dquotes (string (stringFromBytes b))

ppId id
  = text (stringFromId id)

commaBraces doc
  = encloseSep lbrace rbrace comma doc

angled d
  = char '<' <> d <> char '>'
