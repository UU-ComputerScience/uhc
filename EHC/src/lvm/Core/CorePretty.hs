{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CorePretty.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CorePretty ( corePretty ) where

import Lvm.Common.PPrint
import Lvm.Common.Byte         ( stringFromBytes )
import Lvm.Common.Id           ( Id, stringFromId )
import Lvm.Common.IdSet        ( listFromSet )
import Lvm.Core.Core
import Lvm.Lvm.ModulePretty ( modulePretty, ppId, ppVarId, ppConId )
----------------------------------------------------------------
--
----------------------------------------------------------------
corePretty :: CoreModule -> Doc
corePretty  mod
  = modulePretty (ppExpr 0) mod

----------------------------------------------------------------
--
----------------------------------------------------------------
ppExpr :: Int -> Expr -> Doc
ppExpr  p expr
  = case expr of
   --   (Let (Strict (Bind id1 expr)) (Match id2 alts)) | id1 == id2
   --               -> prec 0 $ hang 2 (text "case" <+> ppExpr 0 expr <+> text "of" <+> ppId id1 <$> ppAlts alts)
      Match x as  -> prec 0 $ align (text "match" <+> ppVarId x <+> text "with" <+> text "{" <$> ppAlts  as
                              <+> text "}")
      Let bs x    -> prec 0 $ align (ppLetBinds bs (text "in" <+> ppExpr  0 x))
      Lam id x    -> prec 0 $ text "\\" <> ppVarId id <+> ppLams "->" (</>)  x
      Ap e1 e2    -> prec 9 $ ppExpr  9 e1 <+> ppExpr  10 e2
      Var id      -> ppVarId  id
      Con con     -> ppCon (ppExpr 0) con
      Lit lit     -> ppLit lit
      Note (FreeVar fv) e
                -> align (text "{" <+> sep (map (ppVarId ) (listFromSet fv)) <+> text "}"
                         <$> ppExpr p e)
      Note n e  -> ppExpr p e
      other     -> text "<unknown>"
  where
    prec p'  | p' >= p   = id
             | otherwise = parens

ppCon ppTag con
  = case con of
     ConId id         -> ppConId id
     ConTag tag arity -> text "#(" <> ppTag tag <> char ',' <> pretty arity <> text ")"

----------------------------------------------------------------
--
----------------------------------------------------------------
ppLams arrow next  x
  = case x of
      Lam id x -> ppVarId id <+> ppLams arrow next  x
      other    -> text arrow `next` ppExpr  0 x


ppLetBinds binds doc
  = case binds of
      NonRec bind -> nest 4 (text "let" <+> ppBind bind) <$> doc
      Strict bind -> nest 5 (text "let!" <+> ppBind bind) <$> doc
      Rec recs    -> nest 8 (text "let rec" <+> ppBinds recs) <$> doc

ppBinds  binds
  = vcat (map ppBind binds)

ppBind (Bind id expr)
  = nest 2 (ppId  id <+> ppLams "=" (</>)  expr <> semi)


ppAlts  alts
  = vcat (map ppAlt alts)

ppAlt  (Alt pat expr)
  = nest 4 (text "|" <+> ppPat pat <+> text "->" </> ppExpr  0 expr)

----------------------------------------------------------------
--
----------------------------------------------------------------
ppPat pat
  = case pat of
      PatCon con ids -> hsep (ppCon pretty con : map (ppVarId) ids)
      PatLit lit  -> ppLit lit
      PatDefault  -> text "_"


ppLit lit
  = case lit of
      LitInt i    -> pretty i
      LitDouble d -> pretty d
      LitBytes s  -> text (show (stringFromBytes s))

