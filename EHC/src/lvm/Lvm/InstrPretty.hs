{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: InstrPretty.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Lvm.InstrPretty( instrPretty  ) where

import Lvm.Common.PPrint
import Lvm.Common.Byte   ( Bytes, stringFromBytes )
import Lvm.Common.Id     ( Id, stringFromId )
import Lvm.Lvm.Instr

----------------------------------------------------------------
-- instrPretty
----------------------------------------------------------------
instrPretty :: [Instr] -> Doc
instrPretty instrs
  = ppInstrs instrs


----------------------------------------------------------------
-- Pretty
----------------------------------------------------------------
ppInstrs :: [Instr] -> Doc
ppInstrs instrs
  = align (vcat (map (ppInstr ) instrs))

ppInstr instr
  = let name = nameFromInstr instr
    in case instr of
      -- pseudo instructions
      VAR         id          -> text name <+> ppId id
      PARAM       id          -> text name <+> ppId id
      USE         id          -> text name <+> ppId id
      NOP                     -> text name
      
      ATOM        is          -> nest 2 (text name <$> ppInstrs is)
      INIT        is          -> nest 2 (text name <$> ppInstrs is)

    -- structured instructions
      CATCH instrs            -> nest 2 (text name <$> ppInstrs instrs)
      EVAL d instrs           -> nest 2 (text name <+> pretty d <$> ppInstrs instrs)
      RESULT instrs           -> nest 2 (text name <$> ppInstrs instrs)

      SWITCHCON alts          -> nest 2 (text name <$> ppAlts alts)
      MATCHCON alts           -> nest 2 (text name <$> ppAlts alts)
      MATCHINT alts           -> nest 2 (text name <$> ppAlts alts)
      MATCH alts              -> nest 2 (text name <$> ppAlts alts)


    -- push instructions
      PUSHVAR     var         -> text name <+> ppVar var
      PUSHINT     n           -> text name <+> pretty n
      PUSHBYTES   bs c        -> text name <+> ppBytes bs
      PUSHFLOAT   d           -> text name <+> pretty d
      PUSHCODE    global      -> text name <+> ppGlobal global
      PUSHCONT    ofs         -> text name <+> pretty ofs

    -- stack instructions
      ARGCHK      n           -> text name  <+> pretty n
      SLIDE       n m depth   -> text name  <+> pretty n <+> pretty m <+> pretty depth
      STUB        var         -> text name <+> ppVar var

    -- control
      ENTER                   -> text name
      RAISE                   -> text name
      CALL        global      -> text name <+> ppGlobal global

      ENTERCODE   global      -> text name <+> ppGlobal global
      EVALVAR     var         -> text name <+> ppVar var

      RETURN                  -> text name
      RETURNCON   con         -> text name <+> ppCon con
      RETURNINT   n           -> text name <+> pretty n

    -- applications
      ALLOCAP     arity       -> text name <+> pretty arity
      PACKAP      var arity   -> text name  <+> ppVar var <+> pretty arity
      PACKNAP     var arity   -> text name <+> ppVar var <+> pretty arity
      NEWAP       arity       -> text name   <+> pretty arity
      NEWNAP      arity       -> text name  <+> pretty arity

    -- constructors
      ALLOCCON    con         -> text name <+> ppCon con
      PACKCON     con var     -> text name  <+> ppVar var <+> ppCon con
      NEWCON      con         -> text name   <+> ppCon con
      
      NEW arity               -> text name <+> pretty arity
      PACK arity var          -> text name <+> pretty arity <+> ppVar var
      UNPACK arity            -> text name <+> pretty arity

    -- optimized instructions
      PUSHVARS2  v w          -> text name <+> ppVar v <+> ppVar w

      NEWCON0 con             -> text name <+> ppCon con
      NEWCON1 con             -> text name <+> ppCon con
      NEWCON2 con             -> text name <+> ppCon con
      NEWCON3 con             -> text name <+> ppCon con

      RETURNCON0 con          -> text name <+> ppCon con

    -- others
      other                   -> text name

ppAlts alts
  = vcat (map ppAlt alts)

ppAlt (Alt pat is)
  = nest 2 (ppPat pat <> text ":" <$> ppInstrs is)

ppPat pat
  = case pat of
      PatCon con  -> ppCon con
      PatInt i    -> pretty i
      PatTag t a  -> text "(@" <> pretty t <> char ',' <> pretty a <> text ")"
      PatDefault  -> text "<default>"

ppCon (Con id c arity tag)
  = ppId id <+> pretty arity

ppGlobal (Global id c arity)
  = ppId id <+> pretty arity

ppVar (Var id ofs depth)
  = ppId id <+> parens( pretty ofs <> comma <+> pretty depth )

ppId id
  = text (stringFromId id)

ppBytes bs
  = dquotes (string (stringFromBytes bs))
