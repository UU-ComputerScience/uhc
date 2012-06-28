

-- UUAGC 0.9.39.1 (build/ruler2/AbsSyn/AbsSyn2.ag)
module AbsSyn.AbsSyn2(module Expr.Expr, module Ty.Ty, module ARule.ARule, module ViewSel.ViewSel
, AGItf (..), Decl (..), Decls, RsVwDecl (..), RsVwDecls, AttrAGDecl (..), DataAGDecl (..), DataAGAlt (..), DataAGAlts
, DataAGFld (..), DataAGFlds, VwDecl (..), VwDecls, RlDecl (..), RlDecls, Jd (..), Jds, JdAt (..), JdAts, wrapInChunk) where

import qualified Data.Set as Set
import Opts
import Common
import Expr.Expr
import Ty.Ty
import ARule.ARule
import ViewSel.ViewSel













wrapInChunk :: (Nm -> a->a) -> Opts -> Nm -> a -> a
wrapInChunk mk opts n
  = if optFragWrap opts
    then mk n
    else id
-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf (Decls ) 
-- AttrAGDecl --------------------------------------------------
data AttrAGDecl  = AttrAGDecl_Attr (Nm) (([(Nm,Nm)])) (([(Nm,Nm)])) (([(Nm,Nm)])) 
-- DataAGAlt ---------------------------------------------------
data DataAGAlt  = DataAGAlt_Alt (Nm) (DataAGFlds ) 
-- DataAGAlts --------------------------------------------------
type DataAGAlts  = [DataAGAlt ]
-- DataAGDecl --------------------------------------------------
data DataAGDecl  = DataAGDecl_Data (Nm) (DataAGAlts ) 
-- DataAGFld ---------------------------------------------------
data DataAGFld  = DataAGFld_Fld (Nm) (Ty) (Bool) 
-- DataAGFlds --------------------------------------------------
type DataAGFlds  = [DataAGFld ]
-- Decl --------------------------------------------------------
data Decl  = Decl_AttrAG (AttrAGDecl ) 
           | Decl_Chunk (Nm) (Decl ) 
           | Decl_DataAG (DataAGDecl ) 
           | Decl_Preamble (String) 
           | Decl_RsVw (RsVwDecl ) 
           | Decl_ScVwAtExplain (([(Expr,Expr)])) 
           | Decl_ScVwExplain (Expr) 
-- Decls -------------------------------------------------------
type Decls  = [Decl ]
-- Jd ----------------------------------------------------------
data Jd  = Jd_Ats (Nm) (Nm) (JdAts ) 
         | Jd_Expr (Nm) (Nm) (Expr) (Bool) 
         | Jd_LTX (Nm) (Nm) (Expr) (Bool) 
-- JdAt --------------------------------------------------------
data JdAt  = JdAt_At (Nm) (Expr) 
-- JdAts -------------------------------------------------------
type JdAts  = [JdAt ]
-- Jds ---------------------------------------------------------
type Jds  = [Jd ]
-- RlDecl ------------------------------------------------------
data RlDecl  = RlDecl_AG (Nm) (SPos) (ARule) 
             | RlDecl_Chunk (Nm) (RlDecl ) 
             | RlDecl_LTX (Nm) (Nm) (Nm) (SPos) (Jds ) (Jds ) 
             | RlDecl_LTXAlias (Nm) (Nm) 
             | RlDecl_Rl (Nm) (Nm) (SPos) (Nm) (Jds ) (Jds ) 
-- RlDecls -----------------------------------------------------
type RlDecls  = [RlDecl ]
-- RsVwDecl ----------------------------------------------------
data RsVwDecl  = RsVwDecl_Rs (Nm) (Nm) (String) (VwDecls ) 
-- RsVwDecls ---------------------------------------------------
type RsVwDecls  = [RsVwDecl ]
-- VwDecl ------------------------------------------------------
data VwDecl  = VwDecl_Grp (Nm) (Nm) (([(Nm,Nm)])) 
             | VwDecl_LTX (Nm) (Nm) (Expr) (RlDecls ) 
             | VwDecl_LTXFig (Nm) (Nm) (Nm) (String) (([Nm])) 
             | VwDecl_Vw (Nm) (Nm) (RlDecls ) 
-- VwDecls -----------------------------------------------------
type VwDecls  = [VwDecl ]