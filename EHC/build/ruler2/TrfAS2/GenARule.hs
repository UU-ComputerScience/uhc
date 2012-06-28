

-- UUAGC 0.9.39.1 (build/ruler2/TrfAS2/GenARule.ag)
module TrfAS2.GenARule(as2ARule) where

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Utils
import Opts
import Err
import Common
import KeywParser (propsSynInhMp)
import Expr.Utils
import ARule.Utils
import FmGam
import RwExprGam
import ECnstrGam
import AbsSyn.AbsSyn2
import Admin
import Utils (sc2DATA)
import EH.Util.Pretty















as2ARule :: Opts -> DtInvGam -> ScGam Expr -> FmGam Expr -> RwExprGam -> Decls -> (Decls,PP_Doc,[Err])
as2ARule o dig scg fmg rwg r
  = (self_Syn_AGItf r2,ppDbg_Syn_AGItf r2,errL_Syn_AGItf r2)
  where r1 = sem_AGItf (AGItf_AGItf r)
        r2 = wrap_AGItf r1
                (Inh_AGItf {opts_Inh_AGItf = o, fmGam_Inh_AGItf = fmg, rwGam_Inh_AGItf = rwg, scGam_Inh_AGItf = scg, dtInvGam_Inh_AGItf = dig})

-- AEqn --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local self        : _
      alternative Err:
         child expr           : Expr 
         visit 0:
            local self        : _
-}
-- cata
sem_AEqn :: AEqn  ->
            T_AEqn 
sem_AEqn (AEqn_Eqn _dest _val )  =
    (sem_AEqn_Eqn (sem_AEqnDest _dest ) (sem_AExpr _val ) )
sem_AEqn (AEqn_Err _expr )  =
    (sem_AEqn_Err (sem_Expr _expr ) )
-- semantic domain
type T_AEqn  = ( AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (let _lhsOself :: AEqn 
         _destIself :: AEqnDest 
         _valIself :: AExpr 
         -- self rule
         _self =
             AEqn_Eqn _destIself _valIself
         -- self rule
         _lhsOself =
             _self
         ( _destIself) =
             dest_ 
         ( _valIself) =
             val_ 
     in  ( _lhsOself))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (let _lhsOself :: AEqn 
         _exprIself :: Expr 
         -- self rule
         _self =
             AEqn_Err _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
            local self        : _
      alternative One:
         child anm            : ANm 
         visit 0:
            local self        : _
-}
-- cata
sem_AEqnDest :: AEqnDest  ->
                T_AEqnDest 
sem_AEqnDest (AEqnDest_Many _dests )  =
    (sem_AEqnDest_Many (sem_AEqnDests _dests ) )
sem_AEqnDest (AEqnDest_One _anm )  =
    (sem_AEqnDest_One (sem_ANm _anm ) )
-- semantic domain
type T_AEqnDest  = ( AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (let _lhsOself :: AEqnDest 
         _destsIself :: AEqnDests 
         -- self rule
         _self =
             AEqnDest_Many _destsIself
         -- self rule
         _lhsOself =
             _self
         ( _destsIself) =
             dests_ 
     in  ( _lhsOself))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (let _lhsOself :: AEqnDest 
         _anmIself :: ANm 
         -- self rule
         _self =
             AEqnDest_One _anmIself
         -- self rule
         _lhsOself =
             _self
         ( _anmIself) =
             anm_ 
     in  ( _lhsOself))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_AEqnDests :: AEqnDests  ->
                 T_AEqnDests 
sem_AEqnDests list  =
    (Prelude.foldr sem_AEqnDests_Cons sem_AEqnDests_Nil (Prelude.map sem_AEqnDest list) )
-- semantic domain
type T_AEqnDests  = ( AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (let _lhsOself :: AEqnDests 
         _hdIself :: AEqnDest 
         _tlIself :: AEqnDests 
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (let _lhsOself :: AEqnDests 
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqn 
         child tl             : AEqns 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_AEqns :: AEqns  ->
             T_AEqns 
sem_AEqns list  =
    (Prelude.foldr sem_AEqns_Cons sem_AEqns_Nil (Prelude.map sem_AEqn list) )
-- semantic domain
type T_AEqns  = ( AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (let _lhsOself :: AEqns 
         _hdIself :: AEqn 
         _tlIself :: AEqns 
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (let _lhsOself :: AEqns 
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local self        : _
-}
-- cata
sem_AExpr :: AExpr  ->
             T_AExpr 
sem_AExpr (AExpr_Expr _expr )  =
    (sem_AExpr_Expr (sem_Expr _expr ) )
-- semantic domain
type T_AExpr  = ( AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (let _lhsOself :: AExpr 
         _exprIself :: Expr 
         -- self rule
         _self =
             AExpr_Expr _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : ARule 
   alternatives:
      alternative AGItf:
         child rule           : ARule 
-}
-- cata
sem_AGARuleItf :: AGARuleItf  ->
                  T_AGARuleItf 
sem_AGARuleItf (AGARuleItf_AGItf _rule )  =
    (sem_AGARuleItf_AGItf (sem_ARule _rule ) )
-- semantic domain
type T_AGARuleItf  = ( ARule )
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (let _lhsOself :: ARule 
         _ruleIself :: ARule 
         -- copy rule (up)
         _lhsOself =
             _ruleIself
         ( _ruleIself) =
             rule_ 
     in  ( _lhsOself))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : Expr 
   alternatives:
      alternative AGItf:
         child expr           : Expr 
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = ( Expr )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- copy rule (up)
         _lhsOself =
             _exprIself
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
-- AGItf -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : Decls
   alternatives:
      alternative AGItf:
         child decls          : Decls 
         visit 0:
            local fm          : _
            local opts        : _
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _decls )  =
    (sem_AGItf_AGItf (sem_Decls _decls ) )
-- semantic domain
type T_AGItf  = DtInvGam ->
                (FmGam Expr) ->
                Opts ->
                RwExprGam ->
                (ScGam Expr) ->
                ( ([Err]),PP_Doc,Decls)
data Inh_AGItf  = Inh_AGItf {dtInvGam_Inh_AGItf :: DtInvGam,fmGam_Inh_AGItf :: (FmGam Expr),opts_Inh_AGItf :: Opts,rwGam_Inh_AGItf :: RwExprGam,scGam_Inh_AGItf :: (ScGam Expr)}
data Syn_AGItf  = Syn_AGItf {errL_Syn_AGItf :: ([Err]),ppDbg_Syn_AGItf :: PP_Doc,self_Syn_AGItf :: Decls}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf _lhsIdtInvGam _lhsIfmGam _lhsIopts _lhsIrwGam _lhsIscGam )  =
    (let ( _lhsOerrL,_lhsOppDbg,_lhsOself) = sem _lhsIdtInvGam _lhsIfmGam _lhsIopts _lhsIrwGam _lhsIscGam 
     in  (Syn_AGItf _lhsOerrL _lhsOppDbg _lhsOself ))
sem_AGItf_AGItf :: T_Decls  ->
                   T_AGItf 
sem_AGItf_AGItf decls_  =
    (\ _lhsIdtInvGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decls
              _declsOdtInvGam :: DtInvGam
              _declsOfm :: FmKind
              _declsOfmGam :: (FmGam Expr)
              _declsOopts :: Opts
              _declsOrwGam :: RwExprGam
              _declsOscGam :: (ScGam Expr)
              _declsIerrL :: ([Err])
              _declsIppDbg :: PP_Doc
              _declsIself :: Decls 
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 26, column 21)
              _fm =
                  fmAS2Fm (optGenFM _lhsIopts)
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 26, column 21)
              _opts =
                  _lhsIopts {optGenFM = _fm}
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _declsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _declsIppDbg
              -- copy rule (up)
              _lhsOself =
                  _declsIself
              -- copy rule (down)
              _declsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (from local)
              _declsOfm =
                  _fm
              -- copy rule (down)
              _declsOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _declsOopts =
                  _opts
              -- copy rule (down)
              _declsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declsOscGam =
                  _lhsIscGam
              ( _declsIerrL,_declsIppDbg,_declsIself) =
                  decls_ _declsOdtInvGam _declsOfm _declsOfmGam _declsOopts _declsOrwGam _declsOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local self        : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local self        : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Wild:
         visit 0:
            local self        : _
-}
-- cata
sem_ANm :: ANm  ->
           T_ANm 
sem_ANm (ANm_Fld _nm )  =
    (sem_ANm_Fld _nm )
sem_ANm (ANm_Lhs _nm _props )  =
    (sem_ANm_Lhs _nm _props )
sem_ANm (ANm_Loc _nm _props )  =
    (sem_ANm_Loc _nm _props )
sem_ANm (ANm_Node _ndNm _nm )  =
    (sem_ANm_Node _ndNm _nm )
sem_ANm (ANm_Wild )  =
    (sem_ANm_Wild )
-- semantic domain
type T_ANm  = ( ANm )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Fld nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Lhs nm_ props_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Loc nm_ props_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Node ndNm_ nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (let _lhsOself :: ANm 
         -- self rule
         _self =
             ANm_Wild
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ARule -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local self        : _
-}
-- cata
sem_ARule :: ARule  ->
             T_ARule 
sem_ARule (ARule_Rule _ndNmL _rlNm _info _eqns )  =
    (sem_ARule_Rule _ndNmL _rlNm _info (sem_AEqns _eqns ) )
-- semantic domain
type T_ARule  = ( ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (let _lhsOself :: ARule 
         _eqnsIself :: AEqns 
         -- self rule
         _self =
             ARule_Rule ndNmL_ rlNm_ info_ _eqnsIself
         -- self rule
         _lhsOself =
             _self
         ( _eqnsIself) =
             eqns_ 
     in  ( _lhsOself))
-- ARules ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : ARule 
         child tl             : ARules 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_ARules :: ARules  ->
              T_ARules 
sem_ARules list  =
    (Prelude.foldr sem_ARules_Cons sem_ARules_Nil (Prelude.map sem_ARule list) )
-- semantic domain
type T_ARules  = ( ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (let _lhsOself :: ARules 
         _hdIself :: ARule 
         _tlIself :: ARules 
         -- self rule
         _self =
             (:) _hdIself _tlIself
         -- self rule
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (let _lhsOself :: ARules 
         -- self rule
         _self =
             []
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- AttrAGDecl --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Attr:
         child ndNm           : {Nm}
         child inhAts         : {[(Nm,Nm)]}
         child inhsynAts      : {[(Nm,Nm)]}
         child synAts         : {[(Nm,Nm)]}
         visit 0:
            local self        : _
-}
-- cata
sem_AttrAGDecl :: AttrAGDecl  ->
                  T_AttrAGDecl 
sem_AttrAGDecl (AttrAGDecl_Attr _ndNm _inhAts _inhsynAts _synAts )  =
    (sem_AttrAGDecl_Attr _ndNm _inhAts _inhsynAts _synAts )
-- semantic domain
type T_AttrAGDecl  = DtInvGam ->
                     FmKind ->
                     (FmGam Expr) ->
                     Opts ->
                     RwExprGam ->
                     (ScGam Expr) ->
                     ( ([Err]),PP_Doc,AttrAGDecl )
sem_AttrAGDecl_Attr :: Nm ->
                       ([(Nm,Nm)]) ->
                       ([(Nm,Nm)]) ->
                       ([(Nm,Nm)]) ->
                       T_AttrAGDecl 
sem_AttrAGDecl_Attr ndNm_ inhAts_ inhsynAts_ synAts_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: AttrAGDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  AttrAGDecl_Attr ndNm_ inhAts_ inhsynAts_ synAts_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- DataAGAlt ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Alt:
         child nm             : {Nm}
         child flds           : DataAGFlds 
         visit 0:
            local self        : _
-}
-- cata
sem_DataAGAlt :: DataAGAlt  ->
                 T_DataAGAlt 
sem_DataAGAlt (DataAGAlt_Alt _nm _flds )  =
    (sem_DataAGAlt_Alt _nm (sem_DataAGFlds _flds ) )
-- semantic domain
type T_DataAGAlt  = DtInvGam ->
                    FmKind ->
                    (FmGam Expr) ->
                    Opts ->
                    RwExprGam ->
                    (ScGam Expr) ->
                    ( ([Err]),PP_Doc,DataAGAlt )
sem_DataAGAlt_Alt :: Nm ->
                     T_DataAGFlds  ->
                     T_DataAGAlt 
sem_DataAGAlt_Alt nm_ flds_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGAlt 
              _fldsOdtInvGam :: DtInvGam
              _fldsOfm :: FmKind
              _fldsOfmGam :: (FmGam Expr)
              _fldsOopts :: Opts
              _fldsOrwGam :: RwExprGam
              _fldsOscGam :: (ScGam Expr)
              _fldsIerrL :: ([Err])
              _fldsIppDbg :: PP_Doc
              _fldsIself :: DataAGFlds 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _fldsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _fldsIppDbg
              -- self rule
              _self =
                  DataAGAlt_Alt nm_ _fldsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _fldsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _fldsOfm =
                  _lhsIfm
              -- copy rule (down)
              _fldsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _fldsOopts =
                  _lhsIopts
              -- copy rule (down)
              _fldsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _fldsOscGam =
                  _lhsIscGam
              ( _fldsIerrL,_fldsIppDbg,_fldsIself) =
                  flds_ _fldsOdtInvGam _fldsOfm _fldsOfmGam _fldsOopts _fldsOrwGam _fldsOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- DataAGAlts --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : DataAGAlt 
         child tl             : DataAGAlts 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_DataAGAlts :: DataAGAlts  ->
                  T_DataAGAlts 
sem_DataAGAlts list  =
    (Prelude.foldr sem_DataAGAlts_Cons sem_DataAGAlts_Nil (Prelude.map sem_DataAGAlt list) )
-- semantic domain
type T_DataAGAlts  = DtInvGam ->
                     FmKind ->
                     (FmGam Expr) ->
                     Opts ->
                     RwExprGam ->
                     (ScGam Expr) ->
                     ( ([Err]),PP_Doc,DataAGAlts )
sem_DataAGAlts_Cons :: T_DataAGAlt  ->
                       T_DataAGAlts  ->
                       T_DataAGAlts 
sem_DataAGAlts_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGAlts 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: DataAGAlt 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: DataAGAlts 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrwGam _hdOscGam 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrwGam _tlOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_DataAGAlts_Nil :: T_DataAGAlts 
sem_DataAGAlts_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGAlts 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- DataAGDecl --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Data:
         child ndNm           : {Nm}
         child alts           : DataAGAlts 
         visit 0:
            local self        : _
-}
-- cata
sem_DataAGDecl :: DataAGDecl  ->
                  T_DataAGDecl 
sem_DataAGDecl (DataAGDecl_Data _ndNm _alts )  =
    (sem_DataAGDecl_Data _ndNm (sem_DataAGAlts _alts ) )
-- semantic domain
type T_DataAGDecl  = DtInvGam ->
                     FmKind ->
                     (FmGam Expr) ->
                     Opts ->
                     RwExprGam ->
                     (ScGam Expr) ->
                     ( ([Err]),PP_Doc,DataAGDecl )
sem_DataAGDecl_Data :: Nm ->
                       T_DataAGAlts  ->
                       T_DataAGDecl 
sem_DataAGDecl_Data ndNm_ alts_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGDecl 
              _altsOdtInvGam :: DtInvGam
              _altsOfm :: FmKind
              _altsOfmGam :: (FmGam Expr)
              _altsOopts :: Opts
              _altsOrwGam :: RwExprGam
              _altsOscGam :: (ScGam Expr)
              _altsIerrL :: ([Err])
              _altsIppDbg :: PP_Doc
              _altsIself :: DataAGAlts 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _altsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _altsIppDbg
              -- self rule
              _self =
                  DataAGDecl_Data ndNm_ _altsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _altsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _altsOfm =
                  _lhsIfm
              -- copy rule (down)
              _altsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _altsOopts =
                  _lhsIopts
              -- copy rule (down)
              _altsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _altsOscGam =
                  _lhsIscGam
              ( _altsIerrL,_altsIppDbg,_altsIself) =
                  alts_ _altsOdtInvGam _altsOfm _altsOfmGam _altsOopts _altsOrwGam _altsOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- DataAGFld ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         child ty             : {Ty}
         child tyIsData       : {Bool}
         visit 0:
            local self        : _
-}
-- cata
sem_DataAGFld :: DataAGFld  ->
                 T_DataAGFld 
sem_DataAGFld (DataAGFld_Fld _nm _ty _tyIsData )  =
    (sem_DataAGFld_Fld _nm _ty _tyIsData )
-- semantic domain
type T_DataAGFld  = DtInvGam ->
                    FmKind ->
                    (FmGam Expr) ->
                    Opts ->
                    RwExprGam ->
                    (ScGam Expr) ->
                    ( ([Err]),PP_Doc,DataAGFld )
sem_DataAGFld_Fld :: Nm ->
                     Ty ->
                     Bool ->
                     T_DataAGFld 
sem_DataAGFld_Fld nm_ ty_ tyIsData_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGFld 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  DataAGFld_Fld nm_ ty_ tyIsData_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- DataAGFlds --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : DataAGFld 
         child tl             : DataAGFlds 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_DataAGFlds :: DataAGFlds  ->
                  T_DataAGFlds 
sem_DataAGFlds list  =
    (Prelude.foldr sem_DataAGFlds_Cons sem_DataAGFlds_Nil (Prelude.map sem_DataAGFld list) )
-- semantic domain
type T_DataAGFlds  = DtInvGam ->
                     FmKind ->
                     (FmGam Expr) ->
                     Opts ->
                     RwExprGam ->
                     (ScGam Expr) ->
                     ( ([Err]),PP_Doc,DataAGFlds )
sem_DataAGFlds_Cons :: T_DataAGFld  ->
                       T_DataAGFlds  ->
                       T_DataAGFlds 
sem_DataAGFlds_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGFlds 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: DataAGFld 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: DataAGFlds 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrwGam _hdOscGam 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrwGam _tlOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_DataAGFlds_Nil :: T_DataAGFlds 
sem_DataAGFlds_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: DataAGFlds 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative AttrAG:
         child decl           : AttrAGDecl 
         visit 0:
            local self        : _
      alternative Chunk:
         child nm             : {Nm}
         child decl           : Decl 
         visit 0:
            local self        : _
      alternative DataAG:
         child decl           : DataAGDecl 
         visit 0:
            local self        : _
      alternative Preamble:
         child preamble       : {String}
         visit 0:
            local self        : _
      alternative RsVw:
         child decl           : RsVwDecl 
         visit 0:
            local self        : _
      alternative ScVwAtExplain:
         child atExprs        : {[(Expr,Expr)]}
         visit 0:
            local self        : _
      alternative ScVwExplain:
         child exExpr         : {Expr}
         visit 0:
            local self        : _
-}
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl (Decl_AttrAG _decl )  =
    (sem_Decl_AttrAG (sem_AttrAGDecl _decl ) )
sem_Decl (Decl_Chunk _nm _decl )  =
    (sem_Decl_Chunk _nm (sem_Decl _decl ) )
sem_Decl (Decl_DataAG _decl )  =
    (sem_Decl_DataAG (sem_DataAGDecl _decl ) )
sem_Decl (Decl_Preamble _preamble )  =
    (sem_Decl_Preamble _preamble )
sem_Decl (Decl_RsVw _decl )  =
    (sem_Decl_RsVw (sem_RsVwDecl _decl ) )
sem_Decl (Decl_ScVwAtExplain _atExprs )  =
    (sem_Decl_ScVwAtExplain _atExprs )
sem_Decl (Decl_ScVwExplain _exExpr )  =
    (sem_Decl_ScVwExplain _exExpr )
-- semantic domain
type T_Decl  = DtInvGam ->
               FmKind ->
               (FmGam Expr) ->
               Opts ->
               RwExprGam ->
               (ScGam Expr) ->
               ( ([Err]),PP_Doc,Decl )
sem_Decl_AttrAG :: T_AttrAGDecl  ->
                   T_Decl 
sem_Decl_AttrAG decl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              _declOdtInvGam :: DtInvGam
              _declOfm :: FmKind
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOrwGam :: RwExprGam
              _declOscGam :: (ScGam Expr)
              _declIerrL :: ([Err])
              _declIppDbg :: PP_Doc
              _declIself :: AttrAGDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _declIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _declIppDbg
              -- self rule
              _self =
                  Decl_AttrAG _declIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _declOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _declOfm =
                  _lhsIfm
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declOscGam =
                  _lhsIscGam
              ( _declIerrL,_declIppDbg,_declIself) =
                  decl_ _declOdtInvGam _declOfm _declOfmGam _declOopts _declOrwGam _declOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_Chunk :: Nm ->
                  T_Decl  ->
                  T_Decl 
sem_Decl_Chunk nm_ decl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              _declOdtInvGam :: DtInvGam
              _declOfm :: FmKind
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOrwGam :: RwExprGam
              _declOscGam :: (ScGam Expr)
              _declIerrL :: ([Err])
              _declIppDbg :: PP_Doc
              _declIself :: Decl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _declIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _declIppDbg
              -- self rule
              _self =
                  Decl_Chunk nm_ _declIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _declOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _declOfm =
                  _lhsIfm
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declOscGam =
                  _lhsIscGam
              ( _declIerrL,_declIppDbg,_declIself) =
                  decl_ _declOdtInvGam _declOfm _declOfmGam _declOopts _declOrwGam _declOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_DataAG :: T_DataAGDecl  ->
                   T_Decl 
sem_Decl_DataAG decl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              _declOdtInvGam :: DtInvGam
              _declOfm :: FmKind
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOrwGam :: RwExprGam
              _declOscGam :: (ScGam Expr)
              _declIerrL :: ([Err])
              _declIppDbg :: PP_Doc
              _declIself :: DataAGDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _declIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _declIppDbg
              -- self rule
              _self =
                  Decl_DataAG _declIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _declOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _declOfm =
                  _lhsIfm
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declOscGam =
                  _lhsIscGam
              ( _declIerrL,_declIppDbg,_declIself) =
                  decl_ _declOdtInvGam _declOfm _declOfmGam _declOopts _declOrwGam _declOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_Preamble :: String ->
                     T_Decl 
sem_Decl_Preamble preamble_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  Decl_Preamble preamble_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_RsVw :: T_RsVwDecl  ->
                 T_Decl 
sem_Decl_RsVw decl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              _declOdtInvGam :: DtInvGam
              _declOfm :: FmKind
              _declOfmGam :: (FmGam Expr)
              _declOopts :: Opts
              _declOrwGam :: RwExprGam
              _declOscGam :: (ScGam Expr)
              _declIerrL :: ([Err])
              _declIppDbg :: PP_Doc
              _declIself :: RsVwDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _declIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _declIppDbg
              -- self rule
              _self =
                  Decl_RsVw _declIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _declOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _declOfm =
                  _lhsIfm
              -- copy rule (down)
              _declOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _declOopts =
                  _lhsIopts
              -- copy rule (down)
              _declOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _declOscGam =
                  _lhsIscGam
              ( _declIerrL,_declIppDbg,_declIself) =
                  decl_ _declOdtInvGam _declOfm _declOfmGam _declOopts _declOrwGam _declOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_ScVwAtExplain :: ([(Expr,Expr)]) ->
                          T_Decl 
sem_Decl_ScVwAtExplain atExprs_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  Decl_ScVwAtExplain atExprs_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decl_ScVwExplain :: Expr ->
                        T_Decl 
sem_Decl_ScVwExplain exExpr_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  Decl_ScVwExplain exExpr_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls list  =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )
-- semantic domain
type T_Decls  = DtInvGam ->
                FmKind ->
                (FmGam Expr) ->
                Opts ->
                RwExprGam ->
                (ScGam Expr) ->
                ( ([Err]),PP_Doc,Decls )
sem_Decls_Cons :: T_Decl  ->
                  T_Decls  ->
                  T_Decls 
sem_Decls_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decls 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: Decl 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: Decls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrwGam _hdOscGam 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrwGam _tlOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_Decls_Nil :: T_Decls 
sem_Decls_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Decls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Empty:
         visit 0:
            local self        : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
-}
-- cata
sem_ECnstr :: ECnstr  ->
              T_ECnstr 
sem_ECnstr (ECnstr_Empty )  =
    (sem_ECnstr_Empty )
sem_ECnstr (ECnstr_Ty _nms )  =
    (sem_ECnstr_Ty _nms )
sem_ECnstr (ECnstr_Var _nm )  =
    (sem_ECnstr_Var _nm )
-- semantic domain
type T_ECnstr  = ( ECnstr )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Ty nms_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let _lhsOself :: ECnstr 
         -- self rule
         _self =
             ECnstr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local self        : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
      alternative Empty:
         visit 0:
            local self        : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Int:
         child int            : {String}
         visit 0:
            local self        : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local self        : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local self        : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local self        : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local self        : _
      alternative Undefined:
         visit 0:
            local self        : _
      alternative Uniq:
         visit 0:
            local self        : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local self        : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local self        : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local self        : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_AVar _anm )  =
    (sem_Expr_AVar (sem_ANm _anm ) )
sem_Expr (Expr_App _lExpr _rExpr )  =
    (sem_Expr_App (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_AppTop _expr )  =
    (sem_Expr_AppTop (sem_Expr _expr ) )
sem_Expr (Expr_ChildOrder _seqNr _expr )  =
    (sem_Expr_ChildOrder _seqNr (sem_Expr _expr ) )
sem_Expr (Expr_Cnstr _expr _cnstr )  =
    (sem_Expr_Cnstr (sem_Expr _expr ) (sem_ECnstr _cnstr ) )
sem_Expr (Expr_Empty )  =
    (sem_Expr_Empty )
sem_Expr (Expr_Expr _expr )  =
    (sem_Expr_Expr (sem_Expr _expr ) )
sem_Expr (Expr_Int _int )  =
    (sem_Expr_Int _int )
sem_Expr (Expr_LF _lExpr _rExpr )  =
    (sem_Expr_LF (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Named _nm _expr )  =
    (sem_Expr_Named _nm (sem_Expr _expr ) )
sem_Expr (Expr_Op _nm _nmExpr _lExpr _rExpr )  =
    (sem_Expr_Op _nm (sem_Expr _nmExpr ) (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Paren _expr )  =
    (sem_Expr_Paren (sem_Expr _expr ) )
sem_Expr (Expr_Retain _expr )  =
    (sem_Expr_Retain (sem_Expr _expr ) )
sem_Expr (Expr_SP _lExpr _rExpr )  =
    (sem_Expr_SP (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Sel _expr _selMbExpr )  =
    (sem_Expr_Sel (sem_Expr _expr ) (sem_MbExpr _selMbExpr ) )
sem_Expr (Expr_SelTop _expr )  =
    (sem_Expr_SelTop (sem_Expr _expr ) )
sem_Expr (Expr_StrAsIs _str )  =
    (sem_Expr_StrAsIs _str )
sem_Expr (Expr_StrText _str )  =
    (sem_Expr_StrText _str )
sem_Expr (Expr_Undefined )  =
    (sem_Expr_Undefined )
sem_Expr (Expr_Uniq )  =
    (sem_Expr_Uniq )
sem_Expr (Expr_Var _nm )  =
    (sem_Expr_Var _nm )
sem_Expr (Expr_Wrap _wrKind _expr )  =
    (sem_Expr_Wrap _wrKind (sem_Expr _expr ) )
sem_Expr (Expr_WrapCnstr _cnstr )  =
    (sem_Expr_WrapCnstr (sem_ECnstr _cnstr ) )
-- semantic domain
type T_Expr  = ( Expr )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let _lhsOself :: Expr 
         _anmIself :: ANm 
         -- self rule
         _self =
             Expr_AVar _anmIself
         -- self rule
         _lhsOself =
             _self
         ( _anmIself) =
             anm_ 
     in  ( _lhsOself))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let _lhsOself :: Expr 
         _lExprIself :: Expr 
         _rExprIself :: Expr 
         -- self rule
         _self =
             Expr_App _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIself) =
             lExpr_ 
         ( _rExprIself) =
             rExpr_ 
     in  ( _lhsOself))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_AppTop _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_ChildOrder seqNr_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         _cnstrIself :: ECnstr 
         -- self rule
         _self =
             Expr_Cnstr _exprIself _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
         ( _cnstrIself) =
             cnstr_ 
     in  ( _lhsOself))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_Empty
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Expr _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_Int int_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let _lhsOself :: Expr 
         _lExprIself :: Expr 
         _rExprIself :: Expr 
         -- self rule
         _self =
             Expr_LF _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIself) =
             lExpr_ 
         ( _rExprIself) =
             rExpr_ 
     in  ( _lhsOself))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Named nm_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let _lhsOself :: Expr 
         _nmExprIself :: Expr 
         _lExprIself :: Expr 
         _rExprIself :: Expr 
         -- self rule
         _self =
             Expr_Op nm_ _nmExprIself _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _nmExprIself) =
             nmExpr_ 
         ( _lExprIself) =
             lExpr_ 
         ( _rExprIself) =
             rExpr_ 
     in  ( _lhsOself))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Paren _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Retain _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let _lhsOself :: Expr 
         _lExprIself :: Expr 
         _rExprIself :: Expr 
         -- self rule
         _self =
             Expr_SP _lExprIself _rExprIself
         -- self rule
         _lhsOself =
             _self
         ( _lExprIself) =
             lExpr_ 
         ( _rExprIself) =
             rExpr_ 
     in  ( _lhsOself))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         _selMbExprIself :: MbExpr 
         -- self rule
         _self =
             Expr_Sel _exprIself _selMbExprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
         ( _selMbExprIself) =
             selMbExpr_ 
     in  ( _lhsOself))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_SelTop _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_StrAsIs str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_StrText str_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_Undefined
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_Uniq
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let _lhsOself :: Expr 
         -- self rule
         _self =
             Expr_Var nm_
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let _lhsOself :: Expr 
         _exprIself :: Expr 
         -- self rule
         _self =
             Expr_Wrap wrKind_ _exprIself
         -- self rule
         _lhsOself =
             _self
         ( _exprIself) =
             expr_ 
     in  ( _lhsOself))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let _lhsOself :: Expr 
         _cnstrIself :: ECnstr 
         -- self rule
         _self =
             Expr_WrapCnstr _cnstrIself
         -- self rule
         _lhsOself =
             _self
         ( _cnstrIself) =
             cnstr_ 
     in  ( _lhsOself))
-- Jd ----------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         agStr                : Nm
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         isPre                : Bool
         opts                 : Opts
         pos                  : SPos
         rlNm                 : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
         vwNm                 : Nm
      synthesized attributes:
         coGam                : ChOrdGam
         errL                 : [Err]
         exprEqnL             : [Expr]
         introAD2Gam          : AtDefdGam'
         introADGam           : AtDefdGam
         introECGam           : ECnstrGam
         introFmGam           : FmGam Expr
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Ats:
         child nm             : {Nm}
         child scNm           : {Nm}
         child ats            : JdAts 
         visit 0:
            local sbse        : _
            local sbsn        : _
            local _tup1       : _
            local scInfo      : _
            local vwScInfo    : _
            local isJd        : _
            local nmNd        : _
            local exNdNmS     : _
            local exNdFmGam   : _
            local _tup2       : _
            local exprEqnL    : _
            local introECGam  : _
            local eqnErrs     : _
            local self        : _
      alternative Expr:
         child nm             : {Nm}
         child scNm           : {Nm}
         child expr           : Expr 
         child isSmall        : {Bool}
         visit 0:
            local self        : _
      alternative LTX:
         child nm             : {Nm}
         child scNm           : {Nm}
         child expr           : Expr 
         child isSmall        : {Bool}
         visit 0:
            local self        : _
-}
-- cata
sem_Jd :: Jd  ->
          T_Jd 
sem_Jd (Jd_Ats _nm _scNm _ats )  =
    (sem_Jd_Ats _nm _scNm (sem_JdAts _ats ) )
sem_Jd (Jd_Expr _nm _scNm _expr _isSmall )  =
    (sem_Jd_Expr _nm _scNm (sem_Expr _expr ) _isSmall )
sem_Jd (Jd_LTX _nm _scNm _expr _isSmall )  =
    (sem_Jd_LTX _nm _scNm (sem_Expr _expr ) _isSmall )
-- semantic domain
type T_Jd  = Nm ->
             DtInvGam ->
             FmKind ->
             (FmGam Expr) ->
             Bool ->
             Opts ->
             SPos ->
             Nm ->
             RwExprGam ->
             (ScGam Expr) ->
             Nm ->
             ( ChOrdGam,([Err]),([Expr]),(AtDefdGam'),AtDefdGam,ECnstrGam,(FmGam Expr),PP_Doc,Jd )
sem_Jd_Ats :: Nm ->
              Nm ->
              T_JdAts  ->
              T_Jd 
sem_Jd_Ats nm_ scNm_ ats_  =
    (\ _lhsIagStr
       _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisPre
       _lhsIopts
       _lhsIpos
       _lhsIrlNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _atsOscAtGam :: AtGam
              _lhsOcoGam :: ChOrdGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOintroECGam :: ECnstrGam
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Jd 
              _atsOdtInvGam :: DtInvGam
              _atsOfm :: FmKind
              _atsOfmGam :: (FmGam Expr)
              _atsOisJd :: Bool
              _atsOisPre :: Bool
              _atsOnmNd :: Nm
              _atsOopts :: Opts
              _atsOpos :: SPos
              _atsOrwGam :: RwExprGam
              _atsOscGam :: (ScGam Expr)
              _atsIatFmGam :: (FmGam (Expr,ECnstr))
              _atsIerrL :: ([Err])
              _atsIexprEqnL :: ([Expr])
              _atsIgathExNd :: Expr 
              _atsIgathNmNd :: Nm
              _atsIintroAD2Gam :: (AtDefdGam')
              _atsIintroADGam :: AtDefdGam
              _atsIintroECGam :: ECnstrGam
              _atsIintroFmGam :: (FmGam Expr)
              _atsIppDbg :: PP_Doc
              _atsIself :: JdAts 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 67, column 21)
              _sbse =
                  exprSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 67, column 21)
              _sbsn =
                  nmSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 81, column 33)
              __tup1 =
                  maybe (panic "Jd_Ats: scInfo") id
                  $ scVwGamLookup scNm_ _lhsIvwNm _lhsIscGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 81, column 33)
              (_scInfo,_) =
                  __tup1
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 81, column 33)
              (_,_vwScInfo) =
                  __tup1
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 88, column 21)
              _isJd =
                  scKind _scInfo == ScJudge
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 105, column 21)
              _atsOscAtGam =
                  vwscFullAtGam _vwScInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 138, column 21)
              _nmNd =
                  if _lhsIisPre then exprAsNm $ _sbse $ _atsIgathExNd else nmLhs
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 143, column 21)
              _exNdNmS =
                  exprNmS _atsIgathExNd
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 143, column 21)
              _exNdFmGam =
                  fmGamFromList' _lhsIfm [ (n,mkAFld . _sbsn $ n) | n <- Set.toList _exNdNmS ]
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 145, column 21)
              _lhsOcoGam =
                  let sbse = exprSubst (_lhsIopts {optSubstFullNm=False}) _exNdFmGam
                      cg1 = exprCoGam . sbse $ _atsIgathExNd
                  in  cg1
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 178, column 21)
              _lhsOintroFmGam =
                  case scKind _scInfo of
                    ScJudge | not _lhsIisPre -> _exNdFmGam `fmGamUnion` _atsIintroFmGam
                    _                        -> _atsIintroFmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 194, column 21)
              _lhsOintroECGam =
                  _introECGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 237, column 33)
              __tup2 =
                  case scKind _scInfo of
                    ScJudge
                      -> (_atsIexprEqnL,_atsIintroECGam,[])
                    ScRelation
                      -> (exprLines jd2,introECGam2,concat jd1ErrLL)
                      where jd1      = fkGamLookup exprUnk jdshExpr [_lhsIfm] . vwscJdShpGam $ _vwScInfo
                            (jd1EqlL,jd1ErrLL)
                                     = unzip
                                         [ maybe (Map.empty,[Err_NotAEqnForm _lhsIpos (pp l)]) (\e -> (e,[])) (exprCheckAEqnForm l)
                                         | l <- exprLines jd1
                                         ]
                            jd2      = exprSubst (_lhsIopts {optSubstOnce=True}) (fmGamMap (const fst) _atsIatFmGam) jd1
                            mkG eqm gn
                                     = if null n1 then [] else n1 ++ mkG (foldr (\(e,_) m -> Map.delete e m) eqm n1) n1
                                     where n1 = [ (exprEnsureAppTop e',c) | (e,c) <- gn, e' <- maybeToList (Map.lookup e eqm) ]
                            (introECL,introECGL)
                                     = unzip [ ((e,c),eg) | (_,(e,c)) <- fmGamToList' _lhsIfm _atsIatFmGam, let (e',eg) = exprElimCnstr e ]
                            introECGam2
                                     = (ecGamFromList $ introECL ++ mkG (Map.unions jd1EqlL) introECL)
                                       `gamUnion` gamUnions introECGL
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 237, column 33)
              (_exprEqnL,_,_) =
                  __tup2
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 237, column 33)
              (_,_introECGam,_) =
                  __tup2
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 237, column 33)
              (_,_,_eqnErrs) =
                  __tup2
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 300, column 21)
              _lhsOerrL =
                  _eqnErrs
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  _exprEqnL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  _atsIintroAD2Gam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  _atsIintroADGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _atsIppDbg
              -- self rule
              _self =
                  Jd_Ats nm_ scNm_ _atsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _atsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _atsOfm =
                  _lhsIfm
              -- copy rule (down)
              _atsOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _atsOisJd =
                  _isJd
              -- copy rule (down)
              _atsOisPre =
                  _lhsIisPre
              -- copy rule (from local)
              _atsOnmNd =
                  _nmNd
              -- copy rule (down)
              _atsOopts =
                  _lhsIopts
              -- copy rule (down)
              _atsOpos =
                  _lhsIpos
              -- copy rule (down)
              _atsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _atsOscGam =
                  _lhsIscGam
              ( _atsIatFmGam,_atsIerrL,_atsIexprEqnL,_atsIgathExNd,_atsIgathNmNd,_atsIintroAD2Gam,_atsIintroADGam,_atsIintroECGam,_atsIintroFmGam,_atsIppDbg,_atsIself) =
                  ats_ _atsOdtInvGam _atsOfm _atsOfmGam _atsOisJd _atsOisPre _atsOnmNd _atsOopts _atsOpos _atsOrwGam _atsOscAtGam _atsOscGam 
          in  ( _lhsOcoGam,_lhsOerrL,_lhsOexprEqnL,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
sem_Jd_Expr :: Nm ->
               Nm ->
               T_Expr  ->
               Bool ->
               T_Jd 
sem_Jd_Expr nm_ scNm_ expr_ isSmall_  =
    (\ _lhsIagStr
       _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisPre
       _lhsIopts
       _lhsIpos
       _lhsIrlNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOcoGam :: ChOrdGam
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Jd 
              _exprIself :: Expr 
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 140, column 24)
              _lhsOcoGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  Jd_Expr nm_ scNm_ _exprIself isSmall_
              -- self rule
              _lhsOself =
                  _self
              ( _exprIself) =
                  expr_ 
          in  ( _lhsOcoGam,_lhsOerrL,_lhsOexprEqnL,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
sem_Jd_LTX :: Nm ->
              Nm ->
              T_Expr  ->
              Bool ->
              T_Jd 
sem_Jd_LTX nm_ scNm_ expr_ isSmall_  =
    (\ _lhsIagStr
       _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisPre
       _lhsIopts
       _lhsIpos
       _lhsIrlNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOcoGam :: ChOrdGam
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Jd 
              _exprIself :: Expr 
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 140, column 24)
              _lhsOcoGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  Jd_LTX nm_ scNm_ _exprIself isSmall_
              -- self rule
              _lhsOself =
                  _self
              ( _exprIself) =
                  expr_ 
          in  ( _lhsOcoGam,_lhsOerrL,_lhsOexprEqnL,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
-- JdAt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         isJd                 : Bool
         isPre                : Bool
         nmNd                 : Nm
         opts                 : Opts
         pos                  : SPos
         rwGam                : RwExprGam
         scAtGam              : AtGam
         scGam                : ScGam Expr
      synthesized attributes:
         atFmGam              : FmGam (Expr,ECnstr)
         errL                 : [Err]
         exprEqnL             : [Expr]
         gathExNd             : Expr 
         introAD2Gam          : AtDefdGam'
         introADGam           : AtDefdGam
         introECGam           : ECnstrGam
         introFmGam           : FmGam Expr
         mbNmNd               : Maybe Nm
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative At:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local sbse        : _
            local sbsn        : _
            local atInfo      : _
            local propRetain  : _
            local dir         : _
            local isBi        : _
            local isExtern    : _
            local atTy        : _
            local atCnstr     : _
            local nmAdapt     : _
            local nmSbs       : _
            local defLhs      : _
            local defRhs      : _
            local useLhs      : _
            local useRhs      : _
            local introFmGam  : _
            local _tup3       : _
            local exprEqnL    : _
            local introECGam  : _
            local self        : _
-}
-- cata
sem_JdAt :: JdAt  ->
            T_JdAt 
sem_JdAt (JdAt_At _nm _expr )  =
    (sem_JdAt_At _nm (sem_Expr _expr ) )
-- semantic domain
type T_JdAt  = DtInvGam ->
               FmKind ->
               (FmGam Expr) ->
               Bool ->
               Bool ->
               Nm ->
               Opts ->
               SPos ->
               RwExprGam ->
               AtGam ->
               (ScGam Expr) ->
               ( (FmGam (Expr,ECnstr)),([Err]),([Expr]),Expr ,(AtDefdGam'),AtDefdGam,ECnstrGam,(FmGam Expr),(Maybe Nm),PP_Doc,JdAt )
sem_JdAt_At :: Nm ->
               T_Expr  ->
               T_JdAt 
sem_JdAt_At nm_ expr_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisJd
       _lhsIisPre
       _lhsInmNd
       _lhsIopts
       _lhsIpos
       _lhsIrwGam
       _lhsIscAtGam
       _lhsIscGam ->
         (let _lhsOmbNmNd :: (Maybe Nm)
              _lhsOgathExNd :: Expr 
              _lhsOatFmGam :: (FmGam (Expr,ECnstr))
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: JdAt 
              _exprIself :: Expr 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 63, column 21)
              _sbse =
                  exprSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 63, column 21)
              _sbsn =
                  nmSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 108, column 21)
              _atInfo =
                  maybe emptyAtInfo id $ gamLookup nm_ _lhsIscAtGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 108, column 21)
              _propRetain =
                  [AtRetain] `atFilterProps` _atInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 112, column 21)
              _dir =
                  atDefUse _lhsIisPre _atInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 112, column 21)
              _isBi =
                  Map.elems propsSynInhMp `atHasProps` _atInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 112, column 21)
              _isExtern =
                  AtExtern `atHasProp` _atInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 117, column 21)
              _atTy =
                  atTy _atInfo
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 117, column 21)
              _atCnstr =
                  ECnstr_Ty [_atTy]
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 126, column 21)
              _lhsOmbNmNd =
                  if _dir == ADNode then Just nm_ else Nothing
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 126, column 21)
              _lhsOgathExNd =
                  _exprIself
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 157, column 21)
              _lhsOatFmGam =
                  if False
                  then emptyGam
                  else fmSingleton nm_ _lhsIfm (_exprIself,_atCnstr)
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _nmAdapt =
                  if _isBi then nmInit else id
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _nmSbs =
                  Nm . nmShowAG . _sbsn . _nmAdapt $ nm_
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _defLhs =
                  if _lhsIisPre then mkANd _lhsInmNd _nmSbs else mkALhs' _propRetain _nmSbs
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _defRhs =
                  _exprIself
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _useLhs =
                  _exprIself
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 162, column 21)
              _useRhs =
                  if _lhsIisPre then mkANd _lhsInmNd _nmSbs else mkALhs _nmSbs
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 172, column 21)
              _introFmGam =
                  case _dir of
                    ADUse -> fmGamFromList' _lhsIfm [ (n,mkALoc' n') | (n,n') <- nmRefAscL ]
                          where nmRefAscL = [ (n,Nm . nmShowAG . _sbsn                 $ n) | n <- Set.toList (exprNmS _useLhs), n /= nmWild ]
                    _     -> emptyGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 191, column 21)
              _lhsOintroECGam =
                  _introECGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 204, column 21)
              _lhsOintroADGam =
                  case _dir of
                    ADDef -> gamFromAssocs [(_nmSbs,_isBi)]
                    _     -> emptyGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 214, column 21)
              _lhsOintroAD2Gam =
                  case _dir of
                    ADUse | _lhsIisJd
                      -> gamFromAssocs [(_nmSbs,Set.singleton _lhsInmNd)]
                    _ -> emptyGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 225, column 33)
              __tup3 =
                  let mk l r = ([eqn2],ecGam `gamUnion` ecGamFromList [ (l,_atCnstr), (r,_atCnstr) ])
                             where eqn1         = mkExprEqn l r
                                   (eqn2,ecGam) = exprElimCnstr eqn1
                  in  case _dir of
                        ADUse | not (fmNull _introFmGam)
                            -> mk _useLhs _useRhs
                        ADDef | not _isExtern
                            -> mk _defLhs _defRhs
                        _   -> ([],emptyGam)
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 225, column 33)
              (_exprEqnL,_) =
                  __tup3
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 225, column 33)
              (_,_introECGam) =
                  __tup3
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  _exprEqnL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  _introFmGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  JdAt_At nm_ _exprIself
              -- self rule
              _lhsOself =
                  _self
              ( _exprIself) =
                  expr_ 
          in  ( _lhsOatFmGam,_lhsOerrL,_lhsOexprEqnL,_lhsOgathExNd,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOmbNmNd,_lhsOppDbg,_lhsOself)))
-- JdAts -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         isJd                 : Bool
         isPre                : Bool
         nmNd                 : Nm
         opts                 : Opts
         pos                  : SPos
         rwGam                : RwExprGam
         scAtGam              : AtGam
         scGam                : ScGam Expr
      synthesized attributes:
         atFmGam              : FmGam (Expr,ECnstr)
         errL                 : [Err]
         exprEqnL             : [Expr]
         gathExNd             : Expr 
         gathNmNd             : Nm
         introAD2Gam          : AtDefdGam'
         introADGam           : AtDefdGam
         introECGam           : ECnstrGam
         introFmGam           : FmGam Expr
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : JdAt 
         child tl             : JdAts 
         visit 0:
            local _tup4       : {(Nm,Expr)}
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_JdAts :: JdAts  ->
             T_JdAts 
sem_JdAts list  =
    (Prelude.foldr sem_JdAts_Cons sem_JdAts_Nil (Prelude.map sem_JdAt list) )
-- semantic domain
type T_JdAts  = DtInvGam ->
                FmKind ->
                (FmGam Expr) ->
                Bool ->
                Bool ->
                Nm ->
                Opts ->
                SPos ->
                RwExprGam ->
                AtGam ->
                (ScGam Expr) ->
                ( (FmGam (Expr,ECnstr)),([Err]),([Expr]),Expr ,Nm,(AtDefdGam'),AtDefdGam,ECnstrGam,(FmGam Expr),PP_Doc,JdAts )
sem_JdAts_Cons :: T_JdAt  ->
                  T_JdAts  ->
                  T_JdAts 
sem_JdAts_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisJd
       _lhsIisPre
       _lhsInmNd
       _lhsIopts
       _lhsIpos
       _lhsIrwGam
       _lhsIscAtGam
       _lhsIscGam ->
         (let __tup4 :: ((Nm,Expr))
              _lhsOgathNmNd :: Nm
              _lhsOgathExNd :: Expr 
              _lhsOatFmGam :: (FmGam (Expr,ECnstr))
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: JdAts 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOisJd :: Bool
              _hdOisPre :: Bool
              _hdOnmNd :: Nm
              _hdOopts :: Opts
              _hdOpos :: SPos
              _hdOrwGam :: RwExprGam
              _hdOscAtGam :: AtGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOisJd :: Bool
              _tlOisPre :: Bool
              _tlOnmNd :: Nm
              _tlOopts :: Opts
              _tlOpos :: SPos
              _tlOrwGam :: RwExprGam
              _tlOscAtGam :: AtGam
              _tlOscGam :: (ScGam Expr)
              _hdIatFmGam :: (FmGam (Expr,ECnstr))
              _hdIerrL :: ([Err])
              _hdIexprEqnL :: ([Expr])
              _hdIgathExNd :: Expr 
              _hdIintroAD2Gam :: (AtDefdGam')
              _hdIintroADGam :: AtDefdGam
              _hdIintroECGam :: ECnstrGam
              _hdIintroFmGam :: (FmGam Expr)
              _hdImbNmNd :: (Maybe Nm)
              _hdIppDbg :: PP_Doc
              _hdIself :: JdAt 
              _tlIatFmGam :: (FmGam (Expr,ECnstr))
              _tlIerrL :: ([Err])
              _tlIexprEqnL :: ([Expr])
              _tlIgathExNd :: Expr 
              _tlIgathNmNd :: Nm
              _tlIintroAD2Gam :: (AtDefdGam')
              _tlIintroADGam :: AtDefdGam
              _tlIintroECGam :: ECnstrGam
              _tlIintroFmGam :: (FmGam Expr)
              _tlIppDbg :: PP_Doc
              _tlIself :: JdAts 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 132, column 33)
              __tup4 =
                  maybe (_tlIgathNmNd,_tlIgathExNd) (\n -> (n,_hdIgathExNd)) _hdImbNmNd
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 132, column 33)
              (_lhsOgathNmNd,_) =
                  __tup4
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 132, column 33)
              (_,_lhsOgathExNd) =
                  __tup4
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 154, column 26)
              _lhsOatFmGam =
                  _hdIatFmGam `fmGamUnion` _tlIatFmGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  _hdIexprEqnL ++ _tlIexprEqnL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  _hdIintroAD2Gam `adGamUnion` _tlIintroAD2Gam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  _hdIintroADGam `gamUnion` _tlIintroADGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  _hdIintroECGam `gamUnion` _tlIintroECGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  _hdIintroFmGam `fmGamUnion` _tlIintroFmGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOisJd =
                  _lhsIisJd
              -- copy rule (down)
              _hdOisPre =
                  _lhsIisPre
              -- copy rule (down)
              _hdOnmNd =
                  _lhsInmNd
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOpos =
                  _lhsIpos
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscAtGam =
                  _lhsIscAtGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOisJd =
                  _lhsIisJd
              -- copy rule (down)
              _tlOisPre =
                  _lhsIisPre
              -- copy rule (down)
              _tlOnmNd =
                  _lhsInmNd
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOpos =
                  _lhsIpos
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscAtGam =
                  _lhsIscAtGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIatFmGam,_hdIerrL,_hdIexprEqnL,_hdIgathExNd,_hdIintroAD2Gam,_hdIintroADGam,_hdIintroECGam,_hdIintroFmGam,_hdImbNmNd,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOisJd _hdOisPre _hdOnmNd _hdOopts _hdOpos _hdOrwGam _hdOscAtGam _hdOscGam 
              ( _tlIatFmGam,_tlIerrL,_tlIexprEqnL,_tlIgathExNd,_tlIgathNmNd,_tlIintroAD2Gam,_tlIintroADGam,_tlIintroECGam,_tlIintroFmGam,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOisJd _tlOisPre _tlOnmNd _tlOopts _tlOpos _tlOrwGam _tlOscAtGam _tlOscGam 
          in  ( _lhsOatFmGam,_lhsOerrL,_lhsOexprEqnL,_lhsOgathExNd,_lhsOgathNmNd,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
sem_JdAts_Nil :: T_JdAts 
sem_JdAts_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisJd
       _lhsIisPre
       _lhsInmNd
       _lhsIopts
       _lhsIpos
       _lhsIrwGam
       _lhsIscAtGam
       _lhsIscGam ->
         (let _lhsOgathNmNd :: Nm
              _lhsOgathExNd :: Expr 
              _lhsOatFmGam :: (FmGam (Expr,ECnstr))
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: JdAts 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 130, column 21)
              _lhsOgathNmNd =
                  nmUnk
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 130, column 21)
              _lhsOgathExNd =
                  exprUnk
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 154, column 26)
              _lhsOatFmGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOatFmGam,_lhsOerrL,_lhsOexprEqnL,_lhsOgathExNd,_lhsOgathNmNd,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
-- Jds ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         agStr                : Nm
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         isPre                : Bool
         opts                 : Opts
         pos                  : SPos
         rlNm                 : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
         vwNm                 : Nm
      synthesized attributes:
         coGam                : ChOrdGam
         errL                 : [Err]
         exprEqnL             : [Expr]
         introAD2Gam          : AtDefdGam'
         introADGam           : AtDefdGam
         introECGam           : ECnstrGam
         introFmGam           : FmGam Expr
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : Jd 
         child tl             : Jds 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_Jds :: Jds  ->
           T_Jds 
sem_Jds list  =
    (Prelude.foldr sem_Jds_Cons sem_Jds_Nil (Prelude.map sem_Jd list) )
-- semantic domain
type T_Jds  = Nm ->
              DtInvGam ->
              FmKind ->
              (FmGam Expr) ->
              Bool ->
              Opts ->
              SPos ->
              Nm ->
              RwExprGam ->
              (ScGam Expr) ->
              Nm ->
              ( ChOrdGam,([Err]),([Expr]),(AtDefdGam'),AtDefdGam,ECnstrGam,(FmGam Expr),PP_Doc,Jds )
sem_Jds_Cons :: T_Jd  ->
                T_Jds  ->
                T_Jds 
sem_Jds_Cons hd_ tl_  =
    (\ _lhsIagStr
       _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisPre
       _lhsIopts
       _lhsIpos
       _lhsIrlNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOcoGam :: ChOrdGam
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Jds 
              _hdOagStr :: Nm
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOisPre :: Bool
              _hdOopts :: Opts
              _hdOpos :: SPos
              _hdOrlNm :: Nm
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _hdOvwNm :: Nm
              _tlOagStr :: Nm
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOisPre :: Bool
              _tlOopts :: Opts
              _tlOpos :: SPos
              _tlOrlNm :: Nm
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _tlOvwNm :: Nm
              _hdIcoGam :: ChOrdGam
              _hdIerrL :: ([Err])
              _hdIexprEqnL :: ([Expr])
              _hdIintroAD2Gam :: (AtDefdGam')
              _hdIintroADGam :: AtDefdGam
              _hdIintroECGam :: ECnstrGam
              _hdIintroFmGam :: (FmGam Expr)
              _hdIppDbg :: PP_Doc
              _hdIself :: Jd 
              _tlIcoGam :: ChOrdGam
              _tlIerrL :: ([Err])
              _tlIexprEqnL :: ([Expr])
              _tlIintroAD2Gam :: (AtDefdGam')
              _tlIintroADGam :: AtDefdGam
              _tlIintroECGam :: ECnstrGam
              _tlIintroFmGam :: (FmGam Expr)
              _tlIppDbg :: PP_Doc
              _tlIself :: Jds 
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 140, column 24)
              _lhsOcoGam =
                  _hdIcoGam `gamUnion` _tlIcoGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  _hdIexprEqnL ++ _tlIexprEqnL
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  _hdIintroAD2Gam `adGamUnion` _tlIintroAD2Gam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  _hdIintroADGam `gamUnion` _tlIintroADGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  _hdIintroECGam `gamUnion` _tlIintroECGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  _hdIintroFmGam `fmGamUnion` _tlIintroFmGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOagStr =
                  _lhsIagStr
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOisPre =
                  _lhsIisPre
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOpos =
                  _lhsIpos
              -- copy rule (down)
              _hdOrlNm =
                  _lhsIrlNm
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _hdOvwNm =
                  _lhsIvwNm
              -- copy rule (down)
              _tlOagStr =
                  _lhsIagStr
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOisPre =
                  _lhsIisPre
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOpos =
                  _lhsIpos
              -- copy rule (down)
              _tlOrlNm =
                  _lhsIrlNm
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOvwNm =
                  _lhsIvwNm
              ( _hdIcoGam,_hdIerrL,_hdIexprEqnL,_hdIintroAD2Gam,_hdIintroADGam,_hdIintroECGam,_hdIintroFmGam,_hdIppDbg,_hdIself) =
                  hd_ _hdOagStr _hdOdtInvGam _hdOfm _hdOfmGam _hdOisPre _hdOopts _hdOpos _hdOrlNm _hdOrwGam _hdOscGam _hdOvwNm 
              ( _tlIcoGam,_tlIerrL,_tlIexprEqnL,_tlIintroAD2Gam,_tlIintroADGam,_tlIintroECGam,_tlIintroFmGam,_tlIppDbg,_tlIself) =
                  tl_ _tlOagStr _tlOdtInvGam _tlOfm _tlOfmGam _tlOisPre _tlOopts _tlOpos _tlOrlNm _tlOrwGam _tlOscGam _tlOvwNm 
          in  ( _lhsOcoGam,_lhsOerrL,_lhsOexprEqnL,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
sem_Jds_Nil :: T_Jds 
sem_Jds_Nil  =
    (\ _lhsIagStr
       _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIisPre
       _lhsIopts
       _lhsIpos
       _lhsIrlNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOcoGam :: ChOrdGam
              _lhsOerrL :: ([Err])
              _lhsOexprEqnL :: ([Expr])
              _lhsOintroAD2Gam :: (AtDefdGam')
              _lhsOintroADGam :: AtDefdGam
              _lhsOintroECGam :: ECnstrGam
              _lhsOintroFmGam :: (FmGam Expr)
              _lhsOppDbg :: PP_Doc
              _lhsOself :: Jds 
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 140, column 24)
              _lhsOcoGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 222, column 33)
              _lhsOexprEqnL =
                  []
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 211, column 36)
              _lhsOintroAD2Gam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 201, column 35)
              _lhsOintroADGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 188, column 35)
              _lhsOintroECGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/GenARule.ag"(line 169, column 35)
              _lhsOintroFmGam =
                  emptyGam
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOcoGam,_lhsOerrL,_lhsOexprEqnL,_lhsOintroAD2Gam,_lhsOintroADGam,_lhsOintroECGam,_lhsOintroFmGam,_lhsOppDbg,_lhsOself)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         self                 : SELF 
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local self        : _
      alternative Nothing:
         visit 0:
            local self        : _
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = ( MbExpr )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (let _lhsOself :: MbExpr 
         _justIself :: Expr 
         -- self rule
         _self =
             Just _justIself
         -- self rule
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let _lhsOself :: MbExpr 
         -- self rule
         _self =
             Nothing
         -- self rule
         _lhsOself =
             _self
     in  ( _lhsOself))
-- RlDecl ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rsScNm               : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
         vwNm                 : Nm
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative AG:
         child nm             : {Nm}
         child pos            : {SPos}
         child arule          : ARule 
         visit 0:
            local agStr       : _
            local self        : _
      alternative Chunk:
         child nm             : {Nm}
         child rl             : RlDecl 
         visit 0:
            local agStr       : _
            local self        : _
      alternative LTX:
         child nm             : {Nm}
         child rlNm           : {Nm}
         child vwNm           : {Nm}
         child pos            : {SPos}
         child preJds         : Jds 
         child postJds        : Jds 
         visit 0:
            local pos         : _
            local agStr       : _
            local rlNm        : _
            local self        : _
      alternative LTXAlias:
         child fullAliasNm    : {Nm}
         child fullNm         : {Nm}
         visit 0:
            local agStr       : _
            local self        : _
      alternative Rl:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child pos            : {SPos}
         child agStr          : {Nm}
         child preJds         : Jds 
         child postJds        : Jds 
         visit 0:
            local pos         : _
            local sbse        : _
            local sbsn        : _
            local rsScInfo    : _
            local _tup5       : _
            local daInvInfo   : _
            local agStr       : _
            local coNmL       : _
            local introFmGam  : _
            local fullFmGam   : _
            local introECGam  : _
            local adGam       : _
            local ad2Gam      : _
            local _tup6       : _
            local selfNonOptim1 : _
            local selfNonOptim2 : _
            local rlNm        : _
            local self        : _
-}
-- cata
sem_RlDecl :: RlDecl  ->
              T_RlDecl 
sem_RlDecl (RlDecl_AG _nm _pos _arule )  =
    (sem_RlDecl_AG _nm _pos (sem_ARule _arule ) )
sem_RlDecl (RlDecl_Chunk _nm _rl )  =
    (sem_RlDecl_Chunk _nm (sem_RlDecl _rl ) )
sem_RlDecl (RlDecl_LTX _nm _rlNm _vwNm _pos _preJds _postJds )  =
    (sem_RlDecl_LTX _nm _rlNm _vwNm _pos (sem_Jds _preJds ) (sem_Jds _postJds ) )
sem_RlDecl (RlDecl_LTXAlias _fullAliasNm _fullNm )  =
    (sem_RlDecl_LTXAlias _fullAliasNm _fullNm )
sem_RlDecl (RlDecl_Rl _nm _fullNm _pos _agStr _preJds _postJds )  =
    (sem_RlDecl_Rl _nm _fullNm _pos _agStr (sem_Jds _preJds ) (sem_Jds _postJds ) )
-- semantic domain
type T_RlDecl  = DtInvGam ->
                 FmKind ->
                 (FmGam Expr) ->
                 Opts ->
                 Nm ->
                 RwExprGam ->
                 (ScGam Expr) ->
                 Nm ->
                 ( ([Err]),PP_Doc,RlDecl )
sem_RlDecl_AG :: Nm ->
                 SPos ->
                 T_ARule  ->
                 T_RlDecl 
sem_RlDecl_AG nm_ pos_ arule_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecl 
              _aruleIself :: ARule 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 100, column 21)
              _agStr =
                  nmNone
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  RlDecl_AG nm_ pos_ _aruleIself
              -- self rule
              _lhsOself =
                  _self
              ( _aruleIself) =
                  arule_ 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RlDecl_Chunk :: Nm ->
                    T_RlDecl  ->
                    T_RlDecl 
sem_RlDecl_Chunk nm_ rl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecl 
              _rlOdtInvGam :: DtInvGam
              _rlOfm :: FmKind
              _rlOfmGam :: (FmGam Expr)
              _rlOopts :: Opts
              _rlOrsScNm :: Nm
              _rlOrwGam :: RwExprGam
              _rlOscGam :: (ScGam Expr)
              _rlOvwNm :: Nm
              _rlIerrL :: ([Err])
              _rlIppDbg :: PP_Doc
              _rlIself :: RlDecl 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 100, column 21)
              _agStr =
                  nmNone
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _rlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _rlIppDbg
              -- self rule
              _self =
                  RlDecl_Chunk nm_ _rlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _rlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _rlOfm =
                  _lhsIfm
              -- copy rule (down)
              _rlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _rlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rlOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _rlOvwNm =
                  _lhsIvwNm
              ( _rlIerrL,_rlIppDbg,_rlIself) =
                  rl_ _rlOdtInvGam _rlOfm _rlOfmGam _rlOopts _rlOrsScNm _rlOrwGam _rlOscGam _rlOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RlDecl_LTX :: Nm ->
                  Nm ->
                  Nm ->
                  SPos ->
                  T_Jds  ->
                  T_Jds  ->
                  T_RlDecl 
sem_RlDecl_LTX nm_ rlNm_ vwNm_ pos_ preJds_ postJds_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _preJdsOisPre :: Bool
              _postJdsOisPre :: Bool
              _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecl 
              _preJdsOagStr :: Nm
              _preJdsOdtInvGam :: DtInvGam
              _preJdsOfm :: FmKind
              _preJdsOfmGam :: (FmGam Expr)
              _preJdsOopts :: Opts
              _preJdsOpos :: SPos
              _preJdsOrlNm :: Nm
              _preJdsOrwGam :: RwExprGam
              _preJdsOscGam :: (ScGam Expr)
              _preJdsOvwNm :: Nm
              _postJdsOagStr :: Nm
              _postJdsOdtInvGam :: DtInvGam
              _postJdsOfm :: FmKind
              _postJdsOfmGam :: (FmGam Expr)
              _postJdsOopts :: Opts
              _postJdsOpos :: SPos
              _postJdsOrlNm :: Nm
              _postJdsOrwGam :: RwExprGam
              _postJdsOscGam :: (ScGam Expr)
              _postJdsOvwNm :: Nm
              _preJdsIcoGam :: ChOrdGam
              _preJdsIerrL :: ([Err])
              _preJdsIexprEqnL :: ([Expr])
              _preJdsIintroAD2Gam :: (AtDefdGam')
              _preJdsIintroADGam :: AtDefdGam
              _preJdsIintroECGam :: ECnstrGam
              _preJdsIintroFmGam :: (FmGam Expr)
              _preJdsIppDbg :: PP_Doc
              _preJdsIself :: Jds 
              _postJdsIcoGam :: ChOrdGam
              _postJdsIerrL :: ([Err])
              _postJdsIexprEqnL :: ([Expr])
              _postJdsIintroAD2Gam :: (AtDefdGam')
              _postJdsIintroADGam :: AtDefdGam
              _postJdsIintroECGam :: ECnstrGam
              _postJdsIintroFmGam :: (FmGam Expr)
              _postJdsIppDbg :: PP_Doc
              _postJdsIself :: Jds 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 60, column 21)
              _pos =
                  pos_
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 93, column 21)
              _preJdsOisPre =
                  True
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 94, column 21)
              _postJdsOisPre =
                  False
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 100, column 21)
              _agStr =
                  nmNone
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 47, column 21)
              _rlNm =
                  rlNm_
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _preJdsIerrL ++ _postJdsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _preJdsIppDbg >-< _postJdsIppDbg
              -- self rule
              _self =
                  RlDecl_LTX nm_ _rlNm vwNm_ _pos _preJdsIself _postJdsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (from local)
              _preJdsOagStr =
                  _agStr
              -- copy rule (down)
              _preJdsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _preJdsOfm =
                  _lhsIfm
              -- copy rule (down)
              _preJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _preJdsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _preJdsOpos =
                  _pos
              -- copy rule (from local)
              _preJdsOrlNm =
                  _rlNm
              -- copy rule (down)
              _preJdsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _preJdsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _preJdsOvwNm =
                  _lhsIvwNm
              -- copy rule (from local)
              _postJdsOagStr =
                  _agStr
              -- copy rule (down)
              _postJdsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _postJdsOfm =
                  _lhsIfm
              -- copy rule (down)
              _postJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _postJdsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _postJdsOpos =
                  _pos
              -- copy rule (from local)
              _postJdsOrlNm =
                  _rlNm
              -- copy rule (down)
              _postJdsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _postJdsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _postJdsOvwNm =
                  _lhsIvwNm
              ( _preJdsIcoGam,_preJdsIerrL,_preJdsIexprEqnL,_preJdsIintroAD2Gam,_preJdsIintroADGam,_preJdsIintroECGam,_preJdsIintroFmGam,_preJdsIppDbg,_preJdsIself) =
                  preJds_ _preJdsOagStr _preJdsOdtInvGam _preJdsOfm _preJdsOfmGam _preJdsOisPre _preJdsOopts _preJdsOpos _preJdsOrlNm _preJdsOrwGam _preJdsOscGam _preJdsOvwNm 
              ( _postJdsIcoGam,_postJdsIerrL,_postJdsIexprEqnL,_postJdsIintroAD2Gam,_postJdsIintroADGam,_postJdsIintroECGam,_postJdsIintroFmGam,_postJdsIppDbg,_postJdsIself) =
                  postJds_ _postJdsOagStr _postJdsOdtInvGam _postJdsOfm _postJdsOfmGam _postJdsOisPre _postJdsOopts _postJdsOpos _postJdsOrlNm _postJdsOrwGam _postJdsOscGam _postJdsOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RlDecl_LTXAlias :: Nm ->
                       Nm ->
                       T_RlDecl 
sem_RlDecl_LTXAlias fullAliasNm_ fullNm_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecl 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 100, column 21)
              _agStr =
                  nmNone
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  RlDecl_LTXAlias fullAliasNm_ fullNm_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RlDecl_Rl :: Nm ->
                 Nm ->
                 SPos ->
                 Nm ->
                 T_Jds  ->
                 T_Jds  ->
                 T_RlDecl 
sem_RlDecl_Rl nm_ fullNm_ pos_ agStr_ preJds_ postJds_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _preJdsOisPre :: Bool
              _postJdsOisPre :: Bool
              _lhsOself :: RlDecl 
              _lhsOppDbg :: PP_Doc
              _lhsOerrL :: ([Err])
              _preJdsOagStr :: Nm
              _preJdsOdtInvGam :: DtInvGam
              _preJdsOfm :: FmKind
              _preJdsOfmGam :: (FmGam Expr)
              _preJdsOopts :: Opts
              _preJdsOpos :: SPos
              _preJdsOrlNm :: Nm
              _preJdsOrwGam :: RwExprGam
              _preJdsOscGam :: (ScGam Expr)
              _preJdsOvwNm :: Nm
              _postJdsOagStr :: Nm
              _postJdsOdtInvGam :: DtInvGam
              _postJdsOfm :: FmKind
              _postJdsOfmGam :: (FmGam Expr)
              _postJdsOopts :: Opts
              _postJdsOpos :: SPos
              _postJdsOrlNm :: Nm
              _postJdsOrwGam :: RwExprGam
              _postJdsOscGam :: (ScGam Expr)
              _postJdsOvwNm :: Nm
              _preJdsIcoGam :: ChOrdGam
              _preJdsIerrL :: ([Err])
              _preJdsIexprEqnL :: ([Expr])
              _preJdsIintroAD2Gam :: (AtDefdGam')
              _preJdsIintroADGam :: AtDefdGam
              _preJdsIintroECGam :: ECnstrGam
              _preJdsIintroFmGam :: (FmGam Expr)
              _preJdsIppDbg :: PP_Doc
              _preJdsIself :: Jds 
              _postJdsIcoGam :: ChOrdGam
              _postJdsIerrL :: ([Err])
              _postJdsIexprEqnL :: ([Expr])
              _postJdsIintroAD2Gam :: (AtDefdGam')
              _postJdsIintroADGam :: AtDefdGam
              _postJdsIintroECGam :: ECnstrGam
              _postJdsIintroFmGam :: (FmGam Expr)
              _postJdsIppDbg :: PP_Doc
              _postJdsIself :: Jds 
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 60, column 21)
              _pos =
                  pos_
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 71, column 21)
              _sbse =
                  exprSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 71, column 21)
              _sbsn =
                  nmSubst (_lhsIopts {optSubstFullNm=False}) _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 75, column 21)
              _rsScInfo =
                  maybe (panic "RlDecl_Rl: rsScInfo") id $ gamLookup _lhsIrsScNm _lhsIscGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 76, column 33)
              __tup5 =
                  maybe (undefined,undefined,emptyDtAltInvInfo) id
                  $ dtVwRlInvGamLookup _lhsIrsScNm _lhsIvwNm _rlNm _lhsIdtInvGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 76, column 33)
              (_,_,_daInvInfo) =
                  __tup5
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 93, column 21)
              _preJdsOisPre =
                  True
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 94, column 21)
              _postJdsOisPre =
                  False
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 99, column 21)
              _agStr =
                  agStr_
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 150, column 21)
              _coNmL =
                  let cg1 = _postJdsIcoGam
                      cg2 = gamMapKeys _sbsn $ daiChOrdGam _daInvInfo
                  in  coGamNmL (if gamIsEmpty cg1 then cg2 else cg1)
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 183, column 21)
              _introFmGam =
                  _preJdsIintroFmGam `fmGamUnion` _postJdsIintroFmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 186, column 21)
              _fullFmGam =
                  _introFmGam `fmGamUnion` _lhsIfmGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 197, column 21)
              _introECGam =
                  gamMapKeys
                      (exprSubst (_lhsIopts {optSubstFullNm=False}) _fullFmGam)
                      (_preJdsIintroECGam `gamUnion` _postJdsIintroECGam)
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 209, column 21)
              _adGam =
                  _preJdsIintroADGam `gamUnion` _postJdsIintroADGam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 220, column 21)
              _ad2Gam =
                  _preJdsIintroAD2Gam `adGamUnion` _postJdsIintroAD2Gam
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 260, column 21)
              __tup6 =
                  let (eqnLL,eqnGamL)
                        = unzip
                            [ (es,g)
                            | e <- _preJdsIexprEqnL ++ _postJdsIexprEqnL
                            , let (_,es,g) = exprRewrite' (_lhsIopts {optSubstFullNm=False}) _fullFmGam _lhsIrwGam _introECGam e
                            ]
                      aRule1
                        = ARule_Rule
                            [sc2DATA _rsScInfo _lhsIdtInvGam]
                            agStr_
                            ["rule " ++ show _rlNm,"view " ++ show _lhsIvwNm]
                            (concat eqnLL)
                      aRule2
                        = arlUniq _lhsIfmGam _coNmL
                          $ arlSubst (fmGamUnions eqnGamL)
                          $ aRule1
                      aRule3
                        = arlElimAlphaRename
                          $ aRule2
                      aRule4
                        = arlElimWild
                          $ (if optAGCopyElim _lhsIopts then arlElimCopyRule _coNmL _adGam _ad2Gam else id)
                          $ aRule3
                  in  (RlDecl_AG nm_ pos_ aRule4,aRule2,aRule3)
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 260, column 21)
              (_lhsOself,_,_) =
                  __tup6
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 260, column 21)
              (_,_selfNonOptim1,_) =
                  __tup6
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 260, column 21)
              (_,_,_selfNonOptim2) =
                  __tup6
              -- "build/ruler2/TrfAS2/GenARule.ag"(line 287, column 21)
              _lhsOppDbg =
                  "-- debug info"
                  >-< nm_ >#< ppBracketsCommas [pp pos_,pp agStr_]
                  >-< "introFmGam" >#< ppGam _introFmGam
                  >-< "introECGam" >#< ppECGam _introECGam
                  >-< "lhs.rwGam" >#< ppGam _lhsIrwGam
                  >-< "adGam" >#< ppGam _adGam
                  >-< "ad2Gam" >#< ppADGam _ad2Gam
                  >-< "lhs.opts" >#< text (show _lhsIopts)
                  >-< "coNmL" >#< ppListSep "[" "]" "," _coNmL
                  >-< "non optim1" >#< pp _selfNonOptim1
                  >-< "non optim2" >#< pp _selfNonOptim2
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 46, column 21)
              _rlNm =
                  nm_
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _preJdsIerrL ++ _postJdsIerrL
              -- self rule
              _self =
                  RlDecl_Rl nm_ fullNm_ _pos _agStr _preJdsIself _postJdsIself
              -- copy rule (from local)
              _preJdsOagStr =
                  _agStr
              -- copy rule (down)
              _preJdsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _preJdsOfm =
                  _lhsIfm
              -- copy rule (down)
              _preJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _preJdsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _preJdsOpos =
                  _pos
              -- copy rule (from local)
              _preJdsOrlNm =
                  _rlNm
              -- copy rule (down)
              _preJdsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _preJdsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _preJdsOvwNm =
                  _lhsIvwNm
              -- copy rule (from local)
              _postJdsOagStr =
                  _agStr
              -- copy rule (down)
              _postJdsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _postJdsOfm =
                  _lhsIfm
              -- copy rule (down)
              _postJdsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _postJdsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _postJdsOpos =
                  _pos
              -- copy rule (from local)
              _postJdsOrlNm =
                  _rlNm
              -- copy rule (down)
              _postJdsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _postJdsOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _postJdsOvwNm =
                  _lhsIvwNm
              ( _preJdsIcoGam,_preJdsIerrL,_preJdsIexprEqnL,_preJdsIintroAD2Gam,_preJdsIintroADGam,_preJdsIintroECGam,_preJdsIintroFmGam,_preJdsIppDbg,_preJdsIself) =
                  preJds_ _preJdsOagStr _preJdsOdtInvGam _preJdsOfm _preJdsOfmGam _preJdsOisPre _preJdsOopts _preJdsOpos _preJdsOrlNm _preJdsOrwGam _preJdsOscGam _preJdsOvwNm 
              ( _postJdsIcoGam,_postJdsIerrL,_postJdsIexprEqnL,_postJdsIintroAD2Gam,_postJdsIintroADGam,_postJdsIintroECGam,_postJdsIintroFmGam,_postJdsIppDbg,_postJdsIself) =
                  postJds_ _postJdsOagStr _postJdsOdtInvGam _postJdsOfm _postJdsOfmGam _postJdsOisPre _postJdsOopts _postJdsOpos _postJdsOrlNm _postJdsOrwGam _postJdsOscGam _postJdsOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- RlDecls -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rsScNm               : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
         vwNm                 : Nm
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : RlDecl 
         child tl             : RlDecls 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_RlDecls :: RlDecls  ->
               T_RlDecls 
sem_RlDecls list  =
    (Prelude.foldr sem_RlDecls_Cons sem_RlDecls_Nil (Prelude.map sem_RlDecl list) )
-- semantic domain
type T_RlDecls  = DtInvGam ->
                  FmKind ->
                  (FmGam Expr) ->
                  Opts ->
                  Nm ->
                  RwExprGam ->
                  (ScGam Expr) ->
                  Nm ->
                  ( ([Err]),PP_Doc,RlDecls )
sem_RlDecls_Cons :: T_RlDecl  ->
                    T_RlDecls  ->
                    T_RlDecls 
sem_RlDecls_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecls 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrsScNm :: Nm
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _hdOvwNm :: Nm
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrsScNm :: Nm
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _tlOvwNm :: Nm
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: RlDecl 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: RlDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _hdOvwNm =
                  _lhsIvwNm
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOvwNm =
                  _lhsIvwNm
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrsScNm _hdOrwGam _hdOscGam _hdOvwNm 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrsScNm _tlOrwGam _tlOscGam _tlOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RlDecls_Nil :: T_RlDecls 
sem_RlDecls_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam
       _lhsIvwNm ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RlDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- RsVwDecl ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Rs:
         child nm             : {Nm}
         child scNm           : {Nm}
         child descr          : {String}
         child vwDecls        : VwDecls 
         visit 0:
            local rsScNm      : _
            local self        : _
-}
-- cata
sem_RsVwDecl :: RsVwDecl  ->
                T_RsVwDecl 
sem_RsVwDecl (RsVwDecl_Rs _nm _scNm _descr _vwDecls )  =
    (sem_RsVwDecl_Rs _nm _scNm _descr (sem_VwDecls _vwDecls ) )
-- semantic domain
type T_RsVwDecl  = DtInvGam ->
                   FmKind ->
                   (FmGam Expr) ->
                   Opts ->
                   RwExprGam ->
                   (ScGam Expr) ->
                   ( ([Err]),PP_Doc,RsVwDecl )
sem_RsVwDecl_Rs :: Nm ->
                   Nm ->
                   String ->
                   T_VwDecls  ->
                   T_RsVwDecl 
sem_RsVwDecl_Rs nm_ scNm_ descr_ vwDecls_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RsVwDecl 
              _vwDeclsOdtInvGam :: DtInvGam
              _vwDeclsOfm :: FmKind
              _vwDeclsOfmGam :: (FmGam Expr)
              _vwDeclsOopts :: Opts
              _vwDeclsOrsScNm :: Nm
              _vwDeclsOrwGam :: RwExprGam
              _vwDeclsOscGam :: (ScGam Expr)
              _vwDeclsIerrL :: ([Err])
              _vwDeclsIppDbg :: PP_Doc
              _vwDeclsIself :: VwDecls 
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 36, column 21)
              _rsScNm =
                  scNm_
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _vwDeclsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _vwDeclsIppDbg
              -- self rule
              _self =
                  RsVwDecl_Rs nm_ scNm_ descr_ _vwDeclsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _vwDeclsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _vwDeclsOfm =
                  _lhsIfm
              -- copy rule (down)
              _vwDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _vwDeclsOopts =
                  _lhsIopts
              -- copy rule (from local)
              _vwDeclsOrsScNm =
                  _rsScNm
              -- copy rule (down)
              _vwDeclsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _vwDeclsOscGam =
                  _lhsIscGam
              ( _vwDeclsIerrL,_vwDeclsIppDbg,_vwDeclsIself) =
                  vwDecls_ _vwDeclsOdtInvGam _vwDeclsOfm _vwDeclsOfmGam _vwDeclsOopts _vwDeclsOrsScNm _vwDeclsOrwGam _vwDeclsOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- RsVwDecls ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : RsVwDecl 
         child tl             : RsVwDecls 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_RsVwDecls :: RsVwDecls  ->
                 T_RsVwDecls 
sem_RsVwDecls list  =
    (Prelude.foldr sem_RsVwDecls_Cons sem_RsVwDecls_Nil (Prelude.map sem_RsVwDecl list) )
-- semantic domain
type T_RsVwDecls  = DtInvGam ->
                    FmKind ->
                    (FmGam Expr) ->
                    Opts ->
                    RwExprGam ->
                    (ScGam Expr) ->
                    ( ([Err]),PP_Doc,RsVwDecls )
sem_RsVwDecls_Cons :: T_RsVwDecl  ->
                      T_RsVwDecls  ->
                      T_RsVwDecls 
sem_RsVwDecls_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RsVwDecls 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: RsVwDecl 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: RsVwDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrwGam _hdOscGam 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrwGam _tlOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_RsVwDecls_Nil :: T_RsVwDecls 
sem_RsVwDecls_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: RsVwDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- VwDecl ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rsScNm               : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Grp:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child rlFullNmL      : {[(Nm,Nm)]}
         visit 0:
            local self        : _
      alternative LTX:
         child nm             : {Nm}
         child scMetaNm       : {Nm}
         child scmExpr        : Expr 
         child rlDecls        : RlDecls 
         visit 0:
            local vwNm        : _
            local self        : _
      alternative LTXFig:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child scMetaNm       : {Nm}
         child descr          : {String}
         child rlFullNmL      : {[Nm]}
         visit 0:
            local self        : _
      alternative Vw:
         child nm             : {Nm}
         child fullNm         : {Nm}
         child rlDecls        : RlDecls 
         visit 0:
            local vwNm        : _
            local self        : _
-}
-- cata
sem_VwDecl :: VwDecl  ->
              T_VwDecl 
sem_VwDecl (VwDecl_Grp _nm _fullNm _rlFullNmL )  =
    (sem_VwDecl_Grp _nm _fullNm _rlFullNmL )
sem_VwDecl (VwDecl_LTX _nm _scMetaNm _scmExpr _rlDecls )  =
    (sem_VwDecl_LTX _nm _scMetaNm (sem_Expr _scmExpr ) (sem_RlDecls _rlDecls ) )
sem_VwDecl (VwDecl_LTXFig _nm _fullNm _scMetaNm _descr _rlFullNmL )  =
    (sem_VwDecl_LTXFig _nm _fullNm _scMetaNm _descr _rlFullNmL )
sem_VwDecl (VwDecl_Vw _nm _fullNm _rlDecls )  =
    (sem_VwDecl_Vw _nm _fullNm (sem_RlDecls _rlDecls ) )
-- semantic domain
type T_VwDecl  = DtInvGam ->
                 FmKind ->
                 (FmGam Expr) ->
                 Opts ->
                 Nm ->
                 RwExprGam ->
                 (ScGam Expr) ->
                 ( ([Err]),PP_Doc,VwDecl )
sem_VwDecl_Grp :: Nm ->
                  Nm ->
                  ([(Nm,Nm)]) ->
                  T_VwDecl 
sem_VwDecl_Grp nm_ fullNm_ rlFullNmL_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  VwDecl_Grp nm_ fullNm_ rlFullNmL_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_VwDecl_LTX :: Nm ->
                  Nm ->
                  T_Expr  ->
                  T_RlDecls  ->
                  T_VwDecl 
sem_VwDecl_LTX nm_ scMetaNm_ scmExpr_ rlDecls_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecl 
              _rlDeclsOdtInvGam :: DtInvGam
              _rlDeclsOfm :: FmKind
              _rlDeclsOfmGam :: (FmGam Expr)
              _rlDeclsOopts :: Opts
              _rlDeclsOrsScNm :: Nm
              _rlDeclsOrwGam :: RwExprGam
              _rlDeclsOscGam :: (ScGam Expr)
              _rlDeclsOvwNm :: Nm
              _scmExprIself :: Expr 
              _rlDeclsIerrL :: ([Err])
              _rlDeclsIppDbg :: PP_Doc
              _rlDeclsIself :: RlDecls 
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 41, column 21)
              _vwNm =
                  nm_
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _rlDeclsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _rlDeclsIppDbg
              -- self rule
              _self =
                  VwDecl_LTX nm_ scMetaNm_ _scmExprIself _rlDeclsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _rlDeclsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _rlDeclsOfm =
                  _lhsIfm
              -- copy rule (down)
              _rlDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlDeclsOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlDeclsOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _rlDeclsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rlDeclsOscGam =
                  _lhsIscGam
              -- copy rule (from local)
              _rlDeclsOvwNm =
                  _vwNm
              ( _scmExprIself) =
                  scmExpr_ 
              ( _rlDeclsIerrL,_rlDeclsIppDbg,_rlDeclsIself) =
                  rlDecls_ _rlDeclsOdtInvGam _rlDeclsOfm _rlDeclsOfmGam _rlDeclsOopts _rlDeclsOrsScNm _rlDeclsOrwGam _rlDeclsOscGam _rlDeclsOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_VwDecl_LTXFig :: Nm ->
                     Nm ->
                     Nm ->
                     String ->
                     ([Nm]) ->
                     T_VwDecl 
sem_VwDecl_LTXFig nm_ fullNm_ scMetaNm_ descr_ rlFullNmL_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecl 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  VwDecl_LTXFig nm_ fullNm_ scMetaNm_ descr_ rlFullNmL_
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_VwDecl_Vw :: Nm ->
                 Nm ->
                 T_RlDecls  ->
                 T_VwDecl 
sem_VwDecl_Vw nm_ fullNm_ rlDecls_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecl 
              _rlDeclsOdtInvGam :: DtInvGam
              _rlDeclsOfm :: FmKind
              _rlDeclsOfmGam :: (FmGam Expr)
              _rlDeclsOopts :: Opts
              _rlDeclsOrsScNm :: Nm
              _rlDeclsOrwGam :: RwExprGam
              _rlDeclsOscGam :: (ScGam Expr)
              _rlDeclsOvwNm :: Nm
              _rlDeclsIerrL :: ([Err])
              _rlDeclsIppDbg :: PP_Doc
              _rlDeclsIself :: RlDecls 
              -- "build/ruler2/TrfAS2/CommonAG.ag"(line 41, column 21)
              _vwNm =
                  nm_
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _rlDeclsIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _rlDeclsIppDbg
              -- self rule
              _self =
                  VwDecl_Vw nm_ fullNm_ _rlDeclsIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _rlDeclsOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _rlDeclsOfm =
                  _lhsIfm
              -- copy rule (down)
              _rlDeclsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rlDeclsOopts =
                  _lhsIopts
              -- copy rule (down)
              _rlDeclsOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _rlDeclsOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rlDeclsOscGam =
                  _lhsIscGam
              -- copy rule (from local)
              _rlDeclsOvwNm =
                  _vwNm
              ( _rlDeclsIerrL,_rlDeclsIppDbg,_rlDeclsIself) =
                  rlDecls_ _rlDeclsOdtInvGam _rlDeclsOfm _rlDeclsOfmGam _rlDeclsOopts _rlDeclsOrsScNm _rlDeclsOrwGam _rlDeclsOscGam _rlDeclsOvwNm 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
-- VwDecls -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dtInvGam             : DtInvGam
         fm                   : FmKind
         fmGam                : FmGam Expr
         opts                 : Opts
         rsScNm               : Nm
         rwGam                : RwExprGam
         scGam                : ScGam Expr
      synthesized attributes:
         errL                 : [Err]
         ppDbg                : PP_Doc
         self                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : VwDecl 
         child tl             : VwDecls 
         visit 0:
            local self        : _
      alternative Nil:
         visit 0:
            local self        : _
-}
-- cata
sem_VwDecls :: VwDecls  ->
               T_VwDecls 
sem_VwDecls list  =
    (Prelude.foldr sem_VwDecls_Cons sem_VwDecls_Nil (Prelude.map sem_VwDecl list) )
-- semantic domain
type T_VwDecls  = DtInvGam ->
                  FmKind ->
                  (FmGam Expr) ->
                  Opts ->
                  Nm ->
                  RwExprGam ->
                  (ScGam Expr) ->
                  ( ([Err]),PP_Doc,VwDecls )
sem_VwDecls_Cons :: T_VwDecl  ->
                    T_VwDecls  ->
                    T_VwDecls 
sem_VwDecls_Cons hd_ tl_  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecls 
              _hdOdtInvGam :: DtInvGam
              _hdOfm :: FmKind
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _hdOrsScNm :: Nm
              _hdOrwGam :: RwExprGam
              _hdOscGam :: (ScGam Expr)
              _tlOdtInvGam :: DtInvGam
              _tlOfm :: FmKind
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _tlOrsScNm :: Nm
              _tlOrwGam :: RwExprGam
              _tlOscGam :: (ScGam Expr)
              _hdIerrL :: ([Err])
              _hdIppDbg :: PP_Doc
              _hdIself :: VwDecl 
              _tlIerrL :: ([Err])
              _tlIppDbg :: PP_Doc
              _tlIself :: VwDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  _hdIerrL ++ _tlIerrL
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  _hdIppDbg >-< _tlIppDbg
              -- self rule
              _self =
                  (:) _hdIself _tlIself
              -- self rule
              _lhsOself =
                  _self
              -- copy rule (down)
              _hdOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _hdOfm =
                  _lhsIfm
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _hdOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _hdOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _hdOscGam =
                  _lhsIscGam
              -- copy rule (down)
              _tlOdtInvGam =
                  _lhsIdtInvGam
              -- copy rule (down)
              _tlOfm =
                  _lhsIfm
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOrsScNm =
                  _lhsIrsScNm
              -- copy rule (down)
              _tlOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _tlOscGam =
                  _lhsIscGam
              ( _hdIerrL,_hdIppDbg,_hdIself) =
                  hd_ _hdOdtInvGam _hdOfm _hdOfmGam _hdOopts _hdOrsScNm _hdOrwGam _hdOscGam 
              ( _tlIerrL,_tlIppDbg,_tlIself) =
                  tl_ _tlOdtInvGam _tlOfm _tlOfmGam _tlOopts _tlOrsScNm _tlOrwGam _tlOscGam 
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))
sem_VwDecls_Nil :: T_VwDecls 
sem_VwDecls_Nil  =
    (\ _lhsIdtInvGam
       _lhsIfm
       _lhsIfmGam
       _lhsIopts
       _lhsIrsScNm
       _lhsIrwGam
       _lhsIscGam ->
         (let _lhsOerrL :: ([Err])
              _lhsOppDbg :: PP_Doc
              _lhsOself :: VwDecls 
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 59, column 31)
              _lhsOerrL =
                  []
              -- use rule "build/ruler2/TrfAS2/CommonAG.ag"(line 53, column 31)
              _lhsOppDbg =
                  empty
              -- self rule
              _self =
                  []
              -- self rule
              _lhsOself =
                  _self
          in  ( _lhsOerrL,_lhsOppDbg,_lhsOself)))