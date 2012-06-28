

-- UUAGC 0.9.39.1 (build/ruler2/AS1/Imports.ag)
module AS1.Imports(ImpModMp, as1Imports, as1JoinAGItfs) where

import qualified Data.Map as Map
import Common
import AbsSyn.AbsSyn1









type ImpModMp = Map.Map Nm SPos

as1Imports :: AGItf -> ImpModMp
as1Imports r
  = (impModMp_Syn_AGItf r2)
  where r1 = sem_AGItf r
        r2 = wrap_AGItf r1
                (Inh_AGItf )

as1JoinAGItfs :: [AGItf] -> AGItf
as1JoinAGItfs ais = AGItf_AGItf $ concat $ [ ds | (AGItf_AGItf ds) <- ais ]

-- AGExprItf ---------------------------------------------------
{-
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
type T_AGExprItf  = ( )
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (let 
     in  ( ))
-- AGItf -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         impModMp             : ImpModMp
   alternatives:
      alternative AGItf:
         child decls          : Decls 
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _decls )  =
    (sem_AGItf_AGItf (sem_Decls _decls ) )
-- semantic domain
type T_AGItf  = ( ImpModMp)
data Inh_AGItf  = Inh_AGItf {}
data Syn_AGItf  = Syn_AGItf {impModMp_Syn_AGItf :: ImpModMp}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf )  =
    (let ( _lhsOimpModMp) = sem 
     in  (Syn_AGItf _lhsOimpModMp ))
sem_AGItf_AGItf :: T_Decls  ->
                   T_AGItf 
sem_AGItf_AGItf decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
-- ANm ---------------------------------------------------------
{-
   alternatives:
      alternative Fld:
         child nm             : {Nm}
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
      alternative Wild:
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
type T_ANm  = ( )
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (let 
     in  ( ))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (let 
     in  ( ))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (let 
     in  ( ))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (let 
     in  ( ))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (let 
     in  ( ))
-- AttrEqn -----------------------------------------------------
{-
   alternatives:
      alternative Del:
         child nm             : {Nm}
      alternative Eqn:
         child nm             : {Nm}
         child expr           : Expr 
-}
-- cata
sem_AttrEqn :: AttrEqn  ->
               T_AttrEqn 
sem_AttrEqn (AttrEqn_Del _nm )  =
    (sem_AttrEqn_Del _nm )
sem_AttrEqn (AttrEqn_Eqn _nm _expr )  =
    (sem_AttrEqn_Eqn _nm (sem_Expr _expr ) )
-- semantic domain
type T_AttrEqn  = ( )
sem_AttrEqn_Del :: Nm ->
                   T_AttrEqn 
sem_AttrEqn_Del nm_  =
    (let 
     in  ( ))
sem_AttrEqn_Eqn :: Nm ->
                   T_Expr  ->
                   T_AttrEqn 
sem_AttrEqn_Eqn nm_ expr_  =
    (let 
     in  ( ))
-- AttrEqns ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : AttrEqn 
         child tl             : AttrEqns 
      alternative Nil:
-}
-- cata
sem_AttrEqns :: AttrEqns  ->
                T_AttrEqns 
sem_AttrEqns list  =
    (Prelude.foldr sem_AttrEqns_Cons sem_AttrEqns_Nil (Prelude.map sem_AttrEqn list) )
-- semantic domain
type T_AttrEqns  = ( )
sem_AttrEqns_Cons :: T_AttrEqn  ->
                     T_AttrEqns  ->
                     T_AttrEqns 
sem_AttrEqns_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_AttrEqns_Nil :: T_AttrEqns 
sem_AttrEqns_Nil  =
    (let 
     in  ( ))
-- AttrIntro ---------------------------------------------------
{-
   alternatives:
      alternative Intro:
         child props          : {[AtProp]}
         child nm             : {Nm}
         child ty             : {Nm}
-}
-- cata
sem_AttrIntro :: AttrIntro  ->
                 T_AttrIntro 
sem_AttrIntro (AttrIntro_Intro _props _nm _ty )  =
    (sem_AttrIntro_Intro _props _nm _ty )
-- semantic domain
type T_AttrIntro  = ( )
sem_AttrIntro_Intro :: ([AtProp]) ->
                       Nm ->
                       Nm ->
                       T_AttrIntro 
sem_AttrIntro_Intro props_ nm_ ty_  =
    (let 
     in  ( ))
-- AttrIntroDecl -----------------------------------------------
{-
   alternatives:
      alternative Attrs:
         child inhs           : AttrIntros 
         child inhsyns        : AttrIntros 
         child syns           : AttrIntros 
      alternative AttrsProp:
         child intros         : AttrIntros 
      alternative Scheme:
         child pos            : {SPos}
         child nm             : {Nm}
         child renames        : AttrRenames 
-}
-- cata
sem_AttrIntroDecl :: AttrIntroDecl  ->
                     T_AttrIntroDecl 
sem_AttrIntroDecl (AttrIntroDecl_Attrs _inhs _inhsyns _syns )  =
    (sem_AttrIntroDecl_Attrs (sem_AttrIntros _inhs ) (sem_AttrIntros _inhsyns ) (sem_AttrIntros _syns ) )
sem_AttrIntroDecl (AttrIntroDecl_AttrsProp _intros )  =
    (sem_AttrIntroDecl_AttrsProp (sem_AttrIntros _intros ) )
sem_AttrIntroDecl (AttrIntroDecl_Scheme _pos _nm _renames )  =
    (sem_AttrIntroDecl_Scheme _pos _nm (sem_AttrRenames _renames ) )
-- semantic domain
type T_AttrIntroDecl  = ( )
sem_AttrIntroDecl_Attrs :: T_AttrIntros  ->
                           T_AttrIntros  ->
                           T_AttrIntros  ->
                           T_AttrIntroDecl 
sem_AttrIntroDecl_Attrs inhs_ inhsyns_ syns_  =
    (let 
     in  ( ))
sem_AttrIntroDecl_AttrsProp :: T_AttrIntros  ->
                               T_AttrIntroDecl 
sem_AttrIntroDecl_AttrsProp intros_  =
    (let 
     in  ( ))
sem_AttrIntroDecl_Scheme :: SPos ->
                            Nm ->
                            T_AttrRenames  ->
                            T_AttrIntroDecl 
sem_AttrIntroDecl_Scheme pos_ nm_ renames_  =
    (let 
     in  ( ))
-- AttrIntroDecls ----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : AttrIntroDecl 
         child tl             : AttrIntroDecls 
      alternative Nil:
-}
-- cata
sem_AttrIntroDecls :: AttrIntroDecls  ->
                      T_AttrIntroDecls 
sem_AttrIntroDecls list  =
    (Prelude.foldr sem_AttrIntroDecls_Cons sem_AttrIntroDecls_Nil (Prelude.map sem_AttrIntroDecl list) )
-- semantic domain
type T_AttrIntroDecls  = ( )
sem_AttrIntroDecls_Cons :: T_AttrIntroDecl  ->
                           T_AttrIntroDecls  ->
                           T_AttrIntroDecls 
sem_AttrIntroDecls_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_AttrIntroDecls_Nil :: T_AttrIntroDecls 
sem_AttrIntroDecls_Nil  =
    (let 
     in  ( ))
-- AttrIntros --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : AttrIntro 
         child tl             : AttrIntros 
      alternative Nil:
-}
-- cata
sem_AttrIntros :: AttrIntros  ->
                  T_AttrIntros 
sem_AttrIntros list  =
    (Prelude.foldr sem_AttrIntros_Cons sem_AttrIntros_Nil (Prelude.map sem_AttrIntro list) )
-- semantic domain
type T_AttrIntros  = ( )
sem_AttrIntros_Cons :: T_AttrIntro  ->
                       T_AttrIntros  ->
                       T_AttrIntros 
sem_AttrIntros_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_AttrIntros_Nil :: T_AttrIntros 
sem_AttrIntros_Nil  =
    (let 
     in  ( ))
-- AttrRename --------------------------------------------------
{-
   alternatives:
      alternative EqualTo:
         child pos            : {SPos}
         child nmLeft         : {Nm}
         child nmRight        : {Nm}
      alternative Rename:
         child pos            : {SPos}
         child nmNew          : {Nm}
         child nmOld          : {Nm}
-}
-- cata
sem_AttrRename :: AttrRename  ->
                  T_AttrRename 
sem_AttrRename (AttrRename_EqualTo _pos _nmLeft _nmRight )  =
    (sem_AttrRename_EqualTo _pos _nmLeft _nmRight )
sem_AttrRename (AttrRename_Rename _pos _nmNew _nmOld )  =
    (sem_AttrRename_Rename _pos _nmNew _nmOld )
-- semantic domain
type T_AttrRename  = ( )
sem_AttrRename_EqualTo :: SPos ->
                          Nm ->
                          Nm ->
                          T_AttrRename 
sem_AttrRename_EqualTo pos_ nmLeft_ nmRight_  =
    (let 
     in  ( ))
sem_AttrRename_Rename :: SPos ->
                         Nm ->
                         Nm ->
                         T_AttrRename 
sem_AttrRename_Rename pos_ nmNew_ nmOld_  =
    (let 
     in  ( ))
-- AttrRenames -------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : AttrRename 
         child tl             : AttrRenames 
      alternative Nil:
-}
-- cata
sem_AttrRenames :: AttrRenames  ->
                   T_AttrRenames 
sem_AttrRenames list  =
    (Prelude.foldr sem_AttrRenames_Cons sem_AttrRenames_Nil (Prelude.map sem_AttrRename list) )
-- semantic domain
type T_AttrRenames  = ( )
sem_AttrRenames_Cons :: T_AttrRename  ->
                        T_AttrRenames  ->
                        T_AttrRenames 
sem_AttrRenames_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_AttrRenames_Nil :: T_AttrRenames 
sem_AttrRenames_Nil  =
    (let 
     in  ( ))
-- Decl --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         impModMp             : ImpModMp
   alternatives:
      alternative Attr:
         child intros         : AttrIntroDecls 
      alternative DataAST:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNms      : {[Nm]}
         child decls          : Decls 
      alternative DataASTAlt:
         child pos            : {SPos}
         child nm             : {Nm}
         child ruleNm         : {Nm}
         child mbBasedOnNm    : {Maybe Nm}
         child fldIntros      : FldIntros 
      alternative DataASTView:
         child pos            : {SPos}
         child nm             : {Nm}
         child decls          : Decls 
      alternative Explain:
         child mbNm           : {Maybe Nm}
         child expr           : Expr 
      alternative Extern:
         child nms            : {[Nm]}
      alternative Fmt:
         child fmKind         : {FmKind}
         child atIO           : {AtDir}
         child matchExpr      : Expr 
         child expr           : Expr 
      alternative Include:
         child pos            : {SPos}
         child nm             : {Nm}
      alternative Preamble:
         child fmKind         : {FmKind}
         child preamble       : {String}
      alternative RulView:
         child pos            : {SPos}
         child nm             : {Nm}
         child jdIntros       : RuleJudgeIntros 
         child group          : {[[Nm]]}
      alternative Rule:
         child pos            : {SPos}
         child nm             : {Nm}
         child mbBasedOnNm    : {Maybe Nm}
         child viewSel        : {Maybe ViewSel}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
      alternative Rules:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNm       : {Nm}
         child viewSel        : {ViewSel}
         child info           : {String}
         child decls          : Decls 
      alternative RulesGroup:
         child pos            : {SPos}
         child nm             : {Nm}
         child schemeNm       : {Nm}
         child viewSel        : {ViewSel}
         child info           : {String}
         child rlNms          : {[(Nm,Nm)]}
      alternative Scheme:
         child pos            : {SPos}
         child scKind         : {ScKind}
         child nm             : {Nm}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
      alternative SchemeDeriv:
         child pos            : {SPos}
         child scKind         : {ScKind}
         child nm             : {Nm}
         child scDeriv        : {ScDeriv}
         child mbAGNm         : {Maybe String}
         child decls          : Decls 
      alternative ScmView:
         child nm             : {Nm}
         child decls          : Decls 
      alternative ShpDel:
         child pos            : {SPos}
         child fmKinds        : {[FmKind]}
      alternative ShpJudge:
         child pos            : {SPos}
         child fmKind         : {FmKind}
         child expr           : Expr 
      alternative ViewHierarchy:
         child nmOrder        : {[[Nm]]}
-}
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl (Decl_Attr _intros )  =
    (sem_Decl_Attr (sem_AttrIntroDecls _intros ) )
sem_Decl (Decl_DataAST _pos _nm _schemeNms _decls )  =
    (sem_Decl_DataAST _pos _nm _schemeNms (sem_Decls _decls ) )
sem_Decl (Decl_DataASTAlt _pos _nm _ruleNm _mbBasedOnNm _fldIntros )  =
    (sem_Decl_DataASTAlt _pos _nm _ruleNm _mbBasedOnNm (sem_FldIntros _fldIntros ) )
sem_Decl (Decl_DataASTView _pos _nm _decls )  =
    (sem_Decl_DataASTView _pos _nm (sem_Decls _decls ) )
sem_Decl (Decl_Explain _mbNm _expr )  =
    (sem_Decl_Explain _mbNm (sem_Expr _expr ) )
sem_Decl (Decl_Extern _nms )  =
    (sem_Decl_Extern _nms )
sem_Decl (Decl_Fmt _fmKind _atIO _matchExpr _expr )  =
    (sem_Decl_Fmt _fmKind _atIO (sem_Expr _matchExpr ) (sem_Expr _expr ) )
sem_Decl (Decl_Include _pos _nm )  =
    (sem_Decl_Include _pos _nm )
sem_Decl (Decl_Preamble _fmKind _preamble )  =
    (sem_Decl_Preamble _fmKind _preamble )
sem_Decl (Decl_RulView _pos _nm _jdIntros _group )  =
    (sem_Decl_RulView _pos _nm (sem_RuleJudgeIntros _jdIntros ) _group )
sem_Decl (Decl_Rule _pos _nm _mbBasedOnNm _viewSel _mbAGNm _decls )  =
    (sem_Decl_Rule _pos _nm _mbBasedOnNm _viewSel _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_Rules _pos _nm _schemeNm _viewSel _info _decls )  =
    (sem_Decl_Rules _pos _nm _schemeNm _viewSel _info (sem_Decls _decls ) )
sem_Decl (Decl_RulesGroup _pos _nm _schemeNm _viewSel _info _rlNms )  =
    (sem_Decl_RulesGroup _pos _nm _schemeNm _viewSel _info _rlNms )
sem_Decl (Decl_Scheme _pos _scKind _nm _mbAGNm _decls )  =
    (sem_Decl_Scheme _pos _scKind _nm _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_SchemeDeriv _pos _scKind _nm _scDeriv _mbAGNm _decls )  =
    (sem_Decl_SchemeDeriv _pos _scKind _nm _scDeriv _mbAGNm (sem_Decls _decls ) )
sem_Decl (Decl_ScmView _nm _decls )  =
    (sem_Decl_ScmView _nm (sem_Decls _decls ) )
sem_Decl (Decl_ShpDel _pos _fmKinds )  =
    (sem_Decl_ShpDel _pos _fmKinds )
sem_Decl (Decl_ShpJudge _pos _fmKind _expr )  =
    (sem_Decl_ShpJudge _pos _fmKind (sem_Expr _expr ) )
sem_Decl (Decl_ViewHierarchy _nmOrder )  =
    (sem_Decl_ViewHierarchy _nmOrder )
-- semantic domain
type T_Decl  = ( ImpModMp)
sem_Decl_Attr :: T_AttrIntroDecls  ->
                 T_Decl 
sem_Decl_Attr intros_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_DataAST :: SPos ->
                    Nm ->
                    ([Nm]) ->
                    T_Decls  ->
                    T_Decl 
sem_Decl_DataAST pos_ nm_ schemeNms_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_DataASTAlt :: SPos ->
                       Nm ->
                       Nm ->
                       (Maybe Nm) ->
                       T_FldIntros  ->
                       T_Decl 
sem_Decl_DataASTAlt pos_ nm_ ruleNm_ mbBasedOnNm_ fldIntros_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_DataASTView :: SPos ->
                        Nm ->
                        T_Decls  ->
                        T_Decl 
sem_Decl_DataASTView pos_ nm_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_Explain :: (Maybe Nm) ->
                    T_Expr  ->
                    T_Decl 
sem_Decl_Explain mbNm_ expr_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_Extern :: ([Nm]) ->
                   T_Decl 
sem_Decl_Extern nms_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_Fmt :: FmKind ->
                AtDir ->
                T_Expr  ->
                T_Expr  ->
                T_Decl 
sem_Decl_Fmt fmKind_ atIO_ matchExpr_ expr_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_Include :: SPos ->
                    Nm ->
                    T_Decl 
sem_Decl_Include pos_ nm_  =
    (let _lhsOimpModMp :: ImpModMp
         -- "build/ruler2/AS1/Imports.ag"(line 36, column 21)
         _lhsOimpModMp =
             Map.singleton nm_ pos_
     in  ( _lhsOimpModMp))
sem_Decl_Preamble :: FmKind ->
                     String ->
                     T_Decl 
sem_Decl_Preamble fmKind_ preamble_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_RulView :: SPos ->
                    Nm ->
                    T_RuleJudgeIntros  ->
                    ([[Nm]]) ->
                    T_Decl 
sem_Decl_RulView pos_ nm_ jdIntros_ group_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_Rule :: SPos ->
                 Nm ->
                 (Maybe Nm) ->
                 (Maybe ViewSel) ->
                 (Maybe String) ->
                 T_Decls  ->
                 T_Decl 
sem_Decl_Rule pos_ nm_ mbBasedOnNm_ viewSel_ mbAGNm_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_Rules :: SPos ->
                  Nm ->
                  Nm ->
                  ViewSel ->
                  String ->
                  T_Decls  ->
                  T_Decl 
sem_Decl_Rules pos_ nm_ schemeNm_ viewSel_ info_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_RulesGroup :: SPos ->
                       Nm ->
                       Nm ->
                       ViewSel ->
                       String ->
                       ([(Nm,Nm)]) ->
                       T_Decl 
sem_Decl_RulesGroup pos_ nm_ schemeNm_ viewSel_ info_ rlNms_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_Scheme :: SPos ->
                   ScKind ->
                   Nm ->
                   (Maybe String) ->
                   T_Decls  ->
                   T_Decl 
sem_Decl_Scheme pos_ scKind_ nm_ mbAGNm_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_SchemeDeriv :: SPos ->
                        ScKind ->
                        Nm ->
                        ScDeriv ->
                        (Maybe String) ->
                        T_Decls  ->
                        T_Decl 
sem_Decl_SchemeDeriv pos_ scKind_ nm_ scDeriv_ mbAGNm_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_ScmView :: Nm ->
                    T_Decls  ->
                    T_Decl 
sem_Decl_ScmView nm_ decls_  =
    (let _lhsOimpModMp :: ImpModMp
         _declsIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _declsIimpModMp
         ( _declsIimpModMp) =
             decls_ 
     in  ( _lhsOimpModMp))
sem_Decl_ShpDel :: SPos ->
                   ([FmKind]) ->
                   T_Decl 
sem_Decl_ShpDel pos_ fmKinds_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_ShpJudge :: SPos ->
                     FmKind ->
                     T_Expr  ->
                     T_Decl 
sem_Decl_ShpJudge pos_ fmKind_ expr_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
sem_Decl_ViewHierarchy :: ([[Nm]]) ->
                          T_Decl 
sem_Decl_ViewHierarchy nmOrder_  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
-- Decls -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         impModMp             : ImpModMp
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls list  =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )
-- semantic domain
type T_Decls  = ( ImpModMp)
sem_Decls_Cons :: T_Decl  ->
                  T_Decls  ->
                  T_Decls 
sem_Decls_Cons hd_ tl_  =
    (let _lhsOimpModMp :: ImpModMp
         _hdIimpModMp :: ImpModMp
         _tlIimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             _hdIimpModMp `Map.union` _tlIimpModMp
         ( _hdIimpModMp) =
             hd_ 
         ( _tlIimpModMp) =
             tl_ 
     in  ( _lhsOimpModMp))
sem_Decls_Nil :: T_Decls 
sem_Decls_Nil  =
    (let _lhsOimpModMp :: ImpModMp
         -- use rule "build/ruler2/AS1/Imports.ag"(line 33, column 35)
         _lhsOimpModMp =
             Map.empty
     in  ( _lhsOimpModMp))
-- ECnstr ------------------------------------------------------
{-
   alternatives:
      alternative Empty:
      alternative Ty:
         child nms            : {[Nm]}
      alternative Var:
         child nm             : {Nm}
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
type T_ECnstr  = ( )
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (let 
     in  ( ))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (let 
     in  ( ))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (let 
     in  ( ))
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative AVar:
         child anm            : ANm 
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative AppTop:
         child expr           : Expr 
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
      alternative Empty:
      alternative Expr:
         child expr           : Expr 
      alternative Int:
         child int            : {String}
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Paren:
         child expr           : Expr 
      alternative Retain:
         child expr           : Expr 
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
      alternative SelTop:
         child expr           : Expr 
      alternative StrAsIs:
         child str            : {String}
      alternative StrText:
         child str            : {String}
      alternative Undefined:
      alternative Uniq:
      alternative Var:
         child nm             : {Nm}
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
      alternative WrapCnstr:
         child cnstr          : ECnstr 
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
type T_Expr  = ( )
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (let 
     in  ( ))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (let 
     in  ( ))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (let 
     in  ( ))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (let 
     in  ( ))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (let 
     in  ( ))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (let 
     in  ( ))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (let 
     in  ( ))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (let 
     in  ( ))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (let 
     in  ( ))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (let 
     in  ( ))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (let 
     in  ( ))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (let 
     in  ( ))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (let 
     in  ( ))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (let 
     in  ( ))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (let 
     in  ( ))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (let 
     in  ( ))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (let 
     in  ( ))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (let 
     in  ( ))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (let 
     in  ( ))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (let 
     in  ( ))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (let 
     in  ( ))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (let 
     in  ( ))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (let 
     in  ( ))
-- FldIntro ----------------------------------------------------
{-
   alternatives:
      alternative Intro:
         child nm             : {Nm}
         child ty             : {Ty}
-}
-- cata
sem_FldIntro :: FldIntro  ->
                T_FldIntro 
sem_FldIntro (FldIntro_Intro _nm _ty )  =
    (sem_FldIntro_Intro _nm _ty )
-- semantic domain
type T_FldIntro  = ( )
sem_FldIntro_Intro :: Nm ->
                      Ty ->
                      T_FldIntro 
sem_FldIntro_Intro nm_ ty_  =
    (let 
     in  ( ))
-- FldIntros ---------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : FldIntro 
         child tl             : FldIntros 
      alternative Nil:
-}
-- cata
sem_FldIntros :: FldIntros  ->
                 T_FldIntros 
sem_FldIntros list  =
    (Prelude.foldr sem_FldIntros_Cons sem_FldIntros_Nil (Prelude.map sem_FldIntro list) )
-- semantic domain
type T_FldIntros  = ( )
sem_FldIntros_Cons :: T_FldIntro  ->
                      T_FldIntros  ->
                      T_FldIntros 
sem_FldIntros_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_FldIntros_Nil :: T_FldIntros 
sem_FldIntros_Nil  =
    (let 
     in  ( ))
-- MbExpr ------------------------------------------------------
{-
   alternatives:
      alternative Just:
         child just           : Expr 
      alternative Nothing:
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = ( )
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (let 
     in  ( ))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (let 
     in  ( ))
-- RExpr -------------------------------------------------------
{-
   alternatives:
      alternative Del:
         child pos            : {SPos}
         child nms            : {[Nm]}
      alternative Judge:
         child pos            : {SPos}
         child mbRNm          : {Maybe Nm}
         child schemeNm       : {Nm}
         child eqns           : RExprEqn 
         child isSmallExpr    : {Bool}
-}
-- cata
sem_RExpr :: RExpr  ->
             T_RExpr 
sem_RExpr (RExpr_Del _pos _nms )  =
    (sem_RExpr_Del _pos _nms )
sem_RExpr (RExpr_Judge _pos _mbRNm _schemeNm _eqns _isSmallExpr )  =
    (sem_RExpr_Judge _pos _mbRNm _schemeNm (sem_RExprEqn _eqns ) _isSmallExpr )
-- semantic domain
type T_RExpr  = ( )
sem_RExpr_Del :: SPos ->
                 ([Nm]) ->
                 T_RExpr 
sem_RExpr_Del pos_ nms_  =
    (let 
     in  ( ))
sem_RExpr_Judge :: SPos ->
                   (Maybe Nm) ->
                   Nm ->
                   T_RExprEqn  ->
                   Bool ->
                   T_RExpr 
sem_RExpr_Judge pos_ mbRNm_ schemeNm_ eqns_ isSmallExpr_  =
    (let 
     in  ( ))
-- RExprEqn ----------------------------------------------------
{-
   alternatives:
      alternative Attrs:
         child eqns           : AttrEqns 
      alternative Expr:
         child expr           : Expr 
-}
-- cata
sem_RExprEqn :: RExprEqn  ->
                T_RExprEqn 
sem_RExprEqn (RExprEqn_Attrs _eqns )  =
    (sem_RExprEqn_Attrs (sem_AttrEqns _eqns ) )
sem_RExprEqn (RExprEqn_Expr _expr )  =
    (sem_RExprEqn_Expr (sem_Expr _expr ) )
-- semantic domain
type T_RExprEqn  = ( )
sem_RExprEqn_Attrs :: T_AttrEqns  ->
                      T_RExprEqn 
sem_RExprEqn_Attrs eqns_  =
    (let 
     in  ( ))
sem_RExprEqn_Expr :: T_Expr  ->
                     T_RExprEqn 
sem_RExprEqn_Expr expr_  =
    (let 
     in  ( ))
-- RExprs ------------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : RExpr 
         child tl             : RExprs 
      alternative Nil:
-}
-- cata
sem_RExprs :: RExprs  ->
              T_RExprs 
sem_RExprs list  =
    (Prelude.foldr sem_RExprs_Cons sem_RExprs_Nil (Prelude.map sem_RExpr list) )
-- semantic domain
type T_RExprs  = ( )
sem_RExprs_Cons :: T_RExpr  ->
                   T_RExprs  ->
                   T_RExprs 
sem_RExprs_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_RExprs_Nil :: T_RExprs 
sem_RExprs_Nil  =
    (let 
     in  ( ))
-- RuleJudgeIntro ----------------------------------------------
{-
   alternatives:
      alternative PrePost:
         child extNms         : {[Nm]}
         child pre            : RExprs 
         child post           : RExprs 
      alternative RulesetRule:
         child pos            : {SPos}
         child rsNm           : {Nm}
         child rlNm           : {Nm}
         child schemeRnmL     : {[BldRename]}
-}
-- cata
sem_RuleJudgeIntro :: RuleJudgeIntro  ->
                      T_RuleJudgeIntro 
sem_RuleJudgeIntro (RuleJudgeIntro_PrePost _extNms _pre _post )  =
    (sem_RuleJudgeIntro_PrePost _extNms (sem_RExprs _pre ) (sem_RExprs _post ) )
sem_RuleJudgeIntro (RuleJudgeIntro_RulesetRule _pos _rsNm _rlNm _schemeRnmL )  =
    (sem_RuleJudgeIntro_RulesetRule _pos _rsNm _rlNm _schemeRnmL )
-- semantic domain
type T_RuleJudgeIntro  = ( )
sem_RuleJudgeIntro_PrePost :: ([Nm]) ->
                              T_RExprs  ->
                              T_RExprs  ->
                              T_RuleJudgeIntro 
sem_RuleJudgeIntro_PrePost extNms_ pre_ post_  =
    (let 
     in  ( ))
sem_RuleJudgeIntro_RulesetRule :: SPos ->
                                  Nm ->
                                  Nm ->
                                  ([BldRename]) ->
                                  T_RuleJudgeIntro 
sem_RuleJudgeIntro_RulesetRule pos_ rsNm_ rlNm_ schemeRnmL_  =
    (let 
     in  ( ))
-- RuleJudgeIntros ---------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : RuleJudgeIntro 
         child tl             : RuleJudgeIntros 
      alternative Nil:
-}
-- cata
sem_RuleJudgeIntros :: RuleJudgeIntros  ->
                       T_RuleJudgeIntros 
sem_RuleJudgeIntros list  =
    (Prelude.foldr sem_RuleJudgeIntros_Cons sem_RuleJudgeIntros_Nil (Prelude.map sem_RuleJudgeIntro list) )
-- semantic domain
type T_RuleJudgeIntros  = ( )
sem_RuleJudgeIntros_Cons :: T_RuleJudgeIntro  ->
                            T_RuleJudgeIntros  ->
                            T_RuleJudgeIntros 
sem_RuleJudgeIntros_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_RuleJudgeIntros_Nil :: T_RuleJudgeIntros 
sem_RuleJudgeIntros_Nil  =
    (let 
     in  ( ))