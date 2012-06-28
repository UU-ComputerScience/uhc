

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/JavaScript/Pretty.ag)
module EH101.JavaScript.Pretty(ppJavaScriptModule) where

import EH.Util.Pretty
import EH101.Base.Common
import EH101.JavaScript





ppJavaScriptModule
  :: JavaScriptModule
     -> ( PP_Doc
        , PP_Doc
        )
ppJavaScriptModule ent
  = ( pp_Syn_AGItf t
    ,
        ppMain_Syn_AGItf t
    )
  where t = wrap_AGItf
                 (sem_AGItf (AGItf_AGItf ent))
                 Inh_AGItf



ppStat :: PP_Doc -> PP_Doc
ppStat x = x >|< ";"

-- AGItf -------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppMain               : PP_Doc
   alternatives:
      alternative AGItf:
         child module         : JavaScriptModule 
-}
-- cata
sem_AGItf :: AGItf  ->
             T_AGItf 
sem_AGItf (AGItf_AGItf _module )  =
    (sem_AGItf_AGItf (sem_JavaScriptModule _module ) )
-- semantic domain
type T_AGItf  = ( PP_Doc,PP_Doc)
data Inh_AGItf  = Inh_AGItf {}
data Syn_AGItf  = Syn_AGItf {pp_Syn_AGItf :: !(PP_Doc),ppMain_Syn_AGItf :: !(PP_Doc)}
wrap_AGItf :: T_AGItf  ->
              Inh_AGItf  ->
              Syn_AGItf 
wrap_AGItf sem (Inh_AGItf )  =
    (let ( _lhsOpp,_lhsOppMain) = sem 
     in  (Syn_AGItf _lhsOpp _lhsOppMain ))
sem_AGItf_AGItf :: T_JavaScriptModule  ->
                   T_AGItf 
sem_AGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleIpp,_moduleIppMain) ->
         (case (_moduleIpp) of
          { _lhsOpp ->
          (case (_moduleIppMain) of
           { _lhsOppMain ->
           ( _lhsOpp,_lhsOppMain) }) }) })
-- Alt ---------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Alt:
         child tag            : {Int}
         child stats          : StatL 
-}
-- cata
sem_Alt :: Alt  ->
           T_Alt 
sem_Alt (Alt_Alt _tag _stats )  =
    (sem_Alt_Alt _tag (sem_StatL _stats ) )
-- semantic domain
type T_Alt  = ( PP_Doc)
sem_Alt_Alt :: Int ->
               T_StatL  ->
               T_Alt 
sem_Alt_Alt tag_ stats_  =
    (case (stats_ ) of
     { ( _statsIppL) ->
         (case ("case" >#< tag_ >|< ":" >-< indent 1 (vlist _statsIppL)) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- AltL --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Alt 
         child tl             : AltL 
      alternative Nil:
-}
-- cata
sem_AltL :: AltL  ->
            T_AltL 
sem_AltL list  =
    (Prelude.foldr sem_AltL_Cons sem_AltL_Nil (Prelude.map sem_Alt list) )
-- semantic domain
type T_AltL  = ( ([PP_Doc]))
sem_AltL_Cons :: T_Alt  ->
                 T_AltL  ->
                 T_AltL 
sem_AltL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIppL) ->
         (case (hd_ ) of
          { ( _hdIpp) ->
              (case (_hdIpp : _tlIppL) of
               { _lhsOppL ->
               ( _lhsOppL) }) }) })
sem_AltL_Nil :: T_AltL 
sem_AltL_Nil  =
    (case ([]) of
     { _lhsOppL ->
     ( _lhsOppL) })
-- Expr --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Arr:
         child elts           : ExprL 
         visit 0:
            local pp          : _
      alternative ArrInx:
         child arr            : Expr 
         child inx            : Expr 
         visit 0:
            local pp          : _
      alternative Call:
         child fun            : Expr 
         child args           : ExprL 
         visit 0:
            local pp          : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local pp          : _
      alternative False:
         visit 0:
            local pp          : _
      alternative Fun:
         child mbNm           : {Maybe HsName}
         child args           : {[HsName]}
         child body           : Stat 
         visit 0:
            local pp          : _
      alternative If:
         child c              : Expr 
         child t              : Expr 
         child e              : Expr 
         visit 0:
            local pp          : _
      alternative Inline:
         child str            : {String}
         visit 0:
            local pp          : _
      alternative Int:
         child int            : {Integer}
         visit 0:
            local pp          : _
      alternative New:
         child expr           : Expr 
         visit 0:
            local pp          : _
      alternative Obj:
         child elts           : NmExprL 
         visit 0:
            local pp          : _
      alternative ObjFld:
         child obj            : Expr 
         child fld            : {HsName}
         visit 0:
            local pp          : _
      alternative Op:
         child nm             : {HsName}
         child l              : Expr 
         child r              : Expr 
         visit 0:
            local pp          : _
      alternative Sel:
         child expr           : Expr 
         child sel            : Expr 
         visit 0:
            local pp          : _
      alternative Str:
         child str            : {String}
         visit 0:
            local pp          : _
      alternative This:
         visit 0:
            local pp          : _
      alternative True:
         visit 0:
            local pp          : _
      alternative Undefined:
         visit 0:
            local pp          : _
      alternative Var:
         child nm             : {HsName}
         visit 0:
            local pp          : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_Arr _elts )  =
    (sem_Expr_Arr (sem_ExprL _elts ) )
sem_Expr (Expr_ArrInx _arr _inx )  =
    (sem_Expr_ArrInx (sem_Expr _arr ) (sem_Expr _inx ) )
sem_Expr (Expr_Call _fun _args )  =
    (sem_Expr_Call (sem_Expr _fun ) (sem_ExprL _args ) )
sem_Expr (Expr_Char _char )  =
    (sem_Expr_Char _char )
sem_Expr (Expr_False )  =
    (sem_Expr_False )
sem_Expr (Expr_Fun _mbNm _args _body )  =
    (sem_Expr_Fun _mbNm _args (sem_Stat _body ) )
sem_Expr (Expr_If _c _t _e )  =
    (sem_Expr_If (sem_Expr _c ) (sem_Expr _t ) (sem_Expr _e ) )
sem_Expr (Expr_Inline _str )  =
    (sem_Expr_Inline _str )
sem_Expr (Expr_Int _int )  =
    (sem_Expr_Int _int )
sem_Expr (Expr_New _expr )  =
    (sem_Expr_New (sem_Expr _expr ) )
sem_Expr (Expr_Obj _elts )  =
    (sem_Expr_Obj (sem_NmExprL _elts ) )
sem_Expr (Expr_ObjFld _obj _fld )  =
    (sem_Expr_ObjFld (sem_Expr _obj ) _fld )
sem_Expr (Expr_Op _nm _l _r )  =
    (sem_Expr_Op _nm (sem_Expr _l ) (sem_Expr _r ) )
sem_Expr (Expr_Sel _expr _sel )  =
    (sem_Expr_Sel (sem_Expr _expr ) (sem_Expr _sel ) )
sem_Expr (Expr_Str _str )  =
    (sem_Expr_Str _str )
sem_Expr (Expr_This )  =
    (sem_Expr_This )
sem_Expr (Expr_True )  =
    (sem_Expr_True )
sem_Expr (Expr_Undefined )  =
    (sem_Expr_Undefined )
sem_Expr (Expr_Var _nm )  =
    (sem_Expr_Var _nm )
-- semantic domain
type T_Expr  = ( PP_Doc)
sem_Expr_Arr :: T_ExprL  ->
                T_Expr 
sem_Expr_Arr elts_  =
    (case (elts_ ) of
     { ( _eltsIppL) ->
         (case (ppBracketsCommas _eltsIppL) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Expr_ArrInx :: T_Expr  ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_ArrInx arr_ inx_  =
    (case (inx_ ) of
     { ( _inxIpp) ->
         (case (arr_ ) of
          { ( _arrIpp) ->
              (case (_arrIpp >|< ppBrackets _inxIpp) of
               { _pp ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) })
sem_Expr_Call :: T_Expr  ->
                 T_ExprL  ->
                 T_Expr 
sem_Expr_Call fun_ args_  =
    (case (args_ ) of
     { ( _argsIppL) ->
         (case (fun_ ) of
          { ( _funIpp) ->
              (case (_funIpp >|< ppParensCommas _argsIppL) of
               { _pp ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) })
sem_Expr_Char :: Char ->
                 T_Expr 
sem_Expr_Char char_  =
    (case (pp $ show char_) of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_False :: T_Expr 
sem_Expr_False  =
    (case (pp "false") of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_Fun :: (Maybe HsName) ->
                ([HsName]) ->
                T_Stat  ->
                T_Expr 
sem_Expr_Fun mbNm_ args_ body_  =
    (case (body_ ) of
     { ( _bodyIpp) ->
         (case ("function" >|< maybe empty (\n -> " " >|< n) mbNm_ >|< ppParensCommas args_ >-< _bodyIpp) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Expr_If :: T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_If c_ t_ e_  =
    (case (e_ ) of
     { ( _eIpp) ->
         (case (t_ ) of
          { ( _tIpp) ->
              (case (c_ ) of
               { ( _cIpp) ->
                   (case (ppParens (_cIpp >#< "?" >#< _tIpp >#< ":" >#< _eIpp)) of
                    { _pp ->
                    (case (_pp) of
                     { _lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })
sem_Expr_Inline :: String ->
                   T_Expr 
sem_Expr_Inline str_  =
    (case (pp str_) of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_Int :: Integer ->
                T_Expr 
sem_Expr_Int int_  =
    (case (pp $ show int_) of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_New :: T_Expr  ->
                T_Expr 
sem_Expr_New expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case ("new" >#< _exprIpp) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Expr_Obj :: T_NmExprL  ->
                T_Expr 
sem_Expr_Obj elts_  =
    (case (elts_ ) of
     { ( _eltsIppL) ->
         (case (ppCurlysCommas $ map (\(x,y) -> x >|< ":" >|< y) _eltsIppL) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Expr_ObjFld :: T_Expr  ->
                   HsName ->
                   T_Expr 
sem_Expr_ObjFld obj_ fld_  =
    (case (obj_ ) of
     { ( _objIpp) ->
         (case (_objIpp >|< "." >|< fld_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Expr_Op :: HsName ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ l_ r_  =
    (case (r_ ) of
     { ( _rIpp) ->
         (case (l_ ) of
          { ( _lIpp) ->
              (case (ppParens (_lIpp >|< nm_ >|< _rIpp)) of
               { _pp ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) })
sem_Expr_Sel :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Sel expr_ sel_  =
    (case (sel_ ) of
     { ( _selIpp) ->
         (case (expr_ ) of
          { ( _exprIpp) ->
              (case (_exprIpp >|< "." >|< _selIpp) of
               { _pp ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) })
sem_Expr_Str :: String ->
                T_Expr 
sem_Expr_Str str_  =
    (case (pp $ show str_) of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_This :: T_Expr 
sem_Expr_This  =
    (case (pp "this") of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_True :: T_Expr 
sem_Expr_True  =
    (case (pp "true") of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (case (pp "undefined") of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Expr_Var :: HsName ->
                T_Expr 
sem_Expr_Var nm_  =
    (case (pp nm_) of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
-- ExprL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Expr 
         child tl             : ExprL 
      alternative Nil:
-}
-- cata
sem_ExprL :: ExprL  ->
             T_ExprL 
sem_ExprL list  =
    (Prelude.foldr sem_ExprL_Cons sem_ExprL_Nil (Prelude.map sem_Expr list) )
-- semantic domain
type T_ExprL  = ( ([PP_Doc]))
sem_ExprL_Cons :: T_Expr  ->
                  T_ExprL  ->
                  T_ExprL 
sem_ExprL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIppL) ->
         (case (hd_ ) of
          { ( _hdIpp) ->
              (case (_hdIpp : _tlIppL) of
               { _lhsOppL ->
               ( _lhsOppL) }) }) })
sem_ExprL_Nil :: T_ExprL 
sem_ExprL_Nil  =
    (case ([]) of
     { _lhsOppL ->
     ( _lhsOppL) })
-- JavaScriptModule --------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppMain               : PP_Doc
   alternatives:
      alternative Mod:
         child decls          : StatL 
         child main           : StatL 
-}
-- cata
sem_JavaScriptModule :: JavaScriptModule  ->
                        T_JavaScriptModule 
sem_JavaScriptModule (JavaScriptModule_Mod _decls _main )  =
    (sem_JavaScriptModule_Mod (sem_StatL _decls ) (sem_StatL _main ) )
-- semantic domain
type T_JavaScriptModule  = ( PP_Doc,PP_Doc)
sem_JavaScriptModule_Mod :: T_StatL  ->
                            T_StatL  ->
                            T_JavaScriptModule 
sem_JavaScriptModule_Mod decls_ main_  =
    (case (decls_ ) of
     { ( _declsIppL) ->
         (case (vlist _declsIppL) of
          { _lhsOpp ->
          (case (main_ ) of
           { ( _mainIppL) ->
               (case (vlist _mainIppL) of
                { _lhsOppMain ->
                ( _lhsOpp,_lhsOppMain) }) }) }) })
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppMb                 : Maybe PP_Doc
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
type T_MbExpr  = ( (Maybe PP_Doc))
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (case (just_ ) of
     { ( _justIpp) ->
         (case (Just _justIpp) of
          { _lhsOppMb ->
          ( _lhsOppMb) }) })
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (case (Nothing) of
     { _lhsOppMb ->
     ( _lhsOppMb) })
-- NmExpr ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [(HsName,PP_Doc)]
   alternatives:
      alternative Tuple:
         child x1             : {HsName}
         child x2             : Expr 
-}
-- cata
sem_NmExpr :: NmExpr  ->
              T_NmExpr 
sem_NmExpr ( x1,x2)  =
    (sem_NmExpr_Tuple x1 (sem_Expr x2 ) )
-- semantic domain
type T_NmExpr  = ( ([(HsName,PP_Doc)]))
sem_NmExpr_Tuple :: HsName ->
                    T_Expr  ->
                    T_NmExpr 
sem_NmExpr_Tuple x1_ x2_  =
    (case (x2_ ) of
     { ( _x2Ipp) ->
         (case ([(x1_, _x2Ipp)]) of
          { _lhsOppL ->
          ( _lhsOppL) }) })
-- NmExprL -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [(HsName,PP_Doc)]
   alternatives:
      alternative Cons:
         child hd             : NmExpr 
         child tl             : NmExprL 
      alternative Nil:
-}
-- cata
sem_NmExprL :: NmExprL  ->
               T_NmExprL 
sem_NmExprL list  =
    (Prelude.foldr sem_NmExprL_Cons sem_NmExprL_Nil (Prelude.map sem_NmExpr list) )
-- semantic domain
type T_NmExprL  = ( ([(HsName,PP_Doc)]))
sem_NmExprL_Cons :: T_NmExpr  ->
                    T_NmExprL  ->
                    T_NmExprL 
sem_NmExprL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIppL) ->
         (case (hd_ ) of
          { ( _hdIppL) ->
              (case (_hdIppL ++ _tlIppL) of
               { _lhsOppL ->
               ( _lhsOppL) }) }) })
sem_NmExprL_Nil :: T_NmExprL 
sem_NmExprL_Nil  =
    (case ([]) of
     { _lhsOppL ->
     ( _lhsOppL) })
-- Stat --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Assign:
         child to             : Expr 
         child expr           : Expr 
      alternative Block:
         child stats          : StatL 
      alternative Break:
         visit 0:
            local pp          : _
      alternative Expr:
         child expr           : Expr 
      alternative FunDecl:
         child expr           : Expr 
      alternative Ret:
         child expr           : Expr 
         visit 0:
            local pp          : _
      alternative Switch:
         child expr           : Expr 
         child alts           : AltL 
         visit 0:
            local pp          : _
      alternative Throw:
         child expr           : Expr 
      alternative VarDecl:
         child nm             : {HsName}
         child mbExpr         : MbExpr 
-}
-- cata
sem_Stat :: Stat  ->
            T_Stat 
sem_Stat (Stat_Assign _to _expr )  =
    (sem_Stat_Assign (sem_Expr _to ) (sem_Expr _expr ) )
sem_Stat (Stat_Block _stats )  =
    (sem_Stat_Block (sem_StatL _stats ) )
sem_Stat (Stat_Break )  =
    (sem_Stat_Break )
sem_Stat (Stat_Expr _expr )  =
    (sem_Stat_Expr (sem_Expr _expr ) )
sem_Stat (Stat_FunDecl _expr )  =
    (sem_Stat_FunDecl (sem_Expr _expr ) )
sem_Stat (Stat_Ret _expr )  =
    (sem_Stat_Ret (sem_Expr _expr ) )
sem_Stat (Stat_Switch _expr _alts )  =
    (sem_Stat_Switch (sem_Expr _expr ) (sem_AltL _alts ) )
sem_Stat (Stat_Throw _expr )  =
    (sem_Stat_Throw (sem_Expr _expr ) )
sem_Stat (Stat_VarDecl _nm _mbExpr )  =
    (sem_Stat_VarDecl _nm (sem_MbExpr _mbExpr ) )
-- semantic domain
type T_Stat  = ( PP_Doc)
sem_Stat_Assign :: T_Expr  ->
                   T_Expr  ->
                   T_Stat 
sem_Stat_Assign to_ expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case (to_ ) of
          { ( _toIpp) ->
              (case (ppStat $ _toIpp >|< "=" >-< indent 1 _exprIpp) of
               { _lhsOpp ->
               ( _lhsOpp) }) }) })
sem_Stat_Block :: T_StatL  ->
                  T_Stat 
sem_Stat_Block stats_  =
    (case (stats_ ) of
     { ( _statsIppL) ->
         (case (ppCurlys $ vlist _statsIppL) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_Stat_Break :: T_Stat 
sem_Stat_Break  =
    (case (ppStat $ pp "break") of
     { _pp ->
     (case (_pp) of
      { _lhsOpp ->
      ( _lhsOpp) }) })
sem_Stat_Expr :: T_Expr  ->
                 T_Stat 
sem_Stat_Expr expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case (ppStat $ _exprIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_Stat_FunDecl :: T_Expr  ->
                    T_Stat 
sem_Stat_FunDecl expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case (_exprIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_Stat_Ret :: T_Expr  ->
                T_Stat 
sem_Stat_Ret expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case (ppStat $ "return" >#< _exprIpp) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }) })
sem_Stat_Switch :: T_Expr  ->
                   T_AltL  ->
                   T_Stat 
sem_Stat_Switch expr_ alts_  =
    (case (alts_ ) of
     { ( _altsIppL) ->
         (case (expr_ ) of
          { ( _exprIpp) ->
              (case ("switch" >|< ppParens _exprIpp >-< indent 1 (ppCurlys $ vlist _altsIppL)) of
               { _pp ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }) })
sem_Stat_Throw :: T_Expr  ->
                  T_Stat 
sem_Stat_Throw expr_  =
    (case (expr_ ) of
     { ( _exprIpp) ->
         (case (ppStat $ "throw" >#< _exprIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
sem_Stat_VarDecl :: HsName ->
                    T_MbExpr  ->
                    T_Stat 
sem_Stat_VarDecl nm_ mbExpr_  =
    (case (mbExpr_ ) of
     { ( _mbExprIppMb) ->
         (case (ppStat $
                case _mbExprIppMb of
                  Just e -> pp "var" >#< nm_ >|< "=" >-< indent 1 e
                  _      -> pp "var" >#< nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- StatL -------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Stat 
         child tl             : StatL 
      alternative Nil:
-}
-- cata
sem_StatL :: StatL  ->
             T_StatL 
sem_StatL list  =
    (Prelude.foldr sem_StatL_Cons sem_StatL_Nil (Prelude.map sem_Stat list) )
-- semantic domain
type T_StatL  = ( ([PP_Doc]))
sem_StatL_Cons :: T_Stat  ->
                  T_StatL  ->
                  T_StatL 
sem_StatL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIppL) ->
         (case (hd_ ) of
          { ( _hdIpp) ->
              (case (_hdIpp : _tlIppL) of
               { _lhsOppL ->
               ( _lhsOppL) }) }) })
sem_StatL_Nil :: T_StatL 
sem_StatL_Nil  =
    (case ([]) of
     { _lhsOppL ->
     ( _lhsOppL) })