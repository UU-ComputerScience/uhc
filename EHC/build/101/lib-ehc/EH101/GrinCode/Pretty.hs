

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode/Pretty.ag)
module EH101.GrinCode.Pretty(ppGrModule, ppGrExpr, ppGrExpr2, ppGrPatAlt, ppGrPatLam, ppGrTag) where

import qualified Data.Map as Map
import EH.Util.Pretty
import EH101.Base.Common
import EH101.GrinCode
import EH101.Base.CfgPP
import EH101.Foreign.Pretty









ppGrModule :: GrModule -> PP_Doc
ppGrModule cmod
  =  let  t = wrap_GrAGItf  (sem_GrAGItf (GrAGItf_AGItf cmod))
                            (Inh_GrAGItf )
     in   (pp_Syn_GrAGItf t)


ppGrTag :: GrTag -> PP_Doc
ppGrTag tg
  =  let  t = wrap_GrTag  (sem_GrTag tg)
                          (Inh_GrTag
                             { ppGrNm_Inh_GrTag = cfgppHsName CfgPP_Grin
                             })
     in   (pp_Syn_GrTag t)

ppGrExpr2 :: CfgPP x => x -> GrExpr -> PP_Doc
ppGrExpr2 x tg
  =  let  t = wrap_GrExpr  (sem_GrExpr tg)
                           (Inh_GrExpr
                             { ppGrNm_Inh_GrExpr = cfgppHsName x
                             })
     in   (pp_Syn_GrExpr t)

ppGrExpr :: GrExpr -> PP_Doc
ppGrExpr = ppGrExpr2 CfgPP_Grin

ppGrPatLam :: GrPatLam -> PP_Doc
ppGrPatLam tg
  =  let  t = wrap_GrPatLam  (sem_GrPatLam tg)
                             (Inh_GrPatLam
                                { ppGrNm_Inh_GrPatLam = cfgppHsName CfgPP_Grin
                                })
     in   (pp_Syn_GrPatLam t)

ppGrPatAlt :: GrPatAlt -> PP_Doc
ppGrPatAlt tg
  =  let  t = wrap_GrPatAlt  (sem_GrPatAlt tg)
                             (Inh_GrPatAlt
                                { ppGrNm_Inh_GrPatAlt = cfgppHsName CfgPP_Grin
                                })
     in   (pp_Syn_GrPatAlt t)




instance PP GrExpr where
  pp = ppGrExpr2 CfgPP_Plain



ppGrFFIAnnot :: GrFFIAnnot -> PP_Doc
ppGrFFIAnnot (GrFFIAnnot_IsResEval b) = pp b



type PPGrNm = HsName -> PP_Doc

ppCurlysSemisV :: [PP_Doc] -> PP_Doc
ppCurlysSemisV pL = ppBlock "{ " "} " "; " pL

-- list between {}, but with spaces, to avoid syntax clash with negative integers:
--  {- comment -}
--  {-1, -2}
ppCurlyList :: (a -> PP_Doc) -> [a] -> PP_Doc
ppCurlyList pL xs = ppListSep "{ " " }" ", " $ map pL xs

ppGrAltAnn :: GrAltAnn -> PP_Doc
ppGrAltAnn GrAltAnnNormal         = pp ""
ppGrAltAnn GrAltAnnIdent          = pp "ident "
ppGrAltAnn (GrAltAnnCalling a nm) = pp "calling" >#< show a >#< pp nm >#< ""
ppGrAltAnn GrAltAnnReenter        = pp "reenter "

ppGrBindAnn :: PPGrNm -> GrBindAnn -> PP_Doc
ppGrBindAnn _      GrBindAnnNormal            = pp ""
ppGrBindAnn ppGrNm (GrBindAnnClass xs)        = pp "DICTCLASS"       >|<  ppCurlyList ppTrack xs
ppGrBindAnn ppGrNm (GrBindAnnInstance xs)     = pp "DICTINSTANCE"    >|<  ppCurlyList ppTrack xs
ppGrBindAnn ppGrNm (GrBindAnnSpecialized nm i xs)  = pp "SPECIALIZED"  >#< (ppGrNm nm) >#< show i >#<  ppCurlyList (ppMbGrNm ppGrNm) xs

ppMbGrNm :: PPGrNm -> Maybe HsName -> PP_Doc
ppMbGrNm ppGrNm = maybe (pp "_") ppGrNm

ppManyGrNm :: PPGrNm -> [HsName] -> PP_Doc
ppManyGrNm ppGrNm = ppListSep "" "" " " . map ppGrNm

ppTrack :: Track -> PP_Doc
ppTrack t = text (show t)

ppInt :: Int -> PP_Doc
ppInt n = pp (show n)


-- GrAGItf -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child module         : GrModule 
-}
-- cata
sem_GrAGItf :: GrAGItf  ->
               T_GrAGItf 
sem_GrAGItf (GrAGItf_AGItf _module )  =
    (sem_GrAGItf_AGItf (sem_GrModule _module ) )
-- semantic domain
type T_GrAGItf  = ( PP_Doc)
data Inh_GrAGItf  = Inh_GrAGItf {}
data Syn_GrAGItf  = Syn_GrAGItf {pp_Syn_GrAGItf :: !(PP_Doc)}
wrap_GrAGItf :: T_GrAGItf  ->
                Inh_GrAGItf  ->
                Syn_GrAGItf 
wrap_GrAGItf sem (Inh_GrAGItf )  =
    (let ( _lhsOpp) = sem 
     in  (Syn_GrAGItf _lhsOpp ))
sem_GrAGItf_AGItf :: T_GrModule  ->
                     T_GrAGItf 
sem_GrAGItf_AGItf module_  =
    (case (module_ ) of
     { ( _moduleIpp) ->
         (case (_moduleIpp) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- GrAdapt -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Del:
         child off            : GrVal 
      alternative Ins:
         child off            : GrVal 
         child val            : GrVal 
      alternative Upd:
         child off            : GrVal 
         child val            : GrVal 
-}
-- cata
sem_GrAdapt :: GrAdapt  ->
               T_GrAdapt 
sem_GrAdapt (GrAdapt_Del _off )  =
    (sem_GrAdapt_Del (sem_GrVal _off ) )
sem_GrAdapt (GrAdapt_Ins _off _val )  =
    (sem_GrAdapt_Ins (sem_GrVal _off ) (sem_GrVal _val ) )
sem_GrAdapt (GrAdapt_Upd _off _val )  =
    (sem_GrAdapt_Upd (sem_GrVal _off ) (sem_GrVal _val ) )
-- semantic domain
type T_GrAdapt  = PPGrNm ->
                  ( PP_Doc)
sem_GrAdapt_Del :: T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Del off_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _offOppGrNm ->
          (case (off_ _offOppGrNm ) of
           { ( _offIpp) ->
               (case (_offIpp >|< "-=") of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrAdapt_Ins :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Ins off_ val_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _valOppGrNm ->
          (case (_lhsIppGrNm) of
           { _offOppGrNm ->
           (case (val_ _valOppGrNm ) of
            { ( _valIpp) ->
                (case (off_ _offOppGrNm ) of
                 { ( _offIpp) ->
                     (case (_offIpp >|< "+=" >|< _valIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrAdapt_Upd :: T_GrVal  ->
                   T_GrVal  ->
                   T_GrAdapt 
sem_GrAdapt_Upd off_ val_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _valOppGrNm ->
          (case (_lhsIppGrNm) of
           { _offOppGrNm ->
           (case (val_ _valOppGrNm ) of
            { ( _valIpp) ->
                (case (off_ _offOppGrNm ) of
                 { ( _offIpp) ->
                     (case (_offIpp >|< ":=" >|< _valIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
-- GrAdaptL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrAdapt 
         child tl             : GrAdaptL 
      alternative Nil:
-}
-- cata
sem_GrAdaptL :: GrAdaptL  ->
                T_GrAdaptL 
sem_GrAdaptL list  =
    (Prelude.foldr sem_GrAdaptL_Cons sem_GrAdaptL_Nil (Prelude.map sem_GrAdapt list) )
-- semantic domain
type T_GrAdaptL  = PPGrNm ->
                   ( PP_Doc,([PP_Doc]))
sem_GrAdaptL_Cons :: T_GrAdapt  ->
                     T_GrAdaptL  ->
                     T_GrAdaptL 
sem_GrAdaptL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrAdaptL_Nil :: T_GrAdaptL 
sem_GrAdaptL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- GrAlt -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Alt:
         child ann            : {GrAltAnn}
         child pat            : GrPatAlt 
         child expr           : GrExpr 
-}
-- cata
sem_GrAlt :: GrAlt  ->
             T_GrAlt 
sem_GrAlt (GrAlt_Alt _ann _pat _expr )  =
    (sem_GrAlt_Alt _ann (sem_GrPatAlt _pat ) (sem_GrExpr _expr ) )
-- semantic domain
type T_GrAlt  = PPGrNm ->
                ( PP_Doc)
sem_GrAlt_Alt :: GrAltAnn ->
                 T_GrPatAlt  ->
                 T_GrExpr  ->
                 T_GrAlt 
sem_GrAlt_Alt ann_ pat_ expr_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _exprOppGrNm ->
          (case (_lhsIppGrNm) of
           { _patOppGrNm ->
           (case (expr_ _exprOppGrNm ) of
            { ( _exprIpp) ->
                (case (pat_ _patOppGrNm ) of
                 { ( _patIpp) ->
                     (case (ppGrAltAnn ann_ >|< _patIpp >-< indent 2 ("->" >#< ppCurlysSemisV [_exprIpp])) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
-- GrAltL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrAlt 
         child tl             : GrAltL 
      alternative Nil:
-}
-- cata
sem_GrAltL :: GrAltL  ->
              T_GrAltL 
sem_GrAltL list  =
    (Prelude.foldr sem_GrAltL_Cons sem_GrAltL_Nil (Prelude.map sem_GrAlt list) )
-- semantic domain
type T_GrAltL  = PPGrNm ->
                 ( PP_Doc,([PP_Doc]))
sem_GrAltL_Cons :: T_GrAlt  ->
                   T_GrAltL  ->
                   T_GrAltL 
sem_GrAltL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrAltL_Nil :: T_GrAltL 
sem_GrAltL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- GrBind ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Arity:
         child nm             : {HsName}
         child arity          : {Int}
      alternative Bind:
         child nm             : {HsName}
         child annot          : {GrBindAnn}
         child argNmL         : {[HsName]}
         child expr           : GrExpr 
         visit 0:
            local ppGrNm      : _
      alternative Rec:
         child bindL          : GrBindL 
-}
-- cata
sem_GrBind :: GrBind  ->
              T_GrBind 
sem_GrBind (GrBind_Arity _nm _arity )  =
    (sem_GrBind_Arity _nm _arity )
sem_GrBind (GrBind_Bind _nm _annot _argNmL _expr )  =
    (sem_GrBind_Bind _nm _annot _argNmL (sem_GrExpr _expr ) )
sem_GrBind (GrBind_Rec _bindL )  =
    (sem_GrBind_Rec (sem_GrBindL _bindL ) )
-- semantic domain
type T_GrBind  = ( PP_Doc)
sem_GrBind_Arity :: HsName ->
                    Int ->
                    T_GrBind 
sem_GrBind_Arity nm_ arity_  =
    (case (cfgppHsName CfgPP_Grin nm_
             >#< ":" >#< show arity_) of
     { _lhsOpp ->
     ( _lhsOpp) })
sem_GrBind_Bind :: HsName ->
                   GrBindAnn ->
                   ([HsName]) ->
                   T_GrExpr  ->
                   T_GrBind 
sem_GrBind_Bind nm_ annot_ argNmL_ expr_  =
    (case (cfgppHsName CfgPP_Grin) of
     { _ppGrNm ->
     (case (_ppGrNm) of
      { _exprOppGrNm ->
      (case (expr_ _exprOppGrNm ) of
       { ( _exprIpp) ->
           (case (cfgppHsName CfgPP_Grin nm_
                    >#< ppGrBindAnn _ppGrNm     annot_
                    >#< ppSpaced (map (cfgppHsName CfgPP_Grin) argNmL_)
                    >-< indent 2 ("=" >#< ppCurlysSemisV [_exprIpp])) of
            { _lhsOpp ->
            ( _lhsOpp) }) }) }) })
sem_GrBind_Rec :: T_GrBindL  ->
                  T_GrBind 
sem_GrBind_Rec bindL_  =
    (case (bindL_ ) of
     { ( _bindLIpp,_bindLIppL) ->
         (case ("rec" >-< indent 2 (ppCurlysSemisV _bindLIppL)) of
          { _lhsOpp ->
          ( _lhsOpp) }) })
-- GrBindL -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrBind 
         child tl             : GrBindL 
      alternative Nil:
-}
-- cata
sem_GrBindL :: GrBindL  ->
               T_GrBindL 
sem_GrBindL list  =
    (Prelude.foldr sem_GrBindL_Cons sem_GrBindL_Nil (Prelude.map sem_GrBind list) )
-- semantic domain
type T_GrBindL  = ( PP_Doc,([PP_Doc]))
sem_GrBindL_Cons :: T_GrBind  ->
                    T_GrBindL  ->
                    T_GrBindL 
sem_GrBindL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIpp,_tlIppL) ->
         (case (hd_ ) of
          { ( _hdIpp) ->
              (case (_hdIpp >-< _tlIpp) of
               { _lhsOpp ->
               (case (_hdIpp : _tlIppL) of
                { _lhsOppL ->
                ( _lhsOpp,_lhsOppL) }) }) }) })
sem_GrBindL_Nil :: T_GrBindL 
sem_GrBindL_Nil  =
    (case (empty) of
     { _lhsOpp ->
     (case ([]) of
      { _lhsOppL ->
      ( _lhsOpp,_lhsOppL) }) })
-- GrExpr ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative App:
         child nm             : {HsName}
         child argL           : GrValL 
      alternative Call:
         child nm             : {HsName}
         child argL           : GrValL 
      alternative Case:
         child val            : GrVal 
         child altL           : GrAltL 
      alternative Catch:
         child body           : GrExpr 
         child arg            : {HsName}
         child handler        : GrExpr 
      alternative Eval:
         child nm             : {HsName}
      alternative FFI:
         child callconv       : {FFIWay}
         child impEnt         : {ForeignEnt}
         child ffiAnnot       : {GrFFIAnnot}
         child argL           : GrValL 
         visit 0:
            local ppannot     : _
            local ppent       : _
      alternative FetchField:
         child nm             : {HsName}
         child offset         : {Int}
         child mbTag          : {Maybe GrTag}
      alternative FetchNode:
         child nm             : {HsName}
      alternative FetchUpdate:
         child src            : {HsName}
         child dst            : {HsName}
      alternative Seq:
         child expr           : GrExpr 
         child pat            : GrPatLam 
         child body           : GrExpr 
      alternative Store:
         child val            : GrVal 
      alternative Throw:
         child nm             : {HsName}
      alternative Unit:
         child val            : GrVal 
         child type           : GrType 
      alternative UpdateUnit:
         child nm             : {HsName}
         child val            : GrVal 
-}
-- cata
sem_GrExpr :: GrExpr  ->
              T_GrExpr 
sem_GrExpr (GrExpr_App _nm _argL )  =
    (sem_GrExpr_App _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Call _nm _argL )  =
    (sem_GrExpr_Call _nm (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_Case _val _altL )  =
    (sem_GrExpr_Case (sem_GrVal _val ) (sem_GrAltL _altL ) )
sem_GrExpr (GrExpr_Catch _body _arg _handler )  =
    (sem_GrExpr_Catch (sem_GrExpr _body ) _arg (sem_GrExpr _handler ) )
sem_GrExpr (GrExpr_Eval _nm )  =
    (sem_GrExpr_Eval _nm )
sem_GrExpr (GrExpr_FFI _callconv _impEnt _ffiAnnot _argL )  =
    (sem_GrExpr_FFI _callconv _impEnt _ffiAnnot (sem_GrValL _argL ) )
sem_GrExpr (GrExpr_FetchField _nm _offset _mbTag )  =
    (sem_GrExpr_FetchField _nm _offset _mbTag )
sem_GrExpr (GrExpr_FetchNode _nm )  =
    (sem_GrExpr_FetchNode _nm )
sem_GrExpr (GrExpr_FetchUpdate _src _dst )  =
    (sem_GrExpr_FetchUpdate _src _dst )
sem_GrExpr (GrExpr_Seq _expr _pat _body )  =
    (sem_GrExpr_Seq (sem_GrExpr _expr ) (sem_GrPatLam _pat ) (sem_GrExpr _body ) )
sem_GrExpr (GrExpr_Store _val )  =
    (sem_GrExpr_Store (sem_GrVal _val ) )
sem_GrExpr (GrExpr_Throw _nm )  =
    (sem_GrExpr_Throw _nm )
sem_GrExpr (GrExpr_Unit _val _type )  =
    (sem_GrExpr_Unit (sem_GrVal _val ) (sem_GrType _type ) )
sem_GrExpr (GrExpr_UpdateUnit _nm _val )  =
    (sem_GrExpr_UpdateUnit _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrExpr  = PPGrNm ->
                 ( PP_Doc)
data Inh_GrExpr  = Inh_GrExpr {ppGrNm_Inh_GrExpr :: !(PPGrNm)}
data Syn_GrExpr  = Syn_GrExpr {pp_Syn_GrExpr :: !(PP_Doc)}
wrap_GrExpr :: T_GrExpr  ->
               Inh_GrExpr  ->
               Syn_GrExpr 
wrap_GrExpr sem (Inh_GrExpr _lhsIppGrNm )  =
    (let ( _lhsOpp) = sem _lhsIppGrNm 
     in  (Syn_GrExpr _lhsOpp ))
sem_GrExpr_App :: HsName ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_App nm_ argL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _argLOppGrNm ->
          (case (argL_ _argLOppGrNm ) of
           { ( _argLIpp,_argLIppL) ->
               (case ("apply" >#< ppSpaced (_lhsIppGrNm nm_ : _argLIppL)) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrExpr_Call :: HsName ->
                   T_GrValL  ->
                   T_GrExpr 
sem_GrExpr_Call nm_ argL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _argLOppGrNm ->
          (case (argL_ _argLOppGrNm ) of
           { ( _argLIpp,_argLIppL) ->
               (case ("call"  >#< ppSpaced (_lhsIppGrNm nm_ : _argLIppL)) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrExpr_Case :: T_GrVal  ->
                   T_GrAltL  ->
                   T_GrExpr 
sem_GrExpr_Case val_ altL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _altLOppGrNm ->
          (case (_lhsIppGrNm) of
           { _valOppGrNm ->
           (case (altL_ _altLOppGrNm ) of
            { ( _altLIpp,_altLIppL) ->
                (case (val_ _valOppGrNm ) of
                 { ( _valIpp) ->
                     (case ("case" >#< _valIpp >#< "of" >-<
                                indent 2 (ppCurlysSemisV _altLIppL)) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrExpr_Catch :: T_GrExpr  ->
                    HsName ->
                    T_GrExpr  ->
                    T_GrExpr 
sem_GrExpr_Catch body_ arg_ handler_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _handlerOppGrNm ->
          (case (_lhsIppGrNm) of
           { _bodyOppGrNm ->
           (case (handler_ _handlerOppGrNm ) of
            { ( _handlerIpp) ->
                (case (body_ _bodyOppGrNm ) of
                 { ( _bodyIpp) ->
                     (case ("try" >-<
                            indent 2 (ppCurlysSemisV [_bodyIpp]) >-<
                            "catch" >|< ppParens (_lhsIppGrNm arg_) >-<
                            indent 2 (ppCurlysSemisV [_handlerIpp])) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrExpr_Eval :: HsName ->
                   T_GrExpr 
sem_GrExpr_Eval nm_  =
    (\ _lhsIppGrNm ->
         (case ("eval" >#< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrExpr_FFI :: FFIWay ->
                  ForeignEnt ->
                  GrFFIAnnot ->
                  T_GrValL  ->
                  T_GrExpr 
sem_GrExpr_FFI callconv_ impEnt_ ffiAnnot_ argL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _argLOppGrNm ->
          (case (ppGrFFIAnnot ffiAnnot_) of
           { _ppannot ->
           (case (callconv_ >#< "\"" >|< impEnt_ >|< "\"") of
            { _ppent ->
            (case (argL_ _argLOppGrNm ) of
             { ( _argLIpp,_argLIppL) ->
                 (case ("ffi" >#< ppSpaced (_ppent : _ppannot : _argLIppL)) of
                  { _lhsOpp ->
                  ( _lhsOpp) }) }) }) }) }))
sem_GrExpr_FetchField :: HsName ->
                         Int ->
                         (Maybe GrTag) ->
                         T_GrExpr 
sem_GrExpr_FetchField nm_ offset_ mbTag_  =
    (\ _lhsIppGrNm ->
         (case ("fetchfield" >#<  _lhsIppGrNm nm_ >#< pp offset_ >#< maybe empty ppGrTag mbTag_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrExpr_FetchNode :: HsName ->
                        T_GrExpr 
sem_GrExpr_FetchNode nm_  =
    (\ _lhsIppGrNm ->
         (case ("fetchnode"  >#<  _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrExpr_FetchUpdate :: HsName ->
                          HsName ->
                          T_GrExpr 
sem_GrExpr_FetchUpdate src_ dst_  =
    (\ _lhsIppGrNm ->
         (case ("fetchupdate" >#< _lhsIppGrNm src_ >#< _lhsIppGrNm dst_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrExpr_Seq :: T_GrExpr  ->
                  T_GrPatLam  ->
                  T_GrExpr  ->
                  T_GrExpr 
sem_GrExpr_Seq expr_ pat_ body_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _bodyOppGrNm ->
          (case (_lhsIppGrNm) of
           { _patOppGrNm ->
           (case (_lhsIppGrNm) of
            { _exprOppGrNm ->
            (case (body_ _bodyOppGrNm ) of
             { ( _bodyIpp) ->
                 (case (pat_ _patOppGrNm ) of
                  { ( _patIpp) ->
                      (case (expr_ _exprOppGrNm ) of
                       { ( _exprIpp) ->
                           (case (_exprIpp >#< ";" >#< "\\" >|< _patIpp >#< "->"
                                  >-< _bodyIpp) of
                            { _lhsOpp ->
                            ( _lhsOpp) }) }) }) }) }) }) }))
sem_GrExpr_Store :: T_GrVal  ->
                    T_GrExpr 
sem_GrExpr_Store val_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _valOppGrNm ->
          (case (val_ _valOppGrNm ) of
           { ( _valIpp) ->
               (case ("store" >#< _valIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrExpr_Throw :: HsName ->
                    T_GrExpr 
sem_GrExpr_Throw nm_  =
    (\ _lhsIppGrNm ->
         (case ("throw" >#< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrExpr_Unit :: T_GrVal  ->
                   T_GrType  ->
                   T_GrExpr 
sem_GrExpr_Unit val_ type_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _valOppGrNm ->
          (case (val_ _valOppGrNm ) of
           { ( _valIpp) ->
               (case ("unit" >#< _valIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrExpr_UpdateUnit :: HsName ->
                         T_GrVal  ->
                         T_GrExpr 
sem_GrExpr_UpdateUnit nm_ val_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _valOppGrNm ->
          (case (val_ _valOppGrNm ) of
           { ( _valIpp) ->
               (case ("updateunit" >#< _valIpp >#< _lhsIppGrNm nm_) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- GrFFIAnnot --------------------------------------------------
{-
   alternatives:
      alternative IsResEval:
         child isEvaluated    : {Bool}
-}
-- cata
sem_GrFFIAnnot :: GrFFIAnnot  ->
                  T_GrFFIAnnot 
sem_GrFFIAnnot (GrFFIAnnot_IsResEval _isEvaluated )  =
    (sem_GrFFIAnnot_IsResEval _isEvaluated )
-- semantic domain
type T_GrFFIAnnot  = ( )
sem_GrFFIAnnot_IsResEval :: Bool ->
                            T_GrFFIAnnot 
sem_GrFFIAnnot_IsResEval isEvaluated_  =
    ( )
-- GrGlobal ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Global:
         child nm             : {HsName}
         child val            : GrVal 
         visit 0:
            local ppGrNm      : _
-}
-- cata
sem_GrGlobal :: GrGlobal  ->
                T_GrGlobal 
sem_GrGlobal (GrGlobal_Global _nm _val )  =
    (sem_GrGlobal_Global _nm (sem_GrVal _val ) )
-- semantic domain
type T_GrGlobal  = ( PP_Doc)
sem_GrGlobal_Global :: HsName ->
                       T_GrVal  ->
                       T_GrGlobal 
sem_GrGlobal_Global nm_ val_  =
    (case (cfgppHsName CfgPP_Grin) of
     { _ppGrNm ->
     (case (_ppGrNm) of
      { _valOppGrNm ->
      (case (val_ _valOppGrNm ) of
       { ( _valIpp) ->
           (case (cfgppHsName CfgPP_Grin nm_ >#< "<-" >#< "store" >#< _valIpp) of
            { _lhsOpp ->
            ( _lhsOpp) }) }) }) })
-- GrGlobalL ---------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrGlobal 
         child tl             : GrGlobalL 
      alternative Nil:
-}
-- cata
sem_GrGlobalL :: GrGlobalL  ->
                 T_GrGlobalL 
sem_GrGlobalL list  =
    (Prelude.foldr sem_GrGlobalL_Cons sem_GrGlobalL_Nil (Prelude.map sem_GrGlobal list) )
-- semantic domain
type T_GrGlobalL  = ( PP_Doc,([PP_Doc]))
sem_GrGlobalL_Cons :: T_GrGlobal  ->
                      T_GrGlobalL  ->
                      T_GrGlobalL 
sem_GrGlobalL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIpp,_tlIppL) ->
         (case (hd_ ) of
          { ( _hdIpp) ->
              (case (_hdIpp >-< _tlIpp) of
               { _lhsOpp ->
               (case (_hdIpp : _tlIppL) of
                { _lhsOppL ->
                ( _lhsOpp,_lhsOppL) }) }) }) })
sem_GrGlobalL_Nil :: T_GrGlobalL 
sem_GrGlobalL_Nil  =
    (case (empty) of
     { _lhsOpp ->
     (case ([]) of
      { _lhsOppL ->
      ( _lhsOpp,_lhsOppL) }) })
-- GrModule ----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child globalL        : GrGlobalL 
         child bindL          : GrBindL 
         child tagsMp         : {Map.Map HsName [GrTag]}
-}
-- cata
sem_GrModule :: GrModule  ->
                T_GrModule 
sem_GrModule (GrModule_Mod _moduleNm _globalL _bindL _tagsMp )  =
    (sem_GrModule_Mod _moduleNm (sem_GrGlobalL _globalL ) (sem_GrBindL _bindL ) _tagsMp )
-- semantic domain
type T_GrModule  = ( PP_Doc)
sem_GrModule_Mod :: HsName ->
                    T_GrGlobalL  ->
                    T_GrBindL  ->
                    (Map.Map HsName [GrTag]) ->
                    T_GrModule 
sem_GrModule_Mod moduleNm_ globalL_ bindL_ tagsMp_  =
    (case (bindL_ ) of
     { ( _bindLIpp,_bindLIppL) ->
         (case (globalL_ ) of
          { ( _globalLIpp,_globalLIppL) ->
              (case ("module" >#< cfgppHsName CfgPP_Grin moduleNm_
                     >-< ppCurlysSemisV _globalLIppL
                     >-< ppCurlysSemisV _bindLIppL
                     >-< ppCurlysSemisV
                              (map  (\(tn,ts)
                                         ->  cfgppHsName CfgPP_Grin tn >#< "="
                                             >#< ( ppListSep "" "" " | "
                                                 . map (\(GrTag_Con ann n nm) ->  "#" >|< show n >|< "/" >|< "C" >|< show ann >|< "/" >|< cfgppHsName CfgPP_Grin nm)
                                                 $ ts
                                                 )
                                    )
                                    (Map.assocs tagsMp_))) of
               { _lhsOpp ->
               ( _lhsOpp) }) }) })
-- GrPatAlt ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative LitInt:
         child int            : {Int}
      alternative Node:
         child tag            : GrTag 
         child fldL           : {[HsName]}
      alternative NodeSplit:
         child tag            : GrTag 
         child nm             : {HsName}
         child fldL           : GrSplitL 
      alternative Otherwise:
      alternative Tag:
         child tag            : GrTag 
-}
-- cata
sem_GrPatAlt :: GrPatAlt  ->
                T_GrPatAlt 
sem_GrPatAlt (GrPatAlt_LitInt _int )  =
    (sem_GrPatAlt_LitInt _int )
sem_GrPatAlt (GrPatAlt_Node _tag _fldL )  =
    (sem_GrPatAlt_Node (sem_GrTag _tag ) _fldL )
sem_GrPatAlt (GrPatAlt_NodeSplit _tag _nm _fldL )  =
    (sem_GrPatAlt_NodeSplit (sem_GrTag _tag ) _nm (sem_GrSplitL _fldL ) )
sem_GrPatAlt (GrPatAlt_Otherwise )  =
    (sem_GrPatAlt_Otherwise )
sem_GrPatAlt (GrPatAlt_Tag _tag )  =
    (sem_GrPatAlt_Tag (sem_GrTag _tag ) )
-- semantic domain
type T_GrPatAlt  = PPGrNm ->
                   ( PP_Doc)
data Inh_GrPatAlt  = Inh_GrPatAlt {ppGrNm_Inh_GrPatAlt :: !(PPGrNm)}
data Syn_GrPatAlt  = Syn_GrPatAlt {pp_Syn_GrPatAlt :: !(PP_Doc)}
wrap_GrPatAlt :: T_GrPatAlt  ->
                 Inh_GrPatAlt  ->
                 Syn_GrPatAlt 
wrap_GrPatAlt sem (Inh_GrPatAlt _lhsIppGrNm )  =
    (let ( _lhsOpp) = sem _lhsIppGrNm 
     in  (Syn_GrPatAlt _lhsOpp ))
sem_GrPatAlt_LitInt :: Int ->
                       T_GrPatAlt 
sem_GrPatAlt_LitInt int_  =
    (\ _lhsIppGrNm ->
         (case (pp int_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatAlt_Node :: T_GrTag  ->
                     ([HsName]) ->
                     T_GrPatAlt 
sem_GrPatAlt_Node tag_ fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tagOppGrNm ->
          (case (tag_ _tagOppGrNm ) of
           { ( _tagIpp) ->
               (case (ppListSep "(" ")" " " (_tagIpp : map _lhsIppGrNm fldL_)) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrPatAlt_NodeSplit :: T_GrTag  ->
                          HsName ->
                          T_GrSplitL  ->
                          T_GrPatAlt 
sem_GrPatAlt_NodeSplit tag_ nm_ fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _fldLOppGrNm ->
          (case (_lhsIppGrNm) of
           { _tagOppGrNm ->
           (case (fldL_ _fldLOppGrNm ) of
            { ( _fldLIpp,_fldLIppL) ->
                (case (tag_ _tagOppGrNm ) of
                 { ( _tagIpp) ->
                     (case (ppParens (_tagIpp >#< _lhsIppGrNm nm_ >|< "|" >|< (ppListSep "" "" "," _fldLIppL))) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrPatAlt_Otherwise :: T_GrPatAlt 
sem_GrPatAlt_Otherwise  =
    (\ _lhsIppGrNm ->
         (case (pp "_") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatAlt_Tag :: T_GrTag  ->
                    T_GrPatAlt 
sem_GrPatAlt_Tag tag_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tagOppGrNm ->
          (case (tag_ _tagOppGrNm ) of
           { ( _tagIpp) ->
               (case (_tagIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- GrPatLam ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative BasicAnnot:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative BasicNode:
         child annot          : {BasicAnnot}
         child nm             : {HsName}
      alternative Empty:
      alternative EnumAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative EnumNode:
         child nm             : {HsName}
      alternative OpaqueAnnot:
         child nm             : {HsName}
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrAnnot:
         child tycon          : {HsName}
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrVarL 
-}
-- cata
sem_GrPatLam :: GrPatLam  ->
                T_GrPatLam 
sem_GrPatLam (GrPatLam_BasicAnnot _annot _nm )  =
    (sem_GrPatLam_BasicAnnot _annot _nm )
sem_GrPatLam (GrPatLam_BasicNode _annot _nm )  =
    (sem_GrPatLam_BasicNode _annot _nm )
sem_GrPatLam (GrPatLam_Empty )  =
    (sem_GrPatLam_Empty )
sem_GrPatLam (GrPatLam_EnumAnnot _tycon _nm )  =
    (sem_GrPatLam_EnumAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_EnumNode _nm )  =
    (sem_GrPatLam_EnumNode _nm )
sem_GrPatLam (GrPatLam_OpaqueAnnot _nm )  =
    (sem_GrPatLam_OpaqueAnnot _nm )
sem_GrPatLam (GrPatLam_OpaqueNode _nm )  =
    (sem_GrPatLam_OpaqueNode _nm )
sem_GrPatLam (GrPatLam_PtrAnnot _tycon _nm )  =
    (sem_GrPatLam_PtrAnnot _tycon _nm )
sem_GrPatLam (GrPatLam_PtrNode _nm )  =
    (sem_GrPatLam_PtrNode _nm )
sem_GrPatLam (GrPatLam_Var _nm )  =
    (sem_GrPatLam_Var _nm )
sem_GrPatLam (GrPatLam_VarNode _fldL )  =
    (sem_GrPatLam_VarNode (sem_GrVarL _fldL ) )
-- semantic domain
type T_GrPatLam  = PPGrNm ->
                   ( PP_Doc)
data Inh_GrPatLam  = Inh_GrPatLam {ppGrNm_Inh_GrPatLam :: !(PPGrNm)}
data Syn_GrPatLam  = Syn_GrPatLam {pp_Syn_GrPatLam :: !(PP_Doc)}
wrap_GrPatLam :: T_GrPatLam  ->
                 Inh_GrPatLam  ->
                 Syn_GrPatLam 
wrap_GrPatLam sem (Inh_GrPatLam _lhsIppGrNm )  =
    (let ( _lhsOpp) = sem _lhsIppGrNm 
     in  (Syn_GrPatLam _lhsOpp ))
sem_GrPatLam_BasicAnnot :: BasicAnnot ->
                           HsName ->
                           T_GrPatLam 
sem_GrPatLam_BasicAnnot annot_ nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "basicannot", pp annot_, _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_BasicNode :: BasicAnnot ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_BasicNode annot_ nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "basicnode", pp annot_, _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_Empty :: T_GrPatLam 
sem_GrPatLam_Empty  =
    (\ _lhsIppGrNm ->
         (case (pp "()") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_EnumAnnot :: HsName ->
                          HsName ->
                          T_GrPatLam 
sem_GrPatLam_EnumAnnot tycon_ nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "enumannot" , _lhsIppGrNm tycon_, _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_EnumNode :: HsName ->
                         T_GrPatLam 
sem_GrPatLam_EnumNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "enumnode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_OpaqueAnnot :: HsName ->
                            T_GrPatLam 
sem_GrPatLam_OpaqueAnnot nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "opaqueannot" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_OpaqueNode :: HsName ->
                           T_GrPatLam 
sem_GrPatLam_OpaqueNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "opaquenode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_PtrAnnot :: HsName ->
                         HsName ->
                         T_GrPatLam 
sem_GrPatLam_PtrAnnot tycon_ nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "ptrannot" , _lhsIppGrNm tycon_, _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_PtrNode :: HsName ->
                        T_GrPatLam 
sem_GrPatLam_PtrNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "ptrnode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_Var :: HsName ->
                    T_GrPatLam 
sem_GrPatLam_Var nm_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrPatLam_VarNode :: T_GrVarL  ->
                        T_GrPatLam 
sem_GrPatLam_VarNode fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _fldLOppGrNm ->
          (case (fldL_ _fldLOppGrNm ) of
           { ( _fldLIpp,_fldLIppL) ->
               (case (ppListSep "(" ")" " " _fldLIppL) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- GrSplit -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Sel:
         child nm             : {HsName}
         child off            : GrVal 
-}
-- cata
sem_GrSplit :: GrSplit  ->
               T_GrSplit 
sem_GrSplit (GrSplit_Sel _nm _off )  =
    (sem_GrSplit_Sel _nm (sem_GrVal _off ) )
-- semantic domain
type T_GrSplit  = PPGrNm ->
                  ( PP_Doc)
sem_GrSplit_Sel :: HsName ->
                   T_GrVal  ->
                   T_GrSplit 
sem_GrSplit_Sel nm_ off_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _offOppGrNm ->
          (case (off_ _offOppGrNm ) of
           { ( _offIpp) ->
               (case (_lhsIppGrNm nm_ >|< "=" >|< _offIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- GrSplitL ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrSplit 
         child tl             : GrSplitL 
      alternative Nil:
-}
-- cata
sem_GrSplitL :: GrSplitL  ->
                T_GrSplitL 
sem_GrSplitL list  =
    (Prelude.foldr sem_GrSplitL_Cons sem_GrSplitL_Nil (Prelude.map sem_GrSplit list) )
-- semantic domain
type T_GrSplitL  = PPGrNm ->
                   ( PP_Doc,([PP_Doc]))
sem_GrSplitL_Cons :: T_GrSplit  ->
                     T_GrSplitL  ->
                     T_GrSplitL 
sem_GrSplitL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrSplitL_Nil :: T_GrSplitL 
sem_GrSplitL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- GrTag -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative App:
         child nm             : {HsName}
      alternative Con:
         child grtgAnn        : {GrTagAnn}
         child int            : {Int}
         child nm             : {HsName}
      alternative Fun:
         child nm             : {HsName}
      alternative Hole:
      alternative PApp:
         child needs          : {Int}
         child nm             : {HsName}
      alternative Rec:
      alternative Unboxed:
-}
-- cata
sem_GrTag :: GrTag  ->
             T_GrTag 
sem_GrTag (GrTag_App _nm )  =
    (sem_GrTag_App _nm )
sem_GrTag (GrTag_Con _grtgAnn _int _nm )  =
    (sem_GrTag_Con _grtgAnn _int _nm )
sem_GrTag (GrTag_Fun _nm )  =
    (sem_GrTag_Fun _nm )
sem_GrTag (GrTag_Hole )  =
    (sem_GrTag_Hole )
sem_GrTag (GrTag_PApp _needs _nm )  =
    (sem_GrTag_PApp _needs _nm )
sem_GrTag (GrTag_Rec )  =
    (sem_GrTag_Rec )
sem_GrTag (GrTag_Unboxed )  =
    (sem_GrTag_Unboxed )
-- semantic domain
type T_GrTag  = PPGrNm ->
                ( PP_Doc)
data Inh_GrTag  = Inh_GrTag {ppGrNm_Inh_GrTag :: !(PPGrNm)}
data Syn_GrTag  = Syn_GrTag {pp_Syn_GrTag :: !(PP_Doc)}
wrap_GrTag :: T_GrTag  ->
              Inh_GrTag  ->
              Syn_GrTag 
wrap_GrTag sem (Inh_GrTag _lhsIppGrNm )  =
    (let ( _lhsOpp) = sem _lhsIppGrNm 
     in  (Syn_GrTag _lhsOpp ))
sem_GrTag_App :: HsName ->
                 T_GrTag 
sem_GrTag_App nm_  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< "0" >|< "/" >|< "A" >|< "/" >|< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_Con :: GrTagAnn ->
                 Int ->
                 HsName ->
                 T_GrTag 
sem_GrTag_Con grtgAnn_ int_ nm_  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< int_ >|< "/" >|< "C" >|< show grtgAnn_ >|< "/" >|< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_Fun :: HsName ->
                 T_GrTag 
sem_GrTag_Fun nm_  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< "0"  >|< "/" >|< "F" >|< "/" >|< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_Hole :: T_GrTag 
sem_GrTag_Hole  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< "0" >|< "/" >|< "H" >|< "/" >|< "_") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_PApp :: Int ->
                  HsName ->
                  T_GrTag 
sem_GrTag_PApp needs_ nm_  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< "0"  >|< "/" >|< "P" >|< "/" >|< needs_ >|< "/" >|< _lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_Rec :: T_GrTag 
sem_GrTag_Rec  =
    (\ _lhsIppGrNm ->
         (case ("#" >|< "0" >|< "/" >|< "R" >|< "/" >|< _lhsIppGrNm (mkHNm "()")) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTag_Unboxed :: T_GrTag 
sem_GrTag_Unboxed  =
    (\ _lhsIppGrNm ->
         (case (pp "#U") of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- GrTagL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrTag 
         child tl             : GrTagL 
      alternative Nil:
-}
-- cata
sem_GrTagL :: GrTagL  ->
              T_GrTagL 
sem_GrTagL list  =
    (Prelude.foldr sem_GrTagL_Cons sem_GrTagL_Nil (Prelude.map sem_GrTag list) )
-- semantic domain
type T_GrTagL  = PPGrNm ->
                 ( PP_Doc,([PP_Doc]))
sem_GrTagL_Cons :: T_GrTag  ->
                   T_GrTagL  ->
                   T_GrTagL 
sem_GrTagL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrTagL_Nil :: T_GrTagL 
sem_GrTagL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- GrType ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Arrow:
         child args           : GrTypeBaseL 
         child res            : GrTypeBase 
      alternative None:
-}
-- cata
sem_GrType :: GrType  ->
              T_GrType 
sem_GrType (GrType_Arrow _args _res )  =
    (sem_GrType_Arrow (sem_GrTypeBaseL _args ) (sem_GrTypeBase _res ) )
sem_GrType (GrType_None )  =
    (sem_GrType_None )
-- semantic domain
type T_GrType  = PPGrNm ->
                 ( PP_Doc)
sem_GrType_Arrow :: T_GrTypeBaseL  ->
                    T_GrTypeBase  ->
                    T_GrType 
sem_GrType_Arrow args_ res_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _resOppGrNm ->
          (case (res_ _resOppGrNm ) of
           { ( _resIpp) ->
               (case (_lhsIppGrNm) of
                { _argsOppGrNm ->
                (case (args_ _argsOppGrNm ) of
                 { ( _argsIpp) ->
                     (case (_argsIpp >-< _resIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrType_None :: T_GrType 
sem_GrType_None  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- GrTypeBase --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Node:
      alternative Pointer:
-}
-- cata
sem_GrTypeBase :: GrTypeBase  ->
                  T_GrTypeBase 
sem_GrTypeBase (GrTypeBase_Node )  =
    (sem_GrTypeBase_Node )
sem_GrTypeBase (GrTypeBase_Pointer )  =
    (sem_GrTypeBase_Pointer )
-- semantic domain
type T_GrTypeBase  = PPGrNm ->
                     ( PP_Doc)
sem_GrTypeBase_Node :: T_GrTypeBase 
sem_GrTypeBase_Node  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrTypeBase_Pointer :: T_GrTypeBase 
sem_GrTypeBase_Pointer  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- GrTypeBaseL -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : GrTypeBase 
         child tl             : GrTypeBaseL 
      alternative Nil:
-}
-- cata
sem_GrTypeBaseL :: GrTypeBaseL  ->
                   T_GrTypeBaseL 
sem_GrTypeBaseL list  =
    (Prelude.foldr sem_GrTypeBaseL_Cons sem_GrTypeBaseL_Nil (Prelude.map sem_GrTypeBase list) )
-- semantic domain
type T_GrTypeBaseL  = PPGrNm ->
                      ( PP_Doc)
sem_GrTypeBaseL_Cons :: T_GrTypeBase  ->
                        T_GrTypeBaseL  ->
                        T_GrTypeBaseL 
sem_GrTypeBaseL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (tl_ _tlOppGrNm ) of
           { ( _tlIpp) ->
               (case (_lhsIppGrNm) of
                { _hdOppGrNm ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrTypeBaseL_Nil :: T_GrTypeBaseL 
sem_GrTypeBaseL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- GrVal -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative BasicNode:
         child tag            : GrTag 
         child nm             : {HsName}
      alternative Empty:
      alternative EnumNode:
         child nm             : {HsName}
      alternative LitInt:
         child int            : {Int}
      alternative LitStr:
         child str            : {String}
      alternative Node:
         child tag            : GrTag 
         child fldL           : GrValL 
      alternative NodeAdapt:
         child nm             : {HsName}
         child fldL           : GrAdaptL 
      alternative OpaqueNode:
         child nm             : {HsName}
      alternative PtrNode:
         child nm             : {HsName}
      alternative Tag:
         child tag            : GrTag 
      alternative Var:
         child nm             : {HsName}
      alternative VarNode:
         child fldL           : GrValL 
-}
-- cata
sem_GrVal :: GrVal  ->
             T_GrVal 
sem_GrVal (GrVal_BasicNode _tag _nm )  =
    (sem_GrVal_BasicNode (sem_GrTag _tag ) _nm )
sem_GrVal (GrVal_Empty )  =
    (sem_GrVal_Empty )
sem_GrVal (GrVal_EnumNode _nm )  =
    (sem_GrVal_EnumNode _nm )
sem_GrVal (GrVal_LitInt _int )  =
    (sem_GrVal_LitInt _int )
sem_GrVal (GrVal_LitStr _str )  =
    (sem_GrVal_LitStr _str )
sem_GrVal (GrVal_Node _tag _fldL )  =
    (sem_GrVal_Node (sem_GrTag _tag ) (sem_GrValL _fldL ) )
sem_GrVal (GrVal_NodeAdapt _nm _fldL )  =
    (sem_GrVal_NodeAdapt _nm (sem_GrAdaptL _fldL ) )
sem_GrVal (GrVal_OpaqueNode _nm )  =
    (sem_GrVal_OpaqueNode _nm )
sem_GrVal (GrVal_PtrNode _nm )  =
    (sem_GrVal_PtrNode _nm )
sem_GrVal (GrVal_Tag _tag )  =
    (sem_GrVal_Tag (sem_GrTag _tag ) )
sem_GrVal (GrVal_Var _nm )  =
    (sem_GrVal_Var _nm )
sem_GrVal (GrVal_VarNode _fldL )  =
    (sem_GrVal_VarNode (sem_GrValL _fldL ) )
-- semantic domain
type T_GrVal  = PPGrNm ->
                ( PP_Doc)
sem_GrVal_BasicNode :: T_GrTag  ->
                       HsName ->
                       T_GrVal 
sem_GrVal_BasicNode tag_ nm_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tagOppGrNm ->
          (case (tag_ _tagOppGrNm ) of
           { ( _tagIpp) ->
               (case (ppListSep "(" ")" " " [pp "basicnode", _tagIpp, _lhsIppGrNm nm_]) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrVal_Empty :: T_GrVal 
sem_GrVal_Empty  =
    (\ _lhsIppGrNm ->
         (case (pp "()") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_EnumNode :: HsName ->
                      T_GrVal 
sem_GrVal_EnumNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "enumnode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_LitInt :: Int ->
                    T_GrVal 
sem_GrVal_LitInt int_  =
    (\ _lhsIppGrNm ->
         (case (pp int_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_LitStr :: String ->
                    T_GrVal 
sem_GrVal_LitStr str_  =
    (\ _lhsIppGrNm ->
         (case (pp $ show str_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_Node :: T_GrTag  ->
                  T_GrValL  ->
                  T_GrVal 
sem_GrVal_Node tag_ fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _fldLOppGrNm ->
          (case (_lhsIppGrNm) of
           { _tagOppGrNm ->
           (case (fldL_ _fldLOppGrNm ) of
            { ( _fldLIpp,_fldLIppL) ->
                (case (tag_ _tagOppGrNm ) of
                 { ( _tagIpp) ->
                     (case (ppListSep "(" ")" " " (_tagIpp        : _fldLIppL)) of
                      { _lhsOpp ->
                      ( _lhsOpp) }) }) }) }) }))
sem_GrVal_NodeAdapt :: HsName ->
                       T_GrAdaptL  ->
                       T_GrVal 
sem_GrVal_NodeAdapt nm_ fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _fldLOppGrNm ->
          (case (fldL_ _fldLOppGrNm ) of
           { ( _fldLIpp,_fldLIppL) ->
               (case (ppParens (_lhsIppGrNm nm_ >|< "|" >|< (ppListSep "" "" "," _fldLIppL))) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrVal_OpaqueNode :: HsName ->
                        T_GrVal 
sem_GrVal_OpaqueNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "opaquenode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_PtrNode :: HsName ->
                     T_GrVal 
sem_GrVal_PtrNode nm_  =
    (\ _lhsIppGrNm ->
         (case (ppListSep "(" ")" " " [pp "ptrnode" , _lhsIppGrNm nm_]) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_Tag :: T_GrTag  ->
                 T_GrVal 
sem_GrVal_Tag tag_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tagOppGrNm ->
          (case (tag_ _tagOppGrNm ) of
           { ( _tagIpp) ->
               (case (_tagIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrVal_Var :: HsName ->
                 T_GrVal 
sem_GrVal_Var nm_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVal_VarNode :: T_GrValL  ->
                     T_GrVal 
sem_GrVal_VarNode fldL_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _fldLOppGrNm ->
          (case (fldL_ _fldLOppGrNm ) of
           { ( _fldLIpp,_fldLIppL) ->
               (case (ppListSep "(" ")" " " (                 _fldLIppL)) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- GrValL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrVal 
         child tl             : GrValL 
      alternative Nil:
-}
-- cata
sem_GrValL :: GrValL  ->
              T_GrValL 
sem_GrValL list  =
    (Prelude.foldr sem_GrValL_Cons sem_GrValL_Nil (Prelude.map sem_GrVal list) )
-- semantic domain
type T_GrValL  = PPGrNm ->
                 ( PP_Doc,([PP_Doc]))
sem_GrValL_Cons :: T_GrVal  ->
                   T_GrValL  ->
                   T_GrValL 
sem_GrValL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >-< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrValL_Nil :: T_GrValL 
sem_GrValL_Nil  =
    (\ _lhsIppGrNm ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- GrVar -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Ignore:
      alternative KnownTag:
         child tag            : GrTag 
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_GrVar :: GrVar  ->
             T_GrVar 
sem_GrVar (GrVar_Ignore )  =
    (sem_GrVar_Ignore )
sem_GrVar (GrVar_KnownTag _tag )  =
    (sem_GrVar_KnownTag (sem_GrTag _tag ) )
sem_GrVar (GrVar_Var _nm )  =
    (sem_GrVar_Var _nm )
-- semantic domain
type T_GrVar  = PPGrNm ->
                ( PP_Doc)
sem_GrVar_Ignore :: T_GrVar 
sem_GrVar_Ignore  =
    (\ _lhsIppGrNm ->
         (case (pp "_") of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_GrVar_KnownTag :: T_GrTag  ->
                      T_GrVar 
sem_GrVar_KnownTag tag_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tagOppGrNm ->
          (case (tag_ _tagOppGrNm ) of
           { ( _tagIpp) ->
               (case (_tagIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
sem_GrVar_Var :: HsName ->
                 T_GrVar 
sem_GrVar_Var nm_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- GrVarL ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         ppGrNm               : PPGrNm
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : GrVar 
         child tl             : GrVarL 
      alternative Nil:
-}
-- cata
sem_GrVarL :: GrVarL  ->
              T_GrVarL 
sem_GrVarL list  =
    (Prelude.foldr sem_GrVarL_Cons sem_GrVarL_Nil (Prelude.map sem_GrVar list) )
-- semantic domain
type T_GrVarL  = PPGrNm ->
                 ( PP_Doc,([PP_Doc]))
sem_GrVarL_Cons :: T_GrVar  ->
                   T_GrVarL  ->
                   T_GrVarL 
sem_GrVarL_Cons hd_ tl_  =
    (\ _lhsIppGrNm ->
         (case (_lhsIppGrNm) of
          { _tlOppGrNm ->
          (case (_lhsIppGrNm) of
           { _hdOppGrNm ->
           (case (tl_ _tlOppGrNm ) of
            { ( _tlIpp,_tlIppL) ->
                (case (hd_ _hdOppGrNm ) of
                 { ( _hdIpp) ->
                     (case (_hdIpp >#< _tlIpp) of
                      { _lhsOpp ->
                      (case (_hdIpp : _tlIppL) of
                       { _lhsOppL ->
                       ( _lhsOpp,_lhsOppL) }) }) }) }) }) }))
sem_GrVarL_Nil :: T_GrVarL 
sem_GrVarL_Nil  =
    (\ _lhsIppGrNm ->
         (case (pp "") of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))