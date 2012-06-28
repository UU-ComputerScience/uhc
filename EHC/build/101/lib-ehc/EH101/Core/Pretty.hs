

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core/Pretty.ag)
module EH101.Core.Pretty(ppCModule
, ppCBindL) where

import EH.Util.Pretty
import EH101.Base.Builtin
import EH101.Base.CfgPP
import EH101.Opts.Base
import EH101.Base.Common
import EH101.Ty
import EH101.Ty.Pretty
import EH101.Core
import EH101.Scanner.Common(coreScanOpts)
import EH101.AnaDomain.Pretty
import EH101.LamInfo
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Ty.Pretty
import EH101.Foreign.Pretty












ppCModule :: EHCOpts -> LamMp -> CModule -> PP_Doc
ppCModule opts lamMp cmod
  =  let  t = wrap_CodeAGItf (sem_CodeAGItf (CodeAGItf_AGItf cmod))
                             (Inh_CodeAGItf
                               { lamMp_Inh_CodeAGItf = lamMp
                               })
     in   (pp_Syn_CodeAGItf t)

ppCExpr :: CExpr -> PP_Doc
ppCExpr ce
  =  let  t = wrap_CExpr (sem_CExpr ce)
                         (Inh_CExpr
                            { varPPMp_Inh_CExpr = Map.empty
                            , lamMp_Inh_CExpr = Map.empty
                            })
     in   (pp_Syn_CExpr t)

instance PP CExpr where
  pp ce = ppCExpr ce



ppCNm :: HsName -> PP_Doc
-- ppCNm = cfgppHsName CfgPP_Core
ppCNm = ppHsnNonAlpha coreScanOpts'
  where coreScanOpts' = coreScanOpts emptyEHCOpts

ppMbCNm :: Maybe HsName -> PP_Doc
ppMbCNm = maybe (ppCNm (hsnFromString "_")) ppCNm

ppManyCNm :: [HsName] -> PP_Doc
ppManyCNm  =  ppListSep "" "" " " . map ppCNm




ppTrack :: Track -> PP_Doc
ppTrack t = text (show t)

ppHole :: UID -> PP_Doc
ppHole i = "<" >|< pp i >|< ">"



ppOptCMetas :: CMetas -> PP_Doc
ppOptCMetas x
  =  let  t = wrap_CMetas (sem_CMetas x)
                          (Inh_CMetas
                             { lamMp_Inh_CMetas = Map.empty
                             })
     in   (pp_Syn_CMetas t)



ppCBindL :: CBindL -> PP_Doc
ppCBindL
  = ppAssocL
  . map (\b -> case b of
                 CBind_Bind n [CBound_Bind m v] -> (n,v >|< ppOptCMetas m)
                 CBind_Bind n _ -> (n,pp "..")
        )




ppSignedInt :: Int -> PP_Doc
ppSignedInt n = " " >#< show n

ppDef :: (PP a, PP b) => a -> b -> PP_Doc
ppDef n v   = n >-< indent 2 v

ppDef' :: (PP a, PP b) => a -> b -> PP_Doc
ppDef' n v   = ppOr (n >-< indent 2 v) (n >#< v)
-- ppDef' n v   = ppDef n v

ppOr :: (PP a, PP b) => a -> b -> PP_Doc
-- ppOr a b   = join (a >//< b)
ppOr a b   = pp a

ppOr' :: (PP a, PP b) => a -> b -> PP_Doc
-- ppOr' a b   = ppOr a b
ppOr' a b   = pp a

ppLit :: String -> String -> PP_Doc
ppLit kind val = "#" >|< kind >|< pp (show val)

ppTag :: CTag -> PP_Doc
ppTag t = ppCTag' CfgPP_Core t

ppCurlyList :: (a -> PP_Doc) -> [a] -> PP_Doc
ppCurlyList pL xs = ppListSep "{ " " }" ", " $ map pL xs


-- CAlt --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Alt:
         child pat            : CPat 
         child expr           : CExpr 
-}
-- cata
sem_CAlt :: CAlt  ->
            T_CAlt 
sem_CAlt (CAlt_Alt _pat _expr )  =
    (sem_CAlt_Alt (sem_CPat _pat ) (sem_CExpr _expr ) )
-- semantic domain
type T_CAlt  = LamMp ->
               VarPPMp ->
               ( PP_Doc)
sem_CAlt_Alt :: T_CPat  ->
                T_CExpr  ->
                T_CAlt 
sem_CAlt_Alt pat_ expr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _exprOvarPPMp ->
          (case (_lhsIlamMp) of
           { _exprOlamMp ->
           (case (_lhsIvarPPMp) of
            { _patOvarPPMp ->
            (case (_lhsIlamMp) of
             { _patOlamMp ->
             (case (expr_ _exprOlamMp _exprOvarPPMp ) of
              { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                  (case (pat_ _patOlamMp _patOvarPPMp ) of
                   { ( _patIfldNmL,_patIpp) ->
                       (case (ppDef (_patIpp >#< "->") (_exprIpp)) of
                        { _lhsOpp ->
                        ( _lhsOpp) }) }) }) }) }) }) }))
-- CAltL -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CAlt 
         child tl             : CAltL 
      alternative Nil:
-}
-- cata
sem_CAltL :: CAltL  ->
             T_CAltL 
sem_CAltL list  =
    (Prelude.foldr sem_CAltL_Cons sem_CAltL_Nil (Prelude.map sem_CAlt list) )
-- semantic domain
type T_CAltL  = LamMp ->
                VarPPMp ->
                ( PP_Doc,([PP_Doc]))
sem_CAltL_Cons :: T_CAlt  ->
                  T_CAltL  ->
                  T_CAltL 
sem_CAltL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _tlOvarPPMp ->
          (case (_lhsIlamMp) of
           { _tlOlamMp ->
           (case (_lhsIvarPPMp) of
            { _hdOvarPPMp ->
            (case (_lhsIlamMp) of
             { _hdOlamMp ->
             (case (tl_ _tlOlamMp _tlOvarPPMp ) of
              { ( _tlIpp,_tlIppL) ->
                  (case (hd_ _hdOlamMp _hdOvarPPMp ) of
                   { ( _hdIpp) ->
                       (case (_hdIpp >-< _tlIpp) of
                        { _lhsOpp ->
                        (case (_hdIpp : _tlIppL) of
                         { _lhsOppL ->
                         ( _lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }))
sem_CAltL_Nil :: T_CAltL 
sem_CAltL_Nil  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- CBind -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         nm                   : HsName
         pp                   : PP_Doc
   alternatives:
      alternative Bind:
         child nm             : {HsName}
         child bindAspects    : CBoundL 
-}
-- cata
sem_CBind :: CBind  ->
             T_CBind 
sem_CBind (CBind_Bind _nm _bindAspects )  =
    (sem_CBind_Bind _nm (sem_CBoundL _bindAspects ) )
-- semantic domain
type T_CBind  = LamMp ->
                VarPPMp ->
                ( HsName,PP_Doc)
sem_CBind_Bind :: HsName ->
                  T_CBoundL  ->
                  T_CBind 
sem_CBind_Bind nm_ bindAspects_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (nm_) of
          { _lhsOnm ->
          (case (_lhsIlamMp) of
           { _bindAspectsOlamMp ->
           (case (bindAspects_ ) of
            { ( _bindAspectsIgathVarPPMp,bindAspects_1) ->
                (case (Map.union _bindAspectsIgathVarPPMp _lhsIvarPPMp) of
                 { _bindAspectsOvarPPMp ->
                 (case (nm_) of
                  { _bindAspectsOnm ->
                  (case (bindAspects_1 _bindAspectsOlamMp _bindAspectsOnm _bindAspectsOvarPPMp ) of
                   { ( _bindAspectsIpp,_bindAspectsIppL) ->
                       (case (let p [a] = a
                                  p as  = ppCurlysSemisBlock as
                              in  ppDef (ppCNm nm_) (p $ _bindAspectsIppL ++ (maybe [] (\x -> [pp x]) $ Map.lookup nm_ _lhsIlamMp))) of
                        { _lhsOpp ->
                        ( _lhsOnm,_lhsOpp) }) }) }) }) }) }) }))
-- CBindAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local pp          : _
-}
-- cata
sem_CBindAnn :: CBindAnn  ->
                T_CBindAnn 
sem_CBindAnn (CBindAnn_Coe _coe )  =
    (sem_CBindAnn_Coe _coe )
-- semantic domain
type T_CBindAnn  = LamMp ->
                   VarPPMp ->
                   ( PP_Doc)
sem_CBindAnn_Coe :: RelevCoe ->
                    T_CBindAnn 
sem_CBindAnn_Coe coe_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ("~" >#< ppRelevCoe _lhsIvarPPMp coe_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
-- CBindAnnL ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CBindAnn 
         child tl             : CBindAnnL 
      alternative Nil:
-}
-- cata
sem_CBindAnnL :: CBindAnnL  ->
                 T_CBindAnnL 
sem_CBindAnnL list  =
    (Prelude.foldr sem_CBindAnnL_Cons sem_CBindAnnL_Nil (Prelude.map sem_CBindAnn list) )
-- semantic domain
type T_CBindAnnL  = LamMp ->
                    VarPPMp ->
                    ( PP_Doc,([PP_Doc]))
sem_CBindAnnL_Cons :: T_CBindAnn  ->
                      T_CBindAnnL  ->
                      T_CBindAnnL 
sem_CBindAnnL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _tlOvarPPMp ->
          (case (_lhsIvarPPMp) of
           { _hdOvarPPMp ->
           (case (_lhsIlamMp) of
            { _tlOlamMp ->
            (case (tl_ _tlOlamMp _tlOvarPPMp ) of
             { ( _tlIpp,_tlIppL) ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp ->
                  (case (hd_ _hdOlamMp _hdOvarPPMp ) of
                   { ( _hdIpp) ->
                       (case (_hdIpp >-< _tlIpp) of
                        { _lhsOpp ->
                        (case (_hdIpp : _tlIppL) of
                         { _lhsOppL ->
                         ( _lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }))
sem_CBindAnnL_Nil :: T_CBindAnnL 
sem_CBindAnnL_Nil  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- CBindL ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CBind 
         child tl             : CBindL 
      alternative Nil:
-}
-- cata
sem_CBindL :: CBindL  ->
              T_CBindL 
sem_CBindL list  =
    (Prelude.foldr sem_CBindL_Cons sem_CBindL_Nil (Prelude.map sem_CBind list) )
-- semantic domain
type T_CBindL  = LamMp ->
                 VarPPMp ->
                 ( PP_Doc,([PP_Doc]))
sem_CBindL_Cons :: T_CBind  ->
                   T_CBindL  ->
                   T_CBindL 
sem_CBindL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _tlOvarPPMp ->
          (case (_lhsIlamMp) of
           { _tlOlamMp ->
           (case (_lhsIvarPPMp) of
            { _hdOvarPPMp ->
            (case (_lhsIlamMp) of
             { _hdOlamMp ->
             (case (tl_ _tlOlamMp _tlOvarPPMp ) of
              { ( _tlIpp,_tlIppL) ->
                  (case (hd_ _hdOlamMp _hdOvarPPMp ) of
                   { ( _hdInm,_hdIpp) ->
                       (case (_hdIpp >-< _tlIpp) of
                        { _lhsOpp ->
                        (case (_hdIpp : _tlIppL) of
                         { _lhsOppL ->
                         ( _lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }))
sem_CBindL_Nil :: T_CBindL 
sem_CBindL_Nil  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (empty) of
          { _lhsOpp ->
          (case ([]) of
           { _lhsOppL ->
           ( _lhsOpp,_lhsOppL) }) }))
-- CBound ------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathVarPPMp          : VarPPMp
   visit 1:
      inherited attributes:
         lamMp                : LamMp
         nm                   : HsName
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Bind:
         child bindMeta       : CMetas 
         child expr           : CExpr 
      alternative FFE:
         child callconv       : {FFIWay}
         child expEnt         : {ForeignEnt}
         child expr           : CExpr 
         child ty             : {Ty}
      alternative Meta:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child cmetas         : CMetas 
      alternative RelevTy:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child relevTy        : {RelevTy}
         visit 0:
            local _tup1       : {(PP_Doc,VarPPMp)}
         visit 1:
            local tyPP        : {PP_Doc}
            intra _tup1       : {(PP_Doc,VarPPMp)}
      alternative Ty:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child ty             : {Ty}
         visit 1:
            local tyPP        : {PP_Doc}
      alternative Val:
         child aspectKeyS     : {ACoreBindAspectKeyS}
         child expr           : CExpr 
-}
-- cata
sem_CBound :: CBound  ->
              T_CBound 
sem_CBound (CBound_Bind _bindMeta _expr )  =
    (sem_CBound_Bind (sem_CMetas _bindMeta ) (sem_CExpr _expr ) )
sem_CBound (CBound_FFE _callconv _expEnt _expr _ty )  =
    (sem_CBound_FFE _callconv _expEnt (sem_CExpr _expr ) _ty )
sem_CBound (CBound_Meta _aspectKeyS _cmetas )  =
    (sem_CBound_Meta _aspectKeyS (sem_CMetas _cmetas ) )
sem_CBound (CBound_RelevTy _aspectKeyS _relevTy )  =
    (sem_CBound_RelevTy _aspectKeyS _relevTy )
sem_CBound (CBound_Ty _aspectKeyS _ty )  =
    (sem_CBound_Ty _aspectKeyS _ty )
sem_CBound (CBound_Val _aspectKeyS _expr )  =
    (sem_CBound_Val _aspectKeyS (sem_CExpr _expr ) )
-- semantic domain
type T_CBound  = ( VarPPMp,T_CBound_1 )
type T_CBound_1  = LamMp ->
                   HsName ->
                   VarPPMp ->
                   ( PP_Doc)
sem_CBound_Bind :: T_CMetas  ->
                   T_CExpr  ->
                   T_CBound 
sem_CBound_Bind bindMeta_ expr_  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBound_Bind_1 :: T_CBound_1 
                 sem_CBound_Bind_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (_lhsIvarPPMp) of
                           { _exprOvarPPMp ->
                           (case (_lhsIlamMp) of
                            { _exprOlamMp ->
                            (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                             { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                                 (case (_lhsIlamMp) of
                                  { _bindMetaOlamMp ->
                                  (case (bindMeta_ _bindMetaOlamMp ) of
                                   { ( _bindMetaIpp) ->
                                       (case (_bindMetaIpp >-< "=" >#< _exprIpp) of
                                        { _lhsOpp ->
                                        ( _lhsOpp) }) }) }) }) }) }))
             in  sem_CBound_Bind_1)) of
      { ( sem_CBound_1) ->
      ( _lhsOgathVarPPMp,sem_CBound_1) }) })
sem_CBound_FFE :: FFIWay ->
                  ForeignEnt ->
                  T_CExpr  ->
                  Ty ->
                  T_CBound 
sem_CBound_FFE callconv_ expEnt_ expr_ ty_  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBound_FFE_1 :: T_CBound_1 
                 sem_CBound_FFE_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (_lhsIvarPPMp) of
                           { _exprOvarPPMp ->
                           (case (_lhsIlamMp) of
                            { _exprOlamMp ->
                            (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                             { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                                 (case ("=" >#< ("foreignexport" >#< ppCurlysCommasBlock [pp (show callconv_),"\"" >|< expEnt_ >|< "\"",_exprIpp                   ,ppTyWithCfg' CfgPP_Plain ty_])) of
                                  { _lhsOpp ->
                                  ( _lhsOpp) }) }) }) }))
             in  sem_CBound_FFE_1)) of
      { ( sem_CBound_1) ->
      ( _lhsOgathVarPPMp,sem_CBound_1) }) })
sem_CBound_Meta :: ACoreBindAspectKeyS ->
                   T_CMetas  ->
                   T_CBound 
sem_CBound_Meta aspectKeyS_ cmetas_  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBound_Meta_1 :: T_CBound_1 
                 sem_CBound_Meta_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (_lhsIlamMp) of
                           { _cmetasOlamMp ->
                           (case (cmetas_ _cmetasOlamMp ) of
                            { ( _cmetasIpp) ->
                                (case ("::_M" >#< ppACBaspKeyS aspectKeyS_ >#< _cmetasIpp) of
                                 { _lhsOpp ->
                                 ( _lhsOpp) }) }) }))
             in  sem_CBound_Meta_1)) of
      { ( sem_CBound_1) ->
      ( _lhsOgathVarPPMp,sem_CBound_1) }) })
sem_CBound_RelevTy :: ACoreBindAspectKeyS ->
                      RelevTy ->
                      T_CBound 
sem_CBound_RelevTy aspectKeyS_ relevTy_  =
    (case (ppRelevTy Map.empty relevTy_) of
     { __tup1 ->
     (case (__tup1) of
      { (_,_lhsOgathVarPPMp) ->
      (case ((let sem_CBound_RelevTy_1 :: T_CBound_1 
                  sem_CBound_RelevTy_1  =
                      (\ _lhsIlamMp
                         _lhsInm
                         _lhsIvarPPMp ->
                           (case (__tup1) of
                            { (_tyPP,_) ->
                            (case ("::_R" >#< ppACBaspKeyS aspectKeyS_ >#< _tyPP) of
                             { _lhsOpp ->
                             ( _lhsOpp) }) }))
              in  sem_CBound_RelevTy_1)) of
       { ( sem_CBound_1) ->
       ( _lhsOgathVarPPMp,sem_CBound_1) }) }) })
sem_CBound_Ty :: ACoreBindAspectKeyS ->
                 Ty ->
                 T_CBound 
sem_CBound_Ty aspectKeyS_ ty_  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBound_Ty_1 :: T_CBound_1 
                 sem_CBound_Ty_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (ppTy ty_) of
                           { _tyPP ->
                           (case ("::" >#< ppACBaspKeyS aspectKeyS_ >#< _tyPP) of
                            { _lhsOpp ->
                            ( _lhsOpp) }) }))
             in  sem_CBound_Ty_1)) of
      { ( sem_CBound_1) ->
      ( _lhsOgathVarPPMp,sem_CBound_1) }) })
sem_CBound_Val :: ACoreBindAspectKeyS ->
                  T_CExpr  ->
                  T_CBound 
sem_CBound_Val aspectKeyS_ expr_  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBound_Val_1 :: T_CBound_1 
                 sem_CBound_Val_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (_lhsIvarPPMp) of
                           { _exprOvarPPMp ->
                           (case (_lhsIlamMp) of
                            { _exprOlamMp ->
                            (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                             { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                                 (case ("=" >#< ppACBaspKeyS aspectKeyS_ >#< _exprIpp) of
                                  { _lhsOpp ->
                                  ( _lhsOpp) }) }) }) }))
             in  sem_CBound_Val_1)) of
      { ( sem_CBound_1) ->
      ( _lhsOgathVarPPMp,sem_CBound_1) }) })
-- CBoundL -----------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         gathVarPPMp          : VarPPMp
   visit 1:
      inherited attributes:
         lamMp                : LamMp
         nm                   : HsName
         varPPMp              : VarPPMp
      synthesized attributes:
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CBound 
         child tl             : CBoundL 
      alternative Nil:
-}
-- cata
sem_CBoundL :: CBoundL  ->
               T_CBoundL 
sem_CBoundL list  =
    (Prelude.foldr sem_CBoundL_Cons sem_CBoundL_Nil (Prelude.map sem_CBound list) )
-- semantic domain
type T_CBoundL  = ( VarPPMp,T_CBoundL_1 )
type T_CBoundL_1  = LamMp ->
                    HsName ->
                    VarPPMp ->
                    ( PP_Doc,([PP_Doc]))
sem_CBoundL_Cons :: T_CBound  ->
                    T_CBoundL  ->
                    T_CBoundL 
sem_CBoundL_Cons hd_ tl_  =
    (case (tl_ ) of
     { ( _tlIgathVarPPMp,tl_1) ->
         (case (hd_ ) of
          { ( _hdIgathVarPPMp,hd_1) ->
              (case (_hdIgathVarPPMp `Map.union` _tlIgathVarPPMp) of
               { _lhsOgathVarPPMp ->
               (case ((let sem_CBoundL_Cons_1 :: T_CBoundL_1 
                           sem_CBoundL_Cons_1  =
                               (\ _lhsIlamMp
                                  _lhsInm
                                  _lhsIvarPPMp ->
                                    (case (_lhsIvarPPMp) of
                                     { _tlOvarPPMp ->
                                     (case (_lhsIlamMp) of
                                      { _tlOlamMp ->
                                      (case (_lhsIvarPPMp) of
                                       { _hdOvarPPMp ->
                                       (case (_lhsIlamMp) of
                                        { _hdOlamMp ->
                                        (case (_lhsInm) of
                                         { _tlOnm ->
                                         (case (tl_1 _tlOlamMp _tlOnm _tlOvarPPMp ) of
                                          { ( _tlIpp,_tlIppL) ->
                                              (case (_lhsInm) of
                                               { _hdOnm ->
                                               (case (hd_1 _hdOlamMp _hdOnm _hdOvarPPMp ) of
                                                { ( _hdIpp) ->
                                                    (case (_hdIpp >-< _tlIpp) of
                                                     { _lhsOpp ->
                                                     (case (_hdIpp : _tlIppL) of
                                                      { _lhsOppL ->
                                                      ( _lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }) }))
                       in  sem_CBoundL_Cons_1)) of
                { ( sem_CBoundL_1) ->
                ( _lhsOgathVarPPMp,sem_CBoundL_1) }) }) }) })
sem_CBoundL_Nil :: T_CBoundL 
sem_CBoundL_Nil  =
    (case (Map.empty) of
     { _lhsOgathVarPPMp ->
     (case ((let sem_CBoundL_Nil_1 :: T_CBoundL_1 
                 sem_CBoundL_Nil_1  =
                     (\ _lhsIlamMp
                        _lhsInm
                        _lhsIvarPPMp ->
                          (case (empty) of
                           { _lhsOpp ->
                           (case ([]) of
                            { _lhsOppL ->
                            ( _lhsOpp,_lhsOppL) }) }))
             in  sem_CBoundL_Nil_1)) of
      { ( sem_CBoundL_1) ->
      ( _lhsOgathVarPPMp,sem_CBoundL_1) }) })
-- CExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         appArgPPL            : [PP_Doc]
         appFunPP             : PP_Doc
         lamArgPPL            : [PP_Doc]
         lamBodyPP            : PP_Doc
         pp                   : PP_Doc
   alternatives:
      alternative Ann:
         child ann            : CExprAnn 
         child expr           : CExpr 
         visit 0:
            local pp          : _
      alternative App:
         child func           : CExpr 
         child arg            : CBound 
         visit 0:
            local appArgPPL   : _
            local appFunPP    : _
            local pp          : _
      alternative Case:
         child expr           : CExpr 
         child alts           : CAltL 
         child dflt           : CExpr 
         visit 0:
            local pp          : _
      alternative CaseAltFail:
         child failReason     : {CaseAltFailReason}
         child errorExpr      : CExpr 
         visit 0:
            local pp          : _
      alternative Char:
         child char           : {Char}
         visit 0:
            local pp          : _
      alternative CoeArg:
         visit 0:
            local pp          : _
      alternative FFI:
         child callconv       : {FFIWay}
         child safety         : {String}
         child impEnt         : {ForeignEnt}
         child ty             : {Ty}
         visit 0:
            local ppent       : _
            local pp          : _
      alternative Hole:
         child uid            : {UID}
         visit 0:
            local pp          : _
      alternative HoleLet:
         child bindsUid       : {UID}
         child body           : CExpr 
         visit 0:
            local pp          : _
      alternative ImplsApp:
         child func           : CExpr 
         child uid            : {ImplsVarId}
         visit 0:
            local pp          : _
      alternative ImplsLam:
         child uid            : {ImplsVarId}
         child body           : CExpr 
         visit 0:
            local pp          : _
      alternative Int:
         child int            : {Int}
         visit 0:
            local pp          : _
      alternative Integer:
         child integer        : {Integer}
         visit 0:
            local pp          : _
      alternative Lam:
         child bind           : CBind 
         child body           : CExpr 
         visit 0:
            local argNm       : _
            local lamArgPPL   : _
            local lamBodyPP   : _
            local pp          : _
      alternative Let:
         child categ          : {CBindCateg}
         child binds          : CBindL 
         child body           : CExpr 
         visit 0:
            local ppCateg     : _
            local pp          : _
      alternative String:
         child str            : {String}
         visit 0:
            local pp          : _
      alternative Tup:
         child tag            : {CTag}
         visit 0:
            local pp          : _
      alternative TupDel:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         visit 0:
            local pp          : _
      alternative TupIns:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local pp          : _
      alternative TupUpd:
         child expr           : CExpr 
         child tag            : {CTag}
         child nm             : {HsName}
         child offset         : CExpr 
         child fldExpr        : CExpr 
         visit 0:
            local pp          : _
      alternative Var:
         child ref            : {ACoreBindRef}
         visit 0:
            local pp          : _
-}
-- cata
sem_CExpr :: CExpr  ->
             T_CExpr 
sem_CExpr (CExpr_Ann _ann _expr )  =
    (sem_CExpr_Ann (sem_CExprAnn _ann ) (sem_CExpr _expr ) )
sem_CExpr (CExpr_App _func _arg )  =
    (sem_CExpr_App (sem_CExpr _func ) (sem_CBound _arg ) )
sem_CExpr (CExpr_Case _expr _alts _dflt )  =
    (sem_CExpr_Case (sem_CExpr _expr ) (sem_CAltL _alts ) (sem_CExpr _dflt ) )
sem_CExpr (CExpr_CaseAltFail _failReason _errorExpr )  =
    (sem_CExpr_CaseAltFail _failReason (sem_CExpr _errorExpr ) )
sem_CExpr (CExpr_Char _char )  =
    (sem_CExpr_Char _char )
sem_CExpr (CExpr_CoeArg )  =
    (sem_CExpr_CoeArg )
sem_CExpr (CExpr_FFI _callconv _safety _impEnt _ty )  =
    (sem_CExpr_FFI _callconv _safety _impEnt _ty )
sem_CExpr (CExpr_Hole _uid )  =
    (sem_CExpr_Hole _uid )
sem_CExpr (CExpr_HoleLet _bindsUid _body )  =
    (sem_CExpr_HoleLet _bindsUid (sem_CExpr _body ) )
sem_CExpr (CExpr_ImplsApp _func _uid )  =
    (sem_CExpr_ImplsApp (sem_CExpr _func ) _uid )
sem_CExpr (CExpr_ImplsLam _uid _body )  =
    (sem_CExpr_ImplsLam _uid (sem_CExpr _body ) )
sem_CExpr (CExpr_Int _int )  =
    (sem_CExpr_Int _int )
sem_CExpr (CExpr_Integer _integer )  =
    (sem_CExpr_Integer _integer )
sem_CExpr (CExpr_Lam _bind _body )  =
    (sem_CExpr_Lam (sem_CBind _bind ) (sem_CExpr _body ) )
sem_CExpr (CExpr_Let _categ _binds _body )  =
    (sem_CExpr_Let _categ (sem_CBindL _binds ) (sem_CExpr _body ) )
sem_CExpr (CExpr_String _str )  =
    (sem_CExpr_String _str )
sem_CExpr (CExpr_Tup _tag )  =
    (sem_CExpr_Tup _tag )
sem_CExpr (CExpr_TupDel _expr _tag _nm _offset )  =
    (sem_CExpr_TupDel (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) )
sem_CExpr (CExpr_TupIns _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupIns (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_TupUpd _expr _tag _nm _offset _fldExpr )  =
    (sem_CExpr_TupUpd (sem_CExpr _expr ) _tag _nm (sem_CExpr _offset ) (sem_CExpr _fldExpr ) )
sem_CExpr (CExpr_Var _ref )  =
    (sem_CExpr_Var _ref )
-- semantic domain
type T_CExpr  = LamMp ->
                VarPPMp ->
                ( ([PP_Doc]),PP_Doc,([PP_Doc]),PP_Doc,PP_Doc)
data Inh_CExpr  = Inh_CExpr {lamMp_Inh_CExpr :: !(LamMp),varPPMp_Inh_CExpr :: !(VarPPMp)}
data Syn_CExpr  = Syn_CExpr {appArgPPL_Syn_CExpr :: !(([PP_Doc])),appFunPP_Syn_CExpr :: !(PP_Doc),lamArgPPL_Syn_CExpr :: !(([PP_Doc])),lamBodyPP_Syn_CExpr :: !(PP_Doc),pp_Syn_CExpr :: !(PP_Doc)}
wrap_CExpr :: T_CExpr  ->
              Inh_CExpr  ->
              Syn_CExpr 
wrap_CExpr sem (Inh_CExpr _lhsIlamMp _lhsIvarPPMp )  =
    (let ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) = sem _lhsIlamMp _lhsIvarPPMp 
     in  (Syn_CExpr _lhsOappArgPPL _lhsOappFunPP _lhsOlamArgPPL _lhsOlamBodyPP _lhsOpp ))
sem_CExpr_Ann :: T_CExprAnn  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Ann ann_ expr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _exprOvarPPMp ->
           (case (_lhsIlamMp) of
            { _exprOlamMp ->
            (case (_lhsIvarPPMp) of
             { _annOvarPPMp ->
             (case (expr_ _exprOlamMp _exprOvarPPMp ) of
              { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                  (case (_lhsIlamMp) of
                   { _annOlamMp ->
                   (case (ann_ _annOlamMp _annOvarPPMp ) of
                    { ( _annIpp) ->
                        (case (ppParens (_exprIpp >#< _annIpp)) of
                         { _pp ->
                         (case (_pp) of
                          { _lhsOappFunPP ->
                          (case ([]) of
                           { _lhsOlamArgPPL ->
                           (case (_pp) of
                            { _lhsOlamBodyPP ->
                            (case (_pp) of
                             { _lhsOpp ->
                             ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_App :: T_CExpr  ->
                 T_CBound  ->
                 T_CExpr 
sem_CExpr_App func_ arg_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _argOvarPPMp ->
          (case (_lhsIlamMp) of
           { _argOlamMp ->
           (case (_lhsIvarPPMp) of
            { _funcOvarPPMp ->
            (case (_lhsIlamMp) of
             { _funcOlamMp ->
             (case (arg_ ) of
              { ( _argIgathVarPPMp,arg_1) ->
                  (case (hsnUnknown) of
                   { _argOnm ->
                   (case (arg_1 _argOlamMp _argOnm _argOvarPPMp ) of
                    { ( _argIpp) ->
                        (case (func_ _funcOlamMp _funcOvarPPMp ) of
                         { ( _funcIappArgPPL,_funcIappFunPP,_funcIlamArgPPL,_funcIlamBodyPP,_funcIpp) ->
                             (case ((_argIpp) : _funcIappArgPPL) of
                              { _appArgPPL ->
                              (case (_appArgPPL) of
                               { _lhsOappArgPPL ->
                               (case (_funcIappFunPP) of
                                { _appFunPP ->
                                (case (_appFunPP) of
                                 { _lhsOappFunPP ->
                                 (case ([]) of
                                  { _lhsOlamArgPPL ->
                                  (case (let args = reverse $ map ppParens $ _appArgPPL
                                             fun  = ppParens _appFunPP
                                         in  ppDef fun (vlist args)) of
                                   { _pp ->
                                   (case (_pp) of
                                    { _lhsOlamBodyPP ->
                                    (case (_pp) of
                                     { _lhsOpp ->
                                     ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Case :: T_CExpr  ->
                  T_CAltL  ->
                  T_CExpr  ->
                  T_CExpr 
sem_CExpr_Case expr_ alts_ dflt_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _dfltOvarPPMp ->
           (case (_lhsIlamMp) of
            { _dfltOlamMp ->
            (case (_lhsIvarPPMp) of
             { _altsOvarPPMp ->
             (case (_lhsIlamMp) of
              { _altsOlamMp ->
              (case (_lhsIvarPPMp) of
               { _exprOvarPPMp ->
               (case (_lhsIlamMp) of
                { _exprOlamMp ->
                (case (dflt_ _dfltOlamMp _dfltOvarPPMp ) of
                 { ( _dfltIappArgPPL,_dfltIappFunPP,_dfltIlamArgPPL,_dfltIlamBodyPP,_dfltIpp) ->
                     (case (alts_ _altsOlamMp _altsOvarPPMp ) of
                      { ( _altsIpp,_altsIppL) ->
                          (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                           { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                               (case ("case" >#< _exprIpp >#< "of"
                                      >-< indent 1 (ppCurlysSemisBlock _altsIppL >-< ppCurlysSemisBlock [ppDef "default" _dfltIpp])) of
                                { _pp ->
                                (case (_pp) of
                                 { _lhsOappFunPP ->
                                 (case ([]) of
                                  { _lhsOlamArgPPL ->
                                  (case (_pp) of
                                   { _lhsOlamBodyPP ->
                                   (case (_pp) of
                                    { _lhsOpp ->
                                    ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_CaseAltFail :: CaseAltFailReason ->
                         T_CExpr  ->
                         T_CExpr 
sem_CExpr_CaseAltFail failReason_ errorExpr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (pp "FAIL" >#< failReason_) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_Char :: Char ->
                  T_CExpr 
sem_CExpr_Char char_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppLit "Char"   [char_]) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_CoeArg :: T_CExpr 
sem_CExpr_CoeArg  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (pp "<_>") of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_FFI :: FFIWay ->
                 String ->
                 ForeignEnt ->
                 Ty ->
                 T_CExpr 
sem_CExpr_FFI callconv_ safety_ impEnt_ ty_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case ("\"" >|< impEnt_ >|< "\"") of
           { _ppent ->
           (case (("foreign" >#< ppCurlysCommasBlock [pp (show callconv_),pp (show safety_),_ppent,ppTy ty_])) of
            { _pp ->
            (case (_pp) of
             { _lhsOappFunPP ->
             (case ([]) of
              { _lhsOlamArgPPL ->
              (case (_pp) of
               { _lhsOlamBodyPP ->
               (case (_pp) of
                { _lhsOpp ->
                ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }))
sem_CExpr_Hole :: UID ->
                  T_CExpr 
sem_CExpr_Hole uid_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppHole uid_) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_HoleLet :: UID ->
                     T_CExpr  ->
                     T_CExpr 
sem_CExpr_HoleLet bindsUid_ body_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _bodyOvarPPMp ->
           (case (_lhsIlamMp) of
            { _bodyOlamMp ->
            (case (body_ _bodyOlamMp _bodyOvarPPMp ) of
             { ( _bodyIappArgPPL,_bodyIappFunPP,_bodyIlamArgPPL,_bodyIlamBodyPP,_bodyIpp) ->
                 (case ("let --" >#< ppHole bindsUid_ >-< ppDef "in" _bodyIpp) of
                  { _pp ->
                  (case (_pp) of
                   { _lhsOappFunPP ->
                   (case ([]) of
                    { _lhsOlamArgPPL ->
                    (case (_pp) of
                     { _lhsOlamBodyPP ->
                     (case (_pp) of
                      { _lhsOpp ->
                      ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsApp :: T_CExpr  ->
                      ImplsVarId ->
                      T_CExpr 
sem_CExpr_ImplsApp func_ uid_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _funcOvarPPMp ->
           (case (_lhsIlamMp) of
            { _funcOlamMp ->
            (case (func_ _funcOlamMp _funcOvarPPMp ) of
             { ( _funcIappArgPPL,_funcIappFunPP,_funcIlamArgPPL,_funcIlamBodyPP,_funcIpp) ->
                 (case (ppDef (_funcIpp >#< "-- Impl") (ppHole uid_)) of
                  { _pp ->
                  (case (_pp) of
                   { _lhsOappFunPP ->
                   (case ([]) of
                    { _lhsOlamArgPPL ->
                    (case (_pp) of
                     { _lhsOlamBodyPP ->
                     (case (_pp) of
                      { _lhsOpp ->
                      ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }))
sem_CExpr_ImplsLam :: ImplsVarId ->
                      T_CExpr  ->
                      T_CExpr 
sem_CExpr_ImplsLam uid_ body_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _bodyOvarPPMp ->
           (case (_lhsIlamMp) of
            { _bodyOlamMp ->
            (case (body_ _bodyOlamMp _bodyOvarPPMp ) of
             { ( _bodyIappArgPPL,_bodyIappFunPP,_bodyIlamArgPPL,_bodyIlamBodyPP,_bodyIpp) ->
                 (case (ppDef ("\\" >|< ppHole uid_ >#< "-- Impl") ("->" >#< _bodyIpp)) of
                  { _pp ->
                  (case (_pp) of
                   { _lhsOappFunPP ->
                   (case ([]) of
                    { _lhsOlamArgPPL ->
                    (case (_pp) of
                     { _lhsOlamBodyPP ->
                     (case (_pp) of
                      { _lhsOpp ->
                      ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }))
sem_CExpr_Int :: Int ->
                 T_CExpr 
sem_CExpr_Int int_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppLit "Int"    (show int_)) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_Integer :: Integer ->
                     T_CExpr 
sem_CExpr_Integer integer_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppLit "Integer" (show integer_)) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_Lam :: T_CBind  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Lam bind_ body_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _bodyOvarPPMp ->
           (case (_lhsIlamMp) of
            { _bodyOlamMp ->
            (case (_lhsIvarPPMp) of
             { _bindOvarPPMp ->
             (case (_lhsIlamMp) of
              { _bindOlamMp ->
              (case (bind_ _bindOlamMp _bindOvarPPMp ) of
               { ( _bindInm,_bindIpp) ->
                   (case (_bindInm) of
                    { _argNm ->
                    (case (body_ _bodyOlamMp _bodyOvarPPMp ) of
                     { ( _bodyIappArgPPL,_bodyIappFunPP,_bodyIlamArgPPL,_bodyIlamBodyPP,_bodyIpp) ->
                         (case ((ppCNm _argNm) : _bodyIlamArgPPL) of
                          { _lamArgPPL ->
                          (case (_bodyIlamBodyPP) of
                           { _lamBodyPP ->
                           (case (ppDef ("\\" >|< ppSpaces _lamArgPPL >#< "->") (_lamBodyPP)) of
                            { _pp ->
                            (case (_pp) of
                             { _lhsOappFunPP ->
                             (case (_lamArgPPL) of
                              { _lhsOlamArgPPL ->
                              (case (_lamBodyPP) of
                               { _lhsOlamBodyPP ->
                               (case (_pp) of
                                { _lhsOpp ->
                                ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Let :: CBindCateg ->
                 T_CBindL  ->
                 T_CExpr  ->
                 T_CExpr 
sem_CExpr_Let categ_ binds_ body_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _bodyOvarPPMp ->
           (case (_lhsIlamMp) of
            { _bodyOlamMp ->
            (case (_lhsIvarPPMp) of
             { _bindsOvarPPMp ->
             (case (_lhsIlamMp) of
              { _bindsOlamMp ->
              (case (case categ_ of
                       CBindCateg_Rec     -> pp " rec"
                       CBindCateg_Strict  -> pp " !"
                       CBindCateg_FFI     -> pp " foreign"
                       CBindCateg_FFE     -> pp " foreignexport"
                       _                  -> empty) of
               { _ppCateg ->
               (case (body_ _bodyOlamMp _bodyOvarPPMp ) of
                { ( _bodyIappArgPPL,_bodyIappFunPP,_bodyIlamArgPPL,_bodyIlamBodyPP,_bodyIpp) ->
                    (case (binds_ _bindsOlamMp _bindsOvarPPMp ) of
                     { ( _bindsIpp,_bindsIppL) ->
                         (case (ppDef ("let" >|< _ppCateg) (ppCurlysSemisBlock _bindsIppL) >#< "in" >-< _bodyIpp) of
                          { _pp ->
                          (case (_pp) of
                           { _lhsOappFunPP ->
                           (case ([]) of
                            { _lhsOlamArgPPL ->
                            (case (_pp) of
                             { _lhsOlamBodyPP ->
                             (case (_pp) of
                              { _lhsOpp ->
                              ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_String :: String ->
                    T_CExpr 
sem_CExpr_String str_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppLit "String" str_) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_Tup :: CTag ->
                 T_CExpr 
sem_CExpr_Tup tag_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case ("#Tag" >#< ppTag tag_) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
sem_CExpr_TupDel :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupDel expr_ tag_ nm_ offset_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _offsetOvarPPMp ->
           (case (_lhsIlamMp) of
            { _offsetOlamMp ->
            (case (_lhsIvarPPMp) of
             { _exprOvarPPMp ->
             (case (_lhsIlamMp) of
              { _exprOlamMp ->
              (case (offset_ _offsetOlamMp _offsetOvarPPMp ) of
               { ( _offsetIappArgPPL,_offsetIappFunPP,_offsetIlamArgPPL,_offsetIlamBodyPP,_offsetIpp) ->
                   (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                    { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                        (case (ppDef (ppParens _exprIpp) ("-=" >|< ppCurlysCommas' [ppTag tag_,_offsetIpp,ppCNm nm_])) of
                         { _pp ->
                         (case (_pp) of
                          { _lhsOappFunPP ->
                          (case ([]) of
                           { _lhsOlamArgPPL ->
                           (case (_pp) of
                            { _lhsOlamBodyPP ->
                            (case (_pp) of
                             { _lhsOpp ->
                             ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupIns :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupIns expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _fldExprOvarPPMp ->
           (case (_lhsIlamMp) of
            { _fldExprOlamMp ->
            (case (_lhsIvarPPMp) of
             { _offsetOvarPPMp ->
             (case (_lhsIlamMp) of
              { _offsetOlamMp ->
              (case (_lhsIvarPPMp) of
               { _exprOvarPPMp ->
               (case (_lhsIlamMp) of
                { _exprOlamMp ->
                (case (fldExpr_ _fldExprOlamMp _fldExprOvarPPMp ) of
                 { ( _fldExprIappArgPPL,_fldExprIappFunPP,_fldExprIlamArgPPL,_fldExprIlamBodyPP,_fldExprIpp) ->
                     (case (offset_ _offsetOlamMp _offsetOvarPPMp ) of
                      { ( _offsetIappArgPPL,_offsetIappFunPP,_offsetIlamArgPPL,_offsetIlamBodyPP,_offsetIpp) ->
                          (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                           { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                               (case (ppDef (ppParens _exprIpp) (ppDef ("+=" >|< ppCurlysCommas' [ppTag tag_,_offsetIpp,ppCNm nm_]) (ppParens _fldExprIpp))) of
                                { _pp ->
                                (case (_pp) of
                                 { _lhsOappFunPP ->
                                 (case ([]) of
                                  { _lhsOlamArgPPL ->
                                  (case (_pp) of
                                   { _lhsOlamBodyPP ->
                                   (case (_pp) of
                                    { _lhsOpp ->
                                    ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_TupUpd :: T_CExpr  ->
                    CTag ->
                    HsName ->
                    T_CExpr  ->
                    T_CExpr  ->
                    T_CExpr 
sem_CExpr_TupUpd expr_ tag_ nm_ offset_ fldExpr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (_lhsIvarPPMp) of
           { _fldExprOvarPPMp ->
           (case (_lhsIlamMp) of
            { _fldExprOlamMp ->
            (case (_lhsIvarPPMp) of
             { _offsetOvarPPMp ->
             (case (_lhsIlamMp) of
              { _offsetOlamMp ->
              (case (_lhsIvarPPMp) of
               { _exprOvarPPMp ->
               (case (_lhsIlamMp) of
                { _exprOlamMp ->
                (case (fldExpr_ _fldExprOlamMp _fldExprOvarPPMp ) of
                 { ( _fldExprIappArgPPL,_fldExprIappFunPP,_fldExprIlamArgPPL,_fldExprIlamBodyPP,_fldExprIpp) ->
                     (case (offset_ _offsetOlamMp _offsetOvarPPMp ) of
                      { ( _offsetIappArgPPL,_offsetIappFunPP,_offsetIlamArgPPL,_offsetIlamBodyPP,_offsetIpp) ->
                          (case (expr_ _exprOlamMp _exprOvarPPMp ) of
                           { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                               (case (ppDef (ppParens _exprIpp) (ppDef (":=" >|< ppCurlysCommas' [ppTag tag_,_offsetIpp,ppCNm nm_]) (ppParens _fldExprIpp))) of
                                { _pp ->
                                (case (_pp) of
                                 { _lhsOappFunPP ->
                                 (case ([]) of
                                  { _lhsOlamArgPPL ->
                                  (case (_pp) of
                                   { _lhsOlamBodyPP ->
                                   (case (_pp) of
                                    { _lhsOpp ->
                                    ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }))
sem_CExpr_Var :: ACoreBindRef ->
                 T_CExpr 
sem_CExpr_Var ref_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOappArgPPL ->
          (case (ppACoreBindRef ppCNm ref_) of
           { _pp ->
           (case (_pp) of
            { _lhsOappFunPP ->
            (case ([]) of
             { _lhsOlamArgPPL ->
             (case (_pp) of
              { _lhsOlamBodyPP ->
              (case (_pp) of
               { _lhsOpp ->
               ( _lhsOappArgPPL,_lhsOappFunPP,_lhsOlamArgPPL,_lhsOlamBodyPP,_lhsOpp) }) }) }) }) }) }))
-- CExprAnn ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Coe:
         child coe            : {RelevCoe}
         visit 0:
            local pp          : _
      alternative Debug:
         child info           : {String}
         visit 0:
            local pp          : _
      alternative Ty:
         child ty             : {Ty}
         visit 0:
            local pp          : _
-}
-- cata
sem_CExprAnn :: CExprAnn  ->
                T_CExprAnn 
sem_CExprAnn (CExprAnn_Coe _coe )  =
    (sem_CExprAnn_Coe _coe )
sem_CExprAnn (CExprAnn_Debug _info )  =
    (sem_CExprAnn_Debug _info )
sem_CExprAnn (CExprAnn_Ty _ty )  =
    (sem_CExprAnn_Ty _ty )
-- semantic domain
type T_CExprAnn  = LamMp ->
                   VarPPMp ->
                   ( PP_Doc)
sem_CExprAnn_Coe :: RelevCoe ->
                    T_CExprAnn 
sem_CExprAnn_Coe coe_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ("~" >#< ppRelevCoe _lhsIvarPPMp coe_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
sem_CExprAnn_Debug :: String ->
                      T_CExprAnn 
sem_CExprAnn_Debug info_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (ppCmt $ pp info_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
sem_CExprAnn_Ty :: Ty ->
                   T_CExprAnn 
sem_CExprAnn_Ty ty_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ("::" >#< ppTyWithCfg' CfgPP_Plain ty_) of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
-- CMetaBind ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lamMp                : LamMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Apply0:
         visit 0:
            local pp          : _
      alternative Function0:
         visit 0:
            local pp          : _
      alternative Function1:
         visit 0:
            local pp          : _
      alternative Plain:
         visit 0:
            local pp          : _
-}
-- cata
sem_CMetaBind :: CMetaBind  ->
                 T_CMetaBind 
sem_CMetaBind (CMetaBind_Apply0 )  =
    (sem_CMetaBind_Apply0 )
sem_CMetaBind (CMetaBind_Function0 )  =
    (sem_CMetaBind_Function0 )
sem_CMetaBind (CMetaBind_Function1 )  =
    (sem_CMetaBind_Function1 )
sem_CMetaBind (CMetaBind_Plain )  =
    (sem_CMetaBind_Plain )
-- semantic domain
type T_CMetaBind  = LamMp ->
                    ( PP_Doc)
sem_CMetaBind_Apply0 :: T_CMetaBind 
sem_CMetaBind_Apply0  =
    (\ _lhsIlamMp ->
         (case (pp "BINDAPPLY0") of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
sem_CMetaBind_Function0 :: T_CMetaBind 
sem_CMetaBind_Function0  =
    (\ _lhsIlamMp ->
         (case (pp "BINDFUNCTION0") of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
sem_CMetaBind_Function1 :: T_CMetaBind 
sem_CMetaBind_Function1  =
    (\ _lhsIlamMp ->
         (case (pp "BINDFUNCTION1") of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
sem_CMetaBind_Plain :: T_CMetaBind 
sem_CMetaBind_Plain  =
    (\ _lhsIlamMp ->
         (case (pp "BINDPLAIN") of
          { _pp ->
          (case (_pp) of
           { _lhsOpp ->
           ( _lhsOpp) }) }))
-- CMetaVal ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lamMp                : LamMp
      synthesized attributes:
         optPP                : PP_Doc
         pp                   : PP_Doc
   alternatives:
      alternative Dict:
         visit 0:
            local pp          : _
            local optPP       : _
      alternative DictClass:
         child tracks         : {[Track]}
         visit 0:
            local pp          : _
            local optPP       : _
      alternative DictInstance:
         child tracks         : {[Track]}
         visit 0:
            local pp          : _
            local optPP       : _
      alternative Track:
         child track          : {Track}
         visit 0:
            local pp          : _
            local optPP       : _
      alternative Val:
         visit 0:
            local optPP       : _
            local pp          : _
-}
-- cata
sem_CMetaVal :: CMetaVal  ->
                T_CMetaVal 
sem_CMetaVal (CMetaVal_Dict )  =
    (sem_CMetaVal_Dict )
sem_CMetaVal (CMetaVal_DictClass _tracks )  =
    (sem_CMetaVal_DictClass _tracks )
sem_CMetaVal (CMetaVal_DictInstance _tracks )  =
    (sem_CMetaVal_DictInstance _tracks )
sem_CMetaVal (CMetaVal_Track _track )  =
    (sem_CMetaVal_Track _track )
sem_CMetaVal (CMetaVal_Val )  =
    (sem_CMetaVal_Val )
-- semantic domain
type T_CMetaVal  = LamMp ->
                   ( PP_Doc,PP_Doc)
sem_CMetaVal_Dict :: T_CMetaVal 
sem_CMetaVal_Dict  =
    (\ _lhsIlamMp ->
         (case (pp "DICT") of
          { _pp ->
          (case (" :" >#< _pp) of
           { _optPP ->
           (case (_optPP) of
            { _lhsOoptPP ->
            (case (_pp) of
             { _lhsOpp ->
             ( _lhsOoptPP,_lhsOpp) }) }) }) }))
sem_CMetaVal_DictClass :: ([Track]) ->
                          T_CMetaVal 
sem_CMetaVal_DictClass tracks_  =
    (\ _lhsIlamMp ->
         (case (pp "DICTCLASS"    >|< ppCurlyList ppTrack tracks_) of
          { _pp ->
          (case (" :" >#< _pp) of
           { _optPP ->
           (case (_optPP) of
            { _lhsOoptPP ->
            (case (_pp) of
             { _lhsOpp ->
             ( _lhsOoptPP,_lhsOpp) }) }) }) }))
sem_CMetaVal_DictInstance :: ([Track]) ->
                             T_CMetaVal 
sem_CMetaVal_DictInstance tracks_  =
    (\ _lhsIlamMp ->
         (case (pp "DICTINSTANCE" >|< ppCurlyList ppTrack tracks_) of
          { _pp ->
          (case (" :" >#< _pp) of
           { _optPP ->
           (case (_optPP) of
            { _lhsOoptPP ->
            (case (_pp) of
             { _lhsOpp ->
             ( _lhsOoptPP,_lhsOpp) }) }) }) }))
sem_CMetaVal_Track :: Track ->
                      T_CMetaVal 
sem_CMetaVal_Track track_  =
    (\ _lhsIlamMp ->
         (case (pp "TRACK"        >|< ppCurlys (show track_)) of
          { _pp ->
          (case (" :" >#< _pp) of
           { _optPP ->
           (case (_optPP) of
            { _lhsOoptPP ->
            (case (_pp) of
             { _lhsOpp ->
             ( _lhsOoptPP,_lhsOpp) }) }) }) }))
sem_CMetaVal_Val :: T_CMetaVal 
sem_CMetaVal_Val  =
    (\ _lhsIlamMp ->
         (case (empty) of
          { _optPP ->
          (case (_optPP) of
           { _lhsOoptPP ->
           (case (pp "VAL") of
            { _pp ->
            (case (_pp) of
             { _lhsOpp ->
             ( _lhsOoptPP,_lhsOpp) }) }) }) }))
-- CMetas ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lamMp                : LamMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Tuple:
         child x1             : CMetaBind 
         child x2             : CMetaVal 
         visit 0:
            local pp          : _
-}
-- cata
sem_CMetas :: CMetas  ->
              T_CMetas 
sem_CMetas ( x1,x2)  =
    (sem_CMetas_Tuple (sem_CMetaBind x1 ) (sem_CMetaVal x2 ) )
-- semantic domain
type T_CMetas  = LamMp ->
                 ( PP_Doc)
data Inh_CMetas  = Inh_CMetas {lamMp_Inh_CMetas :: !(LamMp)}
data Syn_CMetas  = Syn_CMetas {pp_Syn_CMetas :: !(PP_Doc)}
wrap_CMetas :: T_CMetas  ->
               Inh_CMetas  ->
               Syn_CMetas 
wrap_CMetas sem (Inh_CMetas _lhsIlamMp )  =
    (let ( _lhsOpp) = sem _lhsIlamMp 
     in  (Syn_CMetas _lhsOpp ))
sem_CMetas_Tuple :: T_CMetaBind  ->
                    T_CMetaVal  ->
                    T_CMetas 
sem_CMetas_Tuple x1_ x2_  =
    (\ _lhsIlamMp ->
         (case (_lhsIlamMp) of
          { _x2OlamMp ->
          (case (x2_ _x2OlamMp ) of
           { ( _x2IoptPP,_x2Ipp) ->
               (case (_lhsIlamMp) of
                { _x1OlamMp ->
                (case (x1_ _x1OlamMp ) of
                 { ( _x1Ipp) ->
                     (case (ppCurlysCommas [_x1Ipp,_x2Ipp]) of
                      { _pp ->
                      (case (_pp) of
                       { _lhsOpp ->
                       ( _lhsOpp) }) }) }) }) }) }))
-- CModule -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lamMp                : LamMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Mod:
         child moduleNm       : {HsName}
         child expr           : CExpr 
         child ctagsMp        : {CTagsMp}
-}
-- cata
sem_CModule :: CModule  ->
               T_CModule 
sem_CModule (CModule_Mod _moduleNm _expr _ctagsMp )  =
    (sem_CModule_Mod _moduleNm (sem_CExpr _expr ) _ctagsMp )
-- semantic domain
type T_CModule  = LamMp ->
                  ( PP_Doc)
sem_CModule_Mod :: HsName ->
                   T_CExpr  ->
                   CTagsMp ->
                   T_CModule 
sem_CModule_Mod moduleNm_ expr_ ctagsMp_  =
    (\ _lhsIlamMp ->
         (case (_lhsIlamMp) of
          { _exprOlamMp ->
          (case (Map.empty) of
           { _exprOvarPPMp ->
           (case (expr_ _exprOlamMp _exprOvarPPMp ) of
            { ( _exprIappArgPPL,_exprIappFunPP,_exprIlamArgPPL,_exprIlamBodyPP,_exprIpp) ->
                (case ("module" >#< ppCNm moduleNm_ >#< "=" >-< _exprIpp >-< "-- data type tag map:" >-< ppCTagsMp CfgPP_Core ctagsMp_) of
                 { _lhsOpp ->
                 ( _lhsOpp) }) }) }) }))
-- CPat --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         fldNmL               : [HsName]
         pp                   : PP_Doc
   alternatives:
      alternative BoolExpr:
         child cexpr          : {CExpr}
      alternative Char:
         child char           : {Char}
      alternative Con:
         child tag            : {CTag}
         child rest           : CPatRest 
         child binds          : CPatFldL 
      alternative Int:
         child int            : {Int}
      alternative Var:
         child pnm            : {HsName}
-}
-- cata
sem_CPat :: CPat  ->
            T_CPat 
sem_CPat (CPat_BoolExpr _cexpr )  =
    (sem_CPat_BoolExpr _cexpr )
sem_CPat (CPat_Char _char )  =
    (sem_CPat_Char _char )
sem_CPat (CPat_Con _tag _rest _binds )  =
    (sem_CPat_Con _tag (sem_CPatRest _rest ) (sem_CPatFldL _binds ) )
sem_CPat (CPat_Int _int )  =
    (sem_CPat_Int _int )
sem_CPat (CPat_Var _pnm )  =
    (sem_CPat_Var _pnm )
-- semantic domain
type T_CPat  = LamMp ->
               VarPPMp ->
               ( ([HsName]),PP_Doc)
sem_CPat_BoolExpr :: CExpr ->
                     T_CPat 
sem_CPat_BoolExpr cexpr_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOfldNmL ->
          (case (empty) of
           { _lhsOpp ->
           ( _lhsOfldNmL,_lhsOpp) }) }))
sem_CPat_Char :: Char ->
                 T_CPat 
sem_CPat_Char char_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOfldNmL ->
          (case (ppLit "Char"   [char_]) of
           { _lhsOpp ->
           ( _lhsOfldNmL,_lhsOpp) }) }))
sem_CPat_Con :: CTag ->
                T_CPatRest  ->
                T_CPatFldL  ->
                T_CPat 
sem_CPat_Con tag_ rest_ binds_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _bindsOvarPPMp ->
          (case (_lhsIlamMp) of
           { _bindsOlamMp ->
           (case (binds_ _bindsOlamMp _bindsOvarPPMp ) of
            { ( _bindsIfldNmL,_bindsIpp,_bindsIppL) ->
                (case (_bindsIfldNmL) of
                 { _lhsOfldNmL ->
                 (case (_lhsIvarPPMp) of
                  { _restOvarPPMp ->
                  (case (_lhsIlamMp) of
                   { _restOlamMp ->
                   (case (rest_ _restOlamMp _restOvarPPMp ) of
                    { ( _restIpp) ->
                        (case (ppDef ("#Tag" >#< ppTag tag_)
                                     (ppCurly (_restIpp >#< "|" >#< ppCommas' _bindsIppL))) of
                         { _lhsOpp ->
                         ( _lhsOfldNmL,_lhsOpp) }) }) }) }) }) }) }) }))
sem_CPat_Int :: Int ->
                T_CPat 
sem_CPat_Int int_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOfldNmL ->
          (case (ppLit "Int"    (show int_)) of
           { _lhsOpp ->
           ( _lhsOfldNmL,_lhsOpp) }) }))
sem_CPat_Var :: HsName ->
                T_CPat 
sem_CPat_Var pnm_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOfldNmL ->
          (case (ppCNm pnm_) of
           { _lhsOpp ->
           ( _lhsOfldNmL,_lhsOpp) }) }))
-- CPatFld -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         fldNmL               : [HsName]
         pp                   : PP_Doc
   alternatives:
      alternative Fld:
         child lbl            : {HsName}
         child offset         : CExpr 
         child bind           : CBind 
         child fldAnns        : CBindAnnL 
         visit 0:
            local fldNm       : _
-}
-- cata
sem_CPatFld :: CPatFld  ->
               T_CPatFld 
sem_CPatFld (CPatFld_Fld _lbl _offset _bind _fldAnns )  =
    (sem_CPatFld_Fld _lbl (sem_CExpr _offset ) (sem_CBind _bind ) (sem_CBindAnnL _fldAnns ) )
-- semantic domain
type T_CPatFld  = LamMp ->
                  VarPPMp ->
                  ( ([HsName]),PP_Doc)
sem_CPatFld_Fld :: HsName ->
                   T_CExpr  ->
                   T_CBind  ->
                   T_CBindAnnL  ->
                   T_CPatFld 
sem_CPatFld_Fld lbl_ offset_ bind_ fldAnns_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _bindOvarPPMp ->
          (case (_lhsIlamMp) of
           { _bindOlamMp ->
           (case (bind_ _bindOlamMp _bindOvarPPMp ) of
            { ( _bindInm,_bindIpp) ->
                (case (_bindInm) of
                 { _fldNm ->
                 (case ([_fldNm]) of
                  { _lhsOfldNmL ->
                  (case (_lhsIvarPPMp) of
                   { _fldAnnsOvarPPMp ->
                   (case (_lhsIvarPPMp) of
                    { _offsetOvarPPMp ->
                    (case (_lhsIlamMp) of
                     { _offsetOlamMp ->
                     (case (_lhsIlamMp) of
                      { _fldAnnsOlamMp ->
                      (case (fldAnns_ _fldAnnsOlamMp _fldAnnsOvarPPMp ) of
                       { ( _fldAnnsIpp,_fldAnnsIppL) ->
                           (case (offset_ _offsetOlamMp _offsetOvarPPMp ) of
                            { ( _offsetIappArgPPL,_offsetIappFunPP,_offsetIlamArgPPL,_offsetIlamBodyPP,_offsetIpp) ->
                                (case (ppCurlysCommas' [ppCNm lbl_,_offsetIpp                 ] >|< "=" >|< ppCNm _fldNm
                                       >|< (if null _fldAnnsIppL then empty else ppParensCommas' _fldAnnsIppL)) of
                                 { _lhsOpp ->
                                 ( _lhsOfldNmL,_lhsOpp) }) }) }) }) }) }) }) }) }) }) }) }))
-- CPatFldL ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attributes:
         fldNmL               : [HsName]
         pp                   : PP_Doc
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : CPatFld 
         child tl             : CPatFldL 
      alternative Nil:
-}
-- cata
sem_CPatFldL :: CPatFldL  ->
                T_CPatFldL 
sem_CPatFldL list  =
    (Prelude.foldr sem_CPatFldL_Cons sem_CPatFldL_Nil (Prelude.map sem_CPatFld list) )
-- semantic domain
type T_CPatFldL  = LamMp ->
                   VarPPMp ->
                   ( ([HsName]),PP_Doc,([PP_Doc]))
sem_CPatFldL_Cons :: T_CPatFld  ->
                     T_CPatFldL  ->
                     T_CPatFldL 
sem_CPatFldL_Cons hd_ tl_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _tlOvarPPMp ->
          (case (_lhsIlamMp) of
           { _tlOlamMp ->
           (case (tl_ _tlOlamMp _tlOvarPPMp ) of
            { ( _tlIfldNmL,_tlIpp,_tlIppL) ->
                (case (_lhsIvarPPMp) of
                 { _hdOvarPPMp ->
                 (case (_lhsIlamMp) of
                  { _hdOlamMp ->
                  (case (hd_ _hdOlamMp _hdOvarPPMp ) of
                   { ( _hdIfldNmL,_hdIpp) ->
                       (case (_hdIfldNmL ++ _tlIfldNmL) of
                        { _lhsOfldNmL ->
                        (case (_hdIpp >-< _tlIpp) of
                         { _lhsOpp ->
                         (case (_hdIpp : _tlIppL) of
                          { _lhsOppL ->
                          ( _lhsOfldNmL,_lhsOpp,_lhsOppL) }) }) }) }) }) }) }) }) }))
sem_CPatFldL_Nil :: T_CPatFldL 
sem_CPatFldL_Nil  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case ([]) of
          { _lhsOfldNmL ->
          (case (empty) of
           { _lhsOpp ->
           (case ([]) of
            { _lhsOppL ->
            ( _lhsOfldNmL,_lhsOpp,_lhsOppL) }) }) }))
-- CPatRest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Empty:
      alternative Var:
         child nm             : {HsName}
-}
-- cata
sem_CPatRest :: CPatRest  ->
                T_CPatRest 
sem_CPatRest (CPatRest_Empty )  =
    (sem_CPatRest_Empty )
sem_CPatRest (CPatRest_Var _nm )  =
    (sem_CPatRest_Var _nm )
-- semantic domain
type T_CPatRest  = LamMp ->
                   VarPPMp ->
                   ( PP_Doc)
sem_CPatRest_Empty :: T_CPatRest 
sem_CPatRest_Empty  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))
sem_CPatRest_Var :: HsName ->
                    T_CPatRest 
sem_CPatRest_Var nm_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (ppCNm nm_) of
          { _lhsOpp ->
          ( _lhsOpp) }))
-- CodeAGItf ---------------------------------------------------
{-
   visit 0:
      inherited attribute:
         lamMp                : LamMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative AGItf:
         child module         : CModule 
-}
-- cata
sem_CodeAGItf :: CodeAGItf  ->
                 T_CodeAGItf 
sem_CodeAGItf (CodeAGItf_AGItf _module )  =
    (sem_CodeAGItf_AGItf (sem_CModule _module ) )
-- semantic domain
type T_CodeAGItf  = LamMp ->
                    ( PP_Doc)
data Inh_CodeAGItf  = Inh_CodeAGItf {lamMp_Inh_CodeAGItf :: !(LamMp)}
data Syn_CodeAGItf  = Syn_CodeAGItf {pp_Syn_CodeAGItf :: !(PP_Doc)}
wrap_CodeAGItf :: T_CodeAGItf  ->
                  Inh_CodeAGItf  ->
                  Syn_CodeAGItf 
wrap_CodeAGItf sem (Inh_CodeAGItf _lhsIlamMp )  =
    (let ( _lhsOpp) = sem _lhsIlamMp 
     in  (Syn_CodeAGItf _lhsOpp ))
sem_CodeAGItf_AGItf :: T_CModule  ->
                       T_CodeAGItf 
sem_CodeAGItf_AGItf module_  =
    (\ _lhsIlamMp ->
         (case (_lhsIlamMp) of
          { _moduleOlamMp ->
          (case (module_ _moduleOlamMp ) of
           { ( _moduleIpp) ->
               (case (_moduleIpp) of
                { _lhsOpp ->
                ( _lhsOpp) }) }) }))
-- MbCExpr -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         lamMp                : LamMp
         varPPMp              : VarPPMp
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Just:
         child just           : CExpr 
      alternative Nothing:
-}
-- cata
sem_MbCExpr :: MbCExpr  ->
               T_MbCExpr 
sem_MbCExpr (Prelude.Just x )  =
    (sem_MbCExpr_Just (sem_CExpr x ) )
sem_MbCExpr Prelude.Nothing  =
    sem_MbCExpr_Nothing
-- semantic domain
type T_MbCExpr  = LamMp ->
                  VarPPMp ->
                  ( PP_Doc)
sem_MbCExpr_Just :: T_CExpr  ->
                    T_MbCExpr 
sem_MbCExpr_Just just_  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (_lhsIvarPPMp) of
          { _justOvarPPMp ->
          (case (_lhsIlamMp) of
           { _justOlamMp ->
           (case (just_ _justOlamMp _justOvarPPMp ) of
            { ( _justIappArgPPL,_justIappFunPP,_justIlamArgPPL,_justIlamBodyPP,_justIpp) ->
                (case (_justIpp) of
                 { _lhsOpp ->
                 ( _lhsOpp) }) }) }) }))
sem_MbCExpr_Nothing :: T_MbCExpr 
sem_MbCExpr_Nothing  =
    (\ _lhsIlamMp
       _lhsIvarPPMp ->
         (case (empty) of
          { _lhsOpp ->
          ( _lhsOpp) }))