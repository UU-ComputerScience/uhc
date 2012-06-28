

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Error/Pretty.ag)
module EH101.Error.Pretty(ppErr, ppErrs, ppErrL
, mkPPErr) where

import Data.List
import Data.Char
import Data.Maybe
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Common
import EH101.Error
import EH101.Ty
import EH101.Ty.Pretty
import EH.Util.ParseErrPrettyPrint hiding (ppErr)
import EH.Util.FPath
import EH101.Core
import EH101.Core.Pretty













ppErrs :: ErrL -> PP_Doc
ppErrs errL = if null errL then empty else ppCmt ("***ERROR(S):" >-< indent 2 (ppErrL errL))



ppErrL :: ErrL -> PP_Doc
ppErrL errL = if null errL then empty else vlist (map ppErr errL)

ppErr :: Err -> PP_Doc
ppErr err =  let  r = wrap_Err (sem_Err err) (Inh_Err {nestDepth_Inh_Err=0})
             in   pp_Syn_Err r



mkPPErr :: PP a => Range -> a -> Err
mkPPErr r = Err_PP r . pp



ppMsgErr :: PP msg => msg -> PP_Doc -> Range -> PP_Doc
ppMsgErr msg err r
  = mke (msg >|< ":" >-< indent 2 err)
  where mke x | isEmptyRange r = x
              | otherwise      = r  >|< ":" >-< indent 2 x



ppUnifyErr :: PP msg => msg -> Ty -> Ty -> FIMode -> Ty -> Ty -> FIMode -> Range -> PP_Doc
ppUnifyErr msg t1 t2 fim t1d t2d fimd r
  =  ppMsgErr msg    (    "failed to fit:"  >#< (let p1 = ppTy t1
                                                     p2 = m fim  >|< ppTy t2
                                                 in  if tyIsSimple t1 then p1 >|< p2 else p1 >-< p2)
                     >-<  "problem with :"  >#< (ppTy t1d >|< m fimd >|< ppTy t2d)
                     )
                     r
  where  m fim = " " ++ show fim ++ " "



ppNmAndRange :: PP x => [(x,Maybe [(Range,Maybe PP_Doc)])] -> PP_Doc
ppNmAndRange nmL
  = case catMaybes $ map snd $ nmL of
      [] -> ppListSep "" "" ", " $ map fst $ nmL
      _  -> vlist [ n >|< (if null rs then empty else ":" >#< vlist rs)
                  | (n,mbrs) <- nmL
                  , let rs = maybe [] (\rs -> [ maybe (pp r) (\i -> r >#< ppParens i) mbinfo
                                              | (r,mbinfo) <- rs, not (isEmptyRange r)
                                              ]
                                      ) mbrs
                  ]

strCapHeading :: String -> String -> String
strCapHeading kind title@(ht:tt)
  = maybeHd title (const (strCapitalize kind ++ " " ++ [toLower ht] ++ tt)) kind

-- Err ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nestDepth            : Int
      synthesized attributes:
         isNestPP             : Bool
         pp                   : PP_Doc
   alternatives:
      alternative AmbigPreds:
         child range          : {Range}
         child preds          : {[(Pred,[Range])]}
         child inQBinds       : {AssocL HsName PP_Doc}
         child inBinds        : {AssocL HsName PP_Doc}
      alternative AmbiguousExport:
         child range          : {Range}
         child name           : {HsName}
         child entities       : {[ThingAndRange HsName]}
      alternative AmbiguousNameRef:
         child range          : {Range}
         child kindName       : {String}
         child kind           : {String}
         child nm             : {HsName}
         child nmAlts         : {[HsName]}
      alternative DeclsNotAllowed:
         child range          : {Range}
         child inside         : {String}
         child decls          : {AssocL IdOccKind [HsName]}
      alternative DuplicateDataFields:
         child range          : {Range}
         child nmL            : {[HsName]}
      alternative EvidenceAltsLeft:
         child range          : {Range}
      alternative FileNotFound:
         child range          : {Range}
         child fileName       : {String}
         child locations      : {[String]}
         child suffixes       : {[FileSuffix]}
      alternative Fixity:
         child range          : {Range}
         child op1            : {PP_Doc}
         child op2            : {PP_Doc}
      alternative FunPatternLengths:
         child range          : {Range}
         child funNm          : {HsName}
      alternative FusionBuildInverse:
         child range          : {Range}
         child ty1            : {Ty}
         child ty2            : {Ty}
      alternative IllegalFFIWay:
         child range          : {Range}
         child ffiWay         : {FFIWay}
      alternative InconsistentHI:
         child range          : {Range}
         child modNm          : {String}
         child file           : {String}
         child expected       : {[String]}
         child inHI           : {[String]}
      alternative InconsistentIntros:
         child range          : {Range}
         child kind           : {String}
         child nmL            : {[HsName]}
      alternative MalformedPred:
         child range          : {Range}
         child pp             : {PP_Doc}
      alternative MalformedTy:
         child range          : {Range}
         child kind           : {String}
         child purpose        : {String}
         child ty             : {Ty}
      alternative MayNotHaveMain:
         child range          : {Range}
         child modNm          : {HsName}
      alternative MayOnlyHaveNrMain:
         child range          : {Range}
         child nrAllowed      : {Int}
         child prevModNmL     : {[HsName]}
         child modNm          : {HsName}
      alternative MissingAnyDataField:
         child range          : {Range}
         child nmL            : {[HsName]}
         child tyNm           : {HsName}
      alternative MissingDataFields:
         child range          : {Range}
         child nmL            : {[HsName]}
         child con            : {HsName}
      alternative MissingRowLabels:
         child range          : {Range}
         child nmL            : {[HsName]}
         child ty             : {Ty}
      alternative ModNameMismatch:
         child range          : {Range}
         child nmOfFile       : {HsName}
         child nmFromSrc      : {HsName}
      alternative MustHaveMain:
         child range          : {Range}
      alternative MutRecModules:
         child range          : {Range}
         child mutRecL        : {[[HsName]]}
      alternative NamesDupIntrod:
         child range          : {Range}
         child kind           : {String}
         child nmL            : {[ThingAndRange HsName]}
      alternative NamesNotIntrod:
         child range          : {Range}
         child kind           : {String}
         child nmL            : {[ThingAndRange PP_Doc]}
      alternative NestedIn:
         child range          : {Range}
         child wher           : {PP_Doc}
         child errL           : ErrL 
         visit 0:
            local pp          : _
      alternative Newtype:
         child range          : {Range}
         child tyNm           : {HsName}
      alternative NoCoerceDerivation:
         child range          : {Range}
         child func           : {Ty}
         child arg            : {Ty}
      alternative NoDerivFor:
         child range          : {Range}
         child pred           : {PP_Doc}
      alternative NoDerivForData:
         child range          : {Range}
         child ty             : {Ty}
         child clNm           : {HsName}
         child reason         : {String}
      alternative NoMostSpecificPred:
         child range          : {Range}
         child pred1          : {Pred}
         child pred2          : {Pred}
      alternative NotProvenPreds:
         child range          : {Range}
         child preds          : {[((Pred,[Range]),PP_Doc)]}
      alternative OccurCycle:
         child range          : {Range}
         child tvar           : {TyVarId}
         child ty             : {Ty}
      alternative OverlapPreds:
         child range          : {Range}
         child overl          : {AssocL Pred [PP_Doc]}
      alternative PP:
         child range          : {Range}
         child pp             : {PP_Doc}
      alternative PatArity:
         child range          : {Range}
         child ty             : {Ty}
         child arity          : {Int}
      alternative PatArity2:
         child range          : {Range}
         child kind           : {String}
         child what           : {PP_Doc}
         child arity          : {Int}
      alternative PrfCutOffReached:
         child range          : {Range}
         child pred           : {PredOcc}
         child depth          : {Int}
      alternative TooManyRowLabels:
         child range          : {Range}
         child nmL            : {[HsName]}
         child ty             : {Ty}
      alternative TyBetaRedLimit:
         child range          : {Range}
         child ty             : {Ty}
         child tyTo           : {Ty}
         child limit          : {Int}
      alternative TyCoreMatchClash:
         child range          : {Range}
         child ty1            : {PP_Doc}
         child ty2            : {PP_Doc}
         child ty1detail      : {PP_Doc}
         child ty2detail      : {PP_Doc}
      alternative TyCoreSeqLevels:
         child range          : {Range}
         child hereLev        : {Int}
         child mustLev        : {Int}
         child ty             : {PP_Doc}
      alternative TyHasFreeTVars:
         child range          : {Range}
         child ty             : {Ty}
      alternative UnifyClash:
         child range          : {Range}
         child ty1            : {Ty}
         child ty2            : {Ty}
         child fiMode         : {FIMode}
         child ty1detail      : {Ty}
         child ty2detail      : {Ty}
         child fiModeD        : {FIMode}
      alternative UnifyOccurs:
         child range          : {Range}
         child ty1            : {Ty}
         child ty2            : {Ty}
         child fiMode         : {FIMode}
         child tvar           : {TyVarId}
         child ty2detail      : {Ty}
         child fiModeD        : {FIMode}
      alternative ValWithoutSig:
         child range          : {Range}
         child nmL            : {[HsName]}
      alternative WrongMagic:
         child range          : {Range}
         child modNm          : {String}
         child file           : {String}
-}
-- cata
sem_Err :: Err  ->
           T_Err 
sem_Err (Err_AmbigPreds _range _preds _inQBinds _inBinds )  =
    (sem_Err_AmbigPreds _range _preds _inQBinds _inBinds )
sem_Err (Err_AmbiguousExport _range _name _entities )  =
    (sem_Err_AmbiguousExport _range _name _entities )
sem_Err (Err_AmbiguousNameRef _range _kindName _kind _nm _nmAlts )  =
    (sem_Err_AmbiguousNameRef _range _kindName _kind _nm _nmAlts )
sem_Err (Err_DeclsNotAllowed _range _inside _decls )  =
    (sem_Err_DeclsNotAllowed _range _inside _decls )
sem_Err (Err_DuplicateDataFields _range _nmL )  =
    (sem_Err_DuplicateDataFields _range _nmL )
sem_Err (Err_EvidenceAltsLeft _range )  =
    (sem_Err_EvidenceAltsLeft _range )
sem_Err (Err_FileNotFound _range _fileName _locations _suffixes )  =
    (sem_Err_FileNotFound _range _fileName _locations _suffixes )
sem_Err (Err_Fixity _range _op1 _op2 )  =
    (sem_Err_Fixity _range _op1 _op2 )
sem_Err (Err_FunPatternLengths _range _funNm )  =
    (sem_Err_FunPatternLengths _range _funNm )
sem_Err (Err_FusionBuildInverse _range _ty1 _ty2 )  =
    (sem_Err_FusionBuildInverse _range _ty1 _ty2 )
sem_Err (Err_IllegalFFIWay _range _ffiWay )  =
    (sem_Err_IllegalFFIWay _range _ffiWay )
sem_Err (Err_InconsistentHI _range _modNm _file _expected _inHI )  =
    (sem_Err_InconsistentHI _range _modNm _file _expected _inHI )
sem_Err (Err_InconsistentIntros _range _kind _nmL )  =
    (sem_Err_InconsistentIntros _range _kind _nmL )
sem_Err (Err_MalformedPred _range _pp )  =
    (sem_Err_MalformedPred _range _pp )
sem_Err (Err_MalformedTy _range _kind _purpose _ty )  =
    (sem_Err_MalformedTy _range _kind _purpose _ty )
sem_Err (Err_MayNotHaveMain _range _modNm )  =
    (sem_Err_MayNotHaveMain _range _modNm )
sem_Err (Err_MayOnlyHaveNrMain _range _nrAllowed _prevModNmL _modNm )  =
    (sem_Err_MayOnlyHaveNrMain _range _nrAllowed _prevModNmL _modNm )
sem_Err (Err_MissingAnyDataField _range _nmL _tyNm )  =
    (sem_Err_MissingAnyDataField _range _nmL _tyNm )
sem_Err (Err_MissingDataFields _range _nmL _con )  =
    (sem_Err_MissingDataFields _range _nmL _con )
sem_Err (Err_MissingRowLabels _range _nmL _ty )  =
    (sem_Err_MissingRowLabels _range _nmL _ty )
sem_Err (Err_ModNameMismatch _range _nmOfFile _nmFromSrc )  =
    (sem_Err_ModNameMismatch _range _nmOfFile _nmFromSrc )
sem_Err (Err_MustHaveMain _range )  =
    (sem_Err_MustHaveMain _range )
sem_Err (Err_MutRecModules _range _mutRecL )  =
    (sem_Err_MutRecModules _range _mutRecL )
sem_Err (Err_NamesDupIntrod _range _kind _nmL )  =
    (sem_Err_NamesDupIntrod _range _kind _nmL )
sem_Err (Err_NamesNotIntrod _range _kind _nmL )  =
    (sem_Err_NamesNotIntrod _range _kind _nmL )
sem_Err (Err_NestedIn _range _wher _errL )  =
    (sem_Err_NestedIn _range _wher (sem_ErrL _errL ) )
sem_Err (Err_Newtype _range _tyNm )  =
    (sem_Err_Newtype _range _tyNm )
sem_Err (Err_NoCoerceDerivation _range _func _arg )  =
    (sem_Err_NoCoerceDerivation _range _func _arg )
sem_Err (Err_NoDerivFor _range _pred )  =
    (sem_Err_NoDerivFor _range _pred )
sem_Err (Err_NoDerivForData _range _ty _clNm _reason )  =
    (sem_Err_NoDerivForData _range _ty _clNm _reason )
sem_Err (Err_NoMostSpecificPred _range _pred1 _pred2 )  =
    (sem_Err_NoMostSpecificPred _range _pred1 _pred2 )
sem_Err (Err_NotProvenPreds _range _preds )  =
    (sem_Err_NotProvenPreds _range _preds )
sem_Err (Err_OccurCycle _range _tvar _ty )  =
    (sem_Err_OccurCycle _range _tvar _ty )
sem_Err (Err_OverlapPreds _range _overl )  =
    (sem_Err_OverlapPreds _range _overl )
sem_Err (Err_PP _range _pp )  =
    (sem_Err_PP _range _pp )
sem_Err (Err_PatArity _range _ty _arity )  =
    (sem_Err_PatArity _range _ty _arity )
sem_Err (Err_PatArity2 _range _kind _what _arity )  =
    (sem_Err_PatArity2 _range _kind _what _arity )
sem_Err (Err_PrfCutOffReached _range _pred _depth )  =
    (sem_Err_PrfCutOffReached _range _pred _depth )
sem_Err (Err_TooManyRowLabels _range _nmL _ty )  =
    (sem_Err_TooManyRowLabels _range _nmL _ty )
sem_Err (Err_TyBetaRedLimit _range _ty _tyTo _limit )  =
    (sem_Err_TyBetaRedLimit _range _ty _tyTo _limit )
sem_Err (Err_TyCoreMatchClash _range _ty1 _ty2 _ty1detail _ty2detail )  =
    (sem_Err_TyCoreMatchClash _range _ty1 _ty2 _ty1detail _ty2detail )
sem_Err (Err_TyCoreSeqLevels _range _hereLev _mustLev _ty )  =
    (sem_Err_TyCoreSeqLevels _range _hereLev _mustLev _ty )
sem_Err (Err_TyHasFreeTVars _range _ty )  =
    (sem_Err_TyHasFreeTVars _range _ty )
sem_Err (Err_UnifyClash _range _ty1 _ty2 _fiMode _ty1detail _ty2detail _fiModeD )  =
    (sem_Err_UnifyClash _range _ty1 _ty2 _fiMode _ty1detail _ty2detail _fiModeD )
sem_Err (Err_UnifyOccurs _range _ty1 _ty2 _fiMode _tvar _ty2detail _fiModeD )  =
    (sem_Err_UnifyOccurs _range _ty1 _ty2 _fiMode _tvar _ty2detail _fiModeD )
sem_Err (Err_ValWithoutSig _range _nmL )  =
    (sem_Err_ValWithoutSig _range _nmL )
sem_Err (Err_WrongMagic _range _modNm _file )  =
    (sem_Err_WrongMagic _range _modNm _file )
-- semantic domain
type T_Err  = Int ->
              ( Bool,PP_Doc)
data Inh_Err  = Inh_Err {nestDepth_Inh_Err :: !(Int)}
data Syn_Err  = Syn_Err {isNestPP_Syn_Err :: !(Bool),pp_Syn_Err :: !(PP_Doc)}
wrap_Err :: T_Err  ->
            Inh_Err  ->
            Syn_Err 
wrap_Err sem (Inh_Err _lhsInestDepth )  =
    (let ( _lhsOisNestPP,_lhsOpp) = sem _lhsInestDepth 
     in  (Syn_Err _lhsOisNestPP _lhsOpp ))
sem_Err_AmbigPreds :: Range ->
                      ([(Pred,[Range])]) ->
                      (AssocL HsName PP_Doc) ->
                      (AssocL HsName PP_Doc) ->
                      T_Err 
sem_Err_AmbigPreds range_ preds_ inQBinds_ inBinds_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Predicates leading to ambiguous type"
                             (    "preds                :" >#< (ppAssocLV $ assocLMapElt vlist preds_)
                             >-<  "bindings             :" >#< ppAssocLV inBinds_
                             >-<  "bindings (quantified):" >#< ppAssocLV inQBinds_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_AmbiguousExport :: Range ->
                           HsName ->
                           ([ThingAndRange HsName]) ->
                           T_Err 
sem_Err_AmbiguousExport range_ name_ entities_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Ambiguous export"
                             (    "name   :" >#< name_
                              >-< "exports:" >#< ppNmAndRange entities_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_AmbiguousNameRef :: Range ->
                            String ->
                            String ->
                            HsName ->
                            ([HsName]) ->
                            T_Err 
sem_Err_AmbiguousNameRef range_ kindName_ kind_ nm_ nmAlts_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Ambiguous " ++ kind_ ++ " name reference")
                             (    (take 12 (kindName_ ++ repeat ' ') ++
                                              ":") >#< nm_
                              >-< "may refer to:"  >#< vlist (map pp nmAlts_)
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_DeclsNotAllowed :: Range ->
                           String ->
                           (AssocL IdOccKind [HsName]) ->
                           T_Err 
sem_Err_DeclsNotAllowed range_ inside_ decls_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Declarations are not allowed " ++ inside_)
                             (vlist [ k >|< ":" >#< ppCommas ns | (k,ns) <- decls_ ]
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_DuplicateDataFields :: Range ->
                               ([HsName]) ->
                               T_Err 
sem_Err_DuplicateDataFields range_ nmL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Duplicate field(s) in data construction/update")
                             ("Field(s):" >#< ppCommas' nmL_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_EvidenceAltsLeft :: Range ->
                            T_Err 
sem_Err_EvidenceAltsLeft range_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Evidence alternatives left (TBD: more info)"
                             (    empty
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_FileNotFound :: Range ->
                        String ->
                        ([String]) ->
                        ([FileSuffix]) ->
                        T_Err 
sem_Err_FileNotFound range_ fileName_ locations_ suffixes_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "File not found"
                             (    "file name         :" >#< fileName_
                              >-< "searched locations:" >#< vlist (map (text.show) locations_)
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_Fixity :: Range ->
                  PP_Doc ->
                  PP_Doc ->
                  T_Err 
sem_Err_Fixity range_ op1_ op2_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Fixity mismatch between" (ppListSep "" "" ", " [op1_,op2_])
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_FunPatternLengths :: Range ->
                             HsName ->
                             T_Err 
sem_Err_FunPatternLengths range_ funNm_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Nr of arguments to function must be equal for all function alternatives")
                             ("Function:" >#< funNm_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_FusionBuildInverse :: Range ->
                              Ty ->
                              Ty ->
                              T_Err 
sem_Err_FusionBuildInverse range_ ty1_ ty2_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Fusion build/unbuild pair are not each others type level inverse")
                             (    "type 1        :" >#< ty1_
                              >-< "type 2 inverse:" >#< ty2_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_IllegalFFIWay :: Range ->
                         FFIWay ->
                         T_Err 
sem_Err_IllegalFFIWay range_ ffiWay_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Illegal foreign interface"
                             (    "to:" >#< ffiWay_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_InconsistentHI :: Range ->
                          String ->
                          String ->
                          ([String]) ->
                          ([String]) ->
                          T_Err 
sem_Err_InconsistentHI range_ modNm_ file_ expected_ inHI_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    (".hi file cannot be used with this compiler")
                             (    "module              :" >#< modNm_
                              >-< "file                :" >#< file_
                              >-< "this compiler       :" >#< (concat $ intersperse " / " expected_)
                              >-< "compiler of .hi file:" >#< (concat $ intersperse " / " inHI_    )
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_InconsistentIntros :: Range ->
                              String ->
                              ([HsName]) ->
                              T_Err 
sem_Err_InconsistentIntros range_ kind_ nmL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Inconsistent " ++ kind_ ++ " introductions for")
                             ("names:" >#< ppCommas' nmL_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MalformedPred :: Range ->
                         PP_Doc ->
                         T_Err 
sem_Err_MalformedPred range_ pp_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Type cannot be parsed as context")
                             (    "type:" >#< pp_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MalformedTy :: Range ->
                       String ->
                       String ->
                       Ty ->
                       T_Err 
sem_Err_MalformedTy range_ kind_ purpose_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Malformed " ++ kind_ ++ " for " ++ purpose_)
                             (    kind_ >|< ":" >#< ppTy ty_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MayNotHaveMain :: Range ->
                          HsName ->
                          T_Err 
sem_Err_MayNotHaveMain range_ modNm_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "May not have a 'main'"
                             ("module:" >#< modNm_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MayOnlyHaveNrMain :: Range ->
                             Int ->
                             ([HsName]) ->
                             HsName ->
                             T_Err 
sem_Err_MayOnlyHaveNrMain range_ nrAllowed_ prevModNmL_ modNm_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Number of definitions for 'main' exceeds limit"
                             (                                         "limit                :" >#< nrAllowed_
                              >-<                                      "module               :" >#< modNm_
                              >-< (if null prevModNmL_ then empty else "previously defined in:" >#< ppCommas' prevModNmL_)
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MissingAnyDataField :: Range ->
                               ([HsName]) ->
                               HsName ->
                               T_Err 
sem_Err_MissingAnyDataField range_ nmL_ tyNm_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("No data constructor has all fields")
                             ("Field(s):" >#< ppCommas' nmL_ >-< "Type    :" >#< pp tyNm_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MissingDataFields :: Range ->
                             ([HsName]) ->
                             HsName ->
                             T_Err 
sem_Err_MissingDataFields range_ nmL_ con_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Missing field(s) in data construction")
                             ("Field(s)   :" >#< ppCommas' nmL_ >-< "Constructor:" >#< pp con_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MissingRowLabels :: Range ->
                            ([HsName]) ->
                            Ty ->
                            T_Err 
sem_Err_MissingRowLabels range_ nmL_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Missing label(s) in row")
                             ("Label(s):" >#< ppCommas' nmL_ >-< "Row     :" >#< ppTy ty_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_ModNameMismatch :: Range ->
                           HsName ->
                           HsName ->
                           T_Err 
sem_Err_ModNameMismatch range_ nmOfFile_ nmFromSrc_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Module names do not match"
                             (    "filename        :" >#< nmOfFile_
                              >-< "name from source:" >#< nmFromSrc_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MustHaveMain :: Range ->
                        T_Err 
sem_Err_MustHaveMain range_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "No 'main' defined"
                             empty
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_MutRecModules :: Range ->
                         ([[HsName]]) ->
                         T_Err 
sem_Err_MutRecModules range_ mutRecL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Mutually recursive modules"
                             (    "modules:" >#< vlist (map ppCommas' mutRecL_)
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NamesDupIntrod :: Range ->
                          String ->
                          ([ThingAndRange HsName]) ->
                          T_Err 
sem_Err_NamesDupIntrod range_ kind_ nmL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Name(s) for " ++ kind_ ++ "(s) introduced more than once")
                             (ppNmAndRange nmL_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NamesNotIntrod :: Range ->
                          String ->
                          ([ThingAndRange PP_Doc]) ->
                          T_Err 
sem_Err_NamesNotIntrod range_ kind_ nmL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    (strCapHeading kind_ "Names not in scope")
                             (ppNmAndRange nmL_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NestedIn :: Range ->
                    PP_Doc ->
                    T_ErrL  ->
                    T_Err 
sem_Err_NestedIn range_ wher_ errL_  =
    (\ _lhsInestDepth ->
         (case (True) of
          { _lhsOisNestPP ->
          (case (errL_ ) of
           { ( _errLIisEmpty,errL_1) ->
               (case (if _errLIisEmpty then 0 else _lhsInestDepth+1) of
                { _errLOnestDepth ->
                (case (errL_1 _errLOnestDepth ) of
                 { ( _errLIisNestPP,_errLIppL) ->
                     (case (let  h =  if _lhsInestDepth == 1 && _errLIisNestPP
                                      then (text "..." >#<)
                                      else if _lhsInestDepth <= 1 || _lhsInestDepth >= 1 && not _errLIisNestPP
                                      then \x -> ppMsgErr ("In `" >|< wher_ >|< "'") x emptyRange
                                      else id
                            in   h (vlist _errLIppL)) of
                      { _pp ->
                      (case (_pp) of
                       { _lhsOpp ->
                       ( _lhsOisNestPP,_lhsOpp) }) }) }) }) }) }))
sem_Err_Newtype :: Range ->
                   HsName ->
                   T_Err 
sem_Err_Newtype range_ tyNm_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Newtype must have exactly 1 constructor with 1 field")
                             ("Type:" >#< tyNm_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NoCoerceDerivation :: Range ->
                              Ty ->
                              Ty ->
                              T_Err 
sem_Err_NoCoerceDerivation range_ func_ arg_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Cannot derive coercion for type application"
                             (    "type function:" >#< pp func_
                              >-< "type arg     :" >#< pp arg_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NoDerivFor :: Range ->
                      PP_Doc ->
                      T_Err 
sem_Err_NoDerivFor range_ pred_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("No deriving for")
                             (    "predicate:" >#< pred_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NoDerivForData :: Range ->
                          Ty ->
                          HsName ->
                          String ->
                          T_Err 
sem_Err_NoDerivForData range_ ty_ clNm_ reason_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Data type has wrong structure for deriving")
                             (    "data type :" >#< ty_
                              >-< "class name:" >#< clNm_
                              >-< "because   :" >#< reason_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NoMostSpecificPred :: Range ->
                              Pred ->
                              Pred ->
                              T_Err 
sem_Err_NoMostSpecificPred range_ pred1_ pred2_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Cannot determine most specific predicate"
                             (    "preds:" >#< vlist [pred1_,pred2_]
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_NotProvenPreds :: Range ->
                          ([((Pred,[Range]),PP_Doc)]) ->
                          T_Err 
sem_Err_NotProvenPreds range_ preds_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Predicates remain unproven"
                             (    "preds:" >#< (vlist [ pp p >-< indent 2 (("at   :" >#< vlist r) >-< ("trace:" >#< t)) | ((p,r),t) <- preds_ ] )
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_OccurCycle :: Range ->
                      TyVarId ->
                      Ty ->
                      T_Err 
sem_Err_OccurCycle range_ tvar_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Occur cycle"
                             (    "type var:" >#< pp tvar_
                              >-< "to type :" >#< pp ty_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_OverlapPreds :: Range ->
                        (AssocL Pred [PP_Doc]) ->
                        T_Err 
sem_Err_OverlapPreds range_ overl_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Proofs for predicates overlap"
                             (    vlist . map (\(p,evs) -> p >#< ":" >#< ppBracketsCommas evs) $ overl_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_PP :: Range ->
              PP_Doc ->
              T_Err 
sem_Err_PP range_ pp_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (pp_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_PatArity :: Range ->
                    Ty ->
                    Int ->
                    T_Err 
sem_Err_PatArity range_ ty_ arity_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Type has wrong arity for pattern")
                             ("type :" >#< ty_ >-< "arity:" >#< arity_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_PatArity2 :: Range ->
                     String ->
                     PP_Doc ->
                     Int ->
                     T_Err 
sem_Err_PatArity2 range_ kind_ what_ arity_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    (strCapHeading kind_ "has wrong arity for pattern")
                             (kind_ >#< ":" >#< what_ >-< "arity:" >#< arity_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_PrfCutOffReached :: Range ->
                            PredOcc ->
                            Int ->
                            T_Err 
sem_Err_PrfCutOffReached range_ pred_ depth_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Proof cut off limit reached"
                             (    "limit:" >#< pp depth_
                              >-< "pred :" >#< pp pred_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_TooManyRowLabels :: Range ->
                            ([HsName]) ->
                            Ty ->
                            T_Err 
sem_Err_TooManyRowLabels range_ nmL_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Too many label(s) to fit in row"
                             ("Label(s):" >#< ppCommas' nmL_ >-< "Row     :" >#< ppTy ty_)
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_TyBetaRedLimit :: Range ->
                          Ty ->
                          Ty ->
                          Int ->
                          T_Err 
sem_Err_TyBetaRedLimit range_ ty_ tyTo_ limit_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Type synonym expansion limit reached"
                             (    "limit          :" >#< pp limit_
                              >-< "type           :" >#< pp ty_
                              >-< "last expansion :" >#< pp tyTo_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_TyCoreMatchClash :: Range ->
                            PP_Doc ->
                            PP_Doc ->
                            PP_Doc ->
                            PP_Doc ->
                            T_Err 
sem_Err_TyCoreMatchClash range_ ty1_ ty2_ ty1detail_ ty2detail_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "TyCore type mismatch"
                             (    "between          :" >#< ty1_
                              >-< "and              :" >#< ty2_
                              >-< "in detail between:" >#< ty1detail_
                              >-< "and              :" >#< ty2detail_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_TyCoreSeqLevels :: Range ->
                           Int ->
                           Int ->
                           PP_Doc ->
                           T_Err 
sem_Err_TyCoreSeqLevels range_ hereLev_ mustLev_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "TyCore sequence nr of levels error"
                             (    "#levels        :" >#< hereLev_
                              >-< "must be #levels:" >#< mustLev_
                              >-< "in             :" >#< ty_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_TyHasFreeTVars :: Range ->
                          Ty ->
                          T_Err 
sem_Err_TyHasFreeTVars range_ ty_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    "Type has free type variables (not allowed)"
                             (    "type:" >#< pp ty_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_UnifyClash :: Range ->
                      Ty ->
                      Ty ->
                      FIMode ->
                      Ty ->
                      Ty ->
                      FIMode ->
                      T_Err 
sem_Err_UnifyClash range_ ty1_ ty2_ fiMode_ ty1detail_ ty2detail_ fiModeD_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppUnifyErr  "Type clash"
                             ty1_ ty2_ fiMode_ ty1detail_ ty2detail_ fiModeD_
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_UnifyOccurs :: Range ->
                       Ty ->
                       Ty ->
                       FIMode ->
                       TyVarId ->
                       Ty ->
                       FIMode ->
                       T_Err 
sem_Err_UnifyOccurs range_ ty1_ ty2_ fiMode_ tvar_ ty2detail_ fiModeD_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppUnifyErr  "Infinite type"
                             ty1_ ty2_ fiMode_ (mkTyVar tvar_) ty2detail_ fiModeD_
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_ValWithoutSig :: Range ->
                         ([HsName]) ->
                         T_Err 
sem_Err_ValWithoutSig range_ nmL_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    ("Instance members without corresponding signature from class")
                             (    "names:" >#< ppCommas nmL_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
sem_Err_WrongMagic :: Range ->
                      String ->
                      String ->
                      T_Err 
sem_Err_WrongMagic range_ modNm_ file_  =
    (\ _lhsInestDepth ->
         (case (False) of
          { _lhsOisNestPP ->
          (case (ppMsgErr    (".hi file has wrong magic number")
                             (    "module              :" >#< modNm_
                              >-< "file                :" >#< file_
                             )
                             range_) of
           { _lhsOpp ->
           ( _lhsOisNestPP,_lhsOpp) }) }))
-- ErrL --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         isEmpty              : Bool
   visit 1:
      inherited attribute:
         nestDepth            : Int
      synthesized attributes:
         isNestPP             : Bool
         ppL                  : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Err 
         child tl             : ErrL 
      alternative Nil:
-}
-- cata
sem_ErrL :: ErrL  ->
            T_ErrL 
sem_ErrL list  =
    (Prelude.foldr sem_ErrL_Cons sem_ErrL_Nil (Prelude.map sem_Err list) )
-- semantic domain
type T_ErrL  = ( Bool,T_ErrL_1 )
type T_ErrL_1  = Int ->
                 ( Bool,([PP_Doc]))
sem_ErrL_Cons :: T_Err  ->
                 T_ErrL  ->
                 T_ErrL 
sem_ErrL_Cons hd_ tl_  =
    (case (False) of
     { _lhsOisEmpty ->
     (case ((let sem_ErrL_Cons_1 :: T_ErrL_1 
                 sem_ErrL_Cons_1  =
                     (\ _lhsInestDepth ->
                          (case (tl_ ) of
                           { ( _tlIisEmpty,tl_1) ->
                               (case (_lhsInestDepth) of
                                { _tlOnestDepth ->
                                (case (tl_1 _tlOnestDepth ) of
                                 { ( _tlIisNestPP,_tlIppL) ->
                                     (case (_lhsInestDepth) of
                                      { _hdOnestDepth ->
                                      (case (hd_ _hdOnestDepth ) of
                                       { ( _hdIisNestPP,_hdIpp) ->
                                           (case (_hdIisNestPP && _tlIisNestPP) of
                                            { _lhsOisNestPP ->
                                            (case (_hdIpp : _tlIppL) of
                                             { _lhsOppL ->
                                             ( _lhsOisNestPP,_lhsOppL) }) }) }) }) }) }) }))
             in  sem_ErrL_Cons_1)) of
      { ( sem_ErrL_1) ->
      ( _lhsOisEmpty,sem_ErrL_1) }) })
sem_ErrL_Nil :: T_ErrL 
sem_ErrL_Nil  =
    (case (True) of
     { _lhsOisEmpty ->
     (case ((let sem_ErrL_Nil_1 :: T_ErrL_1 
                 sem_ErrL_Nil_1  =
                     (\ _lhsInestDepth ->
                          (case (True) of
                           { _lhsOisNestPP ->
                           (case ([]) of
                            { _lhsOppL ->
                            ( _lhsOisNestPP,_lhsOppL) }) }))
             in  sem_ErrL_Nil_1)) of
      { ( sem_ErrL_1) ->
      ( _lhsOisEmpty,sem_ErrL_1) }) })