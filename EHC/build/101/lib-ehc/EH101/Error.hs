

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Error.ag)
module EH101.Error(Err (..), ErrL
, ThingAndRange
, ErrSq
, mkThingAnd1Range
, module EH101.Base.Target
, errLIsFatal
, errIsFatal
, mkNestErr, mkNestErr'
, mkErr_NamesNotIntrod, mkErr_NamesNotIntrod') where

import EH101.Base.Common
import EH101.Ty
import EH.Util.Pretty
import qualified EH.Util.FastSeq as Seq
import EH101.Base.Target (FFIWay)
import EH.Util.FPath













type ThingAndRange x = (x,Maybe [(Range,Maybe PP_Doc)])



type ErrSq = Seq.FastSeq Err



errIsFatal :: Err -> Bool
errIsFatal (Err_MissingDataFields _ _ _) = False
errIsFatal _                             = True



errLIsFatal :: [Err] -> Bool
errLIsFatal es = not (null es) && any errIsFatal es



mkNestErr' :: Range -> PP_Doc -> [ErrSq] -> ErrSq
mkNestErr' r wher errs = Seq.fromList $ mkNestErr r wher $ Seq.toList $ Seq.unions errs

mkNestErr :: Range -> PP_Doc -> ErrL -> ErrL
mkNestErr r wher errL | null ({- forceEval -} errL) = []
                      | otherwise             = [Err_NestedIn r wher errL]



mkThingAnd1Range :: Range -> x -> ThingAndRange x
mkThingAnd1Range r x = (x,Just [(r,Nothing)])



mkErr_NamesNotIntrod' :: PP n => Range -> String -> [ThingAndRange n] -> Err
mkErr_NamesNotIntrod' r m ns = Err_NamesNotIntrod r m (assocLMapKey pp ns)

mkErr_NamesNotIntrod :: Range -> String -> [HsName] -> Err
mkErr_NamesNotIntrod r m ns = mkErr_NamesNotIntrod' r m (zip (map pp ns) (repeat Nothing))

-- Err ---------------------------------------------------------
data Err  = Err_AmbigPreds !(Range) !(([(Pred,[Range])])) !((AssocL HsName PP_Doc)) !((AssocL HsName PP_Doc)) 
          | Err_AmbiguousExport !(Range) !(HsName) !(([ThingAndRange HsName])) 
          | Err_AmbiguousNameRef !(Range) !(String) !(String) !(HsName) !(([HsName])) 
          | Err_DeclsNotAllowed !(Range) !(String) !((AssocL IdOccKind [HsName])) 
          | Err_DuplicateDataFields !(Range) !(([HsName])) 
          | Err_EvidenceAltsLeft !(Range) 
          | Err_FileNotFound !(Range) !(String) !(([String])) !(([FileSuffix])) 
          | Err_Fixity !(Range) !(PP_Doc) !(PP_Doc) 
          | Err_FunPatternLengths !(Range) !(HsName) 
          | Err_FusionBuildInverse !(Range) !(Ty) !(Ty) 
          | Err_IllegalFFIWay !(Range) !(FFIWay) 
          | Err_InconsistentHI !(Range) !(String) !(String) !(([String])) !(([String])) 
          | Err_InconsistentIntros !(Range) !(String) !(([HsName])) 
          | Err_MalformedPred !(Range) !(PP_Doc) 
          | Err_MalformedTy !(Range) !(String) !(String) !(Ty) 
          | Err_MayNotHaveMain !(Range) !(HsName) 
          | Err_MayOnlyHaveNrMain !(Range) !(Int) !(([HsName])) !(HsName) 
          | Err_MissingAnyDataField !(Range) !(([HsName])) !(HsName) 
          | Err_MissingDataFields !(Range) !(([HsName])) !(HsName) 
          | Err_MissingRowLabels !(Range) !(([HsName])) !(Ty) 
          | Err_ModNameMismatch !(Range) !(HsName) !(HsName) 
          | Err_MustHaveMain !(Range) 
          | Err_MutRecModules !(Range) !(([[HsName]])) 
          | Err_NamesDupIntrod !(Range) !(String) !(([ThingAndRange HsName])) 
          | Err_NamesNotIntrod !(Range) !(String) !(([ThingAndRange PP_Doc])) 
          | Err_NestedIn !(Range) !(PP_Doc) !(ErrL ) 
          | Err_Newtype !(Range) !(HsName) 
          | Err_NoCoerceDerivation !(Range) !(Ty) !(Ty) 
          | Err_NoDerivFor !(Range) !(PP_Doc) 
          | Err_NoDerivForData !(Range) !(Ty) !(HsName) !(String) 
          | Err_NoMostSpecificPred !(Range) !(Pred) !(Pred) 
          | Err_NotProvenPreds !(Range) !(([((Pred,[Range]),PP_Doc)])) 
          | Err_OccurCycle !(Range) !(TyVarId) !(Ty) 
          | Err_OverlapPreds !(Range) !((AssocL Pred [PP_Doc])) 
          | Err_PP !(Range) !(PP_Doc) 
          | Err_PatArity !(Range) !(Ty) !(Int) 
          | Err_PatArity2 !(Range) !(String) !(PP_Doc) !(Int) 
          | Err_PrfCutOffReached !(Range) !(PredOcc) !(Int) 
          | Err_TooManyRowLabels !(Range) !(([HsName])) !(Ty) 
          | Err_TyBetaRedLimit !(Range) !(Ty) !(Ty) !(Int) 
          | Err_TyCoreMatchClash !(Range) !(PP_Doc) !(PP_Doc) !(PP_Doc) !(PP_Doc) 
          | Err_TyCoreSeqLevels !(Range) !(Int) !(Int) !(PP_Doc) 
          | Err_TyHasFreeTVars !(Range) !(Ty) 
          | Err_UnifyClash !(Range) !(Ty) !(Ty) !(FIMode) !(Ty) !(Ty) !(FIMode) 
          | Err_UnifyOccurs !(Range) !(Ty) !(Ty) !(FIMode) !(TyVarId) !(Ty) !(FIMode) 
          | Err_ValWithoutSig !(Range) !(([HsName])) 
          | Err_WrongMagic !(Range) !(String) !(String) 
          deriving ( Show)
-- ErrL --------------------------------------------------------
type ErrL  = [Err ]