

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/EH.ag)
module EH101.EH(AGItf (..), Decl (..), Decls, TyExpr (..), Expr (..), PatExpr (..)
, ExprAnn (..), PatExprAnn (..), TyExprAnn (..)
, mkLet
, TyExprs, TyVar (..), TyVars, CaseAlt (..), CaseAlts, DataConstr (..), DataConstrs
, mkCase, mkCase', mkIf, mkIf', mkError, mkStr, mkAlt
, KiExpr (..), KiExprAnn (..)
, RecExpr (..), RecPatExpr (..), RowTyExpr (..), DataField (..), DataFields, DataFieldExpr (..), DataFieldPatExpr (..)
, module EH101.Base.Target
, mkLet', mkIf''
, ehIsDeclVal
, PrExpr (..), PrExprs
, mkUnit, mkError'
, FuncDep (..), FuncDeps
, MbTyExpr
, mkEH) where

import EH101.Base.Common
import EH101.Base.Builtin
import EH101.HS (Range)
import EH101.Ty
import qualified Data.Set as Set
import EH101.Base.Target (FFIWay)
import EH101.Foreign









































ehIsDeclVal :: Decl -> Bool
ehIsDeclVal (Decl_Val _ _ _) = True
ehIsDeclVal _                = False



mkLet' :: Bool -> Range -> Maybe [Decl] -> Expr -> Expr
mkLet' isStrict r md e
  = case md of
      Just d@(_:_) -> rngLift r Expr_Let isStrict d e
      _            -> e

mkLet :: Range -> Maybe [Decl] -> Expr -> Expr
mkLet = mkLet' False



mkUnit :: Range -> Expr
mkUnit r = rngLift r Expr_Rec (rngLift r RecExpr_Empty)



mkError':: HsName -> Range -> String -> Expr
mkError' e r m
  = mkApp [rngLift r Expr_Var e,mkStr r (show r ++ ": " ++ m)]

mkError :: Range -> String -> Expr
mkError = mkError' hsnError

mkStr :: Range -> String -> Expr
mkStr r s
  = rngLift r Expr_SConst s



mkIf'' :: [HsName] -> Range -> Expr -> Expr -> Expr -> Maybe UIDS -> UIDS -> Expr
mkIf'' (bn:tn:fn:_) r c t e i failS
  = case c of
      Expr_Con _ n
          | n == tn -> t
          | n == fn -> e
      _ -> mkCase' r
             ( rngLift r Expr_TypeAs (rngLift r TyExpr_Con bn) c )
             i failS False
             [ mkAlt r (rngLift r PatExpr_Con tn) t
             , mkAlt r (rngLift r PatExpr_Con fn) e
             ]



mkIf' :: [HsName] -> Range -> Expr -> Expr -> Expr -> Expr
mkIf' ns r c t e = mkIf'' ns r c t e Nothing Set.empty



mkIf :: Range -> Expr -> Expr -> Expr -> Expr
mkIf = mkIf' [hsnBool,hsnTrue,hsnFalse]



mkAlt :: Range -> PatExpr -> Expr -> CaseAlt
mkAlt r p e = rngLift r CaseAlt_Pat p e



mkCase' :: Range -> Expr  -> Maybe UIDS -> UIDS -> Bool -> [CaseAlt] -> Expr
mkCase' r c id failS istup as
  = rngLift r Expr_Case c as id failS istup

mkCase :: Range -> Expr -> [(PatExpr,Expr)] -> Expr
mkCase r c as
  = mkCase' r c Nothing Set.empty False [ mkAlt r p e | (p,e) <- as ]



mkEH :: (Range -> x) -> x
mkEH = rngLift emptyRange



instance SemApp Expr where
  semRngApp    r  = rngLift r Expr_App
  semRngAppTop r  = rngLift r Expr_AppTop
  semRngVar    r  = rngLift r Expr_Var . mkHNm
  semRngCon    r  = rngLift r Expr_Con . mkHNm
  semRngParens r  = rngLift r Expr_Parens
  mkProdApp l
    = mkEH Expr_Rec $ mkEH RecExpr_Empty `mkRow` l
    where mkRow = foldl (\r e -> mkEH RecExpr_Ext r Nothing e)



instance SemApp PatExpr where
  semRngApp    r  = rngLift r PatExpr_App
  semRngAppTop r  = rngLift r PatExpr_AppTop
  semRngVar    r  = rngLift r PatExpr_Var . mkHNm
  semRngCon    r  = rngLift r PatExpr_Con . mkHNm
  semRngParens r  = rngLift r PatExpr_Parens
  mkProdApp l
    = mkEH PatExpr_Rec $ mkEH RecPatExpr_Empty `mkRow` l
    where mkRow = foldl (\r e -> mkEH RecPatExpr_Ext r Nothing e)



instance SemApp TyExpr where
  semRngApp    r  = rngLift r TyExpr_App
  semRngAppTop r  = rngLift r TyExpr_AppTop
  semRngVar    r  = rngLift r TyExpr_Var . mkHNm
  semRngCon    r  = rngLift r TyExpr_Con . mkHNm
  semRngParens r  = rngLift r TyExpr_Parens
  mkProdApp l
    = mkEH TyExpr_Row $ mkEH RowTyExpr_Empty `mkRow` l
    where mkRow = foldl (\r e -> mkEH RowTyExpr_Ext r Nothing e)
  unTop (TyExpr_AppTop _ x)       = unTop x
  unTop (TyExpr_Parens _ x)       = unTop x
  unTop (TyExpr_Ann  _ _ x)       = unTop x
  unTop x                         = x
  isApp1 (TyExpr_App _ f a)       = Just (f,a)
  isApp1 _                        = Nothing
  isCon (TyExpr_Con _ n)          = Just n
  isCon _                         = Nothing



instance SemApp KiExpr where
  semRngApp    r  = rngLift r KiExpr_App
  semRngAppTop r  = rngLift r KiExpr_AppTop
  semRngVar    r  = rngLift r KiExpr_Var . mkHNm
  semRngCon    r  = rngLift r KiExpr_Con . mkHNm
  semRngParens r  = rngLift r KiExpr_Parens

-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf !(Expr ) 
-- CaseAlt -----------------------------------------------------
data CaseAlt  = CaseAlt_Pat !(Range) !(PatExpr ) !(Expr ) 
-- CaseAlts ----------------------------------------------------
type CaseAlts  = [CaseAlt ]
-- DataConstr --------------------------------------------------
data DataConstr  = DataConstr_Constr !(Range) !(HsName) !((Maybe (Int,Fixity))) !(DataFields ) !(MbTyExpr ) 
-- DataConstrs -------------------------------------------------
type DataConstrs  = [DataConstr ]
-- DataField ---------------------------------------------------
data DataField  = DataField_Field !(Range) !((Maybe [HsName])) !(TyExpr ) 
-- DataFieldExpr -----------------------------------------------
data DataFieldExpr  = DataFieldExpr_Con !(Range) !(HsName) 
                    | DataFieldExpr_Expr !(Range) !(Expr ) 
                    | DataFieldExpr_Upd !(Range) !(DataFieldExpr ) !(HsName) !(Expr ) 
-- DataFieldPatExpr --------------------------------------------
data DataFieldPatExpr  = DataFieldPatExpr_Con !(Range) !(HsName) 
                       | DataFieldPatExpr_Ext !(Range) !(DataFieldPatExpr ) !(HsName) !(PatExpr ) 
-- DataFields --------------------------------------------------
type DataFields  = [DataField ]
-- Decl --------------------------------------------------------
data Decl  = Decl_Class !(Range) !(TyExpr ) !(FuncDeps ) !((Maybe HsName)) !(Decls ) !(([(HsName,HsName)])) 
           | Decl_Data !(Range) !(Bool) !(HsName) !(TyVars ) !(DataConstrs ) !((Maybe Int)) 
           | Decl_Default !(Range) !(HsName) !(TyExprs ) 
           | Decl_FFE !(Range) !(HsName) !(FFIWay) !(ForeignEnt) !(Expr ) !(TyExpr ) 
           | Decl_FFI !(Range) !(FFIWay) !(String) !(ForeignEnt) !(HsName) !(TyExpr ) 
           | Decl_FusionConv !(Range) !(HsName) !(HsName) 
           | Decl_FusionDecl !(Range) !(HsName) 
           | Decl_GenerRep !(Range) !(Int) !(HsName) !(([HsName])) !(([(HsName,[HsName])])) 
           | Decl_Instance !(Range) !((Maybe (HsName,Bool))) !(InstVariant) !(TyExpr ) !(Decls ) 
           | Decl_InstanceIntro !(Range) !((Maybe (HsName))) !(Expr ) !(PrExpr ) 
           | Decl_KiSig !(Range) !(HsName) !(KiExpr ) 
           | Decl_TySig !(Range) !(HsName) !(TyExpr ) 
           | Decl_Type !(Range) !(HsName) !(TyExpr ) 
           | Decl_Val !(Range) !(PatExpr ) !(Expr ) 
-- Decls -------------------------------------------------------
type Decls  = [Decl ]
-- Expr --------------------------------------------------------
data Expr  = Expr_Ann !(Range) !(ExprAnn ) !(Expr ) 
           | Expr_App !(Range) !(Expr ) !(Expr ) 
           | Expr_AppImpl !(Range) !(Expr ) !(PrExpr ) !(Expr ) 
           | Expr_AppImpred !(Range) !(Expr ) !(Expr ) 
           | Expr_AppTop !(Range) !(Expr ) 
           | Expr_CConst !(Range) !(Char) 
           | Expr_Case !(Range) !(Expr ) !(CaseAlts ) !((Maybe UIDS)) !(UIDS) !(Bool) 
           | Expr_CaseAltFail !(Range) !(UID) 
           | Expr_Con !(Range) !(HsName) 
           | Expr_DataFields !(Range) !(DataFieldExpr ) 
           | Expr_DynVar !(Range) !(HsName) 
           | Expr_IConst !(Range) !(Int) 
           | Expr_IIConst !(Range) !(Integer) 
           | Expr_Lam !(Range) !(PatExpr ) !(Expr ) 
           | Expr_LamImpl !(Range) !(PrExpr ) !(PatExpr ) !(Expr ) 
           | Expr_Let !(Range) !(Bool) !(Decls ) !(Expr ) 
           | Expr_Parens !(Range) !(Expr ) 
           | Expr_Rec !(Range) !(RecExpr ) 
           | Expr_SConst !(Range) !(String) 
           | Expr_Sel !(Range) !(Expr ) !(HsName) 
           | Expr_TypeAs !(Range) !(TyExpr ) !(Expr ) 
           | Expr_Undefined !(Range) 
           | Expr_Var !(Range) !(HsName) 
-- ExprAnn -----------------------------------------------------
data ExprAnn  = ExprAnn_Empty 
-- FuncDep -----------------------------------------------------
data FuncDep  = FuncDep_Dep !(Range) !(TyVars ) !(TyVars ) 
-- FuncDeps ----------------------------------------------------
type FuncDeps  = [FuncDep ]
-- KiExpr ------------------------------------------------------
data KiExpr  = KiExpr_Ann !(Range) !(KiExprAnn ) !(KiExpr ) 
             | KiExpr_App !(Range) !(KiExpr ) !(KiExpr ) 
             | KiExpr_AppTop !(Range) !(KiExpr ) 
             | KiExpr_Con !(Range) !(HsName) 
             | KiExpr_Parens !(Range) !(KiExpr ) 
             | KiExpr_Var !(Range) !(HsName) 
-- KiExprAnn ---------------------------------------------------
data KiExprAnn  = KiExprAnn_Empty 
-- MbTyExpr ----------------------------------------------------
type MbTyExpr  = Maybe TyExpr 
-- PatExpr -----------------------------------------------------
data PatExpr  = PatExpr_Ann !(Range) !(PatExprAnn ) !(PatExpr ) 
              | PatExpr_App !(Range) !(PatExpr ) !(PatExpr ) 
              | PatExpr_AppTop !(Range) !(PatExpr ) 
              | PatExpr_Bang !(Range) !(PatExpr ) 
              | PatExpr_CConst !(Range) !(Char) 
              | PatExpr_Con !(Range) !(HsName) 
              | PatExpr_DataFields !(Range) !(DataFieldPatExpr ) 
              | PatExpr_Expr !(Range) !(Expr ) !((Maybe SrcConst)) 
              | PatExpr_IConst !(Range) !(Int) 
              | PatExpr_Irrefutable !(Range) !(PatExpr ) 
              | PatExpr_Parens !(Range) !(PatExpr ) 
              | PatExpr_Rec !(Range) !(RecPatExpr ) 
              | PatExpr_SConst !(Range) !(String) 
              | PatExpr_TypeAs !(Range) !(TyExpr ) !(PatExpr ) 
              | PatExpr_Var !(Range) !(HsName) 
              | PatExpr_VarAs !(Range) !(HsName) !(PatExpr ) 
-- PatExprAnn --------------------------------------------------
data PatExprAnn  = PatExprAnn_Empty 
-- PrExpr ------------------------------------------------------
data PrExpr  = PrExpr_Arrow !(Range) !(PrExpr ) !(PrExpr ) 
             | PrExpr_Class !(Range) !(HsName) !(TyExprs ) 
             | PrExpr_DynVar !(Range) !(HsName) !(TyExpr ) 
             | PrExpr_Eq !(Range) !(TyExpr ) !(TyExpr ) 
             | PrExpr_Forall !(Range) !(HsName) !(PrExpr ) 
             | PrExpr_Lacks !(Range) !(RowTyExpr ) !(HsName) 
-- PrExprs -----------------------------------------------------
type PrExprs  = [PrExpr ]
-- RecExpr -----------------------------------------------------
data RecExpr  = RecExpr_Empty !(Range) 
              | RecExpr_Expr !(Range) !(Expr ) 
              | RecExpr_Ext !(Range) !(RecExpr ) !((Maybe HsName)) !(Expr ) 
              | RecExpr_Upd !(Range) !(RecExpr ) !(HsName) !(Expr ) 
-- RecPatExpr --------------------------------------------------
data RecPatExpr  = RecPatExpr_Empty !(Range) 
                 | RecPatExpr_Expr !(Range) !(PatExpr ) 
                 | RecPatExpr_Ext !(Range) !(RecPatExpr ) !((Maybe HsName)) !(PatExpr ) 
-- RowTyExpr ---------------------------------------------------
data RowTyExpr  = RowTyExpr_Empty !(Range) 
                | RowTyExpr_Ext !(Range) !(RowTyExpr ) !((Maybe HsName)) !(TyExpr ) 
                | RowTyExpr_Var !(Range) !(HsName) 
-- TyExpr ------------------------------------------------------
data TyExpr  = TyExpr_Ann !(Range) !(TyExprAnn ) !(TyExpr ) 
             | TyExpr_App !(Range) !(TyExpr ) !(TyExpr ) 
             | TyExpr_AppTop !(Range) !(TyExpr ) 
             | TyExpr_Con !(Range) !(HsName) 
             | TyExpr_Impls !(Range) 
             | TyExpr_Lam !(Range) !(HsName) !(TyExpr ) 
             | TyExpr_Mono !(Range) 
             | TyExpr_NoImpls !(Range) 
             | TyExpr_Parens !(Range) !(TyExpr ) 
             | TyExpr_Pred !(Range) !(PrExpr ) 
             | TyExpr_Quant !(Range) !(TyQu) !(HsName) !(TyExpr ) 
             | TyExpr_Row !(Range) !(RowTyExpr ) 
             | TyExpr_Var !(Range) !(HsName) 
             | TyExpr_VarWild !(Range) !(HsName) 
             | TyExpr_Wild !(Range) 
-- TyExprAnn ---------------------------------------------------
data TyExprAnn  = TyExprAnn_Empty 
                | TyExprAnn_Strictness !(Strictness) 
-- TyExprs -----------------------------------------------------
type TyExprs  = [TyExpr ]
-- TyVar -------------------------------------------------------
data TyVar  = TyVar_Var !(Range) !(HsName) 
-- TyVars ------------------------------------------------------
type TyVars  = [TyVar ]