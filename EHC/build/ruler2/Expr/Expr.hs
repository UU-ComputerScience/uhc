

-- UUAGC 0.9.39.1 (build/ruler2/Expr/Expr.ag)
module Expr.Expr where

import qualified Data.Map as Map
import Common







exprIsCnstr :: Expr -> Bool
exprIsCnstr e
  = ic (exprStrip StripFull e)
  where ic (Expr_Cnstr _ _) = True
        ic _                = False



mkExprApp :: Expr -> [Expr] -> Expr
mkExprApp f = Expr_AppTop . foldl Expr_App f

exprUnk :: Expr
exprUnk = Expr_Var nmUnk

exprMbNm :: Expr -> Maybe Nm
exprMbNm (Expr_Var n) = Just n
exprMbNm _            = Nothing

exprAsNm :: Expr -> Nm
exprAsNm = maybe nmUnk id . exprMbNm



mkAFld :: Nm -> Expr
mkAFld n = Expr_AVar (ANm_Fld n)

mkALoc :: Nm -> Expr
mkALoc n = Expr_AVar (ANm_Loc n [])

mkALoc' :: Nm -> Expr
mkALoc' n = mkALoc (nmStrApd n nmWild)

mkALhs' :: [AtProp] -> Nm -> Expr
mkALhs' p n = Expr_AVar (ANm_Lhs n p)

mkALhs :: Nm -> Expr
mkALhs = mkALhs' []

mkANd :: Nm -> Nm -> Expr
mkANd n a = Expr_AVar (ANm_Node n a)



data RnSrc = RnNm ANm | RnExpr Expr | RnNone

rnSrc2Expr :: RnSrc -> Expr
rnSrc2Expr (RnNm   a) = Expr_AVar a
rnSrc2Expr (RnExpr e) = e

type RnMp = Map.Map Nm (Int,RnSrc)

rnMpUnion :: RnMp -> RnMp -> RnMp
rnMpUnion m1 m2
  = Map.unionWith (\(c1,v1) (c2,v2) -> (c1+c2,u v1 v2)) m1 m2
  where u RnNone r = r
        u r      _ = r



exprEnsureAppTop :: Expr -> Expr
exprEnsureAppTop e@(Expr_App _ _    ) = Expr_AppTop e
exprEnsureAppTop e@(Expr_Op  _ _ _ _) = Expr_AppTop e
exprEnsureAppTop e                    = e



exprLines :: Expr -> [Expr]
exprLines (Expr_LF l r) = l : exprLines r
exprLines j             = [j]



data ExprStrip = StripBasicNoPar | StripBasic | StripFullNoTop | StripFull deriving (Eq,Ord)

exprStrip' :: ExprStrip -> Expr -> (Expr,Expr->Expr,[Expr])
exprStrip' s e
  = str e
  where str te@(Expr_AppTop  e)  | s >= StripBasicNoPar && s /= StripFullNoTop
                                                         = sub Expr_AppTop    te e
        str te@(Expr_Paren   e)  | s >= StripBasic       = sub Expr_Paren     te e
        str te@(Expr_Named n e)  | s >= StripBasicNoPar  = sub (Expr_Named n) te e
        str te@(Expr_Retain  e)  | s >= StripFull        = sub Expr_Retain    te e
        str te@(Expr_SelTop  e)  | s >= StripFull        = sub Expr_SelTop    te e
        str te                                           = (te,id,[te])
        sub mkt te e = (e',mkt . mke,te:l)
                     where (e',mke,l) = str e

exprStrip :: ExprStrip -> Expr -> Expr
exprStrip s e
  = e'
  where (e',_,_) = exprStrip' s e
-- AGExprItf ---------------------------------------------------
data AGExprItf  = AGExprItf_AGItf (Expr ) 
-- ANm ---------------------------------------------------------
data ANm  = ANm_Fld (Nm) 
          | ANm_Lhs (Nm) (([AtProp])) 
          | ANm_Loc (Nm) (([AtProp])) 
          | ANm_Node (Nm) (Nm) 
          | ANm_Wild 
          deriving ( Eq,Ord)
-- ECnstr ------------------------------------------------------
data ECnstr  = ECnstr_Empty 
             | ECnstr_Ty (([Nm])) 
             | ECnstr_Var (Nm) 
             deriving ( Eq,Ord)
-- Expr --------------------------------------------------------
data Expr  = Expr_AVar (ANm ) 
           | Expr_App (Expr ) (Expr ) 
           | Expr_AppTop (Expr ) 
           | Expr_ChildOrder (Int) (Expr ) 
           | Expr_Cnstr (Expr ) (ECnstr ) 
           | Expr_Empty 
           | Expr_Expr (Expr ) 
           | Expr_Int (String) 
           | Expr_LF (Expr ) (Expr ) 
           | Expr_Named (Nm) (Expr ) 
           | Expr_Op (Nm) (Expr ) (Expr ) (Expr ) 
           | Expr_Paren (Expr ) 
           | Expr_Retain (Expr ) 
           | Expr_SP (Expr ) (Expr ) 
           | Expr_Sel (Expr ) (MbExpr ) 
           | Expr_SelTop (Expr ) 
           | Expr_StrAsIs (String) 
           | Expr_StrText (String) 
           | Expr_Undefined 
           | Expr_Uniq 
           | Expr_Var (Nm) 
           | Expr_Wrap (WrKind) (Expr ) 
           | Expr_WrapCnstr (ECnstr ) 
           deriving ( Eq,Ord)
-- MbExpr ------------------------------------------------------
type MbExpr  = Maybe Expr 