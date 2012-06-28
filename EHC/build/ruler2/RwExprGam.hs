module RwExprGam
( module FmGam, RwExprGam )
where
import Common
import FmGam
import Expr.Expr

type RwExprGam = RwGam (Expr,Expr)
