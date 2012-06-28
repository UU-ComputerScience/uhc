

-- UUAGC 0.9.39.1 (build/ruler2/ARule/ARule.ag)
module ARule.ARule where

import Common
import Expr.Expr








-- AEqn --------------------------------------------------------
data AEqn  = AEqn_Eqn (AEqnDest ) (AExpr ) 
           | AEqn_Err (Expr) 
-- AEqnDest ----------------------------------------------------
data AEqnDest  = AEqnDest_Many (AEqnDests ) 
               | AEqnDest_One (ANm) 
-- AEqnDests ---------------------------------------------------
type AEqnDests  = [AEqnDest ]
-- AEqns -------------------------------------------------------
type AEqns  = [AEqn ]
-- AExpr -------------------------------------------------------
data AExpr  = AExpr_Expr (Expr) 
-- AGARuleItf --------------------------------------------------
data AGARuleItf  = AGARuleItf_AGItf (ARule ) 
-- ARule -------------------------------------------------------
data ARule  = ARule_Rule (([Nm])) (Nm) (([String])) (AEqns ) 
-- ARules ------------------------------------------------------
type ARules  = [ARule ]