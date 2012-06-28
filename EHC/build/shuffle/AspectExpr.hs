

-- UUAGC 0.9.39.1 (src/shuffle/AspectExpr.ag)
module AspectExpr where

import System.IO
import qualified Data.Set as Set

type AspectRef  = String
type AspectRefReqd = Set.Set AspectRef
-- AGAspectExprItf ---------------------------------------------
data AGAspectExprItf  = AGAspectExprItf_AGItf (AspectExpr ) 
                      deriving ( Eq,Ord,Show)
-- AspectExpr --------------------------------------------------
data AspectExpr  = AspectExpr_And (AspectExpr ) (AspectExpr ) 
                 | AspectExpr_Or (AspectExpr ) (AspectExpr ) 
                 | AspectExpr_Requires (String) 
                 | AspectExpr_True 
                 deriving ( Eq,Ord,Show)