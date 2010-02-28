module EH.Util.Debug
  ( tr, trp,
    (>>>),(<<<)
  )
  where

import EH.Util.Pretty
import EH.Util.PrettyUtils
import Debug.Trace

-------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------

tr m s v = trace (m ++ show s) v
trp m s v = trace (m ++ "\n" ++ disp (m >|< ":" >#< s) 1000 "") v

---------------------------------------------------------------------------------------------------------
--- Debug
---------------------------------------------------------------------------------------------------------

infixl 0 <<<
infixr 0 >>>

b <<< a = trace (a b) b
a >>> b = trace (a b) b

infixl 0 <?<
infixr 0 >?>

b <?< (p,a) = (if p then trace (a b) else id) b
(p,a) >?> b = (if p then trace (a b) else id) b