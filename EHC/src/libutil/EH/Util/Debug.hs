module EH.Util.Debug
  ( tr, trp
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

