

-- UUAGC 0.9.39.1 (build/ruler2/ViewSel/ViewSel.ag)
module ViewSel.ViewSel where

import EH.Util.Nm




-- AGRlSelItf --------------------------------------------------
data AGRlSelItf  = AGRlSelItf_AGItf (RlSel ) 
-- AGViewSelItf ------------------------------------------------
data AGViewSelItf  = AGViewSelItf_AGItf (ViewSel ) 
-- AGViewSelsItf -----------------------------------------------
data AGViewSelsItf  = AGViewSelsItf_AGItf (ViewSels ) 
-- NmSel -------------------------------------------------------
data NmSel  = NmSel_All 
            | NmSel_Nms (([Nm])) 
            deriving ( Show)
-- RlSel -------------------------------------------------------
data RlSel  = RlSel_Sel (ViewSels ) (NmSel ) (NmSel ) 
            deriving ( Show)
-- ViewSel -----------------------------------------------------
data ViewSel  = ViewSel_All 
              | ViewSel_Range (ViewSel ) (ViewSel ) 
              | ViewSel_Renamed (Nm) (Nm) 
              | ViewSel_View (Nm) 
              deriving ( Show)
-- ViewSels ----------------------------------------------------
type ViewSels  = [ViewSel ]