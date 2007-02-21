%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation of Evidence (of Pred) to Core fragments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred.EvidenceToCore} import({%{EH}Pred.Evidence},{%{EH}Pred.CommonCHR})
%%]

%%[9 import(Data.List,qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe)
%%]

%%[9 import({%{EH}Base.Common},{%{EH}Core})
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(evidNm)
evidNm :: UID -> HsName
evidNm = mkHNm
%%]

%%[9 export(evidMpToCore)
evidMpToCore :: UID -> InfoToEvidenceMap p RedHowAnnotation -> ([([UID],[CBind])])
evidMpToCore u evidMp
  = undefined
%%]

