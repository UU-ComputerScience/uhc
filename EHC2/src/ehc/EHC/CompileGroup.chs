%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile group maintains info for a group of simultaneously compiled units.
This is not yet finished!!!!!!!!!!!!!!!!!!!!
Only a placeholder ...

%%[20 module {%{EH}EHC.CompileGroup}
%%]

-- general imports
%%[20 import({%{EH}EHC.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(EHCompileGroup(..),emptyECG)
data EHCompileGroup
  = EHCompileGroup
      { ecgNm                :: HsName
      , ecgModL              :: [HsName]
      }

emptyECG :: EHCompileGroup
emptyECG
  = EHCompileGroup
      { ecgNm                = hsnUnknown
      , ecgModL              = []
      }
%%]

%%[20
%%]
crCompileCG :: Maybe HSState -> [HsName] -> EHCompileRun -> IO EHCompileRun
crCompileCG targHSState modNmL cr
  = do { let grpNm = hsnFromString $ concat $ intersperse "-" $ map show $ modNmL
             crsi  = crStateInfo cr
             cr2   = cr {crStateInfo = crsi {crsiGrpMp = Map.insert grpNm (emptyECG {ecgNm = grpNm, ecgModL = modNmL}) (crsiGrpMp crsi)}}
             crSetNm = crSeq $ map (\n -> crUpdCU n (\ecu -> return (ecu {ecuGrpNm = grpNm}))) modNmL
       ; crSetNm cr2
       }


