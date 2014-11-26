%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation info about a set of (imported) modules and module being compiled
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs module {%{EH}CodeGen.ModuleImportExportImpl}
%%]

%%[(50 codegen) hs import({%{EH}Base.Common})
%%]
%%[(50 codegen) hs import({%{EH}LamInfo})
%%]
%%[(50 codegen) hs import({%{EH}CodeGen.ValAccess})
%%]
%%[(50 codegen) hs import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Admin during codegen for imported and used modules and their names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs export(ModuleImportExportImpl(..), emptyModuleImportExportImpl)
data ModuleImportExportImpl
  = ModuleImportExportImpl
      { mieimplLamMp			:: LamMp			-- ^ LamMp, per binding info
      , mieimplUsedModNmL		:: [HsName]			-- ^ used and referred to modules (i.e. all, also indirectly referred to)
      , mieimplHsName2FldMpMp	:: HsName2FldMpMp	-- ^ imported modules fld mp
      , mieimplHsName2FldMp		:: HsName2FldMp		-- ^ export fld map
      }

emptyModuleImportExportImpl = ModuleImportExportImpl Map.empty [] Map.empty Map.empty
%%]

