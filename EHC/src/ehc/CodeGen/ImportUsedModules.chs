%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Imported modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 hs module {%{EH}CodeGen.ImportUsedModules}
%%]

%%[50 hs import({%{EH}Base.Common})
%%]
%%[50 hs import(qualified Data.Set as Set)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Admin during codegen for imported and used modules and their names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 hs export(ImportUsedModules(..), emptyImportUsedModules)
data ImportUsedModules
  = ImportUsedModules
      { iumHSDeclModules		:: !HsNameS			-- ^ imported modules as declared in src .hs
      , iumHIDeclModules		:: !HsNameS			-- ^ imported modules as declared, either in .hs or .hi
      , iumHIUsedModules		:: !HsNameS			-- ^ imported modules as actually used
      , iumIntrodModules		:: !HsNameS			-- ^ module names for which a introduction/def is made
      }

emptyImportUsedModules = ImportUsedModules Set.empty Set.empty Set.empty Set.empty
%%]

