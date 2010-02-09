%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC common stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Used by all compiler driver code

%%[1 module {%{EH}EHC.Common}
%%]

-- general imports
%%[1 import(Data.List, Data.Char, Data.Maybe) export(module Data.Maybe, module Data.List, module Data.Char)
%%]
%%[1 import(Control.Monad.State, IO, System) export(module IO, module Control.Monad.State, module System)
%%]
%%[1 import(EH.Util.CompileRun, EH.Util.Pretty, EH.Util.FPath, EH.Util.Utils) export(module EH.Util.CompileRun, module EH.Util.Pretty, module EH.Util.FPath, module EH.Util.Utils)
%%]
%%[1 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Base.Opts}) export(module {%{EH}Base.Common}, module {%{EH}Base.Builtin}, module {%{EH}Base.Opts})
%%]
%%[1 import({%{EH}Error},{%{EH}Error.Pretty}) export(module {%{EH}Error},module {%{EH}Error.Pretty})
%%]

%%[8 import({%{EH}Gam.Full}) export(module {%{EH}Gam.Full})
%%]

%%[20 import(System.Time, System.Directory) export(module System.Time, module System.Directory)
%%]

%%[1
-- dummy, so module is not empty for initial variants, and exports will take effect
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The state HS compilation can be in

%%[8 export(HSState(..))
data HSState
  = HSStart					-- starting from .hs
  | HSAllSem				-- done all semantics for .hs
%%[[20
  | HSOnlyImports			-- done imports from .hs
  | HIStart					-- starting from .hi
  | HIAllSem				-- done all semantics for .hi
  | HIOnlyImports			-- done imports from .hi
%%]]
%%[[99
  | LHSStart				-- starting from .lhs
  | LHSOnlyImports			-- done imports from .lhs
%%]]
  deriving (Show,Eq)
%%]

Is a state working on literal haskell input?

%%[20 export(hsstateIsLiteral)
hsstateIsLiteral :: HSState -> Bool
%%[[99
hsstateIsLiteral LHSStart       = True
hsstateIsLiteral LHSOnlyImports = True
%%]]
hsstateIsLiteral _              = False
%%]

%%[20 export(hsstateShowLit)
hsstateShowLit :: HSState -> String
%%[[99
hsstateShowLit LHSStart       = "Literal"
hsstateShowLit LHSOnlyImports = "Literal"
%%]]
hsstateShowLit _              = ""
%%]

The next thing to do for HSState.

%%[20 export(hsstateNext)
hsstateNext :: HSState -> HSState
hsstateNext HSStart       = HSOnlyImports
hsstateNext HIStart       = HIOnlyImports
%%[[99
hsstateNext LHSStart      = LHSOnlyImports
%%]]
hsstateNext st            = st
%%]

The state EH compilation can be in

%%[8 export(EHState(..))
data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)
%%]

The state C compilation can be in, which basically is just administering it has to be compiled

%%[(94 codegen) export(CState(..))
data CState
  = CStart
  | CAllSem
  deriving (Show,Eq)
%%]

The state any compilation can be in

%%[8 export(EHCompileUnitState(..))
data EHCompileUnitState
  = ECUSUnknown
  | ECUSHaskell !HSState
  | ECUSEh      !EHState
%%[[(94 codegen)
  | ECUSC       !CState
%%]]
  | ECUSGrin
  | ECUSFail
  deriving (Show,Eq)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Kind of compilation unit, as known from source as starting point for compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileUnitKind(..))
data EHCompileUnitKind
  = EHCUKind_HS		-- Haskell: .hs .lhs .hi
%%[[94
  | EHCUKind_C		-- C: .c
%%]]
  | EHCUKind_None	-- Nothing
  deriving Eq
%%]

%%[8 export(ecuStateToKind)
ecuStateToKind :: EHCompileUnitState -> EHCompileUnitKind
ecuStateToKind s
  = case s of
      ECUSHaskell _ -> EHCUKind_HS
%%[[94
      ECUSC       _ -> EHCUKind_C
%%]]
      _             -> EHCUKind_None
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% How to compile the final step for a target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FinalCompileHow(..))
data FinalCompileHow
  = FinalCompile_Module
  | FinalCompile_Exec
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shell command construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkShellCmd)
mkShellCmd :: [String] -> String
mkShellCmd = concat . intersperse " "
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name of in/output + possible dir which is preprended
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkInOrOutputFPathDirFor)
mkInOrOutputFPathDirFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> (FPath,Maybe String)
mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix
%%[[8
  = (fpathSetSuff suffix fp', Nothing)
  where fp' = fp
%%][99
  = (fpathSetSuff suffix fp', d)
  where (fp',d) = case inoutputfor of
                    OutputFor_Module   -> f ehcOptOutputDir
                    OutputFor_Pkg      -> f ehcOptOutputPkgLibDir
                    InputFrom_Loc l 
                      | filelocIsPkg l -> f (const Nothing)
                      | otherwise      -> f ehcOptOutputDir
        f g     = case g opts of
                    Just d -> (fpathPrependDir d' $ mkFPath modNm, Just d')
                           where d' = filePathUnPrefix d
                    _      -> (fp,Nothing)
%%]]
%%]

%%[8 export(mkInOrOutputFPathFor)
mkInOrOutputFPathFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> FPath
mkInOrOutputFPathFor inoutputfor opts modNm fp suffix
  = fst $ mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix
%%]

%%[8 export(mkOutputFPath)
mkOutputFPath :: FPATH nm => EHCOpts -> nm -> FPath -> String -> FPath
mkOutputFPath = mkInOrOutputFPathFor OutputFor_Module
%%]

