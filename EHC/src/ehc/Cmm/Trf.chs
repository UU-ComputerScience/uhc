%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encapsulation of Cmm transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen cmm) hs module {%{EH}Cmm.Trf}
%%]

-- general imports
%%[(8 codegen cmm) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 codegen cmm) import(Control.Monad)
%%]

%%[(8 codegen cmm) import({%{EH}Base.Target},{%{EH}Base.Optimize})
%%]

%%[(8 codegen cmm) import({%{EH}EHC.Common})
%%]

-- Cmm
%%[(8 codegen cmm) import({%{EH}Cmm})
%%]

-- Cmm transformations
%%[(8 codegen cmm) import({%{EH}Cmm.Trf.SimplifyLocalNames})
%%]

-- Cmm checks
%%[(8 codegen cmm) import({%{EH}Cmm.Check})
%%]

-- Transformation utils
%%[(8 codegen cmm) import({%{EH}CodeGen.TrfUtils})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen cmm) export(TrfCmm,emptyTrfCmm)
type TrfCmm = TrfState Module ()

emptyTrfCmm :: TrfCmm
emptyTrfCmm = mkEmptyTrfState emptyModule ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen cmm) export(trfCmm)
-- | Perform Cmm transformations.
--   The 'optimScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
trfCmm :: EHCOpts -> OptimizationScope -> HsName -> TrfCmm -> TrfCmm
trfCmm opts optimScope modNm trfcmm
  = runTrf opts modNm ehcOptDumpCmmStages (optimScope `elem`) trfcmm trf
  where trf
          = do { -- initial is just to obtain Cmm for dumping stages
                 t_initial

                 -- simplify local names, trivial stuff like making names shorter
               ; t_simpl_loc_nm

               ; when (ehcOptCmmCheck opts) t_cmm_check
               }
        
        -- actual transformations
        t_initial       = liftTrfModPlain  osm "initial"            $ id
        t_simpl_loc_nm  = liftTrfModPlain  osm "simpl-loc-nm"       $ cmmModTrfSimplifyLocalNames

        -- checks
        t_cmm_check     = liftCheckMod     osm "check"              $ \s -> cmmModCheck

        -- abbreviations for optimatisation scope
        osm  = [OptimizationScope_PerModule]
%%]

