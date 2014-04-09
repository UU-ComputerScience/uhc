%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encapsulation of JavaScript transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) hs module {%{EH}JavaScript.Trf}
%%]

-- general imports
%%[(8 codegen javascript) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 codegen javascript) import(Control.Monad, Control.Monad.State.Strict)
%%]

%%[(8 codegen javascript) import({%{EH}Base.Target},{%{EH}Base.Optimize})
%%]

%%[(8 codegen javascript) import({%{EH}EHC.Common})
%%]

-- JavaScript
%%[(8 codegen javascript) import({%{EH}JavaScript})
%%]

-- JavaScript transformations
%%[(8 codegen javascript) import({%{EH}JavaScript.Trf.InlineSingleUseNames})
%%]
%%[(8 codegen javascript) import({%{EH}JavaScript.Trf.CompactAndSimplify})
%%]

-- Transformation utils
%%[(8 codegen javascript) import({%{EH}CodeGen.TrfUtils})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(TrfJavaScript,emptyTrfJavaScript)
{-
data TrfJavaScript
  = TrfJavaScript
      { trfjsJavaScript    		:: !JavaScriptModule
      , trfjsJavaScriptStages	:: [(String,Maybe JavaScriptModule,ErrL)]
      , trfjsUniq             	:: !UID
      }
-}
type TrfJavaScript = TrfState JavaScriptModule ()

emptyTrfJavaScript :: TrfJavaScript
emptyTrfJavaScript = mkEmptyTrfState emptyJavaScriptModule ()
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen javascript) export(trfJavaScript)
-- | Perform JavaScript transformations.
--   The 'optimScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
trfJavaScript :: EHCOpts -> OptimizationScope -> HsName -> TrfJavaScript -> TrfJavaScript
trfJavaScript opts optimScope modNm trfjs
  -- = execState trf trfjs
  = runTrf opts modNm ehcOptDumpJavaScriptStages (optimScope `elem`) trfjs trf
  where trf
          = do { -- initial is just to obtain JavaScript for dumping stages
                 t_initial
                 
                 -- removal of unnecessary name introductions, in particular singly used ones
               ; t_inl_names

                 -- compactify and simplify further, trivial stuff like making names shorter
               ; t_comp_simpl

               }
        
        -- actual transformations
        t_initial       = liftTrfModPlain  osm "initial"            $ id
        t_inl_names     = liftTrfModPlain  osm "inl-names"          $ cmodTrfInlineSingleUseNames
        t_comp_simpl    = liftTrfModPlain  osm "comp-simpl"         $ cmodTrfCompactAndSimplify

        -- abbreviations for optimatisation scope
        osm  = [OptimizationScope_PerModule]
%%]

