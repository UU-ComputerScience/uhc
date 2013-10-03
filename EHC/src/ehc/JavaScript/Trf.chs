%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encapsulation of JavaScript transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 javascript) hs module {%{EH}JavaScript.Trf}
%%]

-- general imports
%%[(8 javascript) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 javascript) import(Control.Monad, Control.Monad.State.Strict)
%%]

%%[(8 javascript) import({%{EH}Base.Target},{%{EH}Base.Optimize})
%%]

%%[(8 javascript) import({%{EH}EHC.Common})
%%]

-- JavaScript
%%[(8 javascript) import({%{EH}JavaScript})
%%]

-- JavaScript transformations
%%[(8 javascript) import({%{EH}JavaScript.Trf.InlineSingleUseNames})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 javascript) export(TrfJavaScript(..),emptyTrfJavaScript)
data TrfJavaScript
  = TrfJavaScript
      { trfjsJavaScript    		:: !JavaScriptModule
      , trfjsJavaScriptStages	:: [(String,Maybe JavaScriptModule,ErrL)]
      , trfjsUniq             	:: !UID
      }

emptyTrfJavaScript :: TrfJavaScript
emptyTrfJavaScript = TrfJavaScript emptyJavaScriptModule [] uidStart

-- type TrfJavaScriptState x = State TrfJavaScript x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 javascript) export(trfJavaScript)
-- | Perform JavaScript transformations.
--   The 'optimScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
trfJavaScript :: EHCOpts -> OptimizationScope -> HsName -> TrfJavaScript -> TrfJavaScript
trfJavaScript opts optimScope modNm trfjs
  = execState trf trfjs
  where trf
          = do { -- initial is just to obtain JavaScript for dumping stages
                 t_initial
                 
                 -- removal of unnecessary name introductions, in particular singly used ones
               ; t_inl_names

               }

        liftTrfMod :: [OptimizationScope] -> String -> (JavaScriptModule -> JavaScriptModule) -> State TrfJavaScript ()
        liftTrfMod os nm t
          = liftTrf os nm (flip const) (\_ c -> (Just $ t c,(),[]))

        liftTrf os nm update2 t
          | optimScope `elem` os = modify update
          | otherwise            = return ()
          where update s@(TrfJavaScript{trfjsJavaScript=c, trfjsJavaScriptStages=stages})
                  = update2 extra
                    $ s { trfjsJavaScript           = maybe c id c'
                        , trfjsJavaScriptStages     -- = if ehcOptDumpJavaScriptStages opts then stages ++ [(nm,c')] else stages
                                                = stages ++ [(nm,if ehcOptDumpJavaScriptStages opts then c' else Nothing,errl)]
                        }
                  where (c',extra,errl) = t s c
        
        -- actual transformations
        t_initial       = liftTrfMod  osm "initial"            $ id
        t_inl_names     = liftTrfMod  osm "inl-names"          $ cmodTrfInlineSingleUseNames

        -- abbreviations for optimatisation scope
        osm  = [OptimizationScope_PerModule]
%%]

