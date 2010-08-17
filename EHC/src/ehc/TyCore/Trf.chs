%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encapsulation of TyCore transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}TyCore.Trf}
%%]

-- general imports
%%[(8 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 codegen) import(Debug.Trace)
%%]
%%[(8 codegen) import(Control.Monad, Control.Monad.State)
%%]

%%[(8 codegen) import({%{EH}EHC.Common})
%%]

-- LamInfo
%%[(8 codegen) import({%{EH}LamInfo})
%%]

-- TyCore
%%[(8 codegen) import({%{EH}TyCore})
%%]

%%[(20 codegen) import({%{EH}TyCore.Base})
%%]

-- TyCore transformations
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.EliminateExplicitLaziness})
%%]
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.DefinitionSiteArityRaise1of2}, {%{EH}TyCore.Trf.DefinitionSiteArityRaise2of2})
%%]
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.IntroduceExplicitLaziness})
%%]
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.IntroduceWeirdConstructs})
%%]
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.RemoveLazyFunctions1of2}, {%{EH}TyCore.Trf.RemoveLazyFunctions2of2})
%%]
%%[(8 codegen tauphi) import({%{EH}TyCore.Trf.OptimizeStrictness1of2}, {%{EH}TyCore.Trf.OptimizeStrictness2of2})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Monad utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
modifyGets :: MonadState s m => (s -> (a,s)) -> m a
modifyGets update
  = do { s <- get
       ; let (x,s') = update s
       ; put s'
       ; return x
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(TrfTyCore(..),emptyTrfTyCore)
data TrfTyCore
  = TrfTyCore
      { trftycoreTyCore       	:: !Module
      , trftycoreTyCoreStages  	:: [(String,Module)]
      , trftycoreUniq         	:: !UID
%%[[20
      , trftycoreExpNmOffMp     :: !HsName2OffsetMp
%%]]
%%[[99
      , trftycoreInhLamMp       :: !LamMp       -- from context
      , trftycoreGathLamMp      :: !LamMp       -- gathered anew
      , trftycoreExtraExports   :: !FvS         -- extra exported names, introduced by transformations
%%]]
      }

emptyTrfTyCore :: TrfTyCore
emptyTrfTyCore = TrfTyCore emptyModule [] uidStart
%%[[20
                       Map.empty
%%]]
%%[[99
                       Map.empty Map.empty
                       Set.empty
%%]]

-- type TrfTyCoreState x = State TrfTyCore x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(trfTyCore)
trfTyCore :: EHCOpts -> HsName -> TrfTyCore -> TrfTyCore
trfTyCore opts modNm trftycore
  = snd $ runState trf trftycore
  where trf
          = do { t_initial
%%[[(8 tauphi)
              -- ; t_introduceWeirdConstructs
               ; t_introduceExplicitLaziness
               ; t_removeLazyFunctions
               ; t_definitionSiteArityRaise
               ; t_optimizeStrictness
               ; t_eliminateExplicitLaziness
%%]]
               }

        liftTyTrf :: String -> (Module -> Module) -> State TrfTyCore ()
        liftTyTrf nm t
          = liftTyTrf2 nm (flip const) (\c -> (t c,()))

        liftTyTrf2 nm update2 t
          = modify update
          where update s@(TrfTyCore{trftycoreTyCore=c, trftycoreTyCoreStages=stages})
                  = update2 extra
                    $ s { trftycoreTyCore           = c'
                        , trftycoreTyCoreStages     = if ehcOptDumpCoreStages opts then stages ++ [(nm,c')] else stages
                        }
                  where (c',extra) = t c

        uniq s@(TrfTyCore{trftycoreUniq=u})
          = (h,s {trftycoreUniq = n})
          where (n,h) = mkNewLevUID u

        t_initial                    = liftTyTrf "initial"                   $ id
%%[[(8 tauphi)
        t_definitionSiteArityRaise   = liftTyTrf "DefinitionSiteArityRaise"  $ cmodTrfDefinitionSiteArityRaise2of2 . cmodTrfDefinitionSiteArityRaise1of2
        t_eliminateExplicitLaziness  = liftTyTrf "EliminateExplicitLaziness" $ cmodTrfEliminateExplicitLaziness
        t_introduceExplicitLaziness  = liftTyTrf "IntroduceExplicitLaziness" $ cmodTrfIntroduceExplicitLaziness
        t_introduceWeirdConstructs   = liftTyTrf "IntroduceWeirdConstructs"  $ cmodTrfIntroduceWeirdConstructs
        t_removeLazyFunctions        = liftTyTrf "RemoveLazyFunctions"       $ cmodTrfRemoveLazyFunctions2of2 . cmodTrfRemoveLazyFunctions1of2
        t_optimizeStrictness         = liftTyTrf "OptimizeStrictness"        $ cmodTrfOptimizeStrictness2of2 . cmodTrfOptimizeStrictness1of2
%%]]
%%]

