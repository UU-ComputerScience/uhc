%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encapsulation of Core transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}Core.Trf}
%%]

-- general imports
%%[(8 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]
%%[(8 codegen) import(Control.Monad, Control.Monad.State)
%%]

%%[(8 codegen) import({%{EH}Base.Target},{%{EH}Base.Optimize})
%%]

%%[(8 codegen) import({%{EH}EHC.Common})
%%]

-- LamInfo
%%[(8 codegen) import({%{EH}LamInfo})
%%]

-- Core
%%[(8 codegen) import({%{EH}Core})
%%]

-- Core check
%%[(8 codegen coresysf) import({%{EH}Core.SysF.Check})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf.RenUniq}, {%{EH}Core.Trf.ANormal}, {%{EH}Core.Trf.InlineLetAlias}, {%{EH}Core.Trf.LetUnrec})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LetDefBeforeUse})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LamGlobalAsArg}, {%{EH}Core.Trf.CAFGlobalAsArg}, {%{EH}Core.Trf.FloatToGlobal}, {%{EH}Core.Trf.ConstProp})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EtaRed}, {%{EH}Core.Trf.ElimTrivApp})
%%]
%%[(8 codegen wholeprogAnal) import({%{EH}Core.Trf.FindNullaries})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.AnnBasedSimplify})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.AnaRelevance})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LetFlattenStrict})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.OptimizeStrictness})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EraseExtractTysigCore})
%%]
%%[(9 codegen wholeprogAnal) import({%{EH}Core.Trf.FixDictFields})
%%]
%%[(99 codegen) import({%{EH}Core.Trf.ExplicitStackTrace})
%%]
%%[(8 codegen grin) hs import(Debug.Trace)
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

%%[(8 codegen) export(TrfCore(..),emptyTrfCore)
data TrfCore
  = TrfCore
      { trfcoreCore             :: !CModule
      , trfcoreCoreStages       :: [(String,Maybe CModule,ErrL)]
      , trfcoreUniq             :: !UID
      , trfcoreInhLamMp         :: LamMp        -- from context, possibly overridden from gathered one
      , trfcoreGathLamMp        :: !LamMp       -- gathered anew
%%[[50
      , trfcoreExpNmOffMp       :: !HsName2OffsetMp
%%]]
%%[[99
      , trfcoreExtraExports     :: !FvS             -- extra exported names, introduced by transformations
%%]]
      }

emptyTrfCore :: TrfCore
emptyTrfCore = TrfCore emptyCModule [] uidStart
                       Map.empty Map.empty
%%[[50
                       Map.empty
%%]]
%%[[99
                       Set.empty
%%]]

-- type TrfCoreState x = State TrfCore x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(trfCore)
-- | Perform Core transformations.
--   The 'optScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
trfCore :: EHCOpts -> OptimizationScope -> DataGam -> HsName -> TrfCore -> TrfCore
trfCore opts optimScope dataGam modNm trfcore
  = execState trf trfcore
  where trf
          = do { -- initial is just to obtain Core for dumping stages
                 t_initial
                 
%%[[(8 coresysf)
                 -- type check
               ; when (ehcOptCoreSysF opts)
                      (do { when (ehcOptCoreSysFCheck opts) t_sysf_check
                            -- erase ty early as not to confuse later transformation; this should be fixed etc when sysf stuff works (20120531)
                          ; t_erase_ty
                          })
%%]]
                 
                 -- removal of unnecessary constructs: simplifications based on annotations (experimential, temporary)
               ; t_ann_simpl

                 -- removal of unnecessary constructs: eta expansions
               ; t_eta_red

                 -- erase type signatures, extract the core + ty combi at this stage
               ; unless (ehcOptCoreSysF opts)
                        t_erase_ty

                 -- make names unique
               ; t_ren_uniq emptyRenUniqOpts
                 -- from now on INVARIANT: keep all names globally unique
                 --             ASSUME   : no need to shadow identifiers

                 -- removal of unnecessary constructs: mutual recursiveness
               ; t_let_unrec
                 -- flattening of nested strictness
               ; t_let_flatstr

                 -- removal of unnecessary constructs: aliases
               ; t_inl_letali

                 -- removal of unnecessary constructs: trival function applications
               ; t_elim_trivapp

%%[[99
                 -- optionally modify to include explicit stack trace
               ; when (ehcOptTargetFlavor opts == TargetFlavor_Debug)
                      (do { t_expl_trace
                            -- from now on INVARIANT: renaming of identifiers must also rename additional exported names here introduced

                          ; t_let_unrec
                          -- ; t_ren_uniq
                          })
%%]]

                 -- removal of unnecessary constructs: constants
               ; t_const_prop
               ; t_inl_letali
               ; t_elim_trivapp

                 -- put in A-normal form, where args to app only may be identifiers
               ; u1 <- modifyGets uniq
               ; t_anormal u1

%%[[(9 wholeprogAnal)
               ; when (targetDoesHPTAnalysis (ehcOptTarget opts))
                      t_fix_dictfld
%%]]
               
                 -- pass all globals used in lambda explicit as argument
               ; t_lam_asarg

                 -- pass all globals used in CAF explicit as argument
               ; t_caf_asarg
               ; t_let_unrec
               ; u2 <- modifyGets uniq
               ; t_anormal u2
               
                 -- float lam/CAF to global level
               ; t_float_glob
                 -- from now on INVARIANT: no local lambdas
                 --             ASSUME   : 

%%[[(8 wholeprogAnal)
               ; when (targetDoesHPTAnalysis (ehcOptTarget opts))
                      t_find_null
%%]]
               ; when (ehcOptOptimizes Optimize_StrictnessAnalysis opts)
                      (do { t_let_defbefuse
                          ; t_ana_relev
                          ; t_opt_strict
                          })
               ; when (targetIsJavaScript (ehcOptTarget opts))
                      (do { {- t_let_flatstr
                          ; -} t_ren_uniq (emptyRenUniqOpts {renuniqOptResetOnlyInLam = True})
                          })
               }

        liftTrfMod :: [OptimizationScope] -> String -> (CModule -> CModule) -> State TrfCore ()
        liftTrfMod os nm t
          = liftTrf os nm (flip const) (\_ c -> (Just $ t c,(),[]))

        liftTrfInfoMod :: [OptimizationScope] -> String -> (TrfCore -> CModule -> CModule) -> State TrfCore ()
        liftTrfInfoMod os nm t
          = liftTrf os nm (flip const) (\s c -> (Just $ t s c,(),[]))

        liftTrfInfoModExtra :: [OptimizationScope] -> String -> (extra -> TrfCore -> TrfCore) -> (TrfCore -> CModule -> (CModule,extra)) -> State TrfCore ()
        liftTrfInfoModExtra os nm update2 t
          = liftTrf os nm update2 (\s c -> let (c',e) = t s c in (Just c',e,[]))

        liftTrfCheck :: [OptimizationScope] -> String -> (TrfCore -> CModule -> ErrL) -> State TrfCore ()
        liftTrfCheck os nm t
          = liftTrf os nm (flip const) (\s c -> let e = t s c in (Nothing,(),e))

        liftTrf os nm update2 t
          | optimScope `elem` os = modify update
          | otherwise            = return ()
          where update s@(TrfCore{trfcoreCore=c, trfcoreCoreStages=stages})
                  = update2 extra
                    $ s { trfcoreCore           = maybe c id c'
                        , trfcoreCoreStages     -- = if ehcOptDumpCoreStages opts then stages ++ [(nm,c')] else stages
                                                = stages ++ [(nm,if ehcOptDumpCoreStages opts then c' else Nothing,errl)]
                        }
                  where (c',extra,errl) = t s c

        lamMpPropagate l s@(TrfCore {trfcoreGathLamMp=gl, trfcoreInhLamMp=il})
          = s {trfcoreGathLamMp = gl', trfcoreInhLamMp = Map.union gl' il}
          where gl' = Map.union l gl
        
        -- bump uniq counter
        uniq s@(TrfCore{trfcoreUniq=u})
          = (h,s {trfcoreUniq = n})
          where (n,h) = mkNewLevUID u

        -- actual transformations
        t_initial       = liftTrfMod  osmw "initial"            $ id
%%[[(8 coresysf)
        t_sysf_check    = liftTrfCheck  osm "sysf-type-check"  $ \s -> cmodSysfCheck opts (emptyCheckEnv {cenvLamMp = trfcoreInhLamMp s})
%%]]
        t_eta_red       = liftTrfMod  osm "eta-red"            $ cmodTrfEtaRed
        t_erase_ty      = liftTrfInfoModExtra osm "erase-ty" lamMpPropagate
                                                               $ \_ -> cmodTrfEraseExtractTysigCore opts
        t_ann_simpl     = liftTrfMod  osm "ann-simpl"          $ cmodTrfAnnBasedSimplify opts
        t_ren_uniq    o = liftTrfMod  osm "ren-uniq"           $ cmodTrfRenUniq o
        t_let_unrec     = liftTrfMod  osm "let-unrec"          $ cmodTrfLetUnrec
        t_let_defbefuse = liftTrfMod  osm "let-defbefuse"      $ cmodTrfLetDefBeforeUse
        t_let_flatstr   = liftTrfMod  osm "let-flatstr"        $ cmodTrfLetFlattenStrict
        t_inl_letali    = liftTrfMod  osm "inl-letali"         $ cmodTrfInlineLetAlias
%%[[50
                                                              (Map.keysSet $ trfcoreExpNmOffMp trfcore)
%%]]
        t_elim_trivapp  = liftTrfMod  osm "elim-trivapp"       $ cmodTrfElimTrivApp opts
        t_const_prop    = liftTrfMod  osm "const-prop"         $ cmodTrfConstProp opts
        t_anormal     u = liftTrfMod  osm "anormal"            $ cmodTrfANormal modNm u
        t_lam_asarg     = liftTrfMod  osm "lam-asarg"          $ cmodTrfLamGlobalAsArg
        t_caf_asarg     = liftTrfMod  osm "caf-asarg"          $ cmodTrfCAFGlobalAsArg
        t_float_glob    = liftTrfMod  osm "float-glob"         $ cmodTrfFloatToGlobal
%%[[(8 wholeprogAnal)
        t_find_null     = liftTrfMod  osm "find-null"          $ cmodTrfFindNullaries
%%]]
        t_ana_relev     = liftTrfInfoModExtra osm "ana-relev" lamMpPropagate
                                                               $ \s -> cmodTrfAnaRelevance opts dataGam (trfcoreInhLamMp s)
        t_opt_strict    = liftTrfInfoModExtra osm "optim-strict" lamMpPropagate
                                                               $ \s -> cmodTrfOptimizeStrictness opts (trfcoreInhLamMp s)
%%[[(9 wholeprogAnal)
        t_fix_dictfld   = liftTrfMod  osm "fix-dictfld"        $ cmodTrfFixDictFields
%%]]
%%[[99        
        t_expl_trace    = liftTrfInfoModExtra osm "expl-sttrace"
                                                  (\m s@(TrfCore {trfcoreExtraExports=exps})
                                                     -> (lamMpPropagate m s)
                                                          { trfcoreExtraExports   = exps `Set.union`
                                                                                    Set.fromList [ n
                                                                                                 | (n,LamInfo {laminfoStackTrace=(StackTraceInfo_IsStackTraceEquiv _)}) <- Map.toList m
                                                                                                 ]
                                                          }
                                                  )            $ \s -> cmodTrfExplicitStackTrace opts (trfcoreInhLamMp s)
%%]]
        -- abbreviations for optimatisation scope
        osm  = [OptimizationScope_PerModule]
        osmw = 
%%[[50
			   [OptimizationScope_WholeCore] ++
%%]]
			   osm
%%]

