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

-- ValAccess
%%[(8 codegen) hs import({%{EH}CodeGen.ValAccess} as VA)
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
%%[(50 codegen corein) import({%{EH}Core.Trf.FixAfterParse})
%%]

-- Misc
%%[(8 codegen grin) hs import(Debug.Trace)
%%]

-- Transformation utils
%%[(8 codegen) import({%{EH}CodeGen.TrfUtils})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interface to transformations, used internally as state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(TrfCore(..),emptyTrfCore)
type TrfCore = TrfState CModule TrfCoreExtra

emptyTrfCore :: TrfCore
emptyTrfCore = mkEmptyTrfState emptyCModule emptyTrfCoreExtra
%%]

%%[(8 codegen) export(TrfCoreExtra(..),emptyTrfCoreExtra)
data TrfCoreExtra
  = TrfCoreExtra
      { trfcoreInhLamMp         :: LamMp        -- from context, possibly overridden from gathered one
      , trfcoreGathLamMp        :: !LamMp       -- gathered anew
%%[[50
      , trfcoreExpNmOffMp       :: !HsName2FldMp
%%]]
%%[[99
      , trfcoreExtraExports     :: !FvS             -- extra exported names, introduced by transformations
%%]]
      , trfcoreECUState			:: !EHCompileUnitState
      , trfcoreIsLamLifted      :: !Bool
      }

emptyTrfCoreExtra :: TrfCoreExtra
emptyTrfCoreExtra = TrfCoreExtra
                       Map.empty Map.empty
%%[[50
                       Map.empty
%%]]
%%[[99
                       Set.empty
%%]]
                       ECUS_Unknown
                       False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations + checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(trfCore)
-- | Perform Core transformations.
--   The 'optScope' tells at which compilation phase (per module, whole program) the transformations are done, default only per module
trfCore :: EHCOpts -> OptimizationScope -> DataGam -> HsName -> TrfCore -> TrfCore
trfCore opts optimScope dataGam modNm trfcore
  -- = execState trf trfcore
  = runTrf opts modNm ehcOptDumpCoreStages (optimScope `elem`) trfcore trf
  where isFromCoreSrc = ecuStateIsCore $ trfcoreECUState    $ trfstExtra trfcore
        isLamLifted   =                  trfcoreIsLamLifted $ trfstExtra trfcore
        noOptims      = ehcOptOptimizationLevel opts <= OptimizationLevel_Off
        isCoreTarget  = targetIsCore $ ehcOptTarget opts
        trf
          = do { -- initial is just to obtain Core for dumping stages
                 t_initial
                 
%%[[(8 corein)
               ; when isFromCoreSrc $ do
                   { -- fix whatever needs to be fixed when read from Core src
                     t_fix_postparse

                   }
%%]]

%%[[(8 coresysf)
                 -- type check
               ; when (ehcOptCoreSysF opts)
                      (do { when (ehcOptCoreSysFCheck opts) t_sysf_check
                            -- erase ty early as not to confuse later transformation; this should be fixed etc when sysf stuff works (20120531)
                          ; t_erase_ty
                          })
%%]]
                 
               ; unless isFromCoreSrc $ do 
                   { unless noOptims $ do
                       { -- removal of unnecessary constructs: simplifications based on annotations (experimential, temporary)
                         t_ann_simpl
                       }

                     -- removal of unnecessary constructs: eta expansions
                   ; t_eta_red

                     -- erase type signatures, extract the core + ty combi at this stage
                   ; unless (ehcOptCoreSysF opts)
                       t_erase_ty
                   }


               ; unless isLamLifted $ do
                   {
					 -- make names unique
				   ; unless isCoreTarget $
				       t_ren_uniq emptyRenUniqOpts
					 -- from now on INVARIANT: keep all names globally unique
					 --             ASSUME   : no need to shadow identifiers

					 -- removal of unnecessary constructs: mutual recursiveness
				   ; t_let_unrec
                   }

               ; when isFromCoreSrc $ do
                   { -- ensure def before use ordering
                     t_let_defbefuse

                   }

                 -- flattening of nested strictness
               ; t_let_flatstr

               ; unless noOptims $ do
                   {
                     -- removal of unnecessary constructs: aliases
                   ; t_inl_letali

                     -- removal of unnecessary constructs: trival function applications
                   ; t_elim_trivapp
                   }

%%[[99
                 -- optionally modify to include explicit stack trace
               ; when (ehcOptTargetFlavor opts == TargetFlavor_Debug)
                      (do { t_expl_trace
                            -- from now on INVARIANT: renaming of identifiers must also rename additional exported names here introduced

                          ; t_let_unrec
                          -- ; t_ren_uniq
                          })
%%]]

               ; unless noOptims $ do
                   {
                     -- removal of unnecessary constructs: constants
                   ; t_const_prop
                   ; t_inl_letali
                   ; t_elim_trivapp
                   }

               ; when (isCoreTarget || not isLamLifted) $ do
                   {
					 -- put in A-normal form, where args to app only may be identifiers
				   ; u1 <- freshInfUID
				   ; t_anormal u1
                   }

%%[[(9 wholeprogAnal)
               ; when (not isFromCoreSrc && targetDoesHPTAnalysis (ehcOptTarget opts))
                      t_fix_dictfld
%%]]
               
               ; when (not isLamLifted && not isCoreTarget) $ do
                   {
					 -- pass all globals used in lambda explicit as argument
				   ; t_lam_asarg

					 -- pass all globals used in CAF explicit as argument
				   ; t_caf_asarg
				   ; t_let_unrec
				   ; u2 <- freshInfUID
				   ; t_anormal u2

					 -- float lam/CAF to global level
				   ; t_float_glob
					 -- from now on INVARIANT: no local lambdas
					 --             ASSUME   : 
                   }

%%[[(8 wholeprogAnal)
               ; when (targetDoesHPTAnalysis (ehcOptTarget opts))
                      t_find_null
%%]]
               ; when (ehcOptOptimizes Optimize_StrictnessAnalysis opts)
                      (do { t_let_defbefuse
                          ; t_ana_relev
                          ; t_opt_strict
                          })
               ; when (ehcOptIsViaCoreJavaScript opts)
                      (do { {- t_let_flatstr
                          ; -} t_ren_uniq (emptyRenUniqOpts {renuniqOptResetOnlyInLam = True})
                          })
               ; when (not isCoreTarget) {- isFromCoreSrc -} $ do
                   { -- ensure def before use ordering
                     t_let_defbefuse' osmw

                   }
               }

        lamMpPropagate l s@(TrfState {trfstExtra=e@(TrfCoreExtra{trfcoreGathLamMp=gl, trfcoreInhLamMp=il})})
          = s {trfstExtra = e {trfcoreGathLamMp = gl', trfcoreInhLamMp = Map.union gl' il}}
          where gl' = Map.union l gl

        -- actual transformations
        t_initial       = liftTrfModPlain  osmw "initial"           $ id
%%[[(50 corein)
        t_fix_postparse	= liftTrfModPlain  osm "fix-postparse"  	$ cmodTrfFixAfterParse dataGam
%%]]
%%[[(8 coresysf)
        t_sysf_check    = liftCheckMod     osm "sysf-type-check"    $ \s -> cmodSysfCheck opts (emptyCheckEnv {cenvLamMp = trfcoreInhLamMp $ trfstExtra s})
%%]]
        t_eta_red       = liftTrfModPlain  osm "eta-red"            $ cmodTrfEtaRed
        t_erase_ty      = liftTrfModWithStateExtra osmw "erase-ty" lamMpPropagate
                                                               $ \_ -> cmodTrfEraseExtractTysigCore opts
        t_ann_simpl     = liftTrfModPlain  osm "ann-simpl"          $ cmodTrfAnnBasedSimplify opts
        t_ren_uniq    o = liftTrfModPlain  osm "ren-uniq"           $ cmodTrfRenUniq o
        t_let_unrec     = liftTrfModPlain  osm "let-unrec"          $ cmodTrfLetUnrec
        t_let_defbefuse' os
                        = liftTrfModPlain  os  "let-defbefuse"      $ cmodTrfLetDefBeforeUse
        t_let_defbefuse = t_let_defbefuse' osm
        t_let_flatstr   = liftTrfModPlain  osm "let-flatstr"        $ cmodTrfLetFlattenStrict
        t_inl_letali    = liftTrfModPlain  osm "inl-letali"         $ cmodTrfInlineLetAlias
%%[[50
                                                              (Map.keysSet $ trfcoreExpNmOffMp $ trfstExtra trfcore)
%%]]
        t_elim_trivapp  = liftTrfModPlain  osm "elim-trivapp"       $ cmodTrfElimTrivApp opts
        t_const_prop    = liftTrfModPlain  osm "const-prop"         $ cmodTrfConstProp opts
        t_anormal     u = liftTrfModPlain  osm "anormal"            $ cmodTrfANormal modNm u
        t_lam_asarg     = liftTrfModPlain  osm "lam-asarg"          $ cmodTrfLamGlobalAsArg
        t_caf_asarg     = liftTrfModPlain  osm "caf-asarg"          $ cmodTrfCAFGlobalAsArg
        t_float_glob    = liftTrfModPlain  osm "float-glob"         $ cmodTrfFloatToGlobal
%%[[(8 wholeprogAnal)
        t_find_null     = liftTrfModPlain  osm "find-null"          $ cmodTrfFindNullaries
%%]]
        t_ana_relev     = liftTrfModWithStateExtra osm "ana-relev" lamMpPropagate
                                                               $ \s -> cmodTrfAnaRelevance opts dataGam (trfcoreInhLamMp $ trfstExtra s)
        t_opt_strict    = liftTrfModWithStateExtra osm "optim-strict" lamMpPropagate
                                                               $ \s -> cmodTrfOptimizeStrictness opts (trfcoreInhLamMp $ trfstExtra s)
%%[[(9 wholeprogAnal)
        t_fix_dictfld   = liftTrfModPlain  osm "fix-dictfld"        $ cmodTrfFixDictFields
%%]]
%%[[99        
        t_expl_trace    = liftTrfModWithStateExtra osm "expl-sttrace"
                                                  (\m s@(TrfState {trfstExtra=e@(TrfCoreExtra {trfcoreExtraExports=exps})})
                                                     -> (lamMpPropagate m s)
                                                          { trfstExtra = 
                                                              e { trfcoreExtraExports   = exps `Set.union`
                                                                                    Set.fromList [ n
                                                                                                 | (n,LamInfo {laminfoStackTrace=(StackTraceInfo_IsStackTraceEquiv _)}) <- Map.toList m
                                                                                                 ]
                                                          }     }
                                                  )            $ \s -> cmodTrfExplicitStackTrace opts (trfcoreInhLamMp $ trfstExtra s)
%%]]
        -- abbreviations for optimatisation scope
        osm  = [OptimizationScope_PerModule]
        osmw = 
%%[[50
			   [OptimizationScope_WholeCore] ++
%%]]
			   osm
%%]

