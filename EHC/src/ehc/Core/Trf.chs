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

%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%[(8 codegen) import({%{EH}EHC.Common})
%%]

-- LamInfo
%%[(8 codegen) import({%{EH}LamInfo})
%%]

-- Core
%%[(8 codegen) import({%{EH}Core})
%%]

-- Core transformations
%%[(8 codegen) import({%{EH}Core.Trf.RenUniq}, {%{EH}Core.Trf.ANormal}, {%{EH}Core.Trf.InlineLetAlias}, {%{EH}Core.Trf.LetUnrec})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LamGlobalAsArg}, {%{EH}Core.Trf.CAFGlobalAsArg}, {%{EH}Core.Trf.FloatToGlobal}, {%{EH}Core.Trf.ConstProp})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EtaRed}, {%{EH}Core.Trf.ElimTrivApp}, {%{EH}Core.Trf.FindNullaries})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.AnnBasedSimplify})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.AnaRelevance})
%%]
%%[(8 codegen) import({%{EH}Core.Trf.LetFlattenStrict})
%%]
%%[(9 codegen) import({%{EH}Core.Trf.FixDictFields})
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
      { trfcoreCore         	:: !CModule
      , trfcoreCoreStages   	:: [(String,CModule)]
      , trfcoreUniq         	:: !UID
%%[[20
      , trfcoreExpNmOffMp       :: !HsName2OffsetMp
%%]]
%%[[99
      , trfcoreInhLamMp         :: !LamMp       -- from context
      , trfcoreGathLamMp        :: !LamMp       -- gathered anew
      , trfcoreExtraExports     :: !FvS             -- extra exported names, introduced by transformations
%%]]
      }

emptyTrfCore :: TrfCore
emptyTrfCore = TrfCore emptyCModule [] uidStart
%%[[20
                       Map.empty
%%]]
%%[[99
                       Map.empty Map.empty
                       Set.empty
%%]]

-- type TrfCoreState x = State TrfCore x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(trfCore)
trfCore :: EHCOpts -> DataGam -> HsName -> TrfCore -> TrfCore
trfCore opts dataGam modNm trfcore
  = snd $ runState trf trfcore
  where trf
          = do { -- initial is just to obtain Core for dumping stages
                 t_initial
                 
                 -- removal of unnecessary constructs: simplifications based on annotations (experimential, temporary)
               ; t_ann_simpl

                 -- removal of unnecessary constructs: eta expansions
               ; t_eta_red

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

%%[[9
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
               ; when (targetDoesHPTAnalysis (ehcOptTarget opts))
                      t_find_null
               ; when (ehcOptOptimizes Optimize_StrictnessAnalysis opts)
                      t_ana_relev
               ; when (targetIsJScript (ehcOptTarget opts))
                      (do { {- t_let_flatstr
                          ; -} t_ren_uniq (emptyRenUniqOpts {renuniqOptResetOnlyInLam = True})
                          })
               }

        liftTrf :: String -> (CModule -> CModule) -> State TrfCore ()
        liftTrf nm t
          = liftTrf2 nm (flip const) (\c -> (t c,()))

        liftTrf2 nm update2 t
          = modify update
          where update s@(TrfCore{trfcoreCore=c, trfcoreCoreStages=stages})
                  = update2 extra
                    $ s { trfcoreCore           = c'
                        , trfcoreCoreStages     = if ehcOptDumpCoreStages opts then stages ++ [(nm,c')] else stages
                        }
                  where (c',extra) = t c

        uniq s@(TrfCore{trfcoreUniq=u})
          = (h,s {trfcoreUniq = n})
          where (n,h) = mkNewLevUID u

        inlineLetAlias
          = cmodTrfInlineLetAlias
%%[[20
              (Map.keysSet $ trfcoreExpNmOffMp trfcore)
%%]]

        t_initial       = liftTrf  "initial"            $ id
        t_eta_red       = liftTrf  "eta-red"            $ cmodTrfEtaRed
        t_ann_simpl     = liftTrf  "ann-simpl"          $ cmodTrfAnnBasedSimplify opts
        t_ren_uniq    o = liftTrf  "ren-uniq"           $ cmodTrfRenUniq o
        t_let_unrec     = liftTrf  "let-unrec"          $ cmodTrfLetUnrec
        t_let_flatstr   = liftTrf  "let-flatstr"        $ cmodTrfLetFlattenStrict
        t_inl_letali    = liftTrf  "inl-letali"         $ inlineLetAlias
        t_elim_trivapp  = liftTrf  "elim-trivapp"       $ cmodTrfElimTrivApp opts
        t_const_prop    = liftTrf  "const-prop"         $ cmodTrfConstProp opts
        t_anormal     u = liftTrf  "anormal"            $ cmodTrfANormal modNm u
        t_lam_asarg     = liftTrf  "lam-asarg"          $ cmodTrfLamGlobalAsArg
        t_caf_asarg     = liftTrf  "caf-asarg"          $ cmodTrfCAFGlobalAsArg
        t_float_glob    = liftTrf  "float-glob"         $ cmodTrfFloatToGlobal
        t_find_null     = liftTrf  "find-null"          $ cmodTrfFindNullaries
        t_ana_relev     = liftTrf  "ana-relev"          $ cmodTrfAnaRelevance opts dataGam
%%[[9
        t_fix_dictfld   = liftTrf  "fix-dictfld"        $ cmodTrfFixDictFields
%%]]
%%[[99        
        t_expl_trace    = liftTrf2 "expl-sttrace" (\m s@(TrfCore {trfcoreExtraExports=exps})
                                                     -> s { trfcoreGathLamMp      = m
                                                          , trfcoreExtraExports   = exps `Set.union`
                                                                                    Set.fromList [ n
                                                                                                 | (n,LamInfo {laminfoStackTrace=(StackTraceInfo_IsStackTraceEquiv _)}) <- Map.toList m
                                                                                                 ]
                                                          }
                                                  )     $ cmodTrfExplicitStackTrace opts (trfcoreInhLamMp trfcore)
%%]]
%%]
