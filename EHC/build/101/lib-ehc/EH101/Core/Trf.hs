module EH101.Core.Trf
( TrfCore (..), emptyTrfCore
, trfCore )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import EH101.Base.Target
import EH101.EHC.Common
import EH101.LamInfo
import EH101.Core
import EH101.Core.Trf.RenUniq
import EH101.Core.Trf.ANormal
import EH101.Core.Trf.InlineLetAlias
import EH101.Core.Trf.LetUnrec
import EH101.Core.Trf.LetDefBeforeUse
import EH101.Core.Trf.LamGlobalAsArg
import EH101.Core.Trf.CAFGlobalAsArg
import EH101.Core.Trf.FloatToGlobal
import EH101.Core.Trf.ConstProp
import EH101.Core.Trf.EtaRed
import EH101.Core.Trf.ElimTrivApp
import EH101.Core.Trf.AnnBasedSimplify
import EH101.Core.Trf.AnaRelevance
import EH101.Core.Trf.LetFlattenStrict
import EH101.Core.Trf.OptimizeStrictness
import EH101.Core.Trf.EraseExtractTysigCore
import Debug.Trace
import EH101.Core.Trf.ExplicitStackTrace



{-# LINE 61 "src/ehc/Core/Trf.chs" #-}
modifyGets :: MonadState s m => (s -> (a,s)) -> m a
modifyGets update
  = do { s <- get
       ; let (x,s') = update s
       ; put s'
       ; return x
       }

{-# LINE 75 "src/ehc/Core/Trf.chs" #-}
data TrfCore
  = TrfCore
      { trfcoreCore         	:: !CModule
      , trfcoreCoreStages   	:: [(String,CModule)]
      , trfcoreUniq         	:: !UID
      , trfcoreInhLamMp         :: LamMp        -- from context, possibly overridden from gathered one
      , trfcoreGathLamMp        :: !LamMp       -- gathered anew
      , trfcoreExpNmOffMp       :: !HsName2OffsetMp
      , trfcoreExtraExports     :: !FvS             -- extra exported names, introduced by transformations
      }

emptyTrfCore :: TrfCore
emptyTrfCore = TrfCore emptyCModule [] uidStart
                       Map.empty Map.empty
                       Map.empty
                       Set.empty

-- type TrfCoreState x = State TrfCore x

{-# LINE 108 "src/ehc/Core/Trf.chs" #-}
trfCore :: EHCOpts -> DataGam -> HsName -> TrfCore -> TrfCore
trfCore opts dataGam modNm trfcore
  = execState trf trfcore
  where trf
          = do { -- initial is just to obtain Core for dumping stages
                 t_initial

                 -- removal of unnecessary constructs: simplifications based on annotations (experimential, temporary)
               ; t_ann_simpl

                 -- removal of unnecessary constructs: eta expansions
               ; t_eta_red

                 -- erase type signatures, extract the core + ty combi at this stage
               ; t_erase_ty

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

                 -- optionally modify to include explicit stack trace
               ; when (ehcOptTargetFlavor opts == TargetFlavor_Debug)
                      (do { t_expl_trace
                            -- from now on INVARIANT: renaming of identifiers must also rename additional exported names here introduced

                          ; t_let_unrec
                          -- ; t_ren_uniq
                          })

                 -- removal of unnecessary constructs: constants
               ; t_const_prop
               ; t_inl_letali
               ; t_elim_trivapp

                 -- put in A-normal form, where args to app only may be identifiers
               ; u1 <- modifyGets uniq
               ; t_anormal u1


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

        liftTrf :: String -> (CModule -> CModule) -> State TrfCore ()
        liftTrf nm t
          = liftTrf2 nm (flip const) (\_ c -> (t c,()))

        liftTrf' :: String -> (TrfCore -> CModule -> CModule) -> State TrfCore ()
        liftTrf' nm t
          = liftTrf2 nm (flip const) (\s c -> (t s c,()))

        liftTrf2 nm update2 t
          = modify update
          where update s@(TrfCore{trfcoreCore=c, trfcoreCoreStages=stages})
                  = update2 extra
                    $ s { trfcoreCore           = c'
                        , trfcoreCoreStages     = if ehcOptDumpCoreStages opts then stages ++ [(nm,c')] else stages
                        }
                  where (c',extra) = t s c

        lamMpPropagate l s@(TrfCore {trfcoreGathLamMp=gl, trfcoreInhLamMp=il})
          = s {trfcoreGathLamMp = gl', trfcoreInhLamMp = Map.union gl' il}
          where gl' = Map.union l gl

        uniq s@(TrfCore{trfcoreUniq=u})
          = (h,s {trfcoreUniq = n})
          where (n,h) = mkNewLevUID u

        t_initial       = liftTrf  "initial"            $ id
        t_eta_red       = liftTrf  "eta-red"            $ cmodTrfEtaRed
        t_erase_ty      = liftTrf2 "erase-ty" lamMpPropagate
                                                        $ \s -> cmodTrfEraseExtractTysigCore opts
        t_ann_simpl     = liftTrf  "ann-simpl"          $ cmodTrfAnnBasedSimplify opts
        t_ren_uniq    o = liftTrf  "ren-uniq"           $ cmodTrfRenUniq o
        t_let_unrec     = liftTrf  "let-unrec"          $ cmodTrfLetUnrec
        t_let_defbefuse = liftTrf  "let-defbefuse"      $ cmodTrfLetDefBeforeUse
        t_let_flatstr   = liftTrf  "let-flatstr"        $ cmodTrfLetFlattenStrict
        t_inl_letali    = liftTrf  "inl-letali"         $ cmodTrfInlineLetAlias
                                                              (Map.keysSet $ trfcoreExpNmOffMp trfcore)
        t_elim_trivapp  = liftTrf  "elim-trivapp"       $ cmodTrfElimTrivApp opts
        t_const_prop    = liftTrf  "const-prop"         $ cmodTrfConstProp opts
        t_anormal     u = liftTrf  "anormal"            $ cmodTrfANormal modNm u
        t_lam_asarg     = liftTrf  "lam-asarg"          $ cmodTrfLamGlobalAsArg
        t_caf_asarg     = liftTrf  "caf-asarg"          $ cmodTrfCAFGlobalAsArg
        t_float_glob    = liftTrf  "float-glob"         $ cmodTrfFloatToGlobal
        t_ana_relev     = liftTrf2 "ana-relev" lamMpPropagate
                                                        $ \s -> cmodTrfAnaRelevance opts dataGam (trfcoreInhLamMp s)
        t_opt_strict    = liftTrf2 "optim-strict" lamMpPropagate
                                                        $ \s -> cmodTrfOptimizeStrictness opts (trfcoreInhLamMp s)
        t_expl_trace    = liftTrf2 "expl-sttrace" (\m s@(TrfCore {trfcoreExtraExports=exps})
                                                     -> (lamMpPropagate m s)
                                                          { trfcoreExtraExports   = exps `Set.union`
                                                                                    Set.fromList [ n
                                                                                                 | (n,LamInfo {laminfoStackTrace=(StackTraceInfo_IsStackTraceEquiv _)}) <- Map.toList m
                                                                                                 ]
                                                          }
                                                  )     $ \s -> cmodTrfExplicitStackTrace opts (trfcoreInhLamMp s)

