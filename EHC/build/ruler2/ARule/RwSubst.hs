

-- UUAGC 0.9.39.1 (build/ruler2/ARule/RwSubst.ag)
module ARule.RwSubst(exprSubst, exprElimCnstr, exprRewrite', exprRewrite, arlSubst, nmSubst, MtOut (..), exprMatch, fmNmUniq, fmNmFmtCmd) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH.Util.Pretty
import EH.Util.Utils
import Common
import Opts
import Expr.Expr
import Expr.IsRw
import Expr.ToAEqn
import ARule.ARule
import ARule.PrettyPrint
import FmGam
import ECnstrGam
import RwExprGam













wrapARule' :: Opts -> {- [Nm] -> AtDefdGam -> -} FmGam Expr -> ARule -> Syn_AGARuleItf
wrapARule' o {- co ag -} fg rl
  = let r1 = sem_AGARuleItf (AGARuleItf_AGItf rl)
    in  wrap_AGARuleItf r1
            (Inh_AGARuleItf {opts_Inh_AGARuleItf = o
                            -- ,croNmL_Inh_AGARuleItf = co
                            -- ,adGam_Inh_AGARuleItf = ag
                            ,fmGam_Inh_AGARuleItf = fg
                            })

arlSubst :: FmGam Expr -> ARule -> ARule
arlSubst fg = repl_Syn_AGARuleItf . wrapARule' defaultOpts {- [] emptyGam -} fg



wrapExpr' :: Opts -> FmGam Expr -> RwExprGam -> ECnstrGam -> {- RnMp -> -} Expr -> Syn_AGExprItf
wrapExpr' o fmg rwg ecg {- rnm -} e
  = let r1 = sem_AGExprItf (AGExprItf_AGItf e)
    in  wrap_AGExprItf r1
            (Inh_AGExprItf { opts_Inh_AGExprItf = o
                           , fmGam_Inh_AGExprItf = fmg, rwGam_Inh_AGExprItf = rwg, ecGam_Inh_AGExprItf = ecg
                           -- , rnMp_Inh_AGExprItf = rnm
                           })

exprSubst :: Opts -> FmGam Expr -> Expr -> Expr
exprSubst o fmg = repl_Syn_AGExprItf . wrapExpr' o fmg emptyGam emptyGam {- Map.empty -}

exprElimCnstr :: Expr -> (Expr,ECnstrGam)
exprElimCnstr e
  = (replEc_Syn_AGExprItf r,ecElimGam_Syn_AGExprItf r)
  where r = wrapExpr' defaultOpts emptyGam emptyGam emptyGam {- Map.empty -} e

exprRewrite :: Opts -> FmGam Expr -> RwExprGam -> ECnstrGam -> Expr -> Expr
exprRewrite o fmg rwg ecg e
  = r
  where (r,_,_) = exprRewrite' o fmg rwg ecg e

exprRewrite' :: Opts -> FmGam Expr -> RwExprGam -> ECnstrGam -> Expr -> (Expr,[AEqn],FmGam Expr)
exprRewrite' o fmg rwg ecg e
  = (repl_Syn_AGExprItf r,aEqnL_Syn_AGExprItf r,aEqnFmGam_Syn_AGExprItf r)
  where r = wrapExpr' o fmg rwg ecg {- Map.empty -} e

nmSubst :: Opts -> FmGam Expr -> Nm -> Nm
nmSubst o g = exprAsNm . exprSubst o g . Expr_Var




fmNmUniq :: Opts -> FmGam Expr -> Int -> Nm
fmNmUniq o g u = nmSubst (o {optGenFM = FmAG}) g (nmUniq u)

fmNmFmtCmd :: Opts -> FmGam Expr -> Nm -> Nm
fmNmFmtCmd o g n = nmSubst (o {optGenFM = FmFmtCmd}) g n




nmMatch :: Opts -> FmKind -> Nm -> FmGam Expr -> Maybe ([Maybe String],[Maybe String],Expr)
nmMatch opts fk n fmGam
  = match
  where nmL = nmToMbL n
        nmLen = length nmL
        nLL
          = if optSubstFullNm opts
            then [nmL]
            else reverse . tail . inits $ nmL
        match
          = foldr (\nL m
                    -> case fmGamLookup (nmFromMbL nL) fk fmGam of
                         Just e | isOkLen
                           -> Just (nL,drop len nmL,e)
                           where isOkLen = maybe True (\n -> len == length (nmToMbL n) || len == nmLen) (mbNmOfSel e)
                                 len = length nL
                         _ -> m
                  )
                  Nothing nLL

nmLAsSelExpr :: (Expr -> Expr) -> Expr -> [Maybe String] -> Expr
nmLAsSelExpr subst e nL
  = case nL of
      [] -> e
      _  -> Expr_SelTop . foldl Expr_Sel e $ eL
         where eL = map (fmap (subst . Expr_Var . Nm)) nL

mbNmOfSel :: Expr -> Maybe Nm
mbNmOfSel e
  = t e
  where t (Expr_SelTop st) = n st
        t e                = exprMbNm e
        n (Expr_Sel se (Just (Expr_Var (Nm s)))) = fmap (\n -> NmSel n (Just s)) (n se)
        n (Expr_Sel se Nothing)                  = fmap (\n -> NmSel n Nothing) (n se)
        n (Expr_Var n)                           = Just n
        n _                                      = Nothing

mbNmLOfSel :: Expr -> Maybe [Maybe String]
mbNmLOfSel = fmap nmToMbL . mbNmOfSel

exprVarSubst :: Opts -> FmGam Expr -> RwExprGam -> ECnstrGam -> Expr -> Nm -> Expr
exprVarSubst opts fmGam rwGam ecGam dfltRepl nm
  = case nmMatch opts (optGenFM opts) nm fmGam of
      Just (matchNmL,remNmL,matchExpr)
        -> case mbNmLOfSel se of
             Just sNmL
               -> Expr_Var (nmFromMbL (sNmL ++ remNmL))
             _ -> nmLAsSelExpr id se remNmL
        where se = if optSubstOnce opts then matchExpr else sbsWoNm matchExpr
              sbsWoNm = exprSubst opts (nmFromMbL matchNmL `gamDelete` fmGam)
      _ -> dfltRepl



mkRwExpr :: InEqnLoc -> Opts -> FmGam Expr -> RwExprGam -> ECnstrGam -> Expr -> (Expr,FmGam Expr,ECnstrGam)
mkRwExpr inEqnLoc opts fmGam rwGam ecGam repl
  = case exprIsRw repl of
      ExprIsRw n | isJust mbRw
        -> (r,mtFmGam mt,ecg)
        where mbRw = rwGamLookup n (optGenFM opts) (if inEqnLoc == EqnInRhs then AtIn else AtOut) rwGam
              (r,mt,ecg)
                = foldr (\(me,e) r
                          -> let mt = exprMatch opts ecGam repl me
                             in  if mtMatches mt
                                 then let e2 = exprSubst opts (mtFmGam mt `gamUnion` fmGam) e
                                          (e3,ecg) = exprElimCnstr e2
                                      in  (e3,mt,ecg)
                                 else r
                        )
                        (repl,emptyEMtOut,emptyGam)
                        (maybe (panic "mkRwExpr") (\x -> x) mbRw)
      _ -> (repl,emptyGam,emptyGam)



data InEqnLoc
  = EqnInTop | EqnInLhs | EqnInRhs
  deriving (Show,Eq,Ord)



data MtOut e
  = MtOut {mtMatches :: Bool, mtExpr :: e, mtFmGam :: FmGam Expr}

instance Show (MtOut e) where
  show _ = "MtOut"

instance PP e => PP (MtOut e) where
  pp i = "Mt" >#< pp (mtMatches i) >#< pp (mtExpr i) >#< ppGam (mtFmGam i)

emptyMtOut e = MtOut {mtMatches = True, mtExpr = e, mtFmGam = emptyGam}
emptyEMtOut = emptyMtOut Expr_Empty
emptyCMtOut = emptyMtOut ECnstr_Empty

-- lhs into rhs matching, expects rhs, given lhs
exprMatch :: Opts -> ECnstrGam -> Expr -> Expr -> MtOut Expr
exprMatch opts ecGam e1 e2
  = r
  where r = mt e1 e2
        -- r' = trp "XX" (pp e1 >-< pp e2 >-< pp r) $ r
        mt (Expr_Int i1)            e2@(Expr_Int i2)        | i1 == i2      = res e2
        mt (Expr_StrText s1)        e2@(Expr_StrText s2)    | s1 == s2      = res e2
        mt (Expr_StrAsIs s1)        e2@(Expr_StrAsIs s2)    | s1 == s2      = res e2
        mt (Expr_Empty)             (Expr_Empty)                            = res Expr_Empty
        mt (Expr_AppTop e1)         (Expr_AppTop e2)                        = let m = mt e1 e2 in res' (Expr_AppTop (mtExpr m)) m
        mt (Expr_Paren e1)          (Expr_Paren e2@(Expr_Var n2))           = bnd n2 e1 $ res e2
        mt (Expr_Paren e1)          e2@(Expr_Var n2)                        = bnd n2 e1 $ res e2
        mt (Expr_Paren e1)          (Expr_Paren e2)                         = let m = mt e1 e2 in res' (Expr_Paren (mtExpr m)) m
        mt e1                       (Expr_Paren e2)                         = mt e1 e2
        mt (Expr_Op n1 ne1 l1 r1)   e2@(Expr_Var n2)        | not (optMatchROpOpnd opts)
                                                                            = err
        mt e1                       e2@(Expr_Var n2)                        = bnd n2 e1 $ res e2
        mt (Expr_Op n1 ne1 l1 r1)   (Expr_Op n2 ne2 l2 r2)  | n1 == n2
          = bnd' (m2 {mtFmGam = n1 `gamDelete` mtFmGam m2}) m1
          where m1 = app l1 l2 r1 r2 (\l r -> Expr_Op n2 ne2 l r)
                m2 = mt ne1 ne2
        mt (Expr_App l1 r1)         (Expr_App l2 r2)                        = app l1 l2 r1 r2 Expr_App
        mt (Expr_SelTop t1)         e2@(Expr_SelTop t2)                     = bnd' (mt t1 t2) $ res e2
        mt (Expr_Sel e1 (Just s1))  (Expr_Sel e2 (Just s2))                 = app e1 e2 s1 s2 (\l r -> Expr_Sel l (Just r))
        mt (Expr_Named n e1)        e2                                      = let m = mt e1 e2 in res' (Expr_Named n (mtExpr m)) m
        mt e1                       (Expr_Cnstr e2 c2)
          = case ecGamLookup e1 ecGam of
              Just c1 | mtMatches mc
                -> bnd' mc $ mt e1 e2
                where mc = mtc c1 c2
              _ -> err
        mt _                        _                                       = err
        mtc (ECnstr_Ty t1)          c2@(ECnstr_Ty t2)       | not (null t)
          = resc (ECnstr_Ty t)
          where t = t1 `intersect` t2
        mtc c1                      c2@(ECnstr_Var n2)                      = bndc n2 c1 $ resc c2
        mtc _                       _                                       = errc
        app l1 l2 r1 r2 mk
          = foldr1 (\m1 m2 -> if mtMatches m1 then m2 else m1) [ml,mr,m]
          where ml = mt l1 l2
                mr = mt r1 r2
                m  = bnd' ml . res' (mk (mtExpr ml) (mtExpr mr)) $ mr
        bnd' :: MtOut e' -> MtOut e -> MtOut e
        bnd' mn  m = m {mtFmGam = mtFmGam mn `fmGamUnion` mtFmGam m}
        bnd  n e m = bnd' (emptyEMtOut {mtFmGam = fmSingleton n FmAll e}) m
        bndc n e m = bnd' (emptyCMtOut {mtFmGam = fmSingleton n FmCnstr (Expr_WrapCnstr e)}) m
        res' :: e -> MtOut e' -> MtOut e
        res'   e m = m {mtExpr = e}
        res    e   = res' e emptyEMtOut
        resc   e   = res' e emptyEMtOut
        err' :: MtOut e -> MtOut e
        err'     m = m {mtMatches = False}
        err        = err' emptyEMtOut
        errc       = err' emptyCMtOut

-- AEqn --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Eqn:
         child dest           : AEqnDest 
         child val            : AExpr 
         visit 0:
            local repl        : _
      alternative Err:
         child expr           : Expr 
         visit 0:
            local fmGam       : _
            local rwGam       : _
            local ecGam       : _
            local inEqnLoc    : _
            local repl        : _
-}
-- cata
sem_AEqn :: AEqn  ->
            T_AEqn 
sem_AEqn (AEqn_Eqn _dest _val )  =
    (sem_AEqn_Eqn (sem_AEqnDest _dest ) (sem_AExpr _val ) )
sem_AEqn (AEqn_Err _expr )  =
    (sem_AEqn_Err (sem_Expr _expr ) )
-- semantic domain
type T_AEqn  = (FmGam Expr) ->
               Opts ->
               ( AEqn )
sem_AEqn_Eqn :: T_AEqnDest  ->
                T_AExpr  ->
                T_AEqn 
sem_AEqn_Eqn dest_ val_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqn 
              _destOfmGam :: (FmGam Expr)
              _destOopts :: Opts
              _valOfmGam :: (FmGam Expr)
              _valOopts :: Opts
              _destIrepl :: AEqnDest 
              _valIrepl :: AExpr 
              -- self rule
              _repl =
                  AEqn_Eqn _destIrepl _valIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _destOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _destOopts =
                  _lhsIopts
              -- copy rule (down)
              _valOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _valOopts =
                  _lhsIopts
              ( _destIrepl) =
                  dest_ _destOfmGam _destOopts 
              ( _valIrepl) =
                  val_ _valOfmGam _valOopts 
          in  ( _lhsOrepl)))
sem_AEqn_Err :: T_Expr  ->
                T_AEqn 
sem_AEqn_Err expr_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqn 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 99, column 21)
              _fmGam =
                  emptyGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 99, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 99, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 250, column 21)
              _inEqnLoc =
                  EqnInTop
              -- self rule
              _repl =
                  AEqn_Err _exprIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (from local)
              _exprOecGam =
                  _ecGam
              -- copy rule (from local)
              _exprOfmGam =
                  _fmGam
              -- copy rule (from local)
              _exprOinEqnLoc =
                  _inEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (from local)
              _exprOrwGam =
                  _rwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOrepl)))
-- AEqnDest ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Many:
         child dests          : AEqnDests 
         visit 0:
            local repl        : _
      alternative One:
         child anm            : ANm 
         visit 0:
            local repl        : _
-}
-- cata
sem_AEqnDest :: AEqnDest  ->
                T_AEqnDest 
sem_AEqnDest (AEqnDest_Many _dests )  =
    (sem_AEqnDest_Many (sem_AEqnDests _dests ) )
sem_AEqnDest (AEqnDest_One _anm )  =
    (sem_AEqnDest_One (sem_ANm _anm ) )
-- semantic domain
type T_AEqnDest  = (FmGam Expr) ->
                   Opts ->
                   ( AEqnDest )
sem_AEqnDest_Many :: T_AEqnDests  ->
                     T_AEqnDest 
sem_AEqnDest_Many dests_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqnDest 
              _destsOfmGam :: (FmGam Expr)
              _destsOopts :: Opts
              _destsIrepl :: AEqnDests 
              -- self rule
              _repl =
                  AEqnDest_Many _destsIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _destsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _destsOopts =
                  _lhsIopts
              ( _destsIrepl) =
                  dests_ _destsOfmGam _destsOopts 
          in  ( _lhsOrepl)))
sem_AEqnDest_One :: T_ANm  ->
                    T_AEqnDest 
sem_AEqnDest_One anm_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqnDest 
              _anmOopts :: Opts
              _anmIecElimGam :: ECnstrGam
              _anmIrepl :: ANm 
              _anmIreplEc :: ANm 
              _anmIrwEcGam :: ECnstrGam
              _anmIrwMtGam :: (FmGam Expr)
              -- self rule
              _repl =
                  AEqnDest_One _anmIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _anmOopts =
                  _lhsIopts
              ( _anmIecElimGam,_anmIrepl,_anmIreplEc,_anmIrwEcGam,_anmIrwMtGam) =
                  anm_ _anmOopts 
          in  ( _lhsOrepl)))
-- AEqnDests ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqnDest 
         child tl             : AEqnDests 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_AEqnDests :: AEqnDests  ->
                 T_AEqnDests 
sem_AEqnDests list  =
    (Prelude.foldr sem_AEqnDests_Cons sem_AEqnDests_Nil (Prelude.map sem_AEqnDest list) )
-- semantic domain
type T_AEqnDests  = (FmGam Expr) ->
                    Opts ->
                    ( AEqnDests )
sem_AEqnDests_Cons :: T_AEqnDest  ->
                      T_AEqnDests  ->
                      T_AEqnDests 
sem_AEqnDests_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqnDests 
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _hdIrepl :: AEqnDest 
              _tlIrepl :: AEqnDests 
              -- self rule
              _repl =
                  (:) _hdIrepl _tlIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIrepl) =
                  hd_ _hdOfmGam _hdOopts 
              ( _tlIrepl) =
                  tl_ _tlOfmGam _tlOopts 
          in  ( _lhsOrepl)))
sem_AEqnDests_Nil :: T_AEqnDests 
sem_AEqnDests_Nil  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqnDests 
              -- self rule
              _repl =
                  []
              -- self rule
              _lhsOrepl =
                  _repl
          in  ( _lhsOrepl)))
-- AEqns -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : AEqn 
         child tl             : AEqns 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_AEqns :: AEqns  ->
             T_AEqns 
sem_AEqns list  =
    (Prelude.foldr sem_AEqns_Cons sem_AEqns_Nil (Prelude.map sem_AEqn list) )
-- semantic domain
type T_AEqns  = (FmGam Expr) ->
                Opts ->
                ( AEqns )
sem_AEqns_Cons :: T_AEqn  ->
                  T_AEqns  ->
                  T_AEqns 
sem_AEqns_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqns 
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _hdIrepl :: AEqn 
              _tlIrepl :: AEqns 
              -- self rule
              _repl =
                  (:) _hdIrepl _tlIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIrepl) =
                  hd_ _hdOfmGam _hdOopts 
              ( _tlIrepl) =
                  tl_ _tlOfmGam _tlOopts 
          in  ( _lhsOrepl)))
sem_AEqns_Nil :: T_AEqns 
sem_AEqns_Nil  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AEqns 
              -- self rule
              _repl =
                  []
              -- self rule
              _lhsOrepl =
                  _repl
          in  ( _lhsOrepl)))
-- AExpr -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local rwGam       : _
            local ecGam       : _
            local inEqnLoc    : _
            local repl        : _
-}
-- cata
sem_AExpr :: AExpr  ->
             T_AExpr 
sem_AExpr (AExpr_Expr _expr )  =
    (sem_AExpr_Expr (sem_Expr _expr ) )
-- semantic domain
type T_AExpr  = (FmGam Expr) ->
                Opts ->
                ( AExpr )
sem_AExpr_Expr :: T_Expr  ->
                  T_AExpr 
sem_AExpr_Expr expr_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: AExpr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 104, column 21)
              _rwGam =
                  emptyGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 104, column 21)
              _ecGam =
                  emptyGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 247, column 21)
              _inEqnLoc =
                  EqnInTop
              -- self rule
              _repl =
                  AExpr_Expr _exprIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (from local)
              _exprOecGam =
                  _ecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (from local)
              _exprOinEqnLoc =
                  _inEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (from local)
              _exprOrwGam =
                  _rwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOrepl)))
-- AGARuleItf --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : ARule 
   alternatives:
      alternative AGItf:
         child rule           : ARule 
-}
-- cata
sem_AGARuleItf :: AGARuleItf  ->
                  T_AGARuleItf 
sem_AGARuleItf (AGARuleItf_AGItf _rule )  =
    (sem_AGARuleItf_AGItf (sem_ARule _rule ) )
-- semantic domain
type T_AGARuleItf  = (FmGam Expr) ->
                     Opts ->
                     ( ARule )
data Inh_AGARuleItf  = Inh_AGARuleItf {fmGam_Inh_AGARuleItf :: (FmGam Expr),opts_Inh_AGARuleItf :: Opts}
data Syn_AGARuleItf  = Syn_AGARuleItf {repl_Syn_AGARuleItf :: ARule }
wrap_AGARuleItf :: T_AGARuleItf  ->
                   Inh_AGARuleItf  ->
                   Syn_AGARuleItf 
wrap_AGARuleItf sem (Inh_AGARuleItf _lhsIfmGam _lhsIopts )  =
    (let ( _lhsOrepl) = sem _lhsIfmGam _lhsIopts 
     in  (Syn_AGARuleItf _lhsOrepl ))
sem_AGARuleItf_AGItf :: T_ARule  ->
                        T_AGARuleItf 
sem_AGARuleItf_AGItf rule_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: ARule 
              _ruleOfmGam :: (FmGam Expr)
              _ruleOopts :: Opts
              _ruleIrepl :: ARule 
              -- copy rule (up)
              _lhsOrepl =
                  _ruleIrepl
              -- copy rule (down)
              _ruleOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _ruleOopts =
                  _lhsIopts
              ( _ruleIrepl) =
                  rule_ _ruleOfmGam _ruleOopts 
          in  ( _lhsOrepl)))
-- AGExprItf ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ecGam                : ECnstrGam
         fmGam                : FmGam Expr
         opts                 : Opts
         rwGam                : RwExprGam
      synthesized attributes:
         aEqnFmGam            : FmGam Expr
         aEqnL                : [AEqn]
         ecElimGam            : ECnstrGam
         repl                 : Expr 
         replEc               : Expr 
         rwEcGam              : ECnstrGam
         rwMtGam              : FmGam Expr
   alternatives:
      alternative AGItf:
         child expr           : Expr 
-}
-- cata
sem_AGExprItf :: AGExprItf  ->
                 T_AGExprItf 
sem_AGExprItf (AGExprItf_AGItf _expr )  =
    (sem_AGExprItf_AGItf (sem_Expr _expr ) )
-- semantic domain
type T_AGExprItf  = ECnstrGam ->
                    (FmGam Expr) ->
                    Opts ->
                    RwExprGam ->
                    ( (FmGam Expr),([AEqn]),ECnstrGam,Expr ,Expr ,ECnstrGam,(FmGam Expr))
data Inh_AGExprItf  = Inh_AGExprItf {ecGam_Inh_AGExprItf :: ECnstrGam,fmGam_Inh_AGExprItf :: (FmGam Expr),opts_Inh_AGExprItf :: Opts,rwGam_Inh_AGExprItf :: RwExprGam}
data Syn_AGExprItf  = Syn_AGExprItf {aEqnFmGam_Syn_AGExprItf :: (FmGam Expr),aEqnL_Syn_AGExprItf :: ([AEqn]),ecElimGam_Syn_AGExprItf :: ECnstrGam,repl_Syn_AGExprItf :: Expr ,replEc_Syn_AGExprItf :: Expr ,rwEcGam_Syn_AGExprItf :: ECnstrGam,rwMtGam_Syn_AGExprItf :: (FmGam Expr)}
wrap_AGExprItf :: T_AGExprItf  ->
                  Inh_AGExprItf  ->
                  Syn_AGExprItf 
wrap_AGExprItf sem (Inh_AGExprItf _lhsIecGam _lhsIfmGam _lhsIopts _lhsIrwGam )  =
    (let ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam) = sem _lhsIecGam _lhsIfmGam _lhsIopts _lhsIrwGam 
     in  (Syn_AGExprItf _lhsOaEqnFmGam _lhsOaEqnL _lhsOecElimGam _lhsOrepl _lhsOreplEc _lhsOrwEcGam _lhsOrwMtGam ))
sem_AGExprItf_AGItf :: T_Expr  ->
                       T_AGExprItf 
sem_AGExprItf_AGItf expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIopts
       _lhsIrwGam ->
         (let _exprOinEqnLoc :: InEqnLoc
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 244, column 21)
              _exprOinEqnLoc =
                  EqnInTop
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- copy rule (up)
              _lhsOrepl =
                  _exprIrepl
              -- copy rule (up)
              _lhsOreplEc =
                  _exprIreplEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
-- ANm ---------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         opts                 : Opts
      synthesized attributes:
         ecElimGam            : ECnstrGam
         repl                 : SELF 
         replEc               : SELF 
         rwEcGam              : ECnstrGam
         rwMtGam              : FmGam Expr
   alternatives:
      alternative Fld:
         child nm             : {Nm}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Lhs:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Loc:
         child nm             : {Nm}
         child props          : {[AtProp]}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Node:
         child ndNm           : {Nm}
         child nm             : {Nm}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Wild:
         visit 0:
            local repl        : _
            local replEc      : _
-}
-- cata
sem_ANm :: ANm  ->
           T_ANm 
sem_ANm (ANm_Fld _nm )  =
    (sem_ANm_Fld _nm )
sem_ANm (ANm_Lhs _nm _props )  =
    (sem_ANm_Lhs _nm _props )
sem_ANm (ANm_Loc _nm _props )  =
    (sem_ANm_Loc _nm _props )
sem_ANm (ANm_Node _ndNm _nm )  =
    (sem_ANm_Node _ndNm _nm )
sem_ANm (ANm_Wild )  =
    (sem_ANm_Wild )
-- semantic domain
type T_ANm  = Opts ->
              ( ECnstrGam,ANm ,ANm ,ECnstrGam,(FmGam Expr))
sem_ANm_Fld :: Nm ->
               T_ANm 
sem_ANm_Fld nm_  =
    (\ _lhsIopts ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ANm 
              _lhsOreplEc :: ANm 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ANm_Fld nm_
              -- self rule
              _replEc =
                  ANm_Fld nm_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ANm_Lhs :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Lhs nm_ props_  =
    (\ _lhsIopts ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ANm 
              _lhsOreplEc :: ANm 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ANm_Lhs nm_ props_
              -- self rule
              _replEc =
                  ANm_Lhs nm_ props_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ANm_Loc :: Nm ->
               ([AtProp]) ->
               T_ANm 
sem_ANm_Loc nm_ props_  =
    (\ _lhsIopts ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ANm 
              _lhsOreplEc :: ANm 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ANm_Loc nm_ props_
              -- self rule
              _replEc =
                  ANm_Loc nm_ props_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ANm_Node :: Nm ->
                Nm ->
                T_ANm 
sem_ANm_Node ndNm_ nm_  =
    (\ _lhsIopts ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ANm 
              _lhsOreplEc :: ANm 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ANm_Node ndNm_ nm_
              -- self rule
              _replEc =
                  ANm_Node ndNm_ nm_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ANm_Wild :: T_ANm 
sem_ANm_Wild  =
    (\ _lhsIopts ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ANm 
              _lhsOreplEc :: ANm 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ANm_Wild
              -- self rule
              _replEc =
                  ANm_Wild
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
-- ARule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Rule:
         child ndNmL          : {[Nm]}
         child rlNm           : {Nm}
         child info           : {[String]}
         child eqns           : AEqns 
         visit 0:
            local repl        : _
-}
-- cata
sem_ARule :: ARule  ->
             T_ARule 
sem_ARule (ARule_Rule _ndNmL _rlNm _info _eqns )  =
    (sem_ARule_Rule _ndNmL _rlNm _info (sem_AEqns _eqns ) )
-- semantic domain
type T_ARule  = (FmGam Expr) ->
                Opts ->
                ( ARule )
sem_ARule_Rule :: ([Nm]) ->
                  Nm ->
                  ([String]) ->
                  T_AEqns  ->
                  T_ARule 
sem_ARule_Rule ndNmL_ rlNm_ info_ eqns_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: ARule 
              _eqnsOfmGam :: (FmGam Expr)
              _eqnsOopts :: Opts
              _eqnsIrepl :: AEqns 
              -- self rule
              _repl =
                  ARule_Rule ndNmL_ rlNm_ info_ _eqnsIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _eqnsOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _eqnsOopts =
                  _lhsIopts
              ( _eqnsIrepl) =
                  eqns_ _eqnsOfmGam _eqnsOopts 
          in  ( _lhsOrepl)))
-- ARules ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         fmGam                : FmGam Expr
         opts                 : Opts
      synthesized attribute:
         repl                 : SELF 
   alternatives:
      alternative Cons:
         child hd             : ARule 
         child tl             : ARules 
         visit 0:
            local repl        : _
      alternative Nil:
         visit 0:
            local repl        : _
-}
-- cata
sem_ARules :: ARules  ->
              T_ARules 
sem_ARules list  =
    (Prelude.foldr sem_ARules_Cons sem_ARules_Nil (Prelude.map sem_ARule list) )
-- semantic domain
type T_ARules  = (FmGam Expr) ->
                 Opts ->
                 ( ARules )
sem_ARules_Cons :: T_ARule  ->
                   T_ARules  ->
                   T_ARules 
sem_ARules_Cons hd_ tl_  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: ARules 
              _hdOfmGam :: (FmGam Expr)
              _hdOopts :: Opts
              _tlOfmGam :: (FmGam Expr)
              _tlOopts :: Opts
              _hdIrepl :: ARule 
              _tlIrepl :: ARules 
              -- self rule
              _repl =
                  (:) _hdIrepl _tlIrepl
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _hdOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _hdOopts =
                  _lhsIopts
              -- copy rule (down)
              _tlOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _tlOopts =
                  _lhsIopts
              ( _hdIrepl) =
                  hd_ _hdOfmGam _hdOopts 
              ( _tlIrepl) =
                  tl_ _tlOfmGam _tlOopts 
          in  ( _lhsOrepl)))
sem_ARules_Nil :: T_ARules 
sem_ARules_Nil  =
    (\ _lhsIfmGam
       _lhsIopts ->
         (let _lhsOrepl :: ARules 
              -- self rule
              _repl =
                  []
              -- self rule
              _lhsOrepl =
                  _repl
          in  ( _lhsOrepl)))
-- ECnstr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ecGam                : ECnstrGam
         fmGam                : FmGam Expr
         inEqnLoc             : InEqnLoc
         opts                 : Opts
         rwGam                : RwExprGam
      synthesized attributes:
         ecElimGam            : ECnstrGam
         repl                 : SELF 
         replEc               : SELF 
         rwEcGam              : ECnstrGam
         rwMtGam              : FmGam Expr
   alternatives:
      alternative Empty:
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Ty:
         child nms            : {[Nm]}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local repl        : _
            local replEc      : _
-}
-- cata
sem_ECnstr :: ECnstr  ->
              T_ECnstr 
sem_ECnstr (ECnstr_Empty )  =
    (sem_ECnstr_Empty )
sem_ECnstr (ECnstr_Ty _nms )  =
    (sem_ECnstr_Ty _nms )
sem_ECnstr (ECnstr_Var _nm )  =
    (sem_ECnstr_Var _nm )
-- semantic domain
type T_ECnstr  = ECnstrGam ->
                 (FmGam Expr) ->
                 InEqnLoc ->
                 Opts ->
                 RwExprGam ->
                 ( ECnstrGam,ECnstr ,ECnstr ,ECnstrGam,(FmGam Expr))
sem_ECnstr_Empty :: T_ECnstr 
sem_ECnstr_Empty  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ECnstr 
              _lhsOreplEc :: ECnstr 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ECnstr_Empty
              -- self rule
              _replEc =
                  ECnstr_Empty
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ECnstr_Ty :: ([Nm]) ->
                 T_ECnstr 
sem_ECnstr_Ty nms_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: ECnstr 
              _lhsOreplEc :: ECnstr 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ECnstr_Ty nms_
              -- self rule
              _replEc =
                  ECnstr_Ty nms_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_ECnstr_Var :: Nm ->
                  T_ECnstr 
sem_ECnstr_Var nm_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOrepl :: ECnstr 
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOreplEc :: ECnstr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 171, column 21)
              _lhsOrepl =
                  case nmMatch _lhsIopts FmCnstr nm_ _lhsIfmGam of
                      Just (_,_,Expr_WrapCnstr c)
                        -> c
                      _ -> _repl
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  ECnstr_Var nm_
              -- self rule
              _replEc =
                  ECnstr_Var nm_
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ecGam                : ECnstrGam
         fmGam                : FmGam Expr
         inEqnLoc             : InEqnLoc
         opts                 : Opts
         rwGam                : RwExprGam
      synthesized attributes:
         aEqnFmGam            : FmGam Expr
         aEqnL                : [AEqn]
         ecElimGam            : ECnstrGam
         isEqnAtEql           : Bool
         repl                 : SELF 
         replEc               : SELF 
         rwEcGam              : ECnstrGam
         rwMtGam              : FmGam Expr
   alternatives:
      alternative AVar:
         child anm            : ANm 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative App:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative AppTop:
         child expr           : Expr 
         visit 0:
            local forRwEcGam  : _
            local _tup1       : _
            local rwRepl      : _
            local rwMtGam     : _
            local rwEcGam     : _
            local _tup2       : _
            local aEqnFmGam   : _
            local repl        : _
            local replEc      : _
      alternative ChildOrder:
         child seqNr          : {Int}
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Cnstr:
         child expr           : Expr 
         child cnstr          : ECnstr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Empty:
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Expr:
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Int:
         child int            : {String}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative LF:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Named:
         child nm             : {Nm}
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Op:
         child nm             : {Nm}
         child nmExpr         : Expr 
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local isEqnAtEql  : _
            local _tup3       : {(InEqnLoc,InEqnLoc)}
            local repl        : _
            local replEc      : _
      alternative Paren:
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Retain:
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative SP:
         child lExpr          : Expr 
         child rExpr          : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Sel:
         child expr           : Expr 
         child selMbExpr      : MbExpr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative SelTop:
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative StrAsIs:
         child str            : {String}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative StrText:
         child str            : {String}
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Undefined:
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Uniq:
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Var:
         child nm             : {Nm}
         visit 0:
            local replVar     : _
            local _tup4       : _
            local replEcVar   : _
            local varEcGam    : _
            local forRwEcGam  : _
            local _tup5       : _
            local rwEcGam     : _
            local repl        : _
            local replEc      : _
      alternative Wrap:
         child wrKind         : {WrKind}
         child expr           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative WrapCnstr:
         child cnstr          : ECnstr 
         visit 0:
            local repl        : _
            local replEc      : _
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_AVar _anm )  =
    (sem_Expr_AVar (sem_ANm _anm ) )
sem_Expr (Expr_App _lExpr _rExpr )  =
    (sem_Expr_App (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_AppTop _expr )  =
    (sem_Expr_AppTop (sem_Expr _expr ) )
sem_Expr (Expr_ChildOrder _seqNr _expr )  =
    (sem_Expr_ChildOrder _seqNr (sem_Expr _expr ) )
sem_Expr (Expr_Cnstr _expr _cnstr )  =
    (sem_Expr_Cnstr (sem_Expr _expr ) (sem_ECnstr _cnstr ) )
sem_Expr (Expr_Empty )  =
    (sem_Expr_Empty )
sem_Expr (Expr_Expr _expr )  =
    (sem_Expr_Expr (sem_Expr _expr ) )
sem_Expr (Expr_Int _int )  =
    (sem_Expr_Int _int )
sem_Expr (Expr_LF _lExpr _rExpr )  =
    (sem_Expr_LF (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Named _nm _expr )  =
    (sem_Expr_Named _nm (sem_Expr _expr ) )
sem_Expr (Expr_Op _nm _nmExpr _lExpr _rExpr )  =
    (sem_Expr_Op _nm (sem_Expr _nmExpr ) (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Paren _expr )  =
    (sem_Expr_Paren (sem_Expr _expr ) )
sem_Expr (Expr_Retain _expr )  =
    (sem_Expr_Retain (sem_Expr _expr ) )
sem_Expr (Expr_SP _lExpr _rExpr )  =
    (sem_Expr_SP (sem_Expr _lExpr ) (sem_Expr _rExpr ) )
sem_Expr (Expr_Sel _expr _selMbExpr )  =
    (sem_Expr_Sel (sem_Expr _expr ) (sem_MbExpr _selMbExpr ) )
sem_Expr (Expr_SelTop _expr )  =
    (sem_Expr_SelTop (sem_Expr _expr ) )
sem_Expr (Expr_StrAsIs _str )  =
    (sem_Expr_StrAsIs _str )
sem_Expr (Expr_StrText _str )  =
    (sem_Expr_StrText _str )
sem_Expr (Expr_Undefined )  =
    (sem_Expr_Undefined )
sem_Expr (Expr_Uniq )  =
    (sem_Expr_Uniq )
sem_Expr (Expr_Var _nm )  =
    (sem_Expr_Var _nm )
sem_Expr (Expr_Wrap _wrKind _expr )  =
    (sem_Expr_Wrap _wrKind (sem_Expr _expr ) )
sem_Expr (Expr_WrapCnstr _cnstr )  =
    (sem_Expr_WrapCnstr (sem_ECnstr _cnstr ) )
-- semantic domain
type T_Expr  = ECnstrGam ->
               (FmGam Expr) ->
               InEqnLoc ->
               Opts ->
               RwExprGam ->
               ( (FmGam Expr),([AEqn]),ECnstrGam,Bool,Expr ,Expr ,ECnstrGam,(FmGam Expr))
sem_Expr_AVar :: T_ANm  ->
                 T_Expr 
sem_Expr_AVar anm_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _anmOopts :: Opts
              _anmIecElimGam :: ECnstrGam
              _anmIrepl :: ANm 
              _anmIreplEc :: ANm 
              _anmIrwEcGam :: ECnstrGam
              _anmIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _anmIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _anmIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _anmIrwMtGam
              -- self rule
              _repl =
                  Expr_AVar _anmIrepl
              -- self rule
              _replEc =
                  Expr_AVar _anmIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _anmOopts =
                  _lhsIopts
              ( _anmIecElimGam,_anmIrepl,_anmIreplEc,_anmIrwEcGam,_anmIrwMtGam) =
                  anm_ _anmOopts 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App lExpr_ rExpr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lExprOecGam :: ECnstrGam
              _lExprOfmGam :: (FmGam Expr)
              _lExprOinEqnLoc :: InEqnLoc
              _lExprOopts :: Opts
              _lExprOrwGam :: RwExprGam
              _rExprOecGam :: ECnstrGam
              _rExprOfmGam :: (FmGam Expr)
              _rExprOinEqnLoc :: InEqnLoc
              _rExprOopts :: Opts
              _rExprOrwGam :: RwExprGam
              _lExprIaEqnFmGam :: (FmGam Expr)
              _lExprIaEqnL :: ([AEqn])
              _lExprIecElimGam :: ECnstrGam
              _lExprIisEqnAtEql :: Bool
              _lExprIrepl :: Expr 
              _lExprIreplEc :: Expr 
              _lExprIrwEcGam :: ECnstrGam
              _lExprIrwMtGam :: (FmGam Expr)
              _rExprIaEqnFmGam :: (FmGam Expr)
              _rExprIaEqnL :: ([AEqn])
              _rExprIecElimGam :: ECnstrGam
              _rExprIisEqnAtEql :: Bool
              _rExprIrepl :: Expr 
              _rExprIreplEc :: Expr 
              _rExprIrwEcGam :: ECnstrGam
              _rExprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _lExprIaEqnFmGam `fmGamUnion` _rExprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _lExprIaEqnL ++ _rExprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _lExprIecElimGam `gamUnion` _rExprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _lExprIrwEcGam `gamUnion` _rExprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _lExprIrwMtGam `fmGamUnion` _rExprIrwMtGam
              -- self rule
              _repl =
                  Expr_App _lExprIrepl _rExprIrepl
              -- self rule
              _replEc =
                  Expr_App _lExprIreplEc _rExprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _lExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOrwGam =
                  _lhsIrwGam
              ( _lExprIaEqnFmGam,_lExprIaEqnL,_lExprIecElimGam,_lExprIisEqnAtEql,_lExprIrepl,_lExprIreplEc,_lExprIrwEcGam,_lExprIrwMtGam) =
                  lExpr_ _lExprOecGam _lExprOfmGam _lExprOinEqnLoc _lExprOopts _lExprOrwGam 
              ( _rExprIaEqnFmGam,_rExprIaEqnL,_rExprIecElimGam,_rExprIisEqnAtEql,_rExprIrepl,_rExprIreplEc,_rExprIrwEcGam,_rExprIrwMtGam) =
                  rExpr_ _rExprOecGam _rExprOfmGam _rExprOinEqnLoc _rExprOopts _rExprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_AppTop :: T_Expr  ->
                   T_Expr 
sem_Expr_AppTop expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrepl :: Expr 
              _lhsOaEqnL :: ([AEqn])
              _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOecElimGam :: ECnstrGam
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 205, column 21)
              _forRwEcGam =
                  _exprIrwEcGam `gamUnion` _lhsIecGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 206, column 21)
              __tup1 =
                  mkRwExpr _lhsIinEqnLoc _lhsIopts _lhsIfmGam _lhsIrwGam (_forRwEcGam) _repl
              -- "build/ruler2/ARule/RwSubst.ag"(line 206, column 21)
              (_rwRepl,_,_) =
                  __tup1
              -- "build/ruler2/ARule/RwSubst.ag"(line 206, column 21)
              (_,_rwMtGam,_) =
                  __tup1
              -- "build/ruler2/ARule/RwSubst.ag"(line 206, column 21)
              (_,_,_rwEcGam) =
                  __tup1
              -- "build/ruler2/ARule/RwSubst.ag"(line 208, column 21)
              _lhsOrwMtGam =
                  _rwMtGam `fmGamUnion` _exprIrwMtGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 208, column 21)
              _lhsOrwEcGam =
                  _rwEcGam `gamUnion` _exprIrwEcGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 208, column 21)
              _lhsOrepl =
                  _rwRepl
              -- "build/ruler2/ARule/RwSubst.ag"(line 215, column 21)
              __tup2 =
                  case exprMbAEqnRest _rwRepl of
                      Just (aEqn,remEqnL,fmGam) | _exprIisEqnAtEql
                        -> (aEqn : concat rwEqnL, fmGam `fmGamUnion` fmGamUnions fmGamL)
                        where (rwEqnL,fmGamL)
                                = unzip [ (e,g) | eqn <- remEqnL, let (_,e,g) = exprRewrite' _lhsIopts _lhsIfmGam _lhsIrwGam _lhsIecGam eqn ]
                      _ -> ([AEqn_Err _rwRepl],emptyGam)
              -- "build/ruler2/ARule/RwSubst.ag"(line 215, column 21)
              (_lhsOaEqnL,_) =
                  __tup2
              -- "build/ruler2/ARule/RwSubst.ag"(line 215, column 21)
              (_,_aEqnFmGam) =
                  __tup2
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _aEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- self rule
              _repl =
                  Expr_AppTop _exprIrepl
              -- self rule
              _replEc =
                  Expr_AppTop _exprIreplEc
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_ChildOrder :: Int ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ChildOrder seqNr_ expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_ChildOrder seqNr_ _exprIrepl
              -- self rule
              _replEc =
                  Expr_ChildOrder seqNr_ _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Cnstr :: T_Expr  ->
                  T_ECnstr  ->
                  T_Expr 
sem_Expr_Cnstr expr_ cnstr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOreplEc :: Expr 
              _lhsOecElimGam :: ECnstrGam
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _cnstrOecGam :: ECnstrGam
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOinEqnLoc :: InEqnLoc
              _cnstrOopts :: Opts
              _cnstrOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              _cnstrIecElimGam :: ECnstrGam
              _cnstrIrepl :: ECnstr 
              _cnstrIreplEc :: ECnstr 
              _cnstrIrwEcGam :: ECnstrGam
              _cnstrIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- "build/ruler2/ARule/RwSubst.ag"(line 328, column 21)
              _lhsOreplEc =
                  _exprIreplEc
              -- "build/ruler2/ARule/RwSubst.ag"(line 328, column 21)
              _lhsOecElimGam =
                  ecGamInsert _exprIreplEc _cnstrIreplEc _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam `gamUnion` _cnstrIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam `fmGamUnion` _cnstrIrwMtGam
              -- self rule
              _repl =
                  Expr_Cnstr _exprIrepl _cnstrIrepl
              -- self rule
              _replEc =
                  Expr_Cnstr _exprIreplEc _cnstrIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _cnstrOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
              -- copy rule (down)
              _cnstrOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
              ( _cnstrIecElimGam,_cnstrIrepl,_cnstrIreplEc,_cnstrIrwEcGam,_cnstrIrwMtGam) =
                  cnstr_ _cnstrOecGam _cnstrOfmGam _cnstrOinEqnLoc _cnstrOopts _cnstrOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Empty :: T_Expr 
sem_Expr_Empty  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_Empty
              -- self rule
              _replEc =
                  Expr_Empty
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Expr :: T_Expr  ->
                 T_Expr 
sem_Expr_Expr expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_Expr _exprIrepl
              -- self rule
              _replEc =
                  Expr_Expr _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Int :: String ->
                T_Expr 
sem_Expr_Int int_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_Int int_
              -- self rule
              _replEc =
                  Expr_Int int_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_LF :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_LF lExpr_ rExpr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lExprOecGam :: ECnstrGam
              _lExprOfmGam :: (FmGam Expr)
              _lExprOinEqnLoc :: InEqnLoc
              _lExprOopts :: Opts
              _lExprOrwGam :: RwExprGam
              _rExprOecGam :: ECnstrGam
              _rExprOfmGam :: (FmGam Expr)
              _rExprOinEqnLoc :: InEqnLoc
              _rExprOopts :: Opts
              _rExprOrwGam :: RwExprGam
              _lExprIaEqnFmGam :: (FmGam Expr)
              _lExprIaEqnL :: ([AEqn])
              _lExprIecElimGam :: ECnstrGam
              _lExprIisEqnAtEql :: Bool
              _lExprIrepl :: Expr 
              _lExprIreplEc :: Expr 
              _lExprIrwEcGam :: ECnstrGam
              _lExprIrwMtGam :: (FmGam Expr)
              _rExprIaEqnFmGam :: (FmGam Expr)
              _rExprIaEqnL :: ([AEqn])
              _rExprIecElimGam :: ECnstrGam
              _rExprIisEqnAtEql :: Bool
              _rExprIrepl :: Expr 
              _rExprIreplEc :: Expr 
              _rExprIrwEcGam :: ECnstrGam
              _rExprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _lExprIaEqnFmGam `fmGamUnion` _rExprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _lExprIaEqnL ++ _rExprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _lExprIecElimGam `gamUnion` _rExprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _lExprIrwEcGam `gamUnion` _rExprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _lExprIrwMtGam `fmGamUnion` _rExprIrwMtGam
              -- self rule
              _repl =
                  Expr_LF _lExprIrepl _rExprIrepl
              -- self rule
              _replEc =
                  Expr_LF _lExprIreplEc _rExprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _lExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOrwGam =
                  _lhsIrwGam
              ( _lExprIaEqnFmGam,_lExprIaEqnL,_lExprIecElimGam,_lExprIisEqnAtEql,_lExprIrepl,_lExprIreplEc,_lExprIrwEcGam,_lExprIrwMtGam) =
                  lExpr_ _lExprOecGam _lExprOfmGam _lExprOinEqnLoc _lExprOopts _lExprOrwGam 
              ( _rExprIaEqnFmGam,_rExprIaEqnL,_rExprIecElimGam,_rExprIisEqnAtEql,_rExprIrepl,_rExprIreplEc,_rExprIrwEcGam,_rExprIrwMtGam) =
                  rExpr_ _rExprOecGam _rExprOfmGam _rExprOinEqnLoc _rExprOopts _rExprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Named :: Nm ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Named nm_ expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lhsOisEqnAtEql :: Bool
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_Named nm_ _exprIrepl
              -- self rule
              _replEc =
                  Expr_Named nm_ _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (up)
              _lhsOisEqnAtEql =
                  _exprIisEqnAtEql
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Op :: Nm ->
               T_Expr  ->
               T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_Op nm_ nmExpr_ lExpr_ rExpr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let __tup3 :: ((InEqnLoc,InEqnLoc))
              _lExprOinEqnLoc :: InEqnLoc
              _rExprOinEqnLoc :: InEqnLoc
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lhsOisEqnAtEql :: Bool
              _nmExprOecGam :: ECnstrGam
              _nmExprOfmGam :: (FmGam Expr)
              _nmExprOinEqnLoc :: InEqnLoc
              _nmExprOopts :: Opts
              _nmExprOrwGam :: RwExprGam
              _lExprOecGam :: ECnstrGam
              _lExprOfmGam :: (FmGam Expr)
              _lExprOopts :: Opts
              _lExprOrwGam :: RwExprGam
              _rExprOecGam :: ECnstrGam
              _rExprOfmGam :: (FmGam Expr)
              _rExprOopts :: Opts
              _rExprOrwGam :: RwExprGam
              _nmExprIaEqnFmGam :: (FmGam Expr)
              _nmExprIaEqnL :: ([AEqn])
              _nmExprIecElimGam :: ECnstrGam
              _nmExprIisEqnAtEql :: Bool
              _nmExprIrepl :: Expr 
              _nmExprIreplEc :: Expr 
              _nmExprIrwEcGam :: ECnstrGam
              _nmExprIrwMtGam :: (FmGam Expr)
              _lExprIaEqnFmGam :: (FmGam Expr)
              _lExprIaEqnL :: ([AEqn])
              _lExprIecElimGam :: ECnstrGam
              _lExprIisEqnAtEql :: Bool
              _lExprIrepl :: Expr 
              _lExprIreplEc :: Expr 
              _lExprIrwEcGam :: ECnstrGam
              _lExprIrwMtGam :: (FmGam Expr)
              _rExprIaEqnFmGam :: (FmGam Expr)
              _rExprIaEqnL :: ([AEqn])
              _rExprIecElimGam :: ECnstrGam
              _rExprIisEqnAtEql :: Bool
              _rExprIrepl :: Expr 
              _rExprIreplEc :: Expr 
              _rExprIrwEcGam :: ECnstrGam
              _rExprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 235, column 21)
              _isEqnAtEql =
                  _lhsIinEqnLoc == EqnInTop && nm_ == nmEql
              -- "build/ruler2/ARule/RwSubst.ag"(line 236, column 21)
              __tup3 =
                  if _isEqnAtEql
                  then (EqnInLhs,EqnInRhs)
                  else (_lhsIinEqnLoc,_lhsIinEqnLoc)
              -- "build/ruler2/ARule/RwSubst.ag"(line 236, column 21)
              (_lExprOinEqnLoc,_) =
                  __tup3
              -- "build/ruler2/ARule/RwSubst.ag"(line 236, column 21)
              (_,_rExprOinEqnLoc) =
                  __tup3
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _nmExprIaEqnFmGam `fmGamUnion` _lExprIaEqnFmGam `fmGamUnion` _rExprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _nmExprIaEqnL ++ _lExprIaEqnL ++ _rExprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _nmExprIecElimGam `gamUnion` _lExprIecElimGam `gamUnion` _rExprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _nmExprIrwEcGam `gamUnion` _lExprIrwEcGam `gamUnion` _rExprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _nmExprIrwMtGam `fmGamUnion` _lExprIrwMtGam `fmGamUnion` _rExprIrwMtGam
              -- self rule
              _repl =
                  Expr_Op nm_ _nmExprIrepl _lExprIrepl _rExprIrepl
              -- self rule
              _replEc =
                  Expr_Op nm_ _nmExprIreplEc _lExprIreplEc _rExprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (from local)
              _lhsOisEqnAtEql =
                  _isEqnAtEql
              -- copy rule (down)
              _nmExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _nmExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _nmExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _nmExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _nmExprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _lExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOrwGam =
                  _lhsIrwGam
              ( _nmExprIaEqnFmGam,_nmExprIaEqnL,_nmExprIecElimGam,_nmExprIisEqnAtEql,_nmExprIrepl,_nmExprIreplEc,_nmExprIrwEcGam,_nmExprIrwMtGam) =
                  nmExpr_ _nmExprOecGam _nmExprOfmGam _nmExprOinEqnLoc _nmExprOopts _nmExprOrwGam 
              ( _lExprIaEqnFmGam,_lExprIaEqnL,_lExprIecElimGam,_lExprIisEqnAtEql,_lExprIrepl,_lExprIreplEc,_lExprIrwEcGam,_lExprIrwMtGam) =
                  lExpr_ _lExprOecGam _lExprOfmGam _lExprOinEqnLoc _lExprOopts _lExprOrwGam 
              ( _rExprIaEqnFmGam,_rExprIaEqnL,_rExprIecElimGam,_rExprIisEqnAtEql,_rExprIrepl,_rExprIreplEc,_rExprIrwEcGam,_rExprIrwMtGam) =
                  rExpr_ _rExprOecGam _rExprOfmGam _rExprOinEqnLoc _rExprOopts _rExprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Paren :: T_Expr  ->
                  T_Expr 
sem_Expr_Paren expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lhsOisEqnAtEql :: Bool
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_Paren _exprIrepl
              -- self rule
              _replEc =
                  Expr_Paren _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (up)
              _lhsOisEqnAtEql =
                  _exprIisEqnAtEql
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Retain :: T_Expr  ->
                   T_Expr 
sem_Expr_Retain expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_Retain _exprIrepl
              -- self rule
              _replEc =
                  Expr_Retain _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_SP :: T_Expr  ->
               T_Expr  ->
               T_Expr 
sem_Expr_SP lExpr_ rExpr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _lExprOecGam :: ECnstrGam
              _lExprOfmGam :: (FmGam Expr)
              _lExprOinEqnLoc :: InEqnLoc
              _lExprOopts :: Opts
              _lExprOrwGam :: RwExprGam
              _rExprOecGam :: ECnstrGam
              _rExprOfmGam :: (FmGam Expr)
              _rExprOinEqnLoc :: InEqnLoc
              _rExprOopts :: Opts
              _rExprOrwGam :: RwExprGam
              _lExprIaEqnFmGam :: (FmGam Expr)
              _lExprIaEqnL :: ([AEqn])
              _lExprIecElimGam :: ECnstrGam
              _lExprIisEqnAtEql :: Bool
              _lExprIrepl :: Expr 
              _lExprIreplEc :: Expr 
              _lExprIrwEcGam :: ECnstrGam
              _lExprIrwMtGam :: (FmGam Expr)
              _rExprIaEqnFmGam :: (FmGam Expr)
              _rExprIaEqnL :: ([AEqn])
              _rExprIecElimGam :: ECnstrGam
              _rExprIisEqnAtEql :: Bool
              _rExprIrepl :: Expr 
              _rExprIreplEc :: Expr 
              _rExprIrwEcGam :: ECnstrGam
              _rExprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _lExprIaEqnFmGam `fmGamUnion` _rExprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _lExprIaEqnL ++ _rExprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _lExprIecElimGam `gamUnion` _rExprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _lExprIrwEcGam `gamUnion` _rExprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _lExprIrwMtGam `fmGamUnion` _rExprIrwMtGam
              -- self rule
              _repl =
                  Expr_SP _lExprIrepl _rExprIrepl
              -- self rule
              _replEc =
                  Expr_SP _lExprIreplEc _rExprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _lExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _lExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _lExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _lExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _lExprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _rExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _rExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _rExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _rExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _rExprOrwGam =
                  _lhsIrwGam
              ( _lExprIaEqnFmGam,_lExprIaEqnL,_lExprIecElimGam,_lExprIisEqnAtEql,_lExprIrepl,_lExprIreplEc,_lExprIrwEcGam,_lExprIrwMtGam) =
                  lExpr_ _lExprOecGam _lExprOfmGam _lExprOinEqnLoc _lExprOopts _lExprOrwGam 
              ( _rExprIaEqnFmGam,_rExprIaEqnL,_rExprIecElimGam,_rExprIisEqnAtEql,_rExprIrepl,_rExprIreplEc,_rExprIrwEcGam,_rExprIrwMtGam) =
                  rExpr_ _rExprOecGam _rExprOfmGam _rExprOinEqnLoc _rExprOopts _rExprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Sel :: T_Expr  ->
                T_MbExpr  ->
                T_Expr 
sem_Expr_Sel expr_ selMbExpr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _selMbExprOecGam :: ECnstrGam
              _selMbExprOfmGam :: (FmGam Expr)
              _selMbExprOinEqnLoc :: InEqnLoc
              _selMbExprOopts :: Opts
              _selMbExprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              _selMbExprIecElimGam :: ECnstrGam
              _selMbExprIrepl :: MbExpr 
              _selMbExprIreplEc :: MbExpr 
              _selMbExprIrwEcGam :: ECnstrGam
              _selMbExprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam `gamUnion` _selMbExprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam `gamUnion` _selMbExprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam `fmGamUnion` _selMbExprIrwMtGam
              -- self rule
              _repl =
                  Expr_Sel _exprIrepl _selMbExprIrepl
              -- self rule
              _replEc =
                  Expr_Sel _exprIreplEc _selMbExprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              -- copy rule (down)
              _selMbExprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _selMbExprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _selMbExprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _selMbExprOopts =
                  _lhsIopts
              -- copy rule (down)
              _selMbExprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
              ( _selMbExprIecElimGam,_selMbExprIrepl,_selMbExprIreplEc,_selMbExprIrwEcGam,_selMbExprIrwMtGam) =
                  selMbExpr_ _selMbExprOecGam _selMbExprOfmGam _selMbExprOinEqnLoc _selMbExprOopts _selMbExprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_SelTop :: T_Expr  ->
                   T_Expr 
sem_Expr_SelTop expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_SelTop _exprIrepl
              -- self rule
              _replEc =
                  Expr_SelTop _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_StrAsIs :: String ->
                    T_Expr 
sem_Expr_StrAsIs str_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_StrAsIs str_
              -- self rule
              _replEc =
                  Expr_StrAsIs str_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_StrText :: String ->
                    T_Expr 
sem_Expr_StrText str_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_StrText str_
              -- self rule
              _replEc =
                  Expr_StrText str_
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Undefined :: T_Expr 
sem_Expr_Undefined  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_Undefined
              -- self rule
              _replEc =
                  Expr_Undefined
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Uniq :: T_Expr 
sem_Expr_Uniq  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_Uniq
              -- self rule
              _replEc =
                  Expr_Uniq
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Var :: Nm ->
                T_Expr 
sem_Expr_Var nm_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOrepl :: Expr 
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrwEcGam :: ECnstrGam
              _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOreplEc :: Expr 
              -- "build/ruler2/ARule/RwSubst.ag"(line 166, column 21)
              _replVar =
                  exprVarSubst _lhsIopts _lhsIfmGam _lhsIrwGam _lhsIecGam _repl nm_
              -- "build/ruler2/ARule/RwSubst.ag"(line 167, column 33)
              __tup4 =
                  exprElimCnstr _replVar
              -- "build/ruler2/ARule/RwSubst.ag"(line 167, column 33)
              (_replEcVar,_) =
                  __tup4
              -- "build/ruler2/ARule/RwSubst.ag"(line 167, column 33)
              (_,_varEcGam) =
                  __tup4
              -- "build/ruler2/ARule/RwSubst.ag"(line 201, column 21)
              _forRwEcGam =
                  _varEcGam `gamUnion` _lhsIecGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 202, column 21)
              __tup5 =
                  mkRwExpr _lhsIinEqnLoc _lhsIopts _lhsIfmGam _lhsIrwGam (_forRwEcGam) _replEcVar
              -- "build/ruler2/ARule/RwSubst.ag"(line 202, column 21)
              (_lhsOrepl,_,_) =
                  __tup5
              -- "build/ruler2/ARule/RwSubst.ag"(line 202, column 21)
              (_,_lhsOrwMtGam,_) =
                  __tup5
              -- "build/ruler2/ARule/RwSubst.ag"(line 202, column 21)
              (_,_,_rwEcGam) =
                  __tup5
              -- "build/ruler2/ARule/RwSubst.ag"(line 204, column 21)
              _lhsOrwEcGam =
                  _rwEcGam `gamUnion` _varEcGam
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- self rule
              _repl =
                  Expr_Var nm_
              -- self rule
              _replEc =
                  Expr_Var nm_
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_Wrap :: WrKind ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Wrap wrKind_ expr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _exprOecGam :: ECnstrGam
              _exprOfmGam :: (FmGam Expr)
              _exprOinEqnLoc :: InEqnLoc
              _exprOopts :: Opts
              _exprOrwGam :: RwExprGam
              _exprIaEqnFmGam :: (FmGam Expr)
              _exprIaEqnL :: ([AEqn])
              _exprIecElimGam :: ECnstrGam
              _exprIisEqnAtEql :: Bool
              _exprIrepl :: Expr 
              _exprIreplEc :: Expr 
              _exprIrwEcGam :: ECnstrGam
              _exprIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  _exprIaEqnFmGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  _exprIaEqnL
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _exprIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _exprIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _exprIrwMtGam
              -- self rule
              _repl =
                  Expr_Wrap wrKind_ _exprIrepl
              -- self rule
              _replEc =
                  Expr_Wrap wrKind_ _exprIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _exprOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _exprOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _exprOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _exprOopts =
                  _lhsIopts
              -- copy rule (down)
              _exprOrwGam =
                  _lhsIrwGam
              ( _exprIaEqnFmGam,_exprIaEqnL,_exprIecElimGam,_exprIisEqnAtEql,_exprIrepl,_exprIreplEc,_exprIrwEcGam,_exprIrwMtGam) =
                  expr_ _exprOecGam _exprOfmGam _exprOinEqnLoc _exprOopts _exprOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_Expr_WrapCnstr :: T_ECnstr  ->
                      T_Expr 
sem_Expr_WrapCnstr cnstr_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOisEqnAtEql :: Bool
              _lhsOaEqnFmGam :: (FmGam Expr)
              _lhsOaEqnL :: ([AEqn])
              _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: Expr 
              _lhsOreplEc :: Expr 
              _cnstrOecGam :: ECnstrGam
              _cnstrOfmGam :: (FmGam Expr)
              _cnstrOinEqnLoc :: InEqnLoc
              _cnstrOopts :: Opts
              _cnstrOrwGam :: RwExprGam
              _cnstrIecElimGam :: ECnstrGam
              _cnstrIrepl :: ECnstr 
              _cnstrIreplEc :: ECnstr 
              _cnstrIrwEcGam :: ECnstrGam
              _cnstrIrwMtGam :: (FmGam Expr)
              -- "build/ruler2/ARule/RwSubst.ag"(line 241, column 21)
              _lhsOisEqnAtEql =
                  False
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 223, column 37)
              _lhsOaEqnFmGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 212, column 33)
              _lhsOaEqnL =
                  []
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _cnstrIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _cnstrIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _cnstrIrwMtGam
              -- self rule
              _repl =
                  Expr_WrapCnstr _cnstrIrepl
              -- self rule
              _replEc =
                  Expr_WrapCnstr _cnstrIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _cnstrOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _cnstrOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _cnstrOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _cnstrOopts =
                  _lhsIopts
              -- copy rule (down)
              _cnstrOrwGam =
                  _lhsIrwGam
              ( _cnstrIecElimGam,_cnstrIrepl,_cnstrIreplEc,_cnstrIrwEcGam,_cnstrIrwMtGam) =
                  cnstr_ _cnstrOecGam _cnstrOfmGam _cnstrOinEqnLoc _cnstrOopts _cnstrOrwGam 
          in  ( _lhsOaEqnFmGam,_lhsOaEqnL,_lhsOecElimGam,_lhsOisEqnAtEql,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
-- MbExpr ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         ecGam                : ECnstrGam
         fmGam                : FmGam Expr
         inEqnLoc             : InEqnLoc
         opts                 : Opts
         rwGam                : RwExprGam
      synthesized attributes:
         ecElimGam            : ECnstrGam
         repl                 : SELF 
         replEc               : SELF 
         rwEcGam              : ECnstrGam
         rwMtGam              : FmGam Expr
   alternatives:
      alternative Just:
         child just           : Expr 
         visit 0:
            local repl        : _
            local replEc      : _
      alternative Nothing:
         visit 0:
            local repl        : _
            local replEc      : _
-}
-- cata
sem_MbExpr :: MbExpr  ->
              T_MbExpr 
sem_MbExpr (Prelude.Just x )  =
    (sem_MbExpr_Just (sem_Expr x ) )
sem_MbExpr Prelude.Nothing  =
    sem_MbExpr_Nothing
-- semantic domain
type T_MbExpr  = ECnstrGam ->
                 (FmGam Expr) ->
                 InEqnLoc ->
                 Opts ->
                 RwExprGam ->
                 ( ECnstrGam,MbExpr ,MbExpr ,ECnstrGam,(FmGam Expr))
sem_MbExpr_Just :: T_Expr  ->
                   T_MbExpr 
sem_MbExpr_Just just_  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: MbExpr 
              _lhsOreplEc :: MbExpr 
              _justOecGam :: ECnstrGam
              _justOfmGam :: (FmGam Expr)
              _justOinEqnLoc :: InEqnLoc
              _justOopts :: Opts
              _justOrwGam :: RwExprGam
              _justIaEqnFmGam :: (FmGam Expr)
              _justIaEqnL :: ([AEqn])
              _justIecElimGam :: ECnstrGam
              _justIisEqnAtEql :: Bool
              _justIrepl :: Expr 
              _justIreplEc :: Expr 
              _justIrwEcGam :: ECnstrGam
              _justIrwMtGam :: (FmGam Expr)
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  _justIecElimGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  _justIrwEcGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  _justIrwMtGam
              -- self rule
              _repl =
                  Just _justIrepl
              -- self rule
              _replEc =
                  Just _justIreplEc
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
              -- copy rule (down)
              _justOecGam =
                  _lhsIecGam
              -- copy rule (down)
              _justOfmGam =
                  _lhsIfmGam
              -- copy rule (down)
              _justOinEqnLoc =
                  _lhsIinEqnLoc
              -- copy rule (down)
              _justOopts =
                  _lhsIopts
              -- copy rule (down)
              _justOrwGam =
                  _lhsIrwGam
              ( _justIaEqnFmGam,_justIaEqnL,_justIecElimGam,_justIisEqnAtEql,_justIrepl,_justIreplEc,_justIrwEcGam,_justIrwMtGam) =
                  just_ _justOecGam _justOfmGam _justOinEqnLoc _justOopts _justOrwGam 
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))
sem_MbExpr_Nothing :: T_MbExpr 
sem_MbExpr_Nothing  =
    (\ _lhsIecGam
       _lhsIfmGam
       _lhsIinEqnLoc
       _lhsIopts
       _lhsIrwGam ->
         (let _lhsOecElimGam :: ECnstrGam
              _lhsOrwEcGam :: ECnstrGam
              _lhsOrwMtGam :: (FmGam Expr)
              _lhsOrepl :: MbExpr 
              _lhsOreplEc :: MbExpr 
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 325, column 40)
              _lhsOecElimGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 198, column 38)
              _lhsOrwEcGam =
                  emptyGam
              -- use rule "build/ruler2/ARule/RwSubst.ag"(line 197, column 38)
              _lhsOrwMtGam =
                  emptyGam
              -- self rule
              _repl =
                  Nothing
              -- self rule
              _replEc =
                  Nothing
              -- self rule
              _lhsOrepl =
                  _repl
              -- self rule
              _lhsOreplEc =
                  _replEc
          in  ( _lhsOecElimGam,_lhsOrepl,_lhsOreplEc,_lhsOrwEcGam,_lhsOrwMtGam)))