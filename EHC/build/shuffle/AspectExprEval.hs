

-- UUAGC 0.9.39.1 (src/shuffle/AspectExprEval.ag)
module AspectExprEval(aspexpIsAccepted) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import AspectExpr


wrapAGAspectExpr_T :: AspectRefReqd -> T_AGAspectExprItf -> Syn_AGAspectExprItf
wrapAGAspectExpr_T givenRefs d
  = wrap_AGAspectExprItf d
      (Inh_AGAspectExprItf
         { givenRefs_Inh_AGAspectExprItf = givenRefs
         })

wrapAspectExpr :: AspectRefReqd -> AspectExpr -> Syn_AGAspectExprItf
wrapAspectExpr givenRefs d = wrapAGAspectExpr_T givenRefs (sem_AGAspectExprItf (AGAspectExprItf_AGItf d))

aspexpIsAccepted :: AspectRefReqd -> AspectExpr -> Bool
aspexpIsAccepted givenRefs d
  = (isAccepted_Syn_AGAspectExprItf r)
  where r = wrapAspectExpr givenRefs d
-- AGAspectExprItf ---------------------------------------------
{-
   visit 0:
      inherited attribute:
         givenRefs            : AspectRefReqd
      synthesized attribute:
         isAccepted           : Bool
   alternatives:
      alternative AGItf:
         child aexp           : AspectExpr 
-}
-- cata
sem_AGAspectExprItf :: AGAspectExprItf  ->
                       T_AGAspectExprItf 
sem_AGAspectExprItf (AGAspectExprItf_AGItf _aexp )  =
    (sem_AGAspectExprItf_AGItf (sem_AspectExpr _aexp ) )
-- semantic domain
type T_AGAspectExprItf  = AspectRefReqd ->
                          ( Bool)
data Inh_AGAspectExprItf  = Inh_AGAspectExprItf {givenRefs_Inh_AGAspectExprItf :: AspectRefReqd}
data Syn_AGAspectExprItf  = Syn_AGAspectExprItf {isAccepted_Syn_AGAspectExprItf :: Bool}
wrap_AGAspectExprItf :: T_AGAspectExprItf  ->
                        Inh_AGAspectExprItf  ->
                        Syn_AGAspectExprItf 
wrap_AGAspectExprItf sem (Inh_AGAspectExprItf _lhsIgivenRefs )  =
    (let ( _lhsOisAccepted) = sem _lhsIgivenRefs 
     in  (Syn_AGAspectExprItf _lhsOisAccepted ))
sem_AGAspectExprItf_AGItf :: T_AspectExpr  ->
                             T_AGAspectExprItf 
sem_AGAspectExprItf_AGItf aexp_  =
    (\ _lhsIgivenRefs ->
         (let _lhsOisAccepted :: Bool
              _aexpOgivenRefs :: AspectRefReqd
              _aexpIisAccepted :: Bool
              -- copy rule (up)
              _lhsOisAccepted =
                  _aexpIisAccepted
              -- copy rule (down)
              _aexpOgivenRefs =
                  _lhsIgivenRefs
              ( _aexpIisAccepted) =
                  aexp_ _aexpOgivenRefs 
          in  ( _lhsOisAccepted)))
-- AspectExpr --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         givenRefs            : AspectRefReqd
      synthesized attribute:
         isAccepted           : Bool
   alternatives:
      alternative And:
         child asp1           : AspectExpr 
         child asp2           : AspectExpr 
      alternative Or:
         child asp1           : AspectExpr 
         child asp2           : AspectExpr 
      alternative Requires:
         child asp            : {String}
      alternative True:
-}
-- cata
sem_AspectExpr :: AspectExpr  ->
                  T_AspectExpr 
sem_AspectExpr (AspectExpr_And _asp1 _asp2 )  =
    (sem_AspectExpr_And (sem_AspectExpr _asp1 ) (sem_AspectExpr _asp2 ) )
sem_AspectExpr (AspectExpr_Or _asp1 _asp2 )  =
    (sem_AspectExpr_Or (sem_AspectExpr _asp1 ) (sem_AspectExpr _asp2 ) )
sem_AspectExpr (AspectExpr_Requires _asp )  =
    (sem_AspectExpr_Requires _asp )
sem_AspectExpr (AspectExpr_True )  =
    (sem_AspectExpr_True )
-- semantic domain
type T_AspectExpr  = AspectRefReqd ->
                     ( Bool)
sem_AspectExpr_And :: T_AspectExpr  ->
                      T_AspectExpr  ->
                      T_AspectExpr 
sem_AspectExpr_And asp1_ asp2_  =
    (\ _lhsIgivenRefs ->
         (let _lhsOisAccepted :: Bool
              _asp1OgivenRefs :: AspectRefReqd
              _asp2OgivenRefs :: AspectRefReqd
              _asp1IisAccepted :: Bool
              _asp2IisAccepted :: Bool
              -- "src/shuffle/AspectExprEval.ag"(line 43, column 17)
              _lhsOisAccepted =
                  _asp1IisAccepted && _asp2IisAccepted
              -- copy rule (down)
              _asp1OgivenRefs =
                  _lhsIgivenRefs
              -- copy rule (down)
              _asp2OgivenRefs =
                  _lhsIgivenRefs
              ( _asp1IisAccepted) =
                  asp1_ _asp1OgivenRefs 
              ( _asp2IisAccepted) =
                  asp2_ _asp2OgivenRefs 
          in  ( _lhsOisAccepted)))
sem_AspectExpr_Or :: T_AspectExpr  ->
                     T_AspectExpr  ->
                     T_AspectExpr 
sem_AspectExpr_Or asp1_ asp2_  =
    (\ _lhsIgivenRefs ->
         (let _lhsOisAccepted :: Bool
              _asp1OgivenRefs :: AspectRefReqd
              _asp2OgivenRefs :: AspectRefReqd
              _asp1IisAccepted :: Bool
              _asp2IisAccepted :: Bool
              -- "src/shuffle/AspectExprEval.ag"(line 42, column 17)
              _lhsOisAccepted =
                  _asp1IisAccepted || _asp2IisAccepted
              -- copy rule (down)
              _asp1OgivenRefs =
                  _lhsIgivenRefs
              -- copy rule (down)
              _asp2OgivenRefs =
                  _lhsIgivenRefs
              ( _asp1IisAccepted) =
                  asp1_ _asp1OgivenRefs 
              ( _asp2IisAccepted) =
                  asp2_ _asp2OgivenRefs 
          in  ( _lhsOisAccepted)))
sem_AspectExpr_Requires :: String ->
                           T_AspectExpr 
sem_AspectExpr_Requires asp_  =
    (\ _lhsIgivenRefs ->
         (let _lhsOisAccepted :: Bool
              -- "src/shuffle/AspectExprEval.ag"(line 41, column 17)
              _lhsOisAccepted =
                  asp_ `Set.member` _lhsIgivenRefs
          in  ( _lhsOisAccepted)))
sem_AspectExpr_True :: T_AspectExpr 
sem_AspectExpr_True  =
    (\ _lhsIgivenRefs ->
         (let _lhsOisAccepted :: Bool
              -- "src/shuffle/AspectExprEval.ag"(line 40, column 17)
              _lhsOisAccepted =
                  True
          in  ( _lhsOisAccepted)))