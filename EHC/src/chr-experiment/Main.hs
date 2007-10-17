{-# OPTIONS_GHC -fglasgow-exts -package EH16 #-}
module Main where

import EH16.Pred.ToCHR
import EH16.CHR.Solve
import EH16.Ty.FitsIn
import EH16.Ty
import EH16.Pred.CHR
import EH16.Pred.CommonCHR
import EH16.Base.Common
import EH.Util.Pretty


-- list of constraints
constraints :: [Constraint CHRPredOcc RedHowAnnotation]
constraints
  = [ p $ Pred_Eq tyB tyI
    , a $ Pred_Eq tyA tyB
    ]
  where
    a :: Pred -> Constraint CHRPredOcc RedHowAnnotation
    a pr = Assume (mkCHRPredOcc pr initPredScope)
    
    p :: Pred -> Constraint CHRPredOcc RedHowAnnotation
    p pr = Prove (mkCHRPredOcc pr initPredScope)
    
    [tyA, tyB, tyC] = map (Ty_Con . HNm) ["A", "B", "C"]
    
    tyI = Ty_Impls (Impls_Tail u1 []) `mk1Arrow` tyA
    
    [u1] = drop 4 (mkNewLevUIDL 5 uidStart)


-- CHR rules
store :: ScopedPredStore
store
  = chrStoreFromElems
  $ [ -- symmetry (assume)
      [Assume eqT1T2] <==> [Assume eqT2T1]
    , -- symmetry (prove)
      [Prove eqT1T2] ==> [Prove eqT2T1, Reduction eqT1T2 (RedHow_ByEqSymmetry sc) [eqT2T1]]
    , -- transitivity
      [Prove eqT1T2, Assume eqT2T3] ==> [Prove eqT1T3, Reduction eqT1T2 (RedHow_ByEqTrans sc) [eqT2T1]]
    , -- context to nil reduction on equality types
      [Prove eqT1T2] ==> [Prove eqT1T3, Reduction eqT1T2 (RedHow_ByEqTyReduction ty2 ty3 sc) [eqT1T3]]
                     |> [IsCtxNilReduction ty2 ty3]
    {- , -- confluence
      [Prove eqT1T2] ==> [MetaProves newObls, Reduction eqT1T2 () [newObls]]
                     |> [Confluence t1 t2 newObls]  -}
    ]
  where
    eqT1T2 = mkCHRPredOcc (Pred_Eq ty1 ty2) sc
    eqT2T1 = mkCHRPredOcc (Pred_Eq ty2 ty1) sc
    eqT2T3 = mkCHRPredOcc (Pred_Eq ty2 ty3) sc
    eqT1T3 = mkCHRPredOcc (Pred_Eq ty1 ty3) sc
    
    newObls = mkCHRPredOcc undefined sc

    sc = PredScope_Var u1
    [ty1,ty2,ty3] = map mkTyVar [u2,u3,u4]
    [u1,u2,u3,u4] = mkNewLevUIDL 4 uidStart


env :: FIIn
env = emptyFI

results :: [Constraint CHRPredOcc RedHowAnnotation]
results = chrSolve env store constraints

res = vlist (map pp results)

main :: IO ()
main = return ()

