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
  = [ eq a tyA tyB
    , eq a tyB tyC
    , eq a tyK1 ty1
    , eq p ty2 tyK1
    ]
  where
    a :: Pred -> Constraint CHRPredOcc RedHowAnnotation
    a pr = Assume (mkCHRPredOcc pr initPredScope)
    
    p :: Pred -> Constraint CHRPredOcc RedHowAnnotation
    p pr = Prove (mkCHRPredOcc pr initPredScope)
    
    eq f t1 t2 = f (Pred_Eq t1 t2)
    
    [tyA, tyB, tyC, tyK1, tyK2, tyInt] = map (Ty_Con . HNm) ["A", "B", "C", "K1", "K2", "Int"]
    tyCI = Ty_Impls (Impls_Tail u1 []) `mk1Arrow` tyC
    
    ty1 = mkProdApp [tyK2 `mk1Arrow` tyA, tyInt]
    ty2 = mkProdApp [tyK2 `mk1Arrow` tyCI, tyInt]
    
    [u1] = drop 6 (mkNewLevUIDL 7 uidStart)


-- CHR rules
store :: ScopedPredStore
store
  = chrStoreFromElems
  $ [ -- symmetry (assume)
      [Assume eqT1T2] <==> [Assume eqT2T1]
    , -- symmetry (prove)
      [Prove eqT1T2] ==> [Prove eqT2T1, Reduction eqT1T2 RedHow_ByEqSymmetry [eqT2T1]]
    , -- transitivity
      [Prove eqT1T2, Assume eqT2T3] ==> [Prove eqT1T3, Reduction eqT1T2 RedHow_ByEqTrans [eqT2T1]]
    , -- context to nil reduction on equality types
      [Prove eqT1T2] ==> [Prove eqT1T3, Reduction eqT1T2 (RedHow_ByEqTyReduction ty2 ty3) [eqT1T3]]
                     |> [IsCtxNilReduction ty2 ty3]
    , -- congruence (results in new proof obligations, discarding the old by the congruence reduction)
      [Prove eqT1T2] ==> [Prove psPred, Reduction eqT1T2 RedHow_ByEqCongr []]
                     |> [AreOblsByCongruence ty1 ty2 ps]
    , -- unpack predseq (cons)
      [Prove psCons] ==> [Prove psHead, Prove psPred, Reduction psCons RedHow_ByPredSeqUnpack [psHead, psPred]]
    , -- unpack predsec (nil)
      [Prove psNil]  ==> [Reduction psNil RedHow_ByPredSeqUnpack []]
--      [Prove psCons] ==> [Prove (mkCHRPredOcc (Pred_Eq (Ty_Con $ HNm "D") (Ty_Con $ HNm "D")) sc), Prove psPred]
--    , [Prove psNil] ==> [Prove (mkCHRPredOcc (Pred_Eq (Ty_Con $ HNm "E") (Ty_Con $ HNm "E")) sc)]
    ]
  where
    eqT1T2  = mkCHRPredOcc (Pred_Eq ty1 ty2) sc
    eqT2T1  = mkCHRPredOcc (Pred_Eq ty2 ty1) sc
    eqT2T3  = mkCHRPredOcc (Pred_Eq ty2 ty3) sc
    eqT1T3  = mkCHRPredOcc (Pred_Eq ty1 ty3) sc
    psPred  = mkCHRPredOcc (Pred_Preds ps)   sc
    psCons  = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr ps)) sc
    psNil   = mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc
    psHead  = mkCHRPredOcc pr sc

    sc = PredScope_Var u1
    [ty1,ty2,ty3] = map mkTyVar [u2,u3,u4]
    ps = PredSeq_Var u5
    pr = Pred_Var u6
    [u1,u2,u3,u4,u5,u6] = mkNewLevUIDL 6 uidStart


env :: FIIn
env = emptyFI

(work, done, trace) = chrSolve' env store constraints

results :: [Constraint CHRPredOcc RedHowAnnotation]
results = chrSolve env store constraints

res :: PP_Doc
res = vlist (map pp results)

main :: IO ()
main = return ()
