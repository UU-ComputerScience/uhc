{-# OPTIONS_GHC -fglasgow-exts -package EH16 #-}
module Main where

import EH16.CHR
import EH16.Pred.ToCHR
import EH16.CHR.Solve
import EH16.Ty.FitsIn
import EH16.Ty
import EH16.Pred.CHR
import EH16.Pred.CommonCHR
import EH16.Pred.RedGraph
import EH16.Base.Common
import EH16.Pred.Heuristics
import EH16.Pred.Evidence
import EH.Util.Pretty


-- list of constraints
input :: [Constraint CHRPredOcc RedHowAnnotation]
input
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
    
    [u1] = drop 7 (mkNewLevUIDL 8 uidStart)


-- CHR rules
store :: ScopedPredStore
store
  = chrStoreFromElems
  $ [ -- symmetry (prove)
      [Prove eqT1T2] ==> [Prove eqT2T1, Reduction eqT1T2 RedHow_ByEqSymmetry [eqT2T1]]
                     |> [UnequalTy ty1 ty2]
    , -- transitivity (normal case)
      [Prove eqT1T2, Assume eqT2T3] ==> [Prove eqT1T3, Reduction eqT1T2 RedHow_ByEqTrans [eqT1T3, eqT2T3]]
                                    |> [UnequalTy ty2 ty3]
    , -- transitivity (asymmetric case)
      [Prove eqT1T2, Assume eqT3T2] ==> [Prove eqT1T3, Reduction eqT1T2 RedHow_ByEqTrans [eqT1T3, eqT3T2]]
                                    |> [UnequalTy ty2 ty3]
    , -- context to nil reduction on equality types
      [Prove eqT1T2] ==> [Prove eqT1T3, Reduction eqT1T2 (RedHow_ByEqTyReduction ty2 ty3) [eqT1T3]]
                     |> [IsCtxNilReduction ty2 ty3]
    , -- congruence (results in new proof obligations, discarding the old by the congruence reduction)
      [Prove eqT1T2] ==> [Prove psPred, Reduction eqT1T2 RedHow_ByEqCongr [psPred]]
                     |> [AreOblsByCongruence ty1 ty2 ps]
    , -- unpack predseq (cons)
      [Prove psCons] ==> [Prove psHead, Prove psPred, Reduction psCons RedHow_ByPredSeqUnpack [psHead, psPred]]
    , -- unpack predsec (nil)
      [Prove psNil]  ==> [Reduction psNil RedHow_ByPredSeqUnpack []]
    ]
  where
    eqT1T2 = mkCHRPredOcc (Pred_Eq ty1 ty2) sc
    eqT2T1 = mkCHRPredOcc (Pred_Eq ty2 ty1) sc
    eqT2T3 = mkCHRPredOcc (Pred_Eq ty2 ty3) sc
    eqT3T2 = mkCHRPredOcc (Pred_Eq ty3 ty2) sc
    eqT1T3 = mkCHRPredOcc (Pred_Eq ty1 ty3) sc
    psPred = mkCHRPredOcc (Pred_Preds ps)   sc
    psCons = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr ps)) sc
    psNil  = mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc
    psHead = mkCHRPredOcc pr sc

    sc = PredScope_Var u1
    [ty1,ty2,ty3] = map mkTyVar [u2,u3,u4]
    ps = PredSeq_Var u5
    pr = Pred_Var u6
    [u1,u2,u3,u4,u5,u6] = mkNewLevUIDL 6 uidStart


env :: FIIn
env = emptyFI { fiUniq = last $ mkNewLevUIDL 7 uidStart }

producedCnstrs :: [Constraint CHRPredOcc RedHowAnnotation]
producedCnstrs = chrSolve env store input

assumeMap :: CHRPredOccCnstrMp
assumeMap = cnstrMpFromList [ (c, RedHow_Assumption (VarUIDHs_UID uidNull) (PredScope_Var uidNull)) | c <- producedCnstrs ]

graph :: RedGraph CHRPredOcc RedHowAnnotation
graph = addToRedGraphFromReductions producedCnstrs (addToRedGraphFromAssumes assumeMap emptyRedGraph)

evid :: Evidence CHRPredOcc RedHowAnnotation
evid = snd $ head $ (heurScopedEHC env) [RedHow_ProveObl uidNull (PredScope_Var uidNull)] $ redAlternatives graph p
  where (Prove p) = last input

res :: PP_Doc
res = vlist (map pp producedCnstrs)

main :: IO ()
main = return ()
