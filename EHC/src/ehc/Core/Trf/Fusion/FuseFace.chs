%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.FuseFace} export (fusionar, fusionarTau, fusionarSigma, getCata, getAna, HyloT, deriveHylo, inline, getConstantArgCount, getNames, renameHT, WrapHT(..), WrapHA(..))
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HyloRep},{%{EH}Core.Trf.Fusion.HyloContext})
%%]


%%[(8 codegen)

--import HyloRep
--import HyloContext
%%]
%%[(8 codegen) hs import(qualified {%{EH}Core.Trf.Fusion.FunctorRep} as F)
%%]
%%[(8 codegen) hs import(qualified {%{EH}Core.Trf.Fusion.Inline} as I)
import Control.Monad.Error(throwError,catchError)
import Control.Monad.Trans(lift)
import Control.Monad.State(get)
import List((\\))
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.RenVars})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Utils})
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})





data HyloT = HTp (HA Phii)
           | HTi (HA InF)
           | HTt (HA Tau)

%%]

showHT :: HyloT -> IntState String
showHT h = do i<-get;return$ foldHT (show' i False) (show' i False) (show' i True) h
  where show' i t h=foldHA (show'' i t) (show'' i t) (show'' i t) h
        show'' i t hss@(h:hs) = (names ++)$ concat $ (I.showHylo t i h :) $ 
                                map ("\n------------------------------\n"++)$ map (I.showHylo t i) hs
           where names=tuple (map getName hss)
                 tuple [] = "()"
                 tuple (n:ns) = '(': show n ++ concat (map ((',':).show) ns) ++ ") =\n"
        show'' _ t _ = ""

%%[(8 codegen)

data HA a = HAp [Hylo a Psi]
          | HAs [Hylo a Sigma]
          | HAo [Hylo a OutF]

foldHT :: (HA Phii->b)->(HA InF->b)->(HA Tau->b)->HyloT->b
foldHT f1 f2 f3 (HTp a) = f1 a
foldHT f1 f2 f3 (HTi a) = f2 a
foldHT f1 f2 f3 (HTt a) = f3 a

foldHA :: ([Hylo a Psi]->b)->([Hylo a OutF]->b)->([Hylo a Sigma]->b)->HA a->b
foldHA f1 f2 f3 (HAp a) = f1 a
foldHA f1 f2 f3 (HAo a) = f2 a
foldHA f1 f2 f3 (HAs a) = f3 a

class WrapHA ca where
 wrapHA :: [Hylo a ca] -> HA a
instance WrapHA Psi where
 wrapHA h = HAp h
instance WrapHA OutF where
 wrapHA h = HAo h
instance WrapHA Sigma where
 wrapHA h = HAs h
class WrapHT a where
 wrapHT ::  HA a -> HyloT
instance WrapHT Term where
 wrapHT = HTp
instance WrapHT InF where
 wrapHT = HTi
instance WrapHT Tau where
 wrapHT = HTt


getNames :: HyloT -> [Variable]
getNames = foldHT gn gn gn
 where gn = foldHA (map getName) (map getName) (map getName)

getConstantArgCount :: HyloT -> Int
getConstantArgCount = length . foldHT gn gn gn
 where gn = head . foldHA (map (getConstantArgs.getContext)) (map (getConstantArgs.getContext)) (map (getConstantArgs.getContext))

getRecArgCount :: HyloT -> Int -> Int
getRecArgCount h i = foldHT gn gn gn h
 where gn = (!!i) . foldHA (map getRecArgs) (map getRecArgs) (map getRecArgs)
       getRecArgs = (\(bvs,_,_)->length bvs) . getCoalgebra

getConstantArgPos :: HyloT -> Maybe [Int]
getConstantArgPos = foldHT gn gn gn
 where gn = head . foldHA (map (getCntArgPos.getContext)) (map (getCntArgPos.getContext)) (map (getCntArgPos.getContext))


renameHT ::  HyloT -> HyloT -> FusionState (HyloT,HyloT)
renameHT h1 h2 = foldHT (renh1 h2) (renh1 h2) (renh1 h2) h1
  where renh1 h2 h1 = foldHA (ren h2) (ren h2) (ren h2) h1
        ren h2 h1 = foldHT (renh2 h1) (renh2 h1) (renh2 h1) h2
        renh2 h1 h2 = foldHA (ren' h1) (ren' h1) (ren' h1) h2
        ren' [h1] [h2] = do (h1',h2')<-lift (renameVariables h1 h2 [])
                            return (wrapHT.wrapHA$ [h1'],wrapHT.wrapHA$ [h2'])
        ren' _ _ = error "Mutual recursion not handled."

deriveHylo :: [Def] -> FusionState HyloT
deriveHylo dfs = let (ctxs,vs,ts)=unzip3$ map (\(ctx,Defvalue v t)-> (ctx,v,t))$ extractContext$ dfs
                  in buildHylo vs ts >>= (return.wrapHT.wrapHA.zipWith setContext ctxs)


fusionar :: [Variable] -> HyloT -> Int -> Int -> HyloT -> Int -> FusionState (Int,HyloT)
fusionar names h1 ih1 ia' h2 ih2 = 
        -- catchError (fusionar' names h1 ih1 (ia h1 ih1) h2 ih2)$ const$
         do h1' <- lift (inline h1) >>= deriveHylo
            h2' <- lift (inline h2) >>= deriveHylo
            fusionar' names h1' ih1 (ia h1' ih1) h2' ih2 
  where ia'' = maybe (ia'-getConstantArgCount h1) (length.([0..ia'-1]\\))$ getConstantArgPos h1
        ia h1 ih1 = max 0 (min (getRecArgCount h1 ih1-1) ia'')

fusionar' :: [Variable] -> HyloT -> Int -> Int -> HyloT -> Int -> FusionState (Int,HyloT)
fusionar' names h1 ih1 ia h2 ih2 = foldHT (fuseh1 h2) (fuseh1 h2) (fuseh1 h2) h1
   where
       fuseh1 h2 h1 = foldHA (fusePsi h2) (fuseCata h2) (fuseSigma h2) h1
       errorTau _ = throwError NotTau
       fuseCata h2 h1 = foldHT (fuseCataHylo h1) (fuseCataAna h1) errorTau h2
       fusePsi h2 h1 = foldHT (fusePsiPhii h1) (fuseHyloAna h1) errorTau h2
       fuseSigma h2 h1 = foldHT (fuseSigmaPhii h1) (fuseSigmaAna h1) errorTau h2
       fuseCataHylo h1 h2 = let f h2 i2 = fusionarOutF names h1 ih1 h2 i2
                             in foldHA f f f h2 ih2
       fuseCataAna h1 h2 = let f h2 i2 = F.fusionarSimple h1 ih1 h2 i2 >>= wrapHylos
                            in foldHA f f f h2 ih2
       fusePsiPhii h1 h2 =  let f h1=fusionarAmbos names h1 ih1 ia
                             in foldHA (f h1) (f h1) (f h1) h2 ih2
       fuseHyloAna h1 h2 = let f h1=fusionarInF names h1 ih1 ia
                            in foldHA (f h1) (f h1) (f h1) h2 ih2
       fuseSigmaAna h1 h2 = let f h1 h2=F.fusionarSigma h1 ih1 ia h2 ih2 >>= wrapHylos
                             in foldHA (f h1) (f h1) (f h1) h2
       fuseSigmaPhii h1 h2 = let f h1 h2 = do h2' <- mapM (F.getAna h2) h2
                                              F.fusionarSigma h1 ih1 ia h2' ih2>>=wrapHylos
                             in foldHA (f h1) (f h1) (f h1) h2 
       wrapHylos (m,h) = do vs<-sequence $ replicate (length h-length names) (lift (getFreshVar "v"))
                            return (m,wrapHT.wrapHA.zipWith setName (names++vs)$ h)

fusionarTau :: [Variable] -> HyloT -> Int -> HyloT -> Int -> FusionState (Int,HyloT)
fusionarTau names h1 ih1 h2 ih2 = foldHT (fuseh1 h2) (fuseh1 h2) (fuseh1 h2) h1
   where
       fuseh1 h2 h1 = foldHA errorh1 (fuseCata h2) errorh1 h1
       errorh1 _ = throwError (Msg first_Hylo_Not_OutF_Form)
       errorh2 _ = throwError (Msg second_Hylo_Not_Phi_Form)
       fuseCata h2 h1 = foldHT (fuseCataHylo h1) errorh2 errorh2 h2
       fuseCataHylo h1 h2 = let f h1 h2=do res<-lift$ F.fusionarTau h1 ih1 h2 ih2;wrapHylos res
                             in foldHA (f h1) (f h1) (f h1) h2
       wrapHylos (m,h) = do vs<-sequence $ replicate (length h-length names) (lift$ getFreshVar "v")
                            return (m,wrapHT.wrapHA.zipWith setName (names++vs)$ h)

fusionarSigma :: [Variable] -> HyloT -> Int -> Int -> HyloT -> Int -> FusionState (Int,HyloT)
fusionarSigma names h1 ih1 ia h2 ih2 = foldHT (fuseh1 h2) (fuseh1 h2) (fuseh1 h2) h1
   where
       fuseh1 h2 h1 = foldHA (fuseAna h2) errorh1 (fuseSigma h2) h1
       errorh1 _ = throwError (Msg first_Hylo_Not_Psi_Form)
       errorh2 _ = throwError (Msg second_Hylo_Not_InF_Form)
       fuseAna h2 h1 = foldHT errorh2 (fuseHyloAna h1) errorh2 h2
       fuseSigma h2 h1 = foldHT errorh2 (fuseSigmaAna h1) errorh2 h2
       fuseSigmaAna h1 h2 = let f h1 = F.fusionarSigma h1 ih1 ia
                            in wrapHylos$ foldHA (f h1) (f h1) (f h1) h2 ih2
       fuseHyloAna h1 h2 = let f h1 h2 ih2 = do h1'<-F.psiToSigma h1
                                                F.fusionarSigma h1' ih1 ia h2 ih2
                            in wrapHylos$ foldHA (f h1) (f h1) (f h1) h2 ih2
       wrapHylos res = do (m,h)<-res
                          vs<-sequence $ replicate (length h-length names) (lift$ getFreshVar "v")
                          return (m,wrapHT.wrapHA.zipWith setName (names++vs)$ h)


getCata :: HyloT -> FusionState HyloT
getCata h = foldHT f f f h
 where f h=foldHA f' (return.wrapHT.wrapHA) (\_->throwError NotInF) h
       f' h = do h'<-mapM (F.getCata h) h
                 return.wrapHT.wrapHA$ h'

getAna :: HyloT -> FusionState HyloT
getAna h = foldHT f (return.wrapHT) (\_->throwError NotInF) h
 where f h=foldHA f' f' f' h
       f' h = do h'<-mapM (F.getAna h) h
                 return.wrapHT.wrapHA$ h'

wrapHylos :: (WrapHT a,WrapHA b) => [Variable] -> Int -> [Hylo a b] -> FusionState (Int,HyloT)
wrapHylos names m h = do vs<-sequence $ replicate (length h-length names) (lift$ getFreshVar "v")
                         return (m,wrapHT.wrapHA.zipWith setName (names++vs)$ h)




fusionarAmbos names h1 ih1 ia h2 ih2 = catchError (mapM (F.getCata h1) h1 >>= okCata)  badCata
 where
   okCata h1' = catchError (mapM (F.getAna h2) h2 >>= okcataana h1') (okCataBadAna h1')
   badCata _ = catchError (mapM (F.getAna h2) h2 >>= badCataOkAna ) badCataBadAna 
   okcataana h1' h2'= catchError (do h1''<-F.psiToSigma h1
                                     res<-F.fusionarSigma h1'' ih1 ia h2' ih2
                                     oksigma h1' h2' res)
                                 (badsigma h1' h2')
   oksigma h1' h2' (m3,hh3) =
                          do (m1,hh1)<-F.fusionarSimple h1' ih1 h2' ih2
                             (m2,hh2)<-lift$ F.fusionarTau h1' ih1 h2 ih2
                             if m1>=m2
                               then if m1>=m3 then wrapHylos names m1 hh1
                                      else wrapHylos names m3 hh3
                               else if m2>=m3 then wrapHylos names m2 hh2
                                      else wrapHylos names m3 hh3
   okCataBadAna h1' _ = do (m2,hh2)<-lift (F.fusionarTau h1' ih1 h2 ih2); wrapHylos names m2 hh2
   badCataOkAna h2' = do h1''<-F.psiToSigma h1
                         (m3,hh3)<-F.fusionarSigma h1'' ih1 ia h2' ih2; wrapHylos names m3 hh3
   badCataBadAna _ = throwError (Msg couldnt_Fuse_Hylos)
   badsigma h1' h2' _ =     do (m1,hh1)<-F.fusionarSimple h1' ih1 h2' ih2
                               (m2,hh2)<-lift$ F.fusionarTau h1' ih1 h2 ih2
                               if m1>=m2
                                 then wrapHylos names m1 hh1
                                 else wrapHylos names m2 hh2


fusionarOutF :: (WrapHT a,WrapHA b,HasComponents b,TermWrappable a, WrapTau a,Vars b, Substitutable b, VarsB b,VarsB a,Vars a, Substitutable a) =>
                          [Variable] -> [Hylo a OutF] -> Int -> [Hylo Phii b] -> Int -> FusionState (Int,HyloT)
fusionarOutF names h1 ih1 h2 ih2 = catchError (mapM (F.getAna h2) h2 >>= okcataana) okCataBadAna
 where
   okcataana h2'=     do (m1,hh1)<-F.fusionarSimple h1 ih1 h2' ih2
                         (m2,hh2)<-lift$ F.fusionarTau h1 ih1 h2 ih2
                         if m1>=m2
                           then wrapHylos names m1 hh1
                           else wrapHylos names m2 hh2
   okCataBadAna _ = do (m2,hh2)<-lift$ F.fusionarTau h1 ih1 h2 ih2; wrapHylos names m2 hh2




fusionarInF names h1 ih1 ia h2 ih2 = catchError (mapM (F.getCata h1) h1 >>= okCata) badCata
 where
   okCata h1' = catchError (do h1''<-F.psiToSigma h1
                               res<-F.fusionarSigma h1'' ih1 ia h2 ih2
                               oksigma h1' res)
                           (badsigma h1')
   oksigma h1' (m2,hh2)=
                 do (m1,hh1)<-F.fusionarSimple h1' ih1 h2 ih2
                    if m1>=m2
                      then wrapHylos names m1 hh1
                      else wrapHylos names m2 hh2
   badCata _ = catchError (do h1''<- F.psiToSigma h1
                              (m3,hh3)<-F.fusionarSigma h1'' ih1 ia h2 ih2
                              wrapHylos names m3 hh3)
                          error
   badsigma h1' _ = do (m3,hh3)<-F.fusionarSimple h1' ih1 h2 ih2; wrapHylos names m3 hh3
   error _ = throwError (Msg couldnt_Fuse_Hylos)






inline :: HyloT -> IntState [Def]
inline hylo =
      foldHT f f f hylo
 where f h = foldHA g g g h
       g h = do dfs<-mapM (I.inline h) h
                return (mergeContext (zip (map getContext h) dfs))

%%]
