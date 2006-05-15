%%[1 import (Data.List,UU.Pretty,Common,Ty,Cnstr,Substitutable) export(module Common,module Ty,module Cnstr,module Substitutable)
%%]

%%[1 export(mkNewLevUID)
%%]

%%[1 export(Gam,gamLookup)
%%]

%%[1 export(tyInst)
%%]

%%[1.fit export((<=>))
%%]

%%[2 -1.fit export(fitsIn,FIIn(..),emptyFI)
%%]

%%[1 export(mkErr)
%%]

%%[2 export(mkNewLevUID2,mkNewLevUID3,mkNewLevUID4,mkNewLevUID5,mkNewLevUID6,mkNewUID)
%%]

%%[3 import(Data.List,TyElimBoth) export(FIOut(..),emptyFO)
%%]

%%[3 export(FitsIn',mkFitsInWrap')
%%]

%%[3 export(fitsInLWith)
%%]

%%[3 hs export(err_InconsistentAlts)
%%]

%%[3 import(UU.Pretty) export(ppCnstr)
%%]


-------------------------------------------------------------------------
-- Error
-------------------------------------------------------------------------

%%[1
mkErr :: [PP_Doc] -> PP_Doc
mkErr [] = empty
mkErr p  = "<ERR:" >#< vlist p >|< ">"
%%]

%%[3
err_InconsistentAlts :: Ty -> TyVarId -> TyL -> FIMode -> PP_Doc
err_InconsistentAlts t tv tl fm
  = pe "Inconsistent type alternatives for type variable"
       (    "in type :" >#< show t
        >-< "for tvar:" >#< show tv
        >-< "types   :" >#< vlist (map show tl)
        >-< "fit mode:" >#< text (show fm)
       )
  where pe m p = m >-< indent 4 p
%%]

-------------------------------------------------------------------------
-- Unique identifier
-------------------------------------------------------------------------

%%[1
mkNewLevUID :: UID -> (UID,UID)
mkNewLevUID u@(UID ls) = (uidNext u,UID (0:ls))

uidNext :: UID -> UID
uidNext (UID (l:ls)) = UID (l+1:ls)

mkUIDs :: UID -> [UID]
mkUIDs = iterate uidNext
%%]

%%[2
mkNewLevUID2 u = let { (u',u1)          = mkNewLevUID   u; (u'',u2)         = mkNewLevUID   u'} in (u'',u1,u2)
mkNewLevUID3 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3)         = mkNewLevUID   u'} in (u'',u1,u2,u3)
mkNewLevUID4 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4)      = mkNewLevUID2  u'} in (u'',u1,u2,u3,u4)
mkNewLevUID5 u = let { (u',u1,u2)       = mkNewLevUID2  u; (u'',u3,u4,u5)   = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5)
mkNewLevUID6 u = let { (u',u1,u2,u3)    = mkNewLevUID3  u; (u'',u4,u5,u6)   = mkNewLevUID3  u'} in (u'',u1,u2,u3,u4,u5,u6)

mkNewUID :: UID -> (UID,UID)
mkNewUID   uid = (uidNext uid,uid)
%%]

-------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------


-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

%%[1
type Gam = [(String,Ty)]

gamLookup :: String -> Gam -> (Ty,[PP_Doc])
gamLookup n g
  = maybe (Ty_Any,[n >#< "undefined"]) (\t -> (t,[]))
  $ lookup n g
%%]

-------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------

%%[1
instance Substitutable Gam where
  s |=> g  = map (\(i,t) -> (i,s |=> t)) g
  ftv      = foldr union [] . map (ftv . snd)
%%]

%%[3
instance Substitutable TyL where
  s1 |=> tl = map (s1 |=>) tl
  ftv       = foldr union [] . map ftv
%%]

-------------------------------------------------------------------------
-- Support for type matching
-------------------------------------------------------------------------

%%[2
data FIIn  = FIIn  { fiUniq :: UID, fiCnstr :: Cnstr
%%]
%%[3
                   , fiFIOpts :: FIOpts
%%]
%%[2
                   }
%%]

%%[2
emptyFI = FIIn  { fiUniq = uidStart, fiCnstr = emptyCnstr
%%]
%%[3
                , fiFIOpts = strongFIOpts
%%]
%%[2
                }
%%]

%%[2
data FIOut = FIOut {foUniq :: UID, foCnstr :: Cnstr, foErrL :: [PP_Doc], foTy :: Ty}
emptyFO = FIOut {foUniq = uidStart, foCnstr = emptyCnstr, foErrL = [], foTy = Ty_Any}
%%]

%%[3
foHasErrs :: FIOut -> Bool
foHasErrs = not . null . foErrL

manyFO :: [FIOut] -> FIOut
manyFO = foldr1 (\fo1 fo2 -> if foHasErrs fo1 then fo1 else fo2)
%%]

%%[3
type FitsIn' = FIOpts -> UID -> Ty -> Ty -> FIOut
type FitsIn = FIOpts -> UID -> Ty -> Ty -> (Ty,Cnstr,ErrL)

mkFitsInWrap' :: FitsIn'
mkFitsInWrap'
  =  \opt u t1 t2 ->  let  fo = fitsIn' (emptyFI {fiFIOpts = opt, fiUniq = u}) t1 t2
                      in   fo
%%]

%%[3
fitsInLWith :: (FIOut -> FIOut -> FIOut) -> FitsIn' -> FIOpts -> UID -> TyL -> TyL -> (FIOut,[FIOut])
fitsInLWith foCmb elemFits opts uniq tyl1 tyl2
  = (fo,foL)
  where ((_,fo),foL)
          = foldr  (\(t1,t2) ((u,foThr),foL)
                      -> let  (u',ue) = mkNewLevUID u
                              fo = elemFits opts u (foCnstr foThr |=> t1) (foCnstr foThr |=> t2)
                         in   ((u',foCmb fo foThr),fo:foL)
                   )
                   ((uniq,emptyFO),[])
            . zip tyl1
            $ tyl2
%%]

-------------------------------------------------------------------------
-- Type matching (unification)
-------------------------------------------------------------------------

%%[1.fit
(<=>) :: Ty -> Ty -> (Cnstr,[PP_Doc])
Ty_Any          <=> t2          = ([],[])
t1              <=> Ty_Any      = ([],[])
Ty_Int          <=> Ty_Int      = ([],[])
Ty_Var v1       <=> Ty_Var v2
    | v1 == v2                  = ([],[])
Ty_Var v1       <=> t2
    | v1 `notElem` ftv t2       = ([(v1,t2)],[])
t1              <=> Ty_Var v2
    | v2 `notElem` ftv t1       = ([(v2,t1)],[])
Ty_Arr a1 r1    <=> Ty_Arr a2 r2
  = (sr |=> sa,ea ++ er)
  where (sa,ea) = a1 <=> a2
        (sr,er) = (sa |=> r1) <=> (sa |=> r2)
t1              <=> t2          = ([],["could not match"
                                       >#< show t1 >#< "with"
                                       >#< show t2]
                                  )
%%]

%%[2.fit -1.fit
fitsIn :: FIIn -> Ty -> Ty -> (Cnstr,[PP_Doc])
fitsIn fi lty rty
  = (foCnstr fo,foErrL fo)
  where fo = fitsIn' fi lty rty
%%]

%%[3.fit -2.fit
fitsIn :: FIIn -> Ty -> Ty -> (Ty,Cnstr,[PP_Doc])
fitsIn fi lty rty
  = (foTy fo,foCnstr fo,foErrL fo)
  where fo = fitsIn' fi lty rty
%%]

%%[2
fitsIn' :: FIIn -> Ty -> Ty -> FIOut
fitsIn' fi lty rty
  = fo
  where fo       = f fi (fiCnstr fi |=> lty) (fiCnstr fi |=> rty)
%%]
%%[2.fit.res
        --
        res   fi t   = emptyFO {foUniq = fiUniq fi, foTy = t}
        bind  fi v t = emptyFO {foUniq = fiUniq fi, foCnstr = [(v,t)], foTy = t}
        err   fi p   = emptyFO {foUniq = fiUniq fi, foErrL = [p]}
%%]
%%[3.fit.res -2.fit.res
        --
        res   fi t    = fifo fi $ rfo t $ emptyFO
        bind  fi v  t = fifo fi $ b1fo v t $ emptyFO
        bindL fi vl t = fifo fi $ bfo vl t $ emptyFO
        err   fi p    = fifo fi $ emptyFO {foErrL = [p]}
%%]
%%[2
        occurBind fi v t | v `elem` ftv t = err fi ("tvar" >#< show v >#< "occurs in" >#< show t)
                         | otherwise      = bind fi v t
%%]
%%[3
        occurBindAlt fi isR tv t = occurBind fi tv (mkTyAlts fi isR tv t)
%%]
%%[3
        --
        mkc  tvL  t  = [ (v,t) | v <- tvL ]
        ufo  u    fo = fo {foUniq = u}
        fifo fi      = ufo (fiUniq fi)
        rfo  t    fo = fo {foTy = t}
        cfo  c    fo = fo {foCnstr = c |=> foCnstr fo}
        bfo  vl t    = cfo (mkc vl t) . rfo t
        b1fo v       = bfo [v]
%%]
%%[3
        --
        mkTyAlts fi isR tv t    =  if fioBindToTyAlts (fiFIOpts fi)
                                   then Ty_Alts tv [snd (mkTyPlusHard fi isR tv t)]
                                   else t
        mkTyPlusHard fi isR v t =  (fi,TyPlus_Ty t h o)
                                   where h =  if fioIsMeetJoin (fiFIOpts fi)
                                              then TyHard
                                              else TySoft v
                                         o =  if fioIsSubsume (fiFIOpts fi)
                                              then (if isR then TyRequired else TyOffered)
                                              else if fioIsMeetJoin (fiFIOpts fi)
                                              then (if fioMode (fiFIOpts fi) == FitMeet then TyRequired else TyOffered)
                                              else TyNoNeed
        cmbTyAltsL t1L t2L      =  nub (q1 ++ q2) ++ nub (r1 ++ r2)
                                   where  p = partition (tyIsQu . tyPlusTy)
                                          (q1,r1) = p t1L
                                          (q2,r2) = p t2L
%%]
%%[3
        --
        allowBind fi (Ty_Var v)                 = v `notElem` fioDontBind (fiFIOpts fi)
        allowImpredTVBindL fi t1 t2             = allowBind fi t1
        allowImpredTVBindR fi t1 t2             = allowBind fi t1
%%]
%%[3
        --
        f fi t1                     t2
            | fioMode (fiFIOpts fi) == FitSubRL = f  fi' t2 t1
            where  fi' = fi  {fiFIOpts = fioSwapOpts . fioSwapCoCo ContraVariant . fiFIOpts $ fi}
%%]
%%[2
        --
        f fi Ty_Any             	t2          = res fi t2
        f fi t1                 	Ty_Any      = res fi t1
        f fi t@Ty_Int           	Ty_Int      = res fi t
        f fi t@Ty_Char          	Ty_Char     = res fi t
        f fi t@(Ty_Fix v1)      	(Ty_Fix v2)
           | v1 == v2                           = res fi t
        f fi t1@(Ty_Var v1)     	t2@(Ty_Var v2)
           | v1 == v2                           = res fi t1
%%]
%%[3
           | lBefR && allowBind fi t1           = bind fi v1 t2
           | not lBefR && allowBind fi t2       = bind fi v2 t1
           where lBefR = fioBindLBeforeR (fiFIOpts fi)
%%]
%%[3
        f fi t1@(Ty_Both _ _)       t2@(Ty_Alts _ _)
                                                = res fi t1
        f fi t1@(Ty_Alts _ _)       t2@(Ty_Both _ _)
                                                = res fi t2
        f fi t1@(Ty_Both v1 [t1b])  t2@(Ty_Both v2 [t2b])
                                                = manyFO [fo,bfo [v1,v2] (Ty_Both v2 [foTy fo]) fo]
            where  fo = f fi t1b t2b
        f fi t1@(Ty_Both v1 [])     t2@(Ty_Both v2 _)
                                                = bind fi v1 t2
        f fi t1@(Ty_Both v1 _)      t2@(Ty_Both v2 [])
                                                = bind fi v2 t1
        f fi t1@(Ty_Both v1 [])     t2          = bind fi v1 (Ty_Both v1 [t2])
        f fi t1@(Ty_Both v1 [t1b])  t2          = manyFO [fo,b1fo v1 (Ty_Both v1 [foTy fo]) fo]
            where  fo = f fi t1b t2
        f fi t1     t2@(Ty_Both v2 [])          = bind fi v2 (Ty_Both v2 [t1])
        f fi t1     t2@(Ty_Both v2 [t2b])       = manyFO [fo,b1fo v2 (Ty_Both v2 [foTy fo]) fo]
            where  fo = f fi t1 t2b
%%]
%%[2.fit.var
        f fi (Ty_Var v1)        t2              = occurBind fi v1 t2
        f fi t1                 (Ty_Var v2)     = occurBind fi v2 t1
%%]
%%[3.fit.var -2.fit.var
        f fi t1@(Ty_Var v1)       	t2@(Ty_Var v2)
            | allowBind fi t1                   = bind fi v1 t2
            | allowBind fi t2                   = bind fi v2 t1
        f fi t1@(Ty_Var v1)       	t2@(Ty_Alts _ _)
            | allowBind fi t1                   = bind fi v1 t2
        f fi t1@(Ty_Alts _ _)       t2@(Ty_Var v2)
            | allowBind fi t2                   = bind fi v2 t1
        f fi t1@(Ty_Var v1)       	t2
            | allowImpredTVBindL fi t1 t2       = occurBindAlt fi True v1 t2
        f fi t1                     t2@(Ty_Var v2)
            | allowImpredTVBindR fi t2 t1       = occurBindAlt fi False v2 t1
%%]
%%[3
        f fi t1@(Ty_Alts v1 t1L)    t2@(Ty_Alts v2 t2L)
                                                = bindL fi [v1,v2] (Ty_Alts v1 (t1L `cmbTyAltsL` t2L))
        f fi t1@(Ty_Alts v1 t1L)    t2
                                                = bind fipl v1 (Ty_Alts v1 (t1L `cmbTyAltsL` [t2pl]))
            where  (fipl,t2pl) = mkTyPlusHard fi True v1 t2
        f fi t1                     t2@(Ty_Alts v2 t2L)
                                                = bind fipl v2 (Ty_Alts v2 ([t1pl] `cmbTyAltsL` t2L))
            where  (fipl,t1pl) = mkTyPlusHard fi False v2 t1
%%]
%%[2.fit.all
        f fi t1@(Ty_All _ _) 		t2
          = fo {foTy = foCnstr fo |=> t2}
          where (t1',u') = tyInst1 Ty_Var (fiUniq fi) t1
                fo = f (fi {fiUniq = u'}) t1' t2
        f fi t1                 	t2@(Ty_All _ _)
          = fo {foTy = foCnstr fo |=> t2'}
          where (t2',u') = tyInst1 Ty_Fix (fiUniq fi) t2
                fo = f (fi {fiUniq = u'}) t1 t2'
%%]
%%[3.fit.all -2.fit.all
        f fi t1@(Ty_All _ _) 		t2
            | fioIsSubsume (fiFIOpts fi)
          = rfo (foCnstr fo |=> t2) $ fo
          where (t1',u') = tyInst1 Ty_Var (fiUniq fi) t1
                fo = f (fi {fiUniq = u'}) t1' t2
        f fi t1                 	t2@(Ty_All _ _)
            | fioIsSubsume (fiFIOpts fi)
          = rfo (foCnstr fo |=> t2') $ fo
          where (t2',u') = tyInst1 Ty_Fix (fiUniq fi) t2
                fo = f (fi {fiUniq = u'}) t1 t2'
%%]
%%[3
        f fi t1@(Ty_All _ _)   		t2
            | m == FitMeet || m == FitJoin      = manyFO [fo,fo2]
            where  m = fioMode (fiFIOpts fi)
                   (uqt1,u2,rtvs1) = tyInst1' (\v -> Ty_Both v []) (fiUniq fi) t1
                   fi1 = fi {fiUniq = u2}
                   fo = f fi1 uqt1 t2
                   (ebTy,ebCnstr) = tyElimBoth rtvs1 (foTy fo)
                   ebTy' =  if m == FitJoin
                            then ebCnstr |=> ebTy
                            else ebTy
                   tvs = rtvs1 `intersect` ftv ebTy'
                   fo2 = rfo (mkTyAll tvs ebTy') . cfo ebCnstr $ fo
        f fi t1                     t2@(Ty_All _ _)
            | fioIsMeetJoin (fiFIOpts fi)       = f  (fi  {fiFIOpts = fioSwapOpts (fiFIOpts fi)})
                                                     t2 t1
%%]
%%[2.fit.arr
        f fi (Ty_Arr a1 r1)     	(Ty_Arr a2 r2)
          = mr {foCnstr = foCnstr mr |=> foCnstr ma,foErrL = foErrL ma ++ foErrL mr, foTy = Ty_Arr (foCnstr mr |=> foTy ma) (foTy mr)}
          where ma = f fi a2 a1
                mr = f (fi {fiUniq = foUniq ma}) (foCnstr ma |=> r1) (foCnstr ma |=> r2)
%%]
%%[3.fit.arr -2.fit.arr
        f fi (Ty_Arr a1 r1)     	(Ty_Arr a2 r2)
          = manyFO [foa,for,cfo (foCnstr foa) $ rfo (Ty_Arr (foCnstr for |=> foTy foa) (foTy for)) $ for]
          where foa = f (fi {fiFIOpts = fioSwapCoCo ContraVariant $ fiFIOpts $ fi}) a1 a2
                for = f (fi {fiUniq = foUniq foa}) (foCnstr foa |=> r1) (foCnstr foa |=> r2)
%%]
%%[2
        f fi t1                 	t2
          = err fi ("could not match" >#< show t1 >#< "with" >#< show t2)
%%]

-------------------------------------------------------------------------
-- Type instantiation
-------------------------------------------------------------------------

%%[1.tyInst1
tyInst1 :: (UID -> Ty) -> UID -> Ty -> (Ty,UID)
tyInst1 f u (Ty_All v t) = (c |=> t,u')
                         where c = [(v,f u1)]
                               (u',u1) = mkNewLevUID u
tyInst1 _ u t            = (t,u)
%%]

%%[3.tyInst1 -1.tyInst1
tyInst1 :: (UID -> Ty) -> UID -> Ty -> (Ty,UID)
tyInst1 f u t
  = (t',u')
  where (t',u',_) = tyInst1' f u t

tyInst1' :: (UID -> Ty) -> UID -> Ty -> (Ty,UID,TyVarIdL)
tyInst1' f u (Ty_All v t) = (c |=> t,u',[u1])
                         where c = [(v,f u1)]
                               (u',u1) = mkNewLevUID u
tyInst1' _ u t            = (t,u,[])
%%]

%%[1
tyInst :: UID -> Ty -> Ty
tyInst u t@(Ty_All _ _) = tyInst u' t'
                        where (t',u') = tyInst1 Ty_Var u t
tyInst _ t              = t
%%]

-------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------

%%[3
ppCnstr :: Cnstr -> PP_Doc
ppCnstr = vlist . map (\(v,t) -> show v >#< ":->" >#< show t)
%%]

