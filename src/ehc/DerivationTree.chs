%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for pretty printing derivation tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs module {%{EH}DerivationTree} import({%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty.FitsInCommon})
%%]

%%[(99 hmtyinfer) hs import({%{EH}Ty},{%{EH}Ty.Ftv},{%{EH}VarMp},{%{EH}Base.LaTeX},{%{EH}Substitutable},{%{EH}Gam.Full})
%%]

%%[(99 hmtyinfer) hs import(EH.Util.Pretty,{%{EH}Ty.Pretty})
%%]

%%[(99 hmtyinfer) hs import(qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Judgements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtJdg,dtJdgExpr,dtJdgExpr',dtJdgRecExpr)
dtJdg :: String -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdg nm ctx e concl = l2tText (ctx >#< "^^ Sub(:-)(" >|< nm >|< ") ^^" >#< e >#< ":" >#< concl)

dtJdgExpr' :: String -> EHCOpts -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdgExpr' kind opts gam knTy e ty
  = dtJdg kind (ppSemis' ctxt) e ty
  where ctxt = [gam] ++ (if ehcOptEmitDerivTree opts == DerivTreeWay_Infer then [knTy] else [])

dtJdgExpr, dtJdgRecExpr :: EHCOpts -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdgExpr    = dtJdgExpr' "e"
dtJdgRecExpr = dtJdgExpr' "re"
%%]

%%[(99 hmtyinfer) hs export(dtJdgDecl,dtJdgGam,dtJdgMatch,dtJdgStack)
dtJdgDecl :: PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdgDecl gam e ty = dtJdg "d" gam e ty

dtJdgGam :: PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdgGam gam nm ty = l2tText (ppParens (nm >#< ":->" >#< ty) >#< "`elem`" >#< gam)

dtJdgMatch :: EHCOpts -> FIOpts -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc -> PP_Doc
dtJdgMatch opts fiopts t1 t2 t res
  = if ehcOptEmitDerivTree opts == DerivTreeWay_Infer
    then dtJdg howmatch opt (t1 >#< howmatch >#< t2) (t >#< "~>" >#< res)
    else dtJdg howmatch opt (t1 >#< howmatch >#< t2) empty
  where howmatch = show $ fioMode fiopts
        opt      = empty

dtJdgStack :: [PP_Doc] -> PP_Doc
dtJdgStack l
  = ltxEnvironmentArgs "array" [pp "c"] $ ppListSep "" "" ltxNl $ hack l
  where hack [e] = [pp " ",e]
        hack l   = l
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtRule)
dtRule :: Bool -> String -> String -> [PP_Doc] -> PP_Doc -> PP_Doc
dtRule isTop fmt nm pre post = ltxDtOver fmt isTop pre nm post
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name used in DT's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(DtNm,dtNmNext,dtMkNm)
type DtNm = String

dtNmNext :: (Int -> DtNm) -> Int -> (Int,DtNm)
dtNmNext mk i
  = (i', mk i')
  where i' = i+1

dtMkNm :: DtNm -> Int -> DtNm
dtMkNm n i = n ++ "_" ++ show i

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp DtNm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtVmNmBase,dtMkVmNm,dtVmNmNext)
dtVmNmBase :: DtNm
dtVmNmBase = "VarMp"

dtMkVmNm :: Int -> DtNm
dtMkVmNm = dtMkNm dtVmNmBase

dtVmNmNext :: Int -> (Int,DtNm)
dtVmNmNext = dtNmNext dtMkVmNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam DtNm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtGamNmBase,dtMkGamNm,dtGamNmNext)
dtGamNmBase :: DtNm
dtGamNmBase = "Gamma"

dtMkGamNm :: Int -> DtNm
dtMkGamNm = dtMkNm dtGamNmBase

dtGamNmNext :: Int -> (Int,DtNm)
dtGamNmNext = dtNmNext dtMkGamNm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gathered Gam's, printed under a DT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(DtGamL,dtGamL)
type DtGamL
  = AssocL DtNm
           (PP_Doc      -- the gam increment
           ,DtNm        -- on top of which it extends
           )

dtGamL :: DtGamL -> PP_Doc
dtGamL [] = empty
dtGamL g
  = {- ltxEnvironment "flushleft"
    $ -}
      ltxEnvironment "eqnarray*"
        (ppListSep "" "" ltxNl
           [ ppListSep "" "" " & " [l2tText (n >#< "=" >#< n'), l2tText "++", l2tText g] | (n,(g,n')) <- g ]
        )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gathered VarMp's, printed under a DT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(DtVarMpL,dtVarMpL)
type DtVarMpL
  = AssocL DtNm
           PP_Doc       -- the varmp

dtVarMpL :: DtVarMpL -> PP_Doc
dtVarMpL [] = empty
dtVarMpL vm
  = {- ltxEnvironment "flushleft"
    $ -}
      ltxEnvironment "eqnarray*"
        (ppListSep "" "" ltxNl
           [ ppListSep "" "" " & " [l2tText n, l2tText "=", l2tText m] | (n,m) <- vm ]
        )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Elements used in judgements/rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtEltTy,dtEltTy')
dtEltTy' :: (Substitutable x TyVarId VarMp) => (x -> TvCatMp) -> (x -> res) -> VarMp -> VarMp -> x -> (res,VarMp)
dtEltTy' ftvmp mkres m dm t
  = (mkres (dm' |=> t'), dm')
  where t'  = m |=> t
        dm' = dtVmExtend (ftvmp t') dm

dtEltTy :: VarMp -> VarMp -> Ty -> (PP_Doc,VarMp)
dtEltTy = dtEltTy' tyFtvMp ppTyDt
%%]

%%[(99 hmtyinfer) hs export(dtEltGam,dtEltFoVarMp,dtEltVarMp)
dtEltGam :: VarMp -> VarMp -> ValGam -> (PP_Doc,VarMp)
dtEltGam m dm g
  = (ppAssocL' ppBracketsCommas' ":->" $ gamToAssocL g',dm')
  where (g',dm') = gamMapThr (\(k,i) dm -> let (ppty,dm') = dtEltTy m dm (vgiTy i) in ((hsnQualified k,ppty),dm')) dm g

dtEltFoVarMp :: VarMp -> FIOut -> PP_Doc
dtEltFoVarMp dm fo = ppVarMp ppCurlysCommas' (foVarMp fo)

dtEltVarMp :: VarMp -> VarMp -> VarMp -> (PP_Doc,VarMp)
dtEltVarMp m dm vm
  = (ppAssocL' ppBracketsCommas' ":->" [ (ppTyDt $ dm' |=> varmpinfoMkVar tv i,ppVarMpInfoDt i) | (tv,i) <- varmpToAssocL vm'], dm')
  where (vm',dm')
           = varmpMapThr (\_ tv i dm
                            -> let (i',dm2) = dtEltTy' varmpinfoFtvMp id m dm i
                                   (_ ,dm3) = dtEltTy m dm (varmpinfoMkVar tv i)
                               in  (i',dm3)
                         ) dm vm
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Choose between final/infer variant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtChooseDT)
dtChooseDT :: EHCOpts -> x -> x -> x
dtChooseDT opts finalVM inferVM = if ehcOptEmitDerivTree opts == DerivTreeWay_Final then finalVM else inferVM
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extend mapping for pretty printing ty vars (and other vars)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer) hs export(dtVmExtend)
dtVmExtend :: TvCatMp -> VarMp -> VarMp
dtVmExtend fvm dm
  = dmn |=> dm
  where sz  = varmpSize dm
        dmn = varmpUnions
              $ zipWith (\(v,i) inx
                           -> let nm i inx = mkHNm $ show i ++ "_" ++ show inx
                              in  case tvinfoPurpose i of
                                    TvPurpose_Ty _  -> varmpTyUnit    v (semCon $ nm i inx)
                                    TvPurpose_Impls -> varmpImplsUnit v (mkImplsTail $ uidFromInt inx)
                                    _               -> emptyVarMp                                           -- incomplete
                        )
                        (Map.toList $ fvm `Map.difference` varmpToMap dm) [sz ..]
%%]

