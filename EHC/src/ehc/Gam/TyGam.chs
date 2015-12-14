%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam.TyGam}
%%]

%%[1 import(UHC.Util.Pretty,UHC.Util.Utils)
%%]

%%[1 hs import ({%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Base.HsName.Builtin})
%%]
%%[(1 hmtyast || hmtyinfer) hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[1 hs import ({%{EH}Gam})
%%]
%%[1 hs import({%{EH}Error}) 
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(3 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[(8 counting) hs import(qualified {%{EH}CountingAnalysis} as CA) 
%%]

%%[(50 hmtyinfer || hmtyast) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Type of type" and "Kind of type" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.TyGamInfo export(TyGamInfo(..))
-- If this changes, also change {%{EH}ConfigInternalVersions}
data TyGamInfo
  = TyGamInfo 
      {
%%[[(1 hmtyinfer || hmtyast)
      tgiTy :: !Ty
%%]]
%%[[(8 counting)
      , tgiAnnVars      :: ![CA.Var] 
      , tgiAnnTy        :: !CA.AnnotatedType
%%]]
      }
      deriving (Show, Generic)
%%]

%%[(50 hmtyinfer)
deriving instance Typeable TyGamInfo
%%]

%%[(6 hmtyinfer || hmtyast).mkTGIData export(mkTGIData)
mkTGIData :: Ty -> Ty
%%[[(8 counting)
          -> [CA.Var]
          -> CA.AnnotatedType
%%]] 
          -> TyGamInfo
mkTGIData t _ 
%%[[(8 counting)
  annVars annTy
%%]]
  = TyGamInfo t
%%[[(8 counting)
      annVars annTy
%%]]
%%]

%%[(6 hmtyinfer || hmtyast) export(mkTGI)
mkTGI :: Ty
%%[[(8 counting)
          -> [CA.Var]
          -> CA.AnnotatedType
%%]] 
          -> TyGamInfo
mkTGI t 
%%[[(8 counting)
  annVars annTy
%%]]
  = mkTGIData t Ty_Any
%%[[(8 counting)
      annVars annTy
%%]]
%%]

-- %%[(6 hmtyinfer || hmtyast) export(mkTGI)
-- mkTGI :: Ty
-- %%[[(8 counting)
--           -> [CA.Var]
--           -> CA.AnnotatedType
-- %%]] 
--           -> TyGamInfo
-- mkTGI t 
-- %%[[(8 counting)
--   annVars annTy
-- %%]]
--   = mkTGIData t Ty_Any
-- %%[[(8 counting)
--       annVars annTy
-- %%]]
-- %%]

%%[1.emptyTGI export(emptyTGI)
emptyTGI :: TyGamInfo
emptyTGI
  = TyGamInfo
%%[[(1 hmtyinfer || hmtyast)
      Ty_Any
%%]]
%%[[(8 counting)
      [] (CA.AnnotatedType_TyError "emptyTGI")
%%]]
%%]

%%[1.TyGam export(TyGam)
type TyGam = Gam HsName TyGamInfo
%%]

%%[1.tyGamLookup export(tyGamLookup)
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing | hsnIsProd nm 
%%[[(1 hmtyinfer || hmtyast) 
                 -> Just (TyGamInfo (appCon nm))
%%][1
                 -> Just emptyTGI
%%]]
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[1 export(tyGamLookupErr)
tyGamLookupErr :: HsName -> TyGam -> (TyGamInfo,ErrL)
tyGamLookupErr n g
  = case tyGamLookup n g of
      Nothing  -> (emptyTGI,[rngLift emptyRange mkErr_NamesNotIntrod "type" [n]])
      Just tgi -> (tgi,[])
%%]

%%[6.tyGamLookup -1.tyGamLookup export(tyGamLookup)
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
%%[[(6 hmtyinfer || hmtyast) 
                 -> Just (TyGamInfo (appCon nm))
%%][6
                 -> Just emptyTGI
%%]]
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[7.tyGamLookup -6.tyGamLookup export(tyGamLookup)
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup = gamLookup
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of tyGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.initTyGam export(initTyGam)
initTyGam :: TyGam
initTyGam
  = assocLToGam
%%[[(1 hmtyinfer || hmtyast)
      [ (hsnArrow,  TyGamInfo (appCon hsnArrow))
      , (hsnInt,    TyGamInfo tyInt)
      , (hsnChar,   TyGamInfo tyChar)
      ]
%%][1
      [ (hsnArrow,  emptyTGI)
      , (hsnInt,    emptyTGI)
      , (hsnChar,   emptyTGI)
      ]
%%]]
%%]

%%[6.initTyGam -1.initTyGam export(initTyGam)
initTyGam :: TyGam
initTyGam
  = assocLToGam
%%[[(6 hmtyinfer || hmtyast)
      [ (hsnArrow			, mkTGI (appCon hsnArrow)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnArrow")
%%]]
          )
      , (hsnInt				, mkTGI tyInt
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInt")
%%]]
          )
      , (hsnChar			, mkTGI tyChar
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnChar")
%%]]
          )
%%[[7
      , (hsnRow				, mkTGI (appCon hsnUnknown)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnRow")
%%]]
          )
      , (hsnRec				, mkTGI (appCon hsnRec)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnRec")
%%]]
          )
      , (hsnSum				, mkTGI (appCon hsnSum)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnSum")
%%]]
          )
%%]]
%%[[9
      , (hsnPrArrow			, mkTGI (appCon hsnPrArrow)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnPrArrow")
%%]]
          )
%%]]
%%[[18
      , (hsnRecUnboxed		, mkTGI (appCon hsnRecUnboxed)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnRecUnboxed")
%%]]
          )
      , (hsnIntUnboxed		, mkTGI tyIntUnboxed
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnIntUnboxed")
%%]]
          )
%%]]
%%[[31
      , (hsnEqTilde			, mkTGI (appCon hsnEqTilde)
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnEqTilde")
%%]]
          )
%%]]
%%[[97
      , (hsnInteger			, mkTGI tyInteger		
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInteger")
%%]]
          )
%%]]
%%[[9797
      , (hsnInt8Unboxed  	, mkTGI (appCon hsnInt8Unboxed  )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInt8Unboxed")
%%]]
          )
      , (hsnInt16Unboxed 	, mkTGI (appCon hsnInt16Unboxed )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInt16Unboxed")
%%]]
          )
      , (hsnInt32Unboxed 	, mkTGI (appCon hsnInt32Unboxed )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInt32Unboxed")
%%]]
          )
      , (hsnInt64Unboxed 	, mkTGI (appCon hsnInt64Unboxed )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnInt64Unboxed")
%%]]
          )
      , (hsnWordUnboxed  	, mkTGI (appCon hsnWordUnboxed  )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnWordUnboxed")
%%]]
          )
      , (hsnWord8Unboxed 	, mkTGI (appCon hsnWord8Unboxed )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnWord8Unboxed")
%%]]
          )
      , (hsnWord16Unboxed	, mkTGI (appCon hsnWord16Unboxed)	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnWord16Unboxed")
%%]]
          )
      , (hsnWord32Unboxed	, mkTGI (appCon hsnWord32Unboxed)	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnWord32Unboxed")
%%]]
          )
      , (hsnWord64Unboxed	, mkTGI (appCon hsnWord64Unboxed)	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnWord64Unboxed")
%%]]
          )
%%]]
%%[[99
      , (hsnAddrUnboxed		, mkTGI (appCon hsnAddrUnboxed  )	
%%[[(8 counting)
                                                                            [] (CA.AnnotatedType_TyError "hsnAddrUnboxed")
%%]]
          )
%%]]  
      ]
%%][6
      $
      zip [ hsnArrow, hsnInt, hsnChar
%%[[7
          , hsnRow, hsnRec, hsnSum
%%]]
%%[[9
          , hsnPrArrow
%%]]
%%[[18
          , hsnIntUnboxed, hsnRecUnboxed
%%]]
%%[[31
          , hsnEqTilde
%%]]
%%[[97
          , hsnInteger
%%]]
%%[[9797
          , tyInt8Unboxed, tyInt16Unboxed, tyInt32Unboxed, tyInt64Unboxed
          , tyWordUnboxed
          , tyWord8Unboxed, tyWord16Unboxed, tyWord32Unboxed, tyWord64Unboxed
%%]]
%%[[99
          , hsnAddrUnboxed
%%]]
          ]
          (repeat emptyTGI)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable.inst.TyGamInfo
instance VarUpdatable TyGamInfo VarMp where
  s `varUpd`  tgi         =   tgi { tgiTy = s `varUpd` tgiTy tgi }
%%[[4
  s `varUpdCyc` tgi         =   substLift tgiTy (\i x -> i {tgiTy = x}) varUpdCyc s tgi
%%]]

type instance ExtrValVarKey TyGamInfo = TyVarId

instance VarExtractable TyGamInfo where
  varFreeSet tgi         =   varFreeSet (tgiTy tgi)
%%]

%%[(1 hmtyinfer || hmtyast).PP.TyGamInfo
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi)
%%]

%%[(9999 hmtyinfer || hmtyast)
instance ForceEval TyGamInfo where
  forceEval x@(TyGamInfo t) | forceEval t `seq` True = x
%%[[102
  fevCount (TyGamInfo x) = cm1 "TyGamInfo" `cmUnion` fevCount x
%%]]
%%]

%%[(50 hmtyinfer || hmtyast)
instance Serialize TyGamInfo where
  -- sput (TyGamInfo a) = sput a
  -- sget = liftM TyGamInfo sget
%%]
