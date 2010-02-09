%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam.TyGam}
%%]

%%[1 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[1 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[1 hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
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

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Type of type" and "Kind of type" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.TyGamInfo export(TyGamInfo(..))
data TyGamInfo
  = TyGamInfo
%%[[(1 hmtyinfer || hmtyast)
      { tgiTy :: !Ty
      }
%%]]
      deriving Show
%%]

%%[(6 hmtyinfer || hmtyast).mkTGIData export(mkTGIData)
mkTGIData :: Ty -> Ty -> TyGamInfo
mkTGIData t _ = TyGamInfo t
%%]

%%[(6 hmtyinfer || hmtyast) export(mkTGI)
mkTGI :: Ty -> TyGamInfo
mkTGI t = mkTGIData t Ty_Any
%%]

%%[1.emtpyTGI export(emtpyTGI)
emtpyTGI :: TyGamInfo
emtpyTGI
  = TyGamInfo
%%[[(1 hmtyinfer || hmtyast)
      Ty_Any
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
                 -> Just (TyGamInfo (Ty_Con nm))
%%][1
                 -> Just emtpyTGI
%%]]
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[1 export(tyGamLookupErr)
tyGamLookupErr :: HsName -> TyGam -> (TyGamInfo,ErrL)
tyGamLookupErr n g
  = case tyGamLookup n g of
      Nothing  -> (emtpyTGI,[rngLift emptyRange mkErr_NamesNotIntrod "type" [n]])
      Just tgi -> (tgi,[])
%%]

%%[6.tyGamLookup -1.tyGamLookup export(tyGamLookup)
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
%%[[(6 hmtyinfer || hmtyast) 
                 -> Just (TyGamInfo (Ty_Con nm))
%%][6
                 -> Just emtpyTGI
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
      [ (hsnArrow,  TyGamInfo (Ty_Con hsnArrow))
      , (hsnInt,    TyGamInfo tyInt)
      , (hsnChar,   TyGamInfo tyChar)
      ]
%%][1
      [ (hsnArrow,  emtpyTGI)
      , (hsnInt,    emtpyTGI)
      , (hsnChar,   emtpyTGI)
      ]
%%]]
%%]

%%[6.initTyGam -1.initTyGam export(initTyGam)
initTyGam :: TyGam
initTyGam
  = assocLToGam
%%[[(6 hmtyinfer || hmtyast)
      [ (hsnArrow			, mkTGI (Ty_Con hsnArrow))
      , (hsnInt				, mkTGI tyInt)
      , (hsnChar			, mkTGI tyChar)
%%[[7
      , (hsnRow				, mkTGI (Ty_Con hsnUnknown))
      , (hsnRec				, mkTGI (Ty_Con hsnRec))
      , (hsnSum				, mkTGI (Ty_Con hsnSum))
%%]]
%%[[9
      , (hsnPrArrow			, mkTGI (Ty_Con hsnPrArrow))
%%]]
%%[[18
      , (hsnRecUnboxed		, mkTGI (Ty_Con hsnRecUnboxed))
      , (hsnIntUnboxed		, mkTGI tyIntUnboxed)
%%]]
%%[[97
      , (hsnInteger			, mkTGI tyInteger		)
      , (hsnInt8Unboxed  	, mkTGI (Ty_Con hsnInt8Unboxed  )	)
      , (hsnInt16Unboxed 	, mkTGI (Ty_Con hsnInt16Unboxed )	)
      , (hsnInt32Unboxed 	, mkTGI (Ty_Con hsnInt32Unboxed )	)
      , (hsnInt64Unboxed 	, mkTGI (Ty_Con hsnInt64Unboxed )	)
      , (hsnWordUnboxed  	, mkTGI (Ty_Con hsnWordUnboxed  )	)
      , (hsnWord8Unboxed 	, mkTGI (Ty_Con hsnWord8Unboxed )	)
      , (hsnWord16Unboxed	, mkTGI (Ty_Con hsnWord16Unboxed)	)
      , (hsnWord32Unboxed	, mkTGI (Ty_Con hsnWord32Unboxed)	)
      , (hsnWord64Unboxed	, mkTGI (Ty_Con hsnWord64Unboxed)	)
%%]]
%%[[99
      , (hsnAddrUnboxed		, mkTGI (Ty_Con hsnAddrUnboxed  )	)
%%]]  
      ]
%%][6
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
%%[[97
          , hsnInteger
          , tyInt8Unboxed, tyInt16Unboxed, tyInt32Unboxed, tyInt64Unboxed
          , tyWordUnboxed
          , tyWord8Unboxed, tyWord16Unboxed, tyWord32Unboxed, tyWord64Unboxed
%%]]
%%[[99
          , hsnAddrUnboxed
%%]]
          ]
          (repeat emtpyTGI)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable.inst.TyGamInfo
instance Substitutable TyGamInfo TyVarId VarMp where
  s |=>  tgi         =   tgi { tgiTy = s |=> tgiTy tgi }
%%[[4
  s |==> tgi         =   substLift tgiTy (\i x -> i {tgiTy = x}) (|==>) s tgi
%%]]
  ftvSet tgi         =   ftvSet (tgiTy tgi)
%%]

%%[(1 hmtyinfer || hmtyast).PP.TyGamInfo
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi)
%%]

%%[(99 hmtyinfer || hmtyast)
instance ForceEval TyGamInfo where
  forceEval x@(TyGamInfo t) | forceEval t `seq` True = x
%%[[102
  fevCount (TyGamInfo x) = cm1 "TyGamInfo" `cmUnion` fevCount x
%%]]
%%]

