module EH101.Gam.TyGam
( TyGamInfo (..)
, emptyTGI
, TyGam
, tyGamLookupErr
, mkTGIData
, mkTGI
, initTyGam
, tyGamLookup )
where
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Ty
import EH101.Ty.Pretty
import EH101.Gam
import EH101.Error
import qualified Data.Set as Set
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.Trf.Quantify
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize





{-# LINE 40 "src/ehc/Gam/TyGam.chs" #-}
data TyGamInfo
  = TyGamInfo
      { tgiTy :: !Ty
      }
      deriving Show

{-# LINE 50 "src/ehc/Gam/TyGam.chs" #-}
deriving instance Typeable TyGamInfo
deriving instance Data TyGamInfo

{-# LINE 55 "src/ehc/Gam/TyGam.chs" #-}
mkTGIData :: Ty -> Ty -> TyGamInfo
mkTGIData t _ = TyGamInfo t

{-# LINE 60 "src/ehc/Gam/TyGam.chs" #-}
mkTGI :: Ty -> TyGamInfo
mkTGI t = mkTGIData t Ty_Any

{-# LINE 65 "src/ehc/Gam/TyGam.chs" #-}
emptyTGI :: TyGamInfo
emptyTGI
  = TyGamInfo
      Ty_Any

{-# LINE 74 "src/ehc/Gam/TyGam.chs" #-}
type TyGam = Gam HsName TyGamInfo

{-# LINE 92 "src/ehc/Gam/TyGam.chs" #-}
tyGamLookupErr :: HsName -> TyGam -> (TyGamInfo,ErrL)
tyGamLookupErr n g
  = case tyGamLookup n g of
      Nothing  -> (emptyTGI,[rngLift emptyRange mkErr_NamesNotIntrod "type" [n]])
      Just tgi -> (tgi,[])

{-# LINE 115 "src/ehc/Gam/TyGam.chs" #-}
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup = gamLookup

{-# LINE 141 "src/ehc/Gam/TyGam.chs" #-}
initTyGam :: TyGam
initTyGam
  = assocLToGam
      [ (hsnArrow			, mkTGI (Ty_Con hsnArrow))
      , (hsnInt				, mkTGI tyInt)
      , (hsnChar			, mkTGI tyChar)
      , (hsnRow				, mkTGI (Ty_Con hsnUnknown))
      , (hsnRec				, mkTGI (Ty_Con hsnRec))
      , (hsnSum				, mkTGI (Ty_Con hsnSum))
      , (hsnPrArrow			, mkTGI (Ty_Con hsnPrArrow))
      , (hsnRecUnboxed		, mkTGI (Ty_Con hsnRecUnboxed))
      , (hsnIntUnboxed		, mkTGI tyIntUnboxed)
      , (hsnEqTilde			, mkTGI (Ty_Con hsnEqTilde))
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
      , (hsnAddrUnboxed		, mkTGI (Ty_Con hsnAddrUnboxed  )	)
      ]

{-# LINE 212 "src/ehc/Gam/TyGam.chs" #-}
instance VarUpdatable TyGamInfo VarMp where
  s `varUpd`  tgi         =   tgi { tgiTy = s `varUpd` tgiTy tgi }
  s `varUpdCyc` tgi         =   substLift tgiTy (\i x -> i {tgiTy = x}) varUpdCyc s tgi

instance VarExtractable TyGamInfo TyVarId where
  varFreeSet tgi         =   varFreeSet (tgiTy tgi)

{-# LINE 223 "src/ehc/Gam/TyGam.chs" #-}
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi)

{-# LINE 236 "src/ehc/Gam/TyGam.chs" #-}
instance Serialize TyGamInfo where
  sput (TyGamInfo a) = sput a
  sget = liftM TyGamInfo sget
