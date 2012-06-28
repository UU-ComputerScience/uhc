module EH101.Gam.ValGam
( ValGamInfo (..), ValGam
, valGamLookupTy
, valGamMapTy
, valGamLookup
, valGamTyOfDataCon
, valGamTyOfDataFld
, vgiGetSet
, valGamLookupTyDefault
, valGamDoWithVarMp )
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





{-# LINE 40 "src/ehc/Gam/ValGam.chs" #-}
data ValGamInfo
  = ValGamInfo
      { vgiTy :: Ty }		-- strictness has negative mem usage effect. Why??
      deriving Show

type ValGam = Gam HsName ValGamInfo

{-# LINE 51 "src/ehc/Gam/ValGam.chs" #-}
deriving instance Typeable ValGamInfo
deriving instance Data ValGamInfo

{-# LINE 56 "src/ehc/Gam/ValGam.chs" #-}
vgiGetSet = (vgiTy,(\x i -> i {vgiTy = x}))

{-# LINE 65 "src/ehc/Gam/ValGam.chs" #-}
valGamLookupTy :: HsName -> ValGam -> (Ty,ErrL)
valGamLookupTy n g
  =  case valGamLookup n g of
       Nothing    ->  (Ty_Any,[rngLift emptyRange mkErr_NamesNotIntrod "value" [n]])
       Just vgi   ->  (vgiTy vgi,[])

{-# LINE 73 "src/ehc/Gam/ValGam.chs" #-}
-- | lookup Ty in ValGam, defaulting to Ty_Any
valGamLookupTyDefault :: HsName -> ValGam -> Ty
valGamLookupTyDefault n g = maybe (Ty_Dbg $ "valGamLookupTyDefault: " ++ show n) vgiTy $ valGamLookup n g

{-# LINE 79 "src/ehc/Gam/ValGam.chs" #-}
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> let pr = mkPr nm in mkRes (tyProdArgs pr `mkArrow` pr)
         |  hsnIsUn nm && hsnIsProd (hsnUnUn nm)
                 -> let pr = mkPr (hsnUnUn nm) in mkRes ([pr] `mkArrow` pr)
         where  mkPr nm  = mkTyFreshProd (hsnProdArity nm)
                mkRes t  = Just (ValGamInfo (tyQuantifyClosed t))
       Just vgi  -> Just vgi
       _         -> Nothing

{-# LINE 101 "src/ehc/Gam/ValGam.chs" #-}
valGamMapTy :: (Ty -> Ty) -> ValGam -> ValGam
valGamMapTy f = gamMapElts (\vgi -> vgi {vgiTy = f (vgiTy vgi)})

{-# LINE 106 "src/ehc/Gam/ValGam.chs" #-}
-- Do something with each ty in a ValGam.
valGamDoWithVarMp :: (HsName -> (Ty,VarMp) -> VarMp -> thr -> (Ty,VarMp,thr)) -> VarMp -> thr -> ValGam -> (ValGam,VarMp,thr)
valGamDoWithVarMp = gamDoTyWithVarMp vgiGetSet

{-# LINE 117 "src/ehc/Gam/ValGam.chs" #-}
valGamTyOfDataCon :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataCon conNm g
  = (t,rt,e)
  where (t,e) = valGamLookupTy conNm g
        (_,rt) = tyArrowArgsRes t

{-# LINE 125 "src/ehc/Gam/ValGam.chs" #-}
valGamTyOfDataFld :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataFld fldNm g
  | null e    = (t,rt,e)
  | otherwise = (t,Ty_Any,e)
  where (t,e) = valGamLookupTy fldNm g
        ((rt:_),_) = tyArrowArgsRes t


{-# LINE 144 "src/ehc/Gam/ValGam.chs" #-}
instance VarUpdatable ValGamInfo VarMp where
  s `varUpd`  vgi         =   vgi { vgiTy = s `varUpd` vgiTy vgi }
  s `varUpdCyc` vgi         =   substLift vgiTy (\i x -> i {vgiTy = x}) varUpdCyc s vgi

instance VarExtractable ValGamInfo TyVarId where
  varFreeSet vgi         =   varFreeSet (vgiTy vgi)

{-# LINE 155 "src/ehc/Gam/ValGam.chs" #-}
instance PP ValGamInfo where
  pp vgi = ppTy (vgiTy vgi)

{-# LINE 168 "src/ehc/Gam/ValGam.chs" #-}
instance Serialize ValGamInfo where
  sput (ValGamInfo a) = sput a
  sget = liftM ValGamInfo sget

