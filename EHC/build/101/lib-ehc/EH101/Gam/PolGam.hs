module EH101.Gam.PolGam
( PolGamInfo (..), PolGam, mapPolGam, mkPGI
, polGamLookup, polGamLookupErr
, initPolGam )
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
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize




{-# LINE 37 "src/ehc/Gam/PolGam.chs" #-}
data PolGamInfo = PolGamInfo { pgiPol :: Polarity } deriving Show

mkPGI :: Ty -> PolGamInfo
mkPGI t = PolGamInfo t

emptyPGI :: PolGamInfo
emptyPGI = mkPGI Ty_Any

type PolGam = Gam HsName PolGamInfo


mapPolGam :: (Ty -> Ty) -> PolGam -> PolGam
mapPolGam f
  = fst . gamMapThr (\(nm, PolGamInfo ty) thr -> ((nm, PolGamInfo $ f ty), thr)) ()

{-# LINE 54 "src/ehc/Gam/PolGam.chs" #-}
deriving instance Typeable PolGamInfo
deriving instance Data PolGamInfo

{-# LINE 59 "src/ehc/Gam/PolGam.chs" #-}
polGamLookup :: HsName -> PolGam -> Maybe PolGamInfo
polGamLookup = gamLookup

polGamLookupErr :: HsName -> PolGam -> (PolGamInfo,ErrL)
polGamLookupErr n g
  = case polGamLookup n g of
      Nothing  -> (emptyPGI,[rngLift emptyRange mkErr_NamesNotIntrod "polarity" [n]])
      Just i   -> (i,[])

{-# LINE 74 "src/ehc/Gam/PolGam.chs" #-}
initPolGam :: PolGam
initPolGam
  = assocLToGam
      [ (hsnArrow       	, mkPGI $ quant $ [mkPolNegate var, var] `mkArrow` var)
      , (hsnInt         	, mkPGI   quantvar)
      , (hsnChar        	, mkPGI   quantvar)
      , (hsnRec         	, mkPGI $ quant $ [var] `mkArrow` var)
      , (hsnRecUnboxed  	, mkPGI $ quant $ [var] `mkArrow` var)
      , (hsnIntUnboxed  	, mkPGI quantvar)
      , (hsnInteger     	, mkPGI quantvar)
      , (hsnInt8Unboxed  	, mkPGI quantvar)
      , (hsnInt16Unboxed  	, mkPGI quantvar)
      , (hsnInt32Unboxed  	, mkPGI quantvar)
      , (hsnInt64Unboxed  	, mkPGI quantvar)
      , (hsnWordUnboxed  	, mkPGI quantvar)
      , (hsnWord8Unboxed  	, mkPGI quantvar)
      , (hsnWord16Unboxed  	, mkPGI quantvar)
      , (hsnWord32Unboxed  	, mkPGI quantvar)
      , (hsnWord64Unboxed  	, mkPGI quantvar)
      , (hsnAddrUnboxed  	, mkPGI quantvar)
      ]
  where
    u     = uidStart
    quant = mkTyQu tyQu_Forall [(u,kiStar)]	-- TBD
    var   = mkPolVar u
    quantvar = quant var

{-# LINE 113 "src/ehc/Gam/PolGam.chs" #-}
instance VarUpdatable PolGamInfo VarMp where
  s `varUpd` pgi  = pgi { pgiPol = s `varUpd` pgiPol pgi }
  s `varUpdCyc` pgi = substLift pgiPol (\i x -> i {pgiPol = x}) varUpdCyc s pgi

instance VarExtractable PolGamInfo TyVarId where
  varFreeSet pgi = varFreeSet (pgiPol pgi)

{-# LINE 122 "src/ehc/Gam/PolGam.chs" #-}
instance PP PolGamInfo where
  pp i = ppTy (pgiPol i)

{-# LINE 135 "src/ehc/Gam/PolGam.chs" #-}
instance Serialize PolGamInfo where
  sput (PolGamInfo a) = sput a
  sget = liftM PolGamInfo sget

