module EH101.Gam.TyKiGam
( TyKiGamInfo (..), TyKiGam, emptyTKGI
, tyKiGamLookupByTyVar
, tyKiGamLookupByName
, tyKiGamLookup
, tyKiGamLookupErr, tyKiGamLookupKi
, tyKiGamLookupByNameErr
, tyKiGamVarSingleton
, tyKiGamNameSingleton
, tyKiGamSingleton
, tvarKi
, initTyKiGam
, tkgiGetSet
, tyKiGamDoWithVarMp )
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




{-# LINE 37 "src/ehc/Gam/TyKiGam.chs" #-}
data TyKiGamInfo
  = TyKiGamInfo
      { tkgiKi :: !Ty }
      deriving Show

emptyTKGI :: TyKiGamInfo
emptyTKGI
  = TyKiGamInfo
      kiStar

type TyKiGam = Gam TyKiKey TyKiGamInfo

{-# LINE 55 "src/ehc/Gam/TyKiGam.chs" #-}
deriving instance Typeable TyKiGamInfo
deriving instance Data TyKiGamInfo

{-# LINE 60 "src/ehc/Gam/TyKiGam.chs" #-}
tkgiGetSet = (tkgiKi,(\x i -> i {tkgiKi = x}))

{-# LINE 64 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamLookupByTyVar :: TyVarId -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookupByTyVar v g = gamLookup (TyKiKey_TyVar v) g

{-# LINE 69 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamLookupByName :: HsName -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookupByName n g
  = case gamLookup (TyKiKey_Name n) g of
      Nothing
        | hsnIsProd n
            -> Just (TyKiGamInfo (replicate (hsnProdArity n) kiStar `mkArrow` kiStar))
      x     -> x

{-# LINE 83 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamLookup :: Ty -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookup t g
  = case tyMbVar t of
      Just v  -> tyKiGamLookupByTyVar v g
      Nothing ->
                 case tyMbCon t of
                   Just n -> tyKiGamLookupByName n g
                   _      -> Nothing

{-# LINE 94 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamLookupErr :: Ty -> TyKiGam -> (TyKiGamInfo,ErrL)
tyKiGamLookupErr t g
  = case tyKiGamLookup t g of
      Nothing -> (emptyTKGI,[rngLift emptyRange mkErr_NamesNotIntrod "kind" [mkHNm $ show t]])
      Just i  -> (i,[])

tyKiGamLookupKi :: TyKiGam -> Ty -> Ty
tyKiGamLookupKi g t = tkgiKi $ fst $ tyKiGamLookupErr t g

{-# LINE 105 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamLookupByNameErr :: HsName -> TyKiGam -> (TyKiGamInfo,ErrL)
tyKiGamLookupByNameErr n g = tyKiGamLookupErr (semCon n) g

{-# LINE 110 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamVarSingleton :: TyVarId -> TyKiGamInfo -> TyKiGam
tyKiGamVarSingleton v k = gamSingleton (TyKiKey_TyVar v) k

{-# LINE 115 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamNameSingleton :: HsName -> TyKiGamInfo -> TyKiGam
tyKiGamNameSingleton n k = gamSingleton (TyKiKey_Name n) k

{-# LINE 120 "src/ehc/Gam/TyKiGam.chs" #-}
tyKiGamSingleton :: Ty -> TyKiGamInfo -> TyKiGam
tyKiGamSingleton t k
  = case tyMbVar t of
      Just v  -> tyKiGamVarSingleton v k
      Nothing -> case tyMbCon t of
                   Just n -> tyKiGamNameSingleton n k
                   _      -> panic "Gam.tyKiGamSingleton"

{-# LINE 130 "src/ehc/Gam/TyKiGam.chs" #-}
-- Do something with each kind in a TyKiGam.
tyKiGamDoWithVarMp :: (TyKiKey -> (Ty,VarMp) -> VarMp -> thr -> (Ty,VarMp,thr)) -> VarMp -> thr -> TyKiGam -> (TyKiGam,VarMp,thr)
tyKiGamDoWithVarMp = gamDoTyWithVarMp tkgiGetSet

{-# LINE 146 "src/ehc/Gam/TyKiGam.chs" #-}
tvarKi :: TyKiGam -> VarMp -> VarMp -> TyVarId -> Ty
tvarKi tyKiGam tvKiVarMp _ tv
  = case tyKiGamLookup tv' tyKiGam of
      Just tkgi -> tvKiVarMp `varUpd` tkgiKi tkgi
      _         -> tvKiVarMp `varUpd` tv'
  where tv' = {- tyVarMp `varUpd` -} mkTyVar tv

{-# LINE 159 "src/ehc/Gam/TyKiGam.chs" #-}
initTyKiGam :: TyKiGam
initTyKiGam
  = gamUnions
      [ (tyKiGamNameSingleton hsnArrow      (TyKiGamInfo ([kiStar,kiStar] `mkArrow` kiStar)))
      , gamUnions
          (zipWith tyKiGamNameSingleton
               [ hsnInt, hsnChar
               , hsnInteger
               ]
               (repeat star)
          )
      , gamUnions
          (zipWith tyKiGamNameSingleton
               [ hsnIntUnboxed
               , hsnInt8Unboxed, hsnInt16Unboxed, hsnInt32Unboxed, hsnInt64Unboxed
               , hsnWordUnboxed
               , hsnWord8Unboxed, hsnWord16Unboxed, hsnWord32Unboxed, hsnWord64Unboxed
               , hsnAddrUnboxed
               ]
               (repeat unbx)
          )
      , (tyKiGamNameSingleton hsnRow        (TyKiGamInfo kiRow))
      , (tyKiGamNameSingleton hsnRec        (TyKiGamInfo ([kiRow] `mkArrow` kiStar)))
      , (tyKiGamNameSingleton hsnSum        (TyKiGamInfo ([kiRow] `mkArrow` kiStar)))
      , (tyKiGamNameSingleton hsnPrArrow    (TyKiGamInfo ([kiStar,kiStar] `mkArrow` kiStar)))
      , (tyKiGamNameSingleton hsnRecUnboxed (TyKiGamInfo ([kiRow] `mkArrow` kiUnboxed)))
      , (tyKiGamNameSingleton hsnEqTilde    (TyKiGamInfo ([kiStar,kiStar] `mkArrow` kiStar)))	-- TBD: should be polykinded, but does not matter as already rewritten to explicit equality predicate at the time this info is used
      ]
  where star = TyKiGamInfo kiStar
        unbx = TyKiGamInfo kiUnboxed

{-# LINE 218 "src/ehc/Gam/TyKiGam.chs" #-}
instance VarUpdatable TyKiGamInfo VarMp where
  s `varUpd`  tkgi         =   tkgi { tkgiKi = s `varUpd` tkgiKi tkgi }
  s `varUpdCyc` tkgi         =   substLift tkgiKi (\i x -> i {tkgiKi = x}) varUpdCyc s tkgi

instance VarExtractable TyKiGamInfo TyVarId where
  varFreeSet tkgi         =   varFreeSet (tkgiKi tkgi)

{-# LINE 227 "src/ehc/Gam/TyKiGam.chs" #-}
instance PP TyKiGamInfo where
  pp i = ppTy (tkgiKi i)

{-# LINE 240 "src/ehc/Gam/TyKiGam.chs" #-}
instance Serialize TyKiGamInfo where
  sput (TyKiGamInfo a) = sput a
  sget = liftM TyKiGamInfo sget

