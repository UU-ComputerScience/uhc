module EH101.Gam.DataGam
( DataFldMp, DataFldInfo (..), emptyDataFldInfo
, DataConFldAnnInfo (..), emptyDataConFldAnnInfo
, DataTagInfo (..), emptyDataTagInfo, DataConstrTagMp
, DataGamInfo (..)
, DataGam
, mkDGI
, mkDGIPlain
, emptyDataGamInfo, emptyDGI
, dgiDtiOfCon
, dataGamLookup, dataGamLookupErr
, dataGamDgiOfTy
, dtiOffsetOfFld
, DataFldInConstr (..), DataFldInConstrMp
, dataGamDTIsOfTy
, dataGamTagsOfTy
, dataGamLookupTag
, dataGamTagLookup
, dgiIsEnumable
, dgiConstrTagAssocL
, DataGamInfoVariant (..)
, dgiMbNewtype, dgiIsNewtype
, dgiIsRec )
where
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Ty
import EH101.Ty.Pretty
import EH101.Gam
import EH101.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.Trf.Quantify
import EH101.Base.Debug
import EH.Util.Pretty
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Maybe








{-# LINE 48 "src/ehc/Gam/DataGam.chs" #-}
-- | per named field info
data DataFldInfo
  = DataFldInfo
      { dfiOffset 	:: !Int
      }
      deriving Show

type DataFldMp = Map.Map HsName DataFldInfo

emptyDataFldInfo
  = DataFldInfo
      (-1)

{-# LINE 67 "src/ehc/Gam/DataGam.chs" #-}
-- | per positional constructor field annotation like info
data DataConFldAnnInfo
  = DataConFldAnnInfo
      { dcfaiStrictness		:: !Strictness
      }
      deriving Show

emptyDataConFldAnnInfo :: DataConFldAnnInfo
emptyDataConFldAnnInfo
  = DataConFldAnnInfo
      Strictness_NonStrict

{-# LINE 85 "src/ehc/Gam/DataGam.chs" #-}
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    		:: !DataFldMp				-- map of field names to offset
      , dtiFldTyL			:: !FldTyL					-- association list of maybe a field name with types
      , dtiConFldAnnL		:: ![DataConFldAnnInfo]		-- per constructor field (with or without name) annotation info
      , dtiConNm			:: !HsName					-- constructor name (duplicate of key of gamma leading to this info)
      , dtiConTy			:: !Ty						-- type of constructor, without final tyVarMp applied
      , dtiCTag 			:: !CTag					-- tag of constructor
      , dtiMbFixityPrio 	:: !(Maybe (Int,Fixity))	-- if defined as infix, with priority
      } deriving Show

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty [] [] hsnUnknown Ty_Any
      emptyCTag
      Nothing

{-# LINE 114 "src/ehc/Gam/DataGam.chs" #-}
dtiOffsetOfFld :: HsName -> DataTagInfo -> Int
dtiOffsetOfFld fldNm dti = dfiOffset $ panicJust "dtiOffsetOfFld" $ Map.lookup fldNm $ dtiFldMp dti

{-# LINE 119 "src/ehc/Gam/DataGam.chs" #-}
data DataFldInConstr
  = DataFldInConstr
      { dficInTagMp	:: !(Map.Map CTag Int)
      }

type DataFldInConstrMp = Map.Map HsName DataFldInConstr

{-# LINE 128 "src/ehc/Gam/DataGam.chs" #-}
-- | specific info about what a DataGamInfo encodes
data DataGamInfoVariant
  = DataGamInfoVariant_Plain		-- plain data type
  | DataGamInfoVariant_Newtype		-- newtype variation
      Ty							-- the type lambda corresponding to a newtype
  | DataGamInfoVariant_Rec			-- tuple, record
  deriving Eq

{-# LINE 140 "src/ehc/Gam/DataGam.chs" #-}

data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: !HsName				-- type name (duplicate of key of gamma leading to this info)
      , dgiDataTy 			:: !Ty					-- the type sum of product
      , dgiConstrNmL 		:: ![HsName]			-- all constructor names
      , dgiConstrTagMp 		:: !DataConstrTagMp		-- per constructor info
      , dgiFldInConstrMp	:: !DataFldInConstrMp	-- map from field name to all constructors having the field
      , dgiVariant 			:: !DataGamInfoVariant
      , dgiMaxConstrArity   :: !Int
      , dgiMbGenerInfo		:: !(Maybe Int)			-- max kind arity for generic behavior, currently \in {0,1}
      }

instance Show DataGamInfo where
  show _ = "DataGamInfo"

{-# LINE 168 "src/ehc/Gam/DataGam.chs" #-}
dgiMbNewtype :: DataGamInfo -> Maybe Ty
dgiMbNewtype (DataGamInfo {dgiVariant = DataGamInfoVariant_Newtype t}) = Just t
dgiMbNewtype _                                                         = Nothing

dgiIsNewtype :: DataGamInfo -> Bool
dgiIsNewtype = isJust . dgiMbNewtype

{-# LINE 177 "src/ehc/Gam/DataGam.chs" #-}
dgiIsRec :: DataGamInfo -> Bool
dgiIsRec dgi = dgiVariant dgi == DataGamInfoVariant_Rec

{-# LINE 182 "src/ehc/Gam/DataGam.chs" #-}
type DataGam = Gam HsName DataGamInfo

{-# LINE 186 "src/ehc/Gam/DataGam.chs" #-}
mkDGI
  :: HsName
     -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfoVariant
     -> Maybe Int
     -> DataGamInfo
mkDGI tyNm dty cNmL m nt
      mbGener
  = DataGamInfo
      tyNm
      dty
      cNmL
      m
      fm nt mx
      mbGener
  where fm = Map.map DataFldInConstr $ Map.unionsWith Map.union
             $ [ Map.singleton f (Map.singleton (dtiCTag ci) (dfiOffset fi)) | ci <- Map.elems m, (f,fi) <- Map.toList $ dtiFldMp ci ]
        mx = if Map.null m then (-1) else (ctagMaxArity $ dtiCTag $ head $ Map.elems m)

{-# LINE 222 "src/ehc/Gam/DataGam.chs" #-}
mkDGIPlain :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfo
mkDGIPlain tyNm dty cNmL m
  = mkDGI tyNm dty cNmL m
          DataGamInfoVariant_Plain
          Nothing


{-# LINE 237 "src/ehc/Gam/DataGam.chs" #-}
emptyDataGamInfo, emptyDGI :: DataGamInfo
emptyDataGamInfo = mkDGIPlain hsnUnknown Ty_Any [] Map.empty
emptyDGI = emptyDataGamInfo

{-# LINE 243 "src/ehc/Gam/DataGam.chs" #-}
dgiConstrTagAssocL :: DataGamInfo -> AssocL HsName DataTagInfo
dgiConstrTagAssocL dgi = [ (cn,panicJust "dgiConstrTagAssocL" $ Map.lookup cn $ dgiConstrTagMp dgi) | cn <- dgiConstrNmL dgi ]

{-# LINE 248 "src/ehc/Gam/DataGam.chs" #-}
dgiDtiOfCon :: HsName -> DataGamInfo -> DataTagInfo
dgiDtiOfCon conNm dgi = panicJust "dgiDtiOfCon" $ Map.lookup conNm $ dgiConstrTagMp dgi

{-# LINE 253 "src/ehc/Gam/DataGam.chs" #-}
dataGamLookup :: HsName -> DataGam -> Maybe DataGamInfo
dataGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm							-- ??? should not be necessary, in variant 7 where tuples are represented by records
                 -> Just emptyDataGamInfo
       Just dgi  -> Just dgi
       _         -> Nothing

dataGamLookupErr :: HsName -> DataGam -> (DataGamInfo,ErrL)
dataGamLookupErr n g
  = case dataGamLookup n g of
      Nothing  -> (emptyDGI,[rngLift emptyRange mkErr_NamesNotIntrod "data" [n]])
      Just tgi -> (tgi,[])

{-# LINE 270 "src/ehc/Gam/DataGam.chs" #-}
dataGamDgiOfTy :: Ty -> DataGam -> Maybe DataGamInfo
dataGamDgiOfTy conTy dg = dataGamLookup (tyAppFunConNm conTy) dg

{-# LINE 275 "src/ehc/Gam/DataGam.chs" #-}
dataGamDTIsOfTy :: Ty -> DataGam -> Maybe [DataTagInfo]
dataGamDTIsOfTy t g
  = -- tr "dataGamDTIsOfTy" (t >#< tyAppFunConNm (tyArrowRes t)) $
    fmap
      (assocLElts . dgiConstrTagAssocL)
    $ gamLookup (tyAppFunConNm $ tyArrowRes t)
    $ g

{-# LINE 289 "src/ehc/Gam/DataGam.chs" #-}
dataGamTagsOfTy :: Ty -> DataGam -> Maybe [CTag]
dataGamTagsOfTy t g
  = fmap (map dtiCTag) (dataGamDTIsOfTy t g)

{-# LINE 295 "src/ehc/Gam/DataGam.chs" #-}
dataGamLookupTag :: HsName -> HsName -> DataGam -> Maybe CTag
dataGamLookupTag t c g
  = do dgi <- dataGamLookup t g
       dti <- Map.lookup c $ dgiConstrTagMp dgi
       return $ dtiCTag dti

{-# LINE 303 "src/ehc/Gam/DataGam.chs" #-}
dataGamTagLookup :: CTag -> DataGam -> Maybe (DataGamInfo,DataTagInfo)
dataGamTagLookup CTagRec g
  = Nothing
dataGamTagLookup ct g
  = do dgi <- dataGamLookup (ctagTyNm ct) g
       dti <- Map.lookup (ctagNm ct) $ dgiConstrTagMp dgi
       return (dgi,dti)

{-# LINE 315 "src/ehc/Gam/DataGam.chs" #-}
dgiIsEnumable :: DataGamInfo -> Bool
dgiIsEnumable dgi = dgiMaxConstrArity dgi == 0

{-# LINE 324 "src/ehc/Gam/DataGam.chs" #-}
deriving instance Typeable DataFldInfo
deriving instance Data DataFldInfo

deriving instance Typeable DataConFldAnnInfo
deriving instance Data DataConFldAnnInfo

deriving instance Typeable DataTagInfo
deriving instance Data DataTagInfo

deriving instance Typeable DataFldInConstr
deriving instance Data DataFldInConstr

deriving instance Typeable DataGamInfo
deriving instance Data DataGamInfo

{-# LINE 341 "src/ehc/Gam/DataGam.chs" #-}
deriving instance Typeable DataGamInfoVariant
deriving instance Data DataGamInfoVariant

{-# LINE 346 "src/ehc/Gam/DataGam.chs" #-}
instance Serialize DataGamInfoVariant where
  sput (DataGamInfoVariant_Plain    ) = sputWord8 0
  sput (DataGamInfoVariant_Newtype a) = sputWord8 1 >> sput a
  sput (DataGamInfoVariant_Rec      ) = sputWord8 2
  sget = do
    t <- sgetWord8
    case t of
      0 -> return DataGamInfoVariant_Plain
      1 -> liftM  DataGamInfoVariant_Newtype sget
      2 -> return DataGamInfoVariant_Rec

{-# LINE 363 "src/ehc/Gam/DataGam.chs" #-}
instance Serialize DataFldInfo where
  sput (DataFldInfo a) = sput a
  sget = liftM DataFldInfo sget

instance Serialize DataConFldAnnInfo where
  sput (DataConFldAnnInfo a) = sput a
  sget = liftM DataConFldAnnInfo sget

instance Serialize DataTagInfo where
  sput (DataTagInfo a b c d e f g) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g
  sget = liftM7 DataTagInfo sget sget sget sget sget sget sget

instance Serialize DataFldInConstr where
  sput (DataFldInConstr a) = sput a
  sget = liftM DataFldInConstr sget

instance Serialize DataGamInfo where
  sput (DataGamInfo a b c d e f g h) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h
  sget = liftM8 DataGamInfo sget sget sget sget sget sget sget sget
