%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 module {%{EH}Gam.DataGam}
%%]

%%[7 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[7 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[7 hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[7 hs import ({%{EH}Gam})
%%]
%%[7 hs import({%{EH}Error}) 
%%]

%%[(7 hmtyinfer || hmtyast) import(qualified Data.Map as Map)
%%]
%%[(7 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]
%%[(90 hmtyinfer || hmtyast) import(Data.Maybe)
%%]

%%[(7 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(7 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[(50 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

-- debug
%%[(8 codegen) import({%{EH}Base.Debug},EH.Util.Pretty)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data tag/etc info gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(7 hmtyinfer) export(DataFldMp,DataFldInfo(..),emptyDataFldInfo)
-- | per named field info
data DataFldInfo
  = DataFldInfo
%%[[8
      { dfiOffset 	:: !Int
      }
%%]]
      deriving Show

type DataFldMp = Map.Map HsName DataFldInfo

emptyDataFldInfo
  = DataFldInfo
%%[[8
      (-1)
%%]]
%%]

%%[(7 hmtyinfer) export(DataConFldAnnInfo(..),emptyDataConFldAnnInfo)
-- | per positional constructor field annotation like info
data DataConFldAnnInfo
  = DataConFldAnnInfo
%%[[(8 codegen)
      { dcfaiStrictness		:: !Strictness
      }
%%]]
      deriving Show

emptyDataConFldAnnInfo :: DataConFldAnnInfo
emptyDataConFldAnnInfo
  = DataConFldAnnInfo
%%[[(8 codegen)
      Strictness_NonStrict
%%]]
%%]

%%[(7 hmtyinfer) export(DataTagInfo(..),emptyDataTagInfo,DataConstrTagMp)
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    		:: !DataFldMp				-- map of field names to offset
      , dtiFldTyL			:: !FldTyL					-- association list of maybe a field name with types
      , dtiConFldAnnL		:: ![DataConFldAnnInfo]		-- per constructor field (with or without name) annotation info
      , dtiConNm			:: !HsName					-- constructor name (duplicate of key of gamma leading to this info)
      , dtiConTy			:: !Ty						-- type of constructor, without final tyVarMp applied
%%[[8
      , dtiCTag 			:: !CTag					-- tag of constructor
%%]]
%%[[91
      , dtiMbFixityPrio 	:: !(Maybe (Int,Fixity))	-- if defined as infix, with priority
%%]]
      } deriving Show

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty [] [] hsnUnknown Ty_Any
%%[[8
      emptyCTag
%%]]
%%[[91
      Nothing
%%]]
%%]

%%[(8 hmtyinfer) export(dtiOffsetOfFld)
dtiOffsetOfFld :: HsName -> DataTagInfo -> Int
dtiOffsetOfFld fldNm dti = dfiOffset $ panicJust "dtiOffsetOfFld" $ Map.lookup fldNm $ dtiFldMp dti
%%]

%%[(8 hmtyinfer) export(DataFldInConstr(..),DataFldInConstrMp)
data DataFldInConstr
  = DataFldInConstr
      { dficInTagMp	:: !(Map.Map CTag Int)
      }

type DataFldInConstrMp = Map.Map HsName DataFldInConstr
%%]

%%[(90 hmtyinfer) export(DataGamInfoVariant(..))
-- | specific info about what a DataGamInfo encodes
data DataGamInfoVariant
  = DataGamInfoVariant_Plain		-- plain data type
  | DataGamInfoVariant_Newtype		-- newtype variation
      Ty							-- the type lambda corresponding to a newtype
%%[[92
  | DataGamInfoVariant_Rec			-- tuple, record
%%]]
  deriving Eq
%%]

%%[(7 hmtyinfer) export(DataGamInfo(..))

data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: !HsName				-- type name (duplicate of key of gamma leading to this info)
      , dgiDataTy 			:: !Ty					-- the type sum of product
%%[[50
      , dgiConstrNmL 		:: ![HsName]			-- all constructor names
%%]]
      , dgiConstrTagMp 		:: !DataConstrTagMp		-- per constructor info
%%[[8
      , dgiFldInConstrMp	:: !DataFldInConstrMp	-- map from field name to all constructors having the field
%%[[8
      , dgiIsNewtype 		:: !Bool				-- defined as newtype
%%][90
      , dgiVariant 			:: !DataGamInfoVariant
%%]]
      , dgiMaxConstrArity   :: !Int
%%]]
%%[[92
      , dgiMbGenerInfo		:: !(Maybe Int)			-- max kind arity for generic behavior, currently \in {0,1}
%%]]
      }

instance Show DataGamInfo where
  show _ = "DataGamInfo"
%%]

%%[(90 hmtyinfer) export(dgiMbNewtype,dgiIsNewtype)
dgiMbNewtype :: DataGamInfo -> Maybe Ty
dgiMbNewtype (DataGamInfo {dgiVariant = DataGamInfoVariant_Newtype t}) = Just t
dgiMbNewtype _                                                         = Nothing

dgiIsNewtype :: DataGamInfo -> Bool
dgiIsNewtype = isJust . dgiMbNewtype
%%]

%%[(92 hmtyinfer) export(dgiIsRec)
dgiIsRec :: DataGamInfo -> Bool
dgiIsRec dgi = dgiVariant dgi == DataGamInfoVariant_Rec
%%]

%%[(7 hmtyinfer) export(DataGam)
type DataGam = Gam HsName DataGamInfo
%%]

%%[(7 hmtyinfer) export(mkDGI)
%%[[7
mkDGI :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> Bool -> DataGamInfo
%%][90
mkDGI
  :: HsName
     -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfoVariant
%%[[92
     -> Maybe Int
%%]]
     -> DataGamInfo
%%]]
mkDGI tyNm dty cNmL m nt
%%[[92
      mbGener
%%]]
  = DataGamInfo
      tyNm
      dty
%%[[50
      cNmL
%%]]
      m
%%[[8
      fm nt mx
%%]]
%%[[92
      mbGener
%%]]
%%[[8
  where fm = Map.map DataFldInConstr $ Map.unionsWith Map.union
             $ [ Map.singleton f (Map.singleton (dtiCTag ci) (dfiOffset fi)) | ci <- Map.elems m, (f,fi) <- Map.toList $ dtiFldMp ci ]
        mx = if Map.null m then (-1) else (ctagMaxArity $ dtiCTag $ head $ Map.elems m)
%%]]
%%]

%%[7 export(mkDGIPlain)
mkDGIPlain :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfo
mkDGIPlain tyNm dty cNmL m
  = mkDGI tyNm dty cNmL m
%%[[7
          False
%%][90
          DataGamInfoVariant_Plain
%%]]
%%[[92
          Nothing
%%]]

%%]

%%[(7 hmtyinfer) export(emptyDataGamInfo,emptyDGI)
emptyDataGamInfo, emptyDGI :: DataGamInfo
emptyDataGamInfo = mkDGIPlain hsnUnknown Ty_Any [] Map.empty
emptyDGI = emptyDataGamInfo
%%]

%%[(50 hmtyinfer) export(dgiConstrTagAssocL)
dgiConstrTagAssocL :: DataGamInfo -> AssocL HsName DataTagInfo
dgiConstrTagAssocL dgi = [ (cn,panicJust "dgiConstrTagAssocL" $ Map.lookup cn $ dgiConstrTagMp dgi) | cn <- dgiConstrNmL dgi ]
%%]

%%[(7 hmtyinfer) export(dgiDtiOfCon)
dgiDtiOfCon :: HsName -> DataGamInfo -> DataTagInfo
dgiDtiOfCon conNm dgi = panicJust "dgiDtiOfCon" $ Map.lookup conNm $ dgiConstrTagMp dgi
%%]

%%[(7 hmtyinfer) export(dataGamLookup,dataGamLookupErr)
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
%%]

%%[(7 hmtyinfer) export(dataGamDgiOfTy)
dataGamDgiOfTy :: Ty -> DataGam -> Maybe DataGamInfo
dataGamDgiOfTy conTy dg = dataGamLookup (tyAppFunConNm conTy) dg
%%]

%%[(8 hmtyinfer) export(dataGamDTIsOfTy)
dataGamDTIsOfTy :: Ty -> DataGam -> Maybe [DataTagInfo]
dataGamDTIsOfTy t g
  = -- tr "dataGamDTIsOfTy" (t >#< tyAppFunConNm (tyArrowRes t)) $
    fmap
%%[[8
      (Map.elems . dgiConstrTagMp)
%%][91
      (assocLElts . dgiConstrTagAssocL)
%%]]
    $ gamLookup (tyAppFunConNm $ tyArrowRes t)
    $ g
%%]

%%[(8 hmtyinfer) export(dataGamTagsOfTy)
dataGamTagsOfTy :: Ty -> DataGam -> Maybe [CTag]
dataGamTagsOfTy t g
  = fmap (map dtiCTag) (dataGamDTIsOfTy t g)
%%]

%%[(8 hmtyinfer) export(dataGamLookupTag)
dataGamLookupTag :: HsName -> HsName -> DataGam -> Maybe CTag
dataGamLookupTag t c g
  = do dgi <- dataGamLookup t g
       dti <- Map.lookup c $ dgiConstrTagMp dgi
       return $ dtiCTag dti
%%]

%%[(8 hmtyinfer) export(dataGamTagLookup)
dataGamTagLookup :: CTag -> DataGam -> Maybe (DataGamInfo,DataTagInfo)
dataGamTagLookup CTagRec g
  = Nothing
dataGamTagLookup ct g
  = do dgi <- dataGamLookup (ctagTyNm ct) g
       dti <- Map.lookup (ctagNm ct) $ dgiConstrTagMp dgi
       return (dgi,dti)
%%]

Is datatype an enum? I.e. has no field for any constructor.

%%[(8 codegen) export(dgiIsEnumable)
dgiIsEnumable :: DataGamInfo -> Bool
dgiIsEnumable dgi = dgiMaxConstrArity dgi == 0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer)
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
%%]

%%[(90 hmtyinfer)
deriving instance Typeable DataGamInfoVariant
deriving instance Data DataGamInfoVariant
%%]

%%[(90 hmtyinfer)
instance Serialize DataGamInfoVariant where
  sput (DataGamInfoVariant_Plain    ) = sputWord8 0
  sput (DataGamInfoVariant_Newtype a) = sputWord8 1 >> sput a
%%[[92
  sput (DataGamInfoVariant_Rec      ) = sputWord8 2
%%]]
  sget = do 
    t <- sgetWord8
    case t of
      0 -> return DataGamInfoVariant_Plain
      1 -> liftM  DataGamInfoVariant_Newtype sget
%%[[92
      2 -> return DataGamInfoVariant_Rec
%%]]
%%]

%%[(50 hmtyinfer)
instance Serialize DataFldInfo where
  sput (DataFldInfo a) = sput a
  sget = liftM DataFldInfo sget

instance Serialize DataConFldAnnInfo where
  sput (DataConFldAnnInfo a) = sput a
  sget = liftM DataConFldAnnInfo sget

instance Serialize DataTagInfo where
%%[[50
  sput (DataTagInfo a b c d e f) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f
  sget = liftM6 DataTagInfo sget sget sget sget sget sget
%%][91
  sput (DataTagInfo a b c d e f g) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g
  sget = liftM7 DataTagInfo sget sget sget sget sget sget sget
%%]]

instance Serialize DataFldInConstr where
  sput (DataFldInConstr a) = sput a
  sget = liftM DataFldInConstr sget

instance Serialize DataGamInfo where
%%[[50
  sput (DataGamInfo a b c d e f g) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g
  sget = liftM7 DataGamInfo sget sget sget sget sget sget sget
%%][92
  sput (DataGamInfo a b c d e f g h) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h
  sget = liftM8 DataGamInfo sget sget sget sget sget sget sget sget
%%]]
%%]
