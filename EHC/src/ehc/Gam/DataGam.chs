%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(7 hmtyinfer) module {%{EH}Gam.DataGam}
%%]

%%[(7 hmtyinfer) import(UHC.Util.Pretty,UHC.Util.Utils)
%%]

%%[(7 hmtyinfer) hs import ({%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Base.HsName.Builtin})
%%]
%%[(7 hmtyinfer) hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[(7 hmtyinfer) hs import ({%{EH}Gam})
%%]
%%[(7 hmtyinfer) hs import({%{EH}Error}) 
%%]

%%[(8 counting) hs import(qualified {%{EH}Core.CountingAnalysis.Types} as CA) 
%%]

%%[(7 hmtyinfer) import(qualified Data.Map as Map)
%%]
%%[(7 hmtyinfer) import(qualified Data.Set as Set)
%%]
%%[(90 hmtyinfer) import(Data.Maybe)
%%]

%%[(7 hmtyinfer) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(7 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[(50 hmtyinfer) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

-- debug
%%[(8 codegen) import({%{EH}Base.Debug},UHC.Util.Pretty)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data tag/etc info gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(7 hmtyinfer) export(DataFldMp,DataFldInfo(..),emptyDataFldInfo)
-- | per named field info
-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataFldInfo
  = DataFldInfo
%%[[(8 codegen || hmtyinfer)
      { dfiOffset 	:: !Fld
      }
%%]]
      deriving (Show, Generic)

type DataFldMp = Map.Map HsName DataFldInfo

emptyDataFldInfo
  = DataFldInfo
%%[[(8 codegen || hmtyinfer)
      noFld
%%]]
%%]

%%[(7 hmtyinfer) export(DataConFldAnnInfo(..),emptyDataConFldAnnInfo)
-- | per positional constructor field annotation like info
-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataConFldAnnInfo
  = DataConFldAnnInfo
%%[[(8 codegen || hmtyinfer)
      { dcfaiStrictness		:: !Strictness
      }
%%]]
      deriving (Show, Generic)

emptyDataConFldAnnInfo :: DataConFldAnnInfo
emptyDataConFldAnnInfo
  = DataConFldAnnInfo
%%[[(8 codegen || hmtyinfer)
      Strictness_NonStrict
%%]]
%%]

%%[(7 hmtyinfer) export(DataTagInfo(..),emptyDataTagInfo,DataConstrTagMp)
-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    		:: !DataFldMp				-- map of field names to offset
      , dtiFldTyL     :: !FldTyL          -- association list of maybe a field name with types
%%[[(8 counting)
      , dtiFldAnnTyL			:: ![CA.Rho CA.AnnotatedType]					-- list of annotated types (conversion of dtiFldTyL)
%%]]
      , dtiConFldAnnL		:: ![DataConFldAnnInfo]		-- per constructor field (with or without name) annotation info
      , dtiConNm			:: !HsName					-- constructor name (duplicate of key of gamma leading to this info)
      , dtiConTy			:: !Ty						-- type of constructor, without final tyVarMp applied
%%[[8
      , dtiCTag 			:: !CTag					-- tag of constructor
      , dtiFldRefL			:: ![Fld]					-- list of offset/references positionally consistent with (e.g.) dtiFldTyL
%%]]
%%[[91
      , dtiMbFixityPrio 	:: !(Maybe (Int,Fixity))	-- if defined as infix, with priority
%%]]
      } deriving (Show, Generic)

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty []
%%[[(8 counting)
      []
%%]]
      [] hsnUnknown (appDbg "emptyDataTagInfo")
%%[[8
      emptyCTag []
%%]]
%%[[91
      Nothing
%%]]
%%]

%%[(8 hmtyinfer) export(dtiOffsetOfFld)
dtiOffsetOfFld :: HsName -> DataTagInfo -> Fld
dtiOffsetOfFld fldNm dti = dfiOffset $ panicJust "dtiOffsetOfFld" $ Map.lookup fldNm $ dtiFldMp dti
%%]

%%[(8 hmtyinfer) export(DataFldInConstr(..),DataFldInConstrMp)
-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataFldInConstr
  = DataFldInConstr
      { dficInTagMp	:: !(Map.Map CTag Fld)
      }
  deriving Generic

type DataFldInConstrMp = Map.Map HsName DataFldInConstr
%%]

%%[(90 hmtyinfer) export(DataGamInfoVariant(..))
-- | specific info about what a DataGamInfo encodes
-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataGamInfoVariant
  = DataGamInfoVariant_Plain		-- plain data type
  | DataGamInfoVariant_Newtype		-- newtype variation
      Ty							-- the type lambda corresponding to a newtype
%%[[92
  | DataGamInfoVariant_Rec			-- tuple, record
%%]]
  deriving (Eq, Generic)
%%]

%%[(7 hmtyinfer) export(DataGamInfo(..))

-- If this changes, also change {%{EH}ConfigInternalVersions}
data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: !HsName				-- type name (duplicate of key of gamma leading to this info)
      , dgiDataTy 			:: !Ty					-- the type sum of product
      , dgiDataKi 			:: !Ty					-- the kind
%%[[(8 counting)
      , dgiAnnVars      :: ![CA.Var]      -- all annatotation variables 
%%]]
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
  deriving Generic

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
mkDGI :: HsName -> Ty -> Ty -> [HsName] -> DataConstrTagMp -> Bool -> DataGamInfo
%%][90
mkDGI
  :: HsName
     -> Ty -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfoVariant
%%[[92
     -> Maybe Int
%%]]
%%[[(8 counting)
     -> [CA.Var]
%%]]
     -> DataGamInfo
%%]]
mkDGI tyNm dty ki cNmL m nt
%%[[92
      mbGener
%%]]
%%[[(8 counting)
      anns
%%]]
  = DataGamInfo
      tyNm
      dty
      ki
%%[[(8 counting)
      anns
%%]]
%%[[50
      cNmL
%%]]
      m'
%%[[8
      fm nt mx
%%]]
%%[[92
      mbGener
%%]]
%%[[8
  where fm = Map.map DataFldInConstr $ Map.unionsWith Map.union
             $ [ Map.singleton f (Map.singleton (dtiCTag ci) (dfiOffset fi)) | ci <- Map.elems m', (f,fi) <- Map.toList $ dtiFldMp ci ]
        mx = maximum
               ( (if Map.null m then (-1) else (ctagMaxArity $ dtiCTag $ head $ Map.elems m))
               : [ ctagArity $ dtiCTag dti | dti <- Map.elems m ]
               )
        m' = Map.map (\dti -> dti {dtiCTag = patchTyInfoCTag tyNm mx $ dtiCTag dti}) m
%%]]
%%]

%%[(7 hmtyinfer) export(mkDGIPlain)
mkDGIPlain :: HsName -> Ty -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfo
mkDGIPlain tyNm dty dki cNmL m
  = mkDGI tyNm dty dki cNmL m
%%[[7
          False
%%][90
          DataGamInfoVariant_Plain
%%]]
%%[[92
          Nothing
%%]]
%%[[(8 counting)
          []
%%]]

%%]

%%[(8 codegen) export(mkDGIForCodegenOnly)
-- | Construct a datatype info as extracted from (e.g.) Core intended only for codegen (i.e. no type system stuff).
mkDGIForCodegenOnly :: HsName -> DataConstrTagMp -> DataGamInfo
mkDGIForCodegenOnly tyNm m
  = mkDGIPlain tyNm Ty_Any Ty_Any (Map.keys m) m
%%]

%%[(7 hmtyinfer) export(emptyDataGamInfo,emptyDGI)
emptyDataGamInfo, emptyDGI :: DataGamInfo
emptyDataGamInfo = mkDGIPlain hsnUnknown (appDbg "emptyDataGamInfo") (appDbg "mkDGIPlain") [] Map.empty
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

%%[(8 hmtyinfer) export(dataGamDTIsOfTyNm, dataGamDTIsOfTy)
dataGamDTIsOfTyNm :: HsName -> DataGam -> Maybe [DataTagInfo]
dataGamDTIsOfTyNm tn g
  = fmap
%%[[8
      (Map.elems . dgiConstrTagMp)
%%][91
      (assocLElts . dgiConstrTagAssocL)
%%]]
    $ gamLookup tn
    $ g

dataGamDTIsOfTy :: Ty -> DataGam -> Maybe [DataTagInfo]
dataGamDTIsOfTy = dataGamDTIsOfTyNm . tyDataTyNm
{-# INLINE dataGamDTIsOfTy #-}
%%]

%%[(8 hmtyinfer) export(dataGamTagsOfTy, dataGamTagsOfTyNm)
dataGamTagsOf :: (t -> DataGam -> Maybe [DataTagInfo]) -> t -> DataGam -> Maybe [CTag]
dataGamTagsOf lkup t g = fmap (map dtiCTag) (lkup t g)
{-# INLINE dataGamTagsOf #-}

dataGamTagsOfTy :: Ty -> DataGam -> Maybe [CTag]
dataGamTagsOfTy = dataGamTagsOf dataGamDTIsOfTy

dataGamTagsOfTyNm :: HsName -> DataGam -> Maybe [CTag]
dataGamTagsOfTyNm = dataGamTagsOf dataGamDTIsOfTyNm
%%]

%%[(8 hmtyinfer) export(dataGamLookupTag)
dataGamLookupTag :: HsName -> HsName -> DataGam -> Maybe CTag
dataGamLookupTag t c g
  = do dgi <- dataGamLookup t g
       dti <- Map.lookup c $ dgiConstrTagMp dgi
       return $ dtiCTag dti
%%]

%%[(8 hmtyinfer) export(dataGamTagLookup)
dataGamTagLookup :: TagLike t => t -> DataGam -> Maybe (DataGamInfo,DataTagInfo)
dataGamTagLookup tag g
  | tagIsData tag
      = do dgi <- dataGamLookup (tagDataTypeNm tag) g
           dti <- Map.lookup (tagDataConstrNm tag) $ dgiConstrTagMp dgi
           return (dgi,dti)
  | otherwise
      = Nothing
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
--   sput (DataGamInfoVariant_Plain    ) = sputWord8 0
--   sput (DataGamInfoVariant_Newtype a) = sputWord8 1 >> sput a
-- %%[[92
--   sput (DataGamInfoVariant_Rec      ) = sputWord8 2
-- %%]]
--   sget = do 
--     t <- sgetWord8
--     case t of
--       0 -> return DataGamInfoVariant_Plain
--       1 -> liftM  DataGamInfoVariant_Newtype sget
-- %%[[92
--       2 -> return DataGamInfoVariant_Rec
-- %%]]
%%]

%%[(50 hmtyinfer)
instance Serialize DataFldInfo where
  -- sput (DataFldInfo a) = sput a
  -- sget = liftM DataFldInfo sget

instance Serialize DataConFldAnnInfo where
  -- sput (DataConFldAnnInfo a) = sput a
  -- sget = liftM DataConFldAnnInfo sget

instance Serialize DataTagInfo where
-- %%[[(50 counting)
--   sput (DataTagInfo a b c d e f g h) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h
--   sget = liftM8 DataTagInfo sget sget sget sget sget sget sget sget
-- %%][50
--   sput (DataTagInfo a b c d e f g) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g
--   sget = liftM7 DataTagInfo sget sget sget sget sget sget sget
-- %%][(91 counting)
--   sput (DataTagInfo a b c d e f g h i) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h >> sput i
--   sget = liftM9 DataTagInfo sget sget sget sget sget sget sget sget sget
-- %%][91
--   sput (DataTagInfo a b c d e f g h) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h
--   sget = liftM8 DataTagInfo sget sget sget sget sget sget sget sget
-- %%]]

instance Serialize DataFldInConstr where
  -- sput (DataFldInConstr a) = sput a
  -- sget = liftM DataFldInConstr sget

instance Serialize DataGamInfo where
-- %%[[(50 counting)
--   sput (DataGamInfo a b c d e f g h i) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h >> sput i
--   sget = liftM9 DataGamInfo sget sget sget sget sget sget sget sget sget
-- %%][50
--   sput (DataGamInfo a b c d e f g h) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h
--   sget = liftM8 DataGamInfo sget sget sget sget sget sget sget sget
-- %%][(92 counting)
--   sput (DataGamInfo a b c d e f g h i j) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h >> sput i >> sput j
--   sget = liftM10 DataGamInfo sget sget sget sget sget sget sget sget sget sget
-- %%][92
--   sput (DataGamInfo a b c d e f g h i) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g >> sput h >> sput i
--   sget = liftM9 DataGamInfo sget sget sget sget sget sget sget sget sget
-- %%]]

-- liftM10  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> r)
--            -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m r
-- liftM10 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10
--   = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; x10 <- m10
--        ; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
--        }
%%]
