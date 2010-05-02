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
%%[(94 hmtyinfer || hmtyast) import(Data.Maybe)
%%]

%%[(7 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(7 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[(20 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
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

%%[(7 hmtyinfer) export(DataTagInfo(..),emptyDataTagInfo,DataConstrTagMp)
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    		:: !DataFldMp
      , dtiConNm			:: !HsName
%%[[8
      , dtiCTag 			:: !CTag
%%]]
%%[[95
      , dtiMbFixityPrio 	:: !(Maybe Int)
%%]]
      } deriving Show

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty hsnUnknown
%%[[8
      emptyCTag
%%]]
%%[[95
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

%%[(7 hmtyinfer) export(DataGam,DataGamInfo(..),mkDGI)
data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: !HsName
      , dgiDataTy 			:: !Ty
%%[[20
      , dgiConstrNmL 		:: ![HsName]
%%]]
      , dgiConstrTagMp 		:: !DataConstrTagMp
%%[[8
      , dgiFldInConstrMp	:: !DataFldInConstrMp
%%[[8
      , dgiIsNewtype 		:: !Bool
%%][94
      , dgiMbNewtype 		:: !(Maybe Ty)			-- the type lambda corresponding to a newtype
%%]]
      , dgiMaxConstrArity   :: !Int
%%]]
      }


type DataGam = Gam HsName DataGamInfo

instance Show DataGamInfo where
  show _ = "DataGamInfo"

%%[[7
mkDGI :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> Bool -> DataGamInfo
%%][94
mkDGI :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> Maybe Ty -> DataGamInfo
%%]]
mkDGI tyNm dty cNmL m nt
  = DataGamInfo
      tyNm
      dty
%%[[20
      cNmL
%%]]
      m
%%[[8
      fm nt mx
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
%%][94
          Nothing
%%]]

%%]

%%[94 export(dgiIsNewtype)
dgiIsNewtype :: DataGamInfo -> Bool
dgiIsNewtype = isJust . dgiMbNewtype
%%]

%%[(7 hmtyinfer) export(emptyDataGamInfo,emptyDGI)
emptyDataGamInfo, emptyDGI :: DataGamInfo
emptyDataGamInfo = mkDGI hsnUnknown Ty_Any [] Map.empty
%%[[7
                         False
%%][94
                         Nothing
%%]]
emptyDGI = emptyDataGamInfo
%%]

%%[(20 hmtyinfer) export(dgiConstrTagAssocL)
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
%%][95
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

Lookup by constructor name:

%%[8
%%]
valDataGamLookup :: HsName -> ValGam -> DataGam -> Maybe DataGamInfo
valDataGamLookup nm vg dg
  = do { vgi <- valGamLookup nm vg
       ; dgi <- dataGamDgiOfTy (vgiTy vgi) dg
       }

Is datatype an enum? I.e. has no field for any constructor.

%%[(8 codegen) export(dgiIsEnumable)
dgiIsEnumable :: DataGamInfo -> Bool
dgiIsEnumable dgi = dgiMaxConstrArity dgi == 0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 hmtyinfer)
deriving instance Typeable DataFldInfo
deriving instance Data DataFldInfo

deriving instance Typeable DataTagInfo
deriving instance Data DataTagInfo

deriving instance Typeable DataFldInConstr
deriving instance Data DataFldInConstr

deriving instance Typeable DataGamInfo
deriving instance Data DataGamInfo

%%]

%%[(9999 hmtyinfer)
instance ForceEval DataFldInfo
%%[[102
  where
    fevCount (DataFldInfo x) = cm1 "DataFldInfo" `cmUnion` fevCount x
%%]]

instance ForceEval DataTagInfo where
  forceEval x@(DataTagInfo m n t p) | forceEval m `seq` forceEval p `seq` True = x
%%[[102
  fevCount (DataTagInfo m n t p) = cmUnions [cm1 "DataTagInfo",fevCount m,fevCount n,fevCount t,fevCount p]
%%]]

instance ForceEval DataFldInConstr where
  forceEval x@(DataFldInConstr m) | forceEval m `seq` True = x
%%[[102
  fevCount (DataFldInConstr x) = cm1 "DataFldInConstr" `cmUnion` fevCount x
%%]]

instance ForceEval DataGamInfo where
  forceEval x@(DataGamInfo n t nl tm cm nt mx) | forceEval nl `seq` forceEval tm `seq` forceEval cm `seq` True = x
%%[[102
  fevCount (DataGamInfo n t nl tm cm nt mx) = cmUnions [cm1 "DataGamInfo",fevCount n,fevCount t,fevCount nl,fevCount tm,fevCount cm,fevCount nt,fevCount mx]
%%]]
%%]

%%[(20 hmtyinfer)
instance Serialize DataFldInfo where
  sput (DataFldInfo a) = sput a
  sget = liftM DataFldInfo sget

instance Serialize DataTagInfo where
%%[[20
  sput (DataTagInfo a b c) = sput a >> sput b >> sput c 
  sget = liftM3 DataTagInfo sget sget sget
%%][95
  sput (DataTagInfo a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 DataTagInfo sget sget sget sget
%%]]

instance Serialize DataFldInConstr where
  sput (DataFldInConstr a) = sput a
  sget = liftM DataFldInConstr sget

instance Serialize DataGamInfo where
  sput (DataGamInfo a b c d e f g) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f >> sput g
  sget = liftM7 DataGamInfo sget sget sget sget sget sget sget

%%]
