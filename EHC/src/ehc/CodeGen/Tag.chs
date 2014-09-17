%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tag for datatypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}CodeGen.Tag}
%%]

%%[8 import(Data.Maybe)
%%]

%%[8 import(Control.Monad)
%%]

%%[8 import(UHC.Util.Pretty, UHC.Util.AssocL)
%%]

%%[8 import({%{EH}Base.HsName}, {%{EH}Base.HsName.Builtin})
%%]

%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tags (of data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(CTag(..),ctagIsRec,ctagTag,ctagChar,ctagInt,emptyCTag)
data CTag
  = CTagRec
  | CTag
      { ctagTyNm        :: !HsName
      , ctagNm          :: !HsName
      , ctagTag'        :: !Int
      , ctagArity       :: !Int
      , ctagMaxArity    :: !Int
      }
  deriving (Show,Eq,Ord)

ctagIsRec :: CTag -> Bool
ctagIsRec CTagRec = True
ctagIsRec t       = False

ctagTag :: CTag -> Int
ctagTag CTagRec = 0
ctagTag t       = ctagTag' t

ctagInt  =  CTag hsnInt  hsnInt  0 1 1
{-# INLINE ctagInt #-}
ctagChar =  CTag hsnChar hsnChar 0 1 1
{-# INLINE ctagChar #-}

emptyCTag = CTag hsnUnknown hsnUnknown 0 0 0
{-# INLINE emptyCTag #-}
%%]

%%[8 export(mkOnlyConInfoCTag, patchTyInfoCTag)
-- | Construct a minimal datatype tag which still must be completed wrt more global datatype info
mkOnlyConInfoCTag :: HsName -> Int -> Int -> CTag
mkOnlyConInfoCTag conNm tg arity = emptyCTag {ctagNm = conNm, ctagTag' = tg, ctagArity = arity}

-- | Patch a datatype tag with datatype global info
patchTyInfoCTag :: HsName -> Int -> CTag -> CTag
patchTyInfoCTag tyNm maxArity t = t {ctagTyNm = tyNm, ctagMaxArity = maxArity}
%%]

%%[9 export(mkClassCTag)
-- only used when `not ehcCfgClassViaRec'
mkClassCTag :: HsName -> Int -> CTag
mkClassCTag n sz = CTag n n 0 sz sz
%%]

%%[8 hs export(ctag,ppCTag,ppCTagInt)
ctag :: a -> (HsName -> HsName -> Int -> Int -> Int -> a) -> CTag -> a
ctag n t tg = case tg of {CTag tn cn i a ma -> t tn cn i a ma; _ -> n}
{-# INLINE ctag #-}

ppCTag :: CTag -> PP_Doc
ppCTag = ctag (pp "Rec") (\tn cn t a ma -> pp t >|< "/" >|< pp cn >|< "/" >|< pp a >|< "/" >|< pp ma)

ppCTagInt :: CTag -> PP_Doc
ppCTagInt = ctag (pp "-1") (\_ _ t _ _ -> pp t)

instance PP CTag where
  pp = ppCTag
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tags abstraction/interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(TagDataInfo(..))
-- | datatype info about tag: type name & constr name, required throughout various codegen stages
data TagDataInfo = TagDataInfo
  { tagDataInfoTypeNm 	:: !HsName
  , tagDataInfoConstrNm	:: !HsName
  }
  deriving (Show)

instance Eq TagDataInfo where
  i1 == i2 = tagDataInfoConstrNm i1 == tagDataInfoConstrNm i2

instance Ord TagDataInfo where
  i1 `compare` i2 = tagDataInfoConstrNm i1 `compare` tagDataInfoConstrNm i2
%%]

%%[8 hs export(mkTyIsConTagInfo, mkConTagInfo, emptyTagDataInfo)
mkTyConTagInfo :: HsName -> HsName -> TagDataInfo
mkTyConTagInfo = TagDataInfo
{-# INLINE mkTyConTagInfo #-}

-- | Construct info when Ty and Con name are equal
mkTyIsConTagInfo :: HsName -> TagDataInfo
mkTyIsConTagInfo n = mkTyConTagInfo n n
{-# INLINE mkTyIsConTagInfo #-}

mkConTagInfo :: HsName -> TagDataInfo
mkConTagInfo cn = mkTyConTagInfo hsnUnknown cn
{-# INLINE mkConTagInfo #-}

emptyTagDataInfo = mkTyConTagInfo hsnUnknown hsnUnknown
%%]

%%[8 hs export(tagInfoInt, tagInfoChar)
tagInfoInt  = mkTyIsConTagInfo hsnInt
tagInfoChar = mkTyIsConTagInfo hsnChar
%%]

%%[8 hs export(TagLike(..), tagDataInfo)
class TagLike t where
  tagIsData			:: t -> Bool
  tagIsTup			:: t -> Bool
  
  -- | extract data related info, only allowed when tagIsData
  tagMbDataInfo		:: t -> Maybe TagDataInfo
  tagDataTypeNm   	:: t -> HsName
  tagDataConstrNm 	:: t -> HsName
  tagDataTag		:: t -> Int
  
  -- defaults: either tagDataInfo or tagDataTypeNm and tagDataConstrNm and tagIsData
  tagMbDataInfo		t	= if tagIsData t then Just (emptyTagDataInfo {tagDataInfoTypeNm = tagDataTypeNm t, tagDataInfoConstrNm = tagDataConstrNm t}) else Nothing
  tagDataTypeNm			= tagDataInfoTypeNm . tagDataInfo
  tagDataConstrNm		= tagDataInfoConstrNm . tagDataInfo
  tagIsData 			= isJust . tagMbDataInfo
  
  -- defaults
  tagIsTup				= not . tagIsData

-- | Assuming a datatype, return info
tagDataInfo :: TagLike t => t -> TagDataInfo
tagDataInfo = fromJust . tagMbDataInfo
{-# INLINE tagDataInfo #-}

instance TagLike CTag where
  tagMbDataInfo	 	= ctag Nothing (\tn cn _ _ _ -> Just (emptyTagDataInfo {tagDataInfoTypeNm = tn, tagDataInfoConstrNm = cn}))
  tagDataTag 		= ctagTag'
  -- not necessary:
  tagIsTup  		= ctagIsRec
  tagDataTypeNm 	= ctagTyNm
  tagDataConstrNm 	= ctagNm
%%]

%%[8 hs
instance PP TagDataInfo where
  pp i = tagDataInfoTypeNm i >|< "#" >|< tagDataInfoConstrNm i
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc info passed to backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(CTagsMp, emptyCTagsMp)
type CTagsMp = AssocL HsName (AssocL HsName CTag)

emptyCTagsMp :: CTagsMp
emptyCTagsMp = []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Typeable CTag
deriving instance Data CTag

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Serialize CTag where
  sput = sputShared
  sget = sgetShared
  sputNested (CTagRec          ) = sputWord8 0
  sputNested (CTag    a b c d e) = sputWord8 1 >> sput a >> sput b >> sput c >> sput d >> sput e
  sgetNested
    = do t <- sgetWord8
         case t of
           0 -> return CTagRec
           1 -> liftM5 CTag    sget sget sget sget sget
%%]
