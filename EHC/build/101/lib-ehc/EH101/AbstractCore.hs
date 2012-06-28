module EH101.AbstractCore
( AbstractCore (..)
, acoreMetaLift
, ACoreBindAspectKey (..), ACoreBindAspectKeyS, ACoreBindAspMp
, acbaspkeyDefaultTy, acbaspkeyDefaultCore, acbaspkeyNone, acbaspkeyDefault, acbaspkeyDefaultRelevTy, acbaspkeyStrict, acbaspkeyDebug
, ppACBaspKeyS
, hsnUniqifyACoreBindAspectKeyS
, ACoreBindRef (..)
, acbrefAspAnd
, ppACoreBindRef
, acoreApp1, acoreApp
, acoreLam1Ty, acoreLam1, acoreLamTy, acoreLam
, acoreTagTup, acoreTupTy, acoreTup, acoreTag
, acoreBind1CatLevMetaTyWith, acoreBind1CatLevMetaTy, acoreBind1CatLevTy, acoreBind1CatMetaTy, acoreBind1CatTy, acoreBind1Cat, acoreBind1Ty, acoreBind1
, acoreBind1MetasTy, acoreBind1CatMeta, acoreBind1MetaTy
, acoreBind1Asp1, acoreBind1Nm1
, acoreBoundVal1CatLevMetaTy, acoreBoundVal1CatLevTy, acoreBoundVal1CatMetaTy, acoreBoundVal1CatTy, acoreBoundVal1Cat
, acoreBoundVal1Metas, acoreBoundVal1Meta
, acoreBound1Val
, acoreTyLift
, acoreLetMerge, acoreLet, acoreLetRec
, acoreLetN
, acoreLet1PlainTy, acoreLet1Plain
, acoreLet1StrictTy, acoreLet1Strict
, acoreLet1StrictInMetaTyWith, acoreLet1StrictInMetaTy, acoreLet1StrictInMeta, acoreLet1StrictIn, acoreLet1StrictInTy
, acoreBindcategDflt
, acoreChar, acoreInt, acoreInt2
, acoreBuiltinApp
, acoreBuiltinAddInt
, acoreBuiltinGtInt
, acoreBuiltinString
, acoreBuiltinError, acoreBuiltinUndefined
, acorePatConMbTag, acoreAltMbTag
, acoreBindNm
, acorePatFldTy
, acoreUnBoundVal
, Coe' (..)
, acoreCoeId, acoreCoeMap
, acoreCoeApp1, acoreCoeAppN, acoreCoeAppNbyName
, acoreCoeLam1Ty, acoreCoeLam1
, acoreCoeCompose
, acoreCoeIsId
, CSubstKey (..)
, CSubstInfo' (..)
, CSubst', emptyCSubst
, acoreCSubstFromUidTyL
, acoreCSubstFromUidExprL
, cSubstAppSubst
, CSubstitutable (..)
, RAlt' (..), RPat' (..), RPatConBind' (..), RPatFld' (..), RCEAltL'
, rcaPat, raltLPatNms
, rcaTag
, raltIsVar, raltIsConst
, raltIsConMany
, raltIsIrrefutable
, rpatConBindUnFlatten
, acoreRPat2Pat
, ctagTrue, ctagFalse
, ctagCons, ctagNil
, CaseAltFailReason (..)
, cafailHasId
, AppFunKind (..)
, WhatExpr (..)
, whatExprMbApp, whatExprAppArity
, acoreMetaLiftDict
, acoreNmHolePred, acoreNmHole
, acoreCoeLamLetTy, acoreCoeLamLet, acoreCoeLetRec
, acoreCoePoiLApp, acoreCoeImplsApp
, acoreCoePoiLLamTy, acoreCoePoiLLam, acoreCoeImplsLam
, acoreCSubstFromUidImplsL, acoreCSubstFromUidBindLL
, acoreIf
, acbaspkeyFusionRole
, acoreBuiltinInteger
, raltMbBoolExpr, raltIsBoolExpr
, acoreBuiltinEqChar
, acoreBuiltinListSingleton
, acoreMatchChar )
where
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts.Base
import EH101.Ty
import EH.Util.Pretty
import EH.Util.Utils
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>),(<$>))
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Typeable (Typeable)
import Data.Generics (Data)



{-# LINE 35 "src/ehc/AbstractCore.chs" #-}
class AbstractCore  expr metaval bind bound bindcateg metabind ty pat patrest patfld alt
    | expr       ->      metaval bind bound bindcateg metabind ty pat patrest patfld alt
    , metaval    -> expr
    , bind       -> expr
    , bound      -> expr
    , bindcateg  -> expr
    , metabind   -> expr
    , ty         -> expr
    , pat        -> expr
    , patrest    -> expr
    , patfld	 -> expr
    , alt    	 -> expr
  where
  ------------------------- constructing: expr -------------------------
  -- | 1 arg application, together with meta info about the argument, packaged in the bind
  acoreLam1Bind :: bind -> expr -> expr

  -- | 1 arg application, together with meta info about the argument
  -- acoreLam1Ty :: HsName -> ty -> expr -> expr

  -- | 1 lam abstraction, together with meta info about, and type of the argument
  acoreApp1Bound :: expr -> bound -> expr

  -- | a tuple, with tag, and ty
  acoreTagTupTy :: CTag -> ty -> [expr] -> expr

  -- | a value binding, for a name to value + type + metas + meta level
  acoreBind1CatLevMetasTy :: bindcateg -> HsName -> MetaLev -> (metabind,metaval) -> ty -> expr -> bind

  -- | a value binding aspect, for a name + value, optionally type + metas + meta level
  acoreBoundVal1CatLevMetasTy :: bindcateg -> HsName -> MetaLev -> (metabind,metaval) -> ty -> expr -> bound

  -- | a type for value binding aspect, for a name + type, optionally meta level
  acoreBoundValTy1CatLev :: bindcateg -> HsName -> MetaLev -> ty -> bound

  -- | a expr binding aspect, for a name
  acoreBound1AspkeyVal :: ACoreBindAspectKeyS -> expr -> bound

  -- | a binding, for/from a single aspect (for now, later multiple)
  acoreBind1Asp :: HsName -> [bound] -> bind

  -- | basic let binding
  acoreLetBase :: bindcateg -> [bind] -> expr -> expr

  -- | case, with possible default
  acoreCaseDflt  :: expr -> [alt] -> Maybe expr -> expr

  -- | var
  acoreVar  :: HsName -> expr

  -- | string
  acoreStringTy  :: ty -> String -> expr

  -- | char
  acoreCharTy  :: ty -> Char -> expr

  -- | int as Int
  acoreIntTy  :: ty -> Int -> expr

  -- | int as Integer
  acoreIntTy2 :: ty -> Integer -> expr

  -- | hole: placeholder for CSubst to fill in
  acoreUidHole :: UID -> expr

  -- | hole: let with hole for bindings to be filled in later by means of a CSubst
  acoreHoleLet :: UID -> expr -> expr

  ------------------------- constructing: ty -------------------------
  -- | construct ty from Ty, usable in Core context
  acoreTy2ty :: Ty -> ty

  ------------------------- constructing: ty constants -------------------------
  -- Int
  -- acoreTyInt2 :: ty

  -- Bool
  acoreTyBool :: EHCOpts -> ty

  ------------------------- constructing: pat -------------------------
  -- | pat var, with type
  acorePatVarTy :: HsName -> ty -> pat

  -- | pat con
  acorePatCon :: CTag -> patrest -> [patfld] -> pat

  -- | pat int
  acorePatIntTy :: ty -> Int -> pat

  -- | pat Integer
  acorePatIntTy2 :: ty -> Integer -> pat

  -- | pat char
  acorePatCharTy :: ty -> Char -> pat

  -- | pat boolean guard
  acorePatBoolExpr :: expr -> pat
  ------------------------- constructing: pat field -------------------------
  -- | pat field
  acorePatFldBind :: (HsName,expr) -> bind -> patfld
  -- acorePatFldTy :: ty -> (HsName,expr) -> HsName -> patfld

  ------------------------- constructing: patrest -------------------------
  -- | patrest, empty
  acorePatRestEmpty :: patrest

  -- | patrest, var
  acorePatRestVar :: HsName -> patrest

  ------------------------- constructing: alt -------------------------
  -- | 1 arg application, together with meta info about the argument
  acoreAlt :: pat -> expr -> alt

  ------------------------- type related -------------------------
  -- | convert Ty to ty
  -- acoreTy2ty :: Ty -> ty

  ------------------------- defaults -------------------------
  -- | get default for metaval
  acoreMetavalDflt :: metaval

  -- | get default for metaval, for dicts
  acoreMetavalDfltDict :: metaval

  -- | get default for metabind
  acoreMetabindDflt :: metabind

  -- | get error/default ty, type indexed by ty
  acoreTyErr :: String -> ty

  -- | get the ty representing the absent type, no type info
  acoreTyNone :: ty

  -- | get char ty
  acoreTyChar :: ty

  -- | get int ty
  acoreTyInt :: ty

  -- | get String ty
  acoreTyString :: EHCOpts -> ty

  ------------------------- bindcateg values -------------------------
  -- | get recursive bindcateg
  acoreBindcategRec :: bindcateg

  -- | get strict bindcateg
  acoreBindcategStrict :: bindcateg

  -- | get plain bindcateg
  acoreBindcategPlain :: bindcateg

  ------------------------- inspecting/deconstructing -------------------------
  -- | is expr a lambda?
  acoreExprMbLam :: expr -> Maybe (bind,expr)

  -- | is expr a let?
  acoreExprMbLet :: expr -> Maybe (bindcateg,[bind],expr)

  -- | is expr a var?
  acoreExprMbVar :: expr -> Maybe HsName

  -- | is expr a int?
  acoreExprMbInt :: expr -> Maybe (ty,Integer)

  -- | is bindcateg recursive?
  acoreBindcategMbRec :: bindcateg -> Maybe bindcateg

  -- | is bindcateg strict?
  acoreBindcategMbStrict :: bindcateg -> Maybe bindcateg

  -- | is pat a con?
  acorePatMbCon :: pat -> Maybe(CTag,patrest,[patfld])

  -- | is pat a int?
  acorePatMbInt :: pat -> Maybe(ty,Integer)

  -- | is pat a char?
  acorePatMbChar :: pat -> Maybe(ty,Char)

  -- | 'un' alt
  acoreUnAlt :: alt -> (pat,expr)

  -- | 'un' patfld
  acoreUnPatFld :: patfld -> ((HsName,expr),bind)

  -- | 'un' patfld
  -- acoreUnPatFld :: patfld -> (ty,(HsName,expr),HsName)

  -- | 'un' bind
  acoreUnBind :: bind -> (HsName,[bound])

  -- | is bound a expr?
  acoreBoundMbVal :: bound -> Maybe (ACoreBindAspectKeyS,expr)

  ------------------------- transforming -------------------------
  -- | unthunk expr
  acoreExprUnThunk :: expr -> expr
  acoreExprUnThunk = id

  -- | unthunk ty
  acoreTyUnThunk :: ty -> ty
  acoreTyUnThunk = id

  ------------------------- coercion related: construction -------------------------
  -- | coercion arg placeholder
  acoreCoeArg :: expr

  ------------------------- coercion related: inspecting/deconstructing -------------------------
  -- | coercion arg placeholder
  acoreExprIsCoeArg :: expr -> Bool


{-# LINE 260 "src/ehc/AbstractCore.chs" #-}
acoreMetaLift :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => f x -> f (x,m)
acoreMetaLift = fmap2Tuple acoreMetavalDflt

{-# LINE 265 "src/ehc/AbstractCore.chs" #-}
acoreMetaLiftDict :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => f x -> f (x,m)
acoreMetaLiftDict = fmap2Tuple acoreMetavalDfltDict

{-# LINE 275 "src/ehc/AbstractCore.chs" #-}
-- | A ACoreBindAspectKeyS formed out of multiple ACoreBindAspectKey identifies a particular binding aspect
data ACoreBindAspectKey
  = ACoreBindAspectKey_Default				-- identifies the default binding, if omitted in a reference this aspect is the one chosen.
  | ACoreBindAspectKey_Ty					-- the normal ty
  | ACoreBindAspectKey_RelevTy				-- the relevance ty
  | ACoreBindAspectKey_Strict				-- the as strict as possible variant
  | ACoreBindAspectKey_Debug				-- internal debugging only
  | ACoreBindAspectKey_Core					-- core
  | ACoreBindAspectKey_FusionRole			-- fusion role
  deriving (Eq,Ord,Enum)

instance Show ACoreBindAspectKey where
  show ACoreBindAspectKey_Default 		= "dft"
  show ACoreBindAspectKey_Strict 		= "str"
  show ACoreBindAspectKey_Ty 			= "ty"
  show ACoreBindAspectKey_RelevTy 		= "rty"
  show ACoreBindAspectKey_Debug 		= "dbg"
  show ACoreBindAspectKey_Core 			= "core"
  show ACoreBindAspectKey_FusionRole	= "fusionrole"

instance PP ACoreBindAspectKey where
  pp = pp . show

type ACoreBindAspectKeyS		=	Set.Set ACoreBindAspectKey
type ACoreBindAspMp x			=	Map.Map ACoreBindAspectKeyS x

acbaspkeyMk :: [ACoreBindAspectKey] -> ACoreBindAspectKeyS
acbaspkeyMk = Set.fromList

{-# LINE 310 "src/ehc/AbstractCore.chs" #-}
-- | predefined:
acbaspkeyNone :: ACoreBindAspectKeyS
acbaspkeyNone = acbaspkeyMk
  [  ]

-- | predefined:
acbaspkeyDefault :: ACoreBindAspectKeyS
acbaspkeyDefault = acbaspkeyMk
  [ ACoreBindAspectKey_Default ]

-- | predefined:
acbaspkeyDefaultTy :: ACoreBindAspectKeyS
acbaspkeyDefaultTy = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_Ty ]

-- | predefined:
acbaspkeyDefaultCore :: ACoreBindAspectKeyS
acbaspkeyDefaultCore = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_Core ]

-- | predefined:
acbaspkeyDefaultRelevTy :: ACoreBindAspectKeyS
acbaspkeyDefaultRelevTy = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_RelevTy ]

-- | predefined:
acbaspkeyStrict :: ACoreBindAspectKeyS
acbaspkeyStrict = acbaspkeyMk
  [ ACoreBindAspectKey_Strict ]

-- | predefined:
acbaspkeyDebug :: ACoreBindAspectKeyS
acbaspkeyDebug = acbaspkeyMk
  [ ACoreBindAspectKey_Debug ]

{-# LINE 347 "src/ehc/AbstractCore.chs" #-}
-- | predefined:
acbaspkeyFusionRole :: ACoreBindAspectKeyS
acbaspkeyFusionRole = acbaspkeyMk
  [ ACoreBindAspectKey_FusionRole ]

{-# LINE 354 "src/ehc/AbstractCore.chs" #-}
ppACBaspKeyS :: ACoreBindAspectKeyS -> PP_Doc
ppACBaspKeyS = ppCurlysCommas . Set.toList

{-# LINE 359 "src/ehc/AbstractCore.chs" #-}
-- | uniqify with ACoreBindAspectKeyS, omitting the default
hsnUniqifyACoreBindAspectKeyS :: ACoreBindAspectKeyS -> HsName -> HsName
hsnUniqifyACoreBindAspectKeyS as n
  = foldr mk n $ Set.toList as
  where mk ACoreBindAspectKey_Strict = hsnUniqify    HsNameUniqifier_Strict
        mk a                         = hsnUniqifyStr HsNameUniqifier_BindAspect (show a)

{-# LINE 368 "src/ehc/AbstractCore.chs" #-}
deriving instance Typeable ACoreBindAspectKey
deriving instance Data ACoreBindAspectKey

{-# LINE 377 "src/ehc/AbstractCore.chs" #-}
-- | reference to binding aspect: name + aspect keys
data ACoreBindRef
  = ACoreBindRef
      { acbrefNm		:: !HsName
      , acbrefMbAspKey	:: !(Maybe ACoreBindAspectKeyS)
      }
  deriving (Eq,Ord)

instance HSNM ACoreBindRef where
  mkHNm (ACoreBindRef n ma) = maybe n (\a -> hsnUniqifyACoreBindAspectKeyS a n) ma

instance Show ACoreBindRef where
  show = show . mkHNm

{-# LINE 393 "src/ehc/AbstractCore.chs" #-}
acbrefAspKey :: ACoreBindRef -> ACoreBindAspectKeyS
acbrefAspKey = maybe acbaspkeyNone id . acbrefMbAspKey
{-# INLINE acbrefAspKey #-}

{-# LINE 399 "src/ehc/AbstractCore.chs" #-}
-- | narrow down aspects by adding more to ref; assume extra aspects non empty
acbrefAspAnd :: ACoreBindAspectKeyS -> ACoreBindRef -> ACoreBindRef
acbrefAspAnd a r = r {acbrefMbAspKey = Just $ a `Set.union` acbrefAspKey r }

{-# LINE 405 "src/ehc/AbstractCore.chs" #-}
ppACoreBindRef :: (HsName -> PP_Doc) -> ACoreBindRef -> PP_Doc
ppACoreBindRef ppN r = ppN (acbrefNm r) >|< (maybe empty (ppCurlysCommas . Set.toList) $ acbrefMbAspKey r)

instance PP ACoreBindRef where
  pp = ppACoreBindRef pp

{-# LINE 413 "src/ehc/AbstractCore.chs" #-}
deriving instance Typeable ACoreBindRef
deriving instance Data ACoreBindRef

{-# LINE 422 "src/ehc/AbstractCore.chs" #-}
acoreApp1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> e -> e
acoreApp1 f a = acoreApp1Bound f (acoreBound1Val a)
{-# INLINE acoreApp1 #-}

acoreApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> [e] -> e
acoreApp f as = foldl (\f a -> acoreApp1 f a) f as

{-# LINE 435 "src/ehc/AbstractCore.chs" #-}
acoreLam1Ty :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> e
acoreLam1Ty a t e = acoreLam1Bind (acoreBind1Nm1 a) e
-- acoreLam1Ty a t e = acoreLam1Bind (acoreBind1Ty a t) e		-- 20120418, TBD: ignore type for now
{-# INLINE acoreLam1Ty #-}

acoreLam1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e
acoreLam1 a e = acoreLam1Ty a (acoreTyErr "acoreLam1") e
{-# INLINE acoreLam1 #-}

acoreLamTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [(HsName,t)] -> e -> e
acoreLamTy as e = foldr (\(n,t) e -> acoreLam1Ty n t e) e as

acoreLam :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [HsName] -> e -> e
acoreLam as e = foldr (\(n) e -> acoreLam1 n e) e as

{-# LINE 452 "src/ehc/AbstractCore.chs" #-}
acoreTagTup :: (AbstractCore e m b basp bcat mbind t p pr pf a) => CTag -> [e] -> e
acoreTagTup tg es = acoreTagTupTy tg (acoreTyErr "acoreTupTy") es

acoreTupTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => t -> [e] -> e
acoreTupTy t es = acoreTagTupTy CTagRec t es
{-# INLINE acoreTupTy #-}

acoreTup :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [e] -> e
acoreTup es = acoreTagTup CTagRec es
{-# INLINE acoreTup #-}

acoreTag :: (AbstractCore e m b basp bcat mbind t p pr pf a) => CTag -> e
acoreTag tg = acoreTagTup tg []
{-# INLINE acoreTag #-}

{-# LINE 480 "src/ehc/AbstractCore.chs" #-}
acoreBind1CatLevMetaTyWith :: (AbstractCore e m b basp bcat mbind t p pr pf a) => (t->t) -> (e->e) -> bcat -> HsName -> MetaLev -> m -> t -> e -> b
acoreBind1CatLevMetaTyWith mkT mkE cat n l m t e = acoreBind1CatLevMetasTy cat n l (acoreMetabindDflt,m) (mkT t) (mkE e)
{-# INLINE acoreBind1CatLevMetaTyWith #-}

acoreBind1CatLevMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> m -> t -> e -> b
acoreBind1CatLevMetaTy = acoreBind1CatLevMetaTyWith id id
{-# INLINE acoreBind1CatLevMetaTy #-}

acoreBind1CatLevTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> t -> e -> b
acoreBind1CatLevTy cat n l t e = acoreBind1CatLevMetaTy cat n l acoreMetavalDflt t e
{-# INLINE acoreBind1CatLevTy #-}

acoreBind1CatMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> m -> t -> e -> b
acoreBind1CatMetaTy cat n m t e = acoreBind1CatLevMetaTy cat n metaLevVal m t e
{-# INLINE acoreBind1CatMetaTy #-}

acoreBind1CatTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> t -> e -> b
acoreBind1CatTy cat n t e = acoreBind1CatLevTy cat n metaLevVal t e
{-# INLINE acoreBind1CatTy #-}

acoreBind1Ty :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> b
acoreBind1Ty n t e = acoreBind1CatTy (acoreBindcategDflt e) n t e
{-# INLINE acoreBind1Ty #-}

acoreBind1Cat :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> e -> b
acoreBind1Cat cat n e = acoreBind1CatTy cat n acoreTyNone {- (acoreTyErr "acoreBind1Cat") -} e
{-# INLINE acoreBind1Cat #-}

acoreBind1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> b
acoreBind1 n e = acoreBind1Cat (acoreBindcategDflt e) n e
{-# INLINE acoreBind1 #-}

{-# LINE 514 "src/ehc/AbstractCore.chs" #-}
acoreBind1MetasTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> (mbind,m) -> t -> e -> b
acoreBind1MetasTy n m t e = acoreBind1CatLevMetasTy (acoreBindcategDflt e) n metaLevVal m t e
{-# INLINE acoreBind1MetasTy #-}

{-
acoreBind1Metas :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> (mbind,m) -> e -> b
acoreBind1Metas n m e = aacoreBind1MetasTy n m acoreTyNone e
{-# INLINE acoreBind1Metas #-}
-}

acoreBind1CatMeta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> m -> e -> b
acoreBind1CatMeta cat n m e = acoreBind1CatLevMetaTy cat n metaLevVal m (acoreTyErr "acoreBind1CatMeta") e
{-# INLINE acoreBind1CatMeta #-}

acoreBind1MetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> t -> e -> b
acoreBind1MetaTy n m t e = acoreBind1MetasTy n (acoreMetabindDflt,m) t e
{-# INLINE acoreBind1MetaTy #-}

{-
acoreBind1Meta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> e -> b
acoreBind1Meta n m e = acoreBind1MetaTy n m acoreTyNone e
{-# INLINE acoreBind1Meta #-}
-}


{-# LINE 541 "src/ehc/AbstractCore.chs" #-}
acoreBind1Asp1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> basp -> b
acoreBind1Asp1 n ba = acoreBind1Asp n [ba]
{-# INLINE acoreBind1Asp1 #-}

acoreBind1Nm1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> b
acoreBind1Nm1 n = acoreBind1Asp n []
{-# INLINE acoreBind1Nm1 #-}

{-# LINE 555 "src/ehc/AbstractCore.chs" #-}
acoreBoundVal1CatLevMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> m -> t -> e -> basp
acoreBoundVal1CatLevMetaTy bcat n mlev m t e = acoreBoundVal1CatLevMetasTy bcat n mlev (acoreMetabindDflt,m) t e
{-# INLINE acoreBoundVal1CatLevMetaTy #-}

acoreBoundVal1CatLevTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> t -> e -> basp
acoreBoundVal1CatLevTy cat n l t e = acoreBoundVal1CatLevMetaTy cat n l acoreMetavalDflt t e
{-# INLINE acoreBoundVal1CatLevTy #-}

acoreBoundVal1CatMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> m -> t -> e -> basp
acoreBoundVal1CatMetaTy cat n m t e = acoreBoundVal1CatLevMetaTy cat n metaLevVal m t e
{-# INLINE acoreBoundVal1CatMetaTy #-}

acoreBoundVal1CatTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> t -> e -> basp
acoreBoundVal1CatTy cat n t e = acoreBoundVal1CatLevTy cat n metaLevVal t e
{-# INLINE acoreBoundVal1CatTy #-}

acoreBoundVal1Cat :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> e -> basp
acoreBoundVal1Cat cat n e = acoreBoundVal1CatTy cat n (acoreTyErr "acoreBoundVal1Cat") e
{-# INLINE acoreBoundVal1Cat #-}

{-# LINE 577 "src/ehc/AbstractCore.chs" #-}
acoreBoundVal1Metas :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> (mbind,m) -> e -> basp
acoreBoundVal1Metas n m e = acoreBoundVal1CatLevMetasTy (acoreBindcategDflt e) n metaLevVal m (acoreTyErr "acoreBoundVal1Metas") e
{-# INLINE acoreBoundVal1Metas #-}

acoreBoundVal1Meta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> e -> basp
acoreBoundVal1Meta n m e = acoreBoundVal1Metas n (acoreMetabindDflt,m) e
{-# INLINE acoreBoundVal1Meta #-}


{-# LINE 588 "src/ehc/AbstractCore.chs" #-}
acoreBound1Val :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> basp
acoreBound1Val e = acoreBound1AspkeyVal acbaspkeyDefault e
{-# INLINE acoreBound1Val #-}

{-# LINE 604 "src/ehc/AbstractCore.chs" #-}
acoreTyLift :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => String -> f x -> f (x,t)
acoreTyLift msg = fmap (\n -> (n,acoreTyErr msg))
{-# INLINE acoreTyLift #-}

{-# LINE 614 "src/ehc/AbstractCore.chs" #-}
-- | Construct let, possibly merging bindings
acoreLetMerge :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => Bool -> bcat -> [b] -> e -> e
acoreLetMerge merge c bs e
  = if null bs
    then e
    else case acoreBindcategMbStrict c of
           {-
           Just _
             ->
           -}
           _ -> case acoreExprMbLet e of
                  Just (c',bs',e') | merge && c' == c
                    -> mk c (bs++bs') e'
                  _ -> mk c bs e
  where mk c bs e
          = case acoreBindcategMbRec c of
              Just c -> acoreLetBase c bs e
              _      -> foldr (\b e -> acoreLetBase c [b] e) e bs

acoreLet :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> [b] -> e -> e
acoreLet c bs e = acoreLetMerge False c bs e
{-# INLINE acoreLet #-}

acoreLetRec :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => [b] -> e -> e
acoreLetRec bs e = acoreLet (acoreBindcategRec) bs e
{-# INLINE acoreLetRec #-}

{-# LINE 643 "src/ehc/AbstractCore.chs" #-}
acoreLetN :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => [(bcat,[b])] -> e -> e
acoreLetN cbs e = foldr (\(c,bs) e -> acoreLet c bs e) e cbs

{-# LINE 648 "src/ehc/AbstractCore.chs" #-}
acoreLet1PlainTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1PlainTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategPlain

acoreLet1Plain :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e -> e
acoreLet1Plain nm e = acoreLet1PlainTy nm (acoreTyErr "acoreLet1Plain") e
{-# INLINE acoreLet1Plain #-}

{-# LINE 659 "src/ehc/AbstractCore.chs" #-}
acoreLet1StrictTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1StrictTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategStrict

acoreLet1Strict :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e -> e
acoreLet1Strict nm e = acoreLet1StrictTy nm (acoreTyErr "acoreLet1Strict") e
{-# INLINE acoreLet1Strict #-}

{-# LINE 670 "src/ehc/AbstractCore.chs" #-}
-- | evaluate an expr, with a continuation for the evaluated expr
acoreLet1StrictInMetaTyWith :: (AbstractCore e m b basp bcat mbind t p pr pf a) => (t->t) -> (e->e) -> HsName -> m -> t -> e -> (e -> e) -> e
acoreLet1StrictInMetaTyWith mkT mkE nm m t e mkC
  = acoreLetBase cat [acoreBind1CatMetaTy cat nm m (mkT t) (mkE e)] (mkC (acoreVar nm))
  where cat = acoreBindcategStrict

acoreMbLet1StrictInMetaTyWith :: (AbstractCore e m b basp bcat mbind t p pr pf a) => (t->t) -> (e->e) -> Maybe (HsName,t) -> m -> e -> (e -> e) -> e
acoreMbLet1StrictInMetaTyWith mkT mkE (Just (nm,t)) m e mkC = acoreLet1StrictInMetaTyWith mkT mkE nm m t e mkC
acoreMbLet1StrictInMetaTyWith _   _   _             m e mkC = mkC e

acoreLet1StrictInMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> t -> e -> (e -> e) -> e
acoreLet1StrictInMetaTy = acoreLet1StrictInMetaTyWith acoreTyUnThunk acoreExprUnThunk
{-# INLINE acoreLet1StrictInMetaTy #-}

acoreLet1StrictInMeta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> e -> (e -> e) -> e
acoreLet1StrictInMeta nm m e mkC = acoreLet1StrictInMetaTy nm m (acoreTyErr "acoreLet1StrictInMeta") e mkC
{-# INLINE acoreLet1StrictInMeta #-}

acoreLet1StrictInTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> (e -> e) -> e
acoreLet1StrictInTy nm t e mkC = acoreLet1StrictInMetaTy nm acoreMetavalDflt t e mkC
{-# INLINE acoreLet1StrictInTy #-}

acoreLet1StrictIn :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> (e -> e) -> e
acoreLet1StrictIn nm e mkC = acoreLet1StrictInMeta nm acoreMetavalDflt e mkC
{-# INLINE acoreLet1StrictIn #-}

{-# LINE 702 "src/ehc/AbstractCore.chs" #-}
acoreNmHole :: (AbstractCore e m b basp bcat mbind t p pr pf a) => UID -> e
acoreNmHole = acoreVar . mkHNm

acoreNmHolePred :: (AbstractCore e m b basp bcat mbind t p pr pf a) => PredOccId -> e
acoreNmHolePred = acoreNmHole . poiId

{-# LINE 714 "src/ehc/AbstractCore.chs" #-}
-- | get default for bindcateg
acoreBindcategDflt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> bcat
acoreBindcategDflt _ = acoreBindcategPlain
{-# INLINE acoreBindcategDflt #-}

{-# LINE 725 "src/ehc/AbstractCore.chs" #-}
acoreChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Char -> e
acoreChar i = let x = acoreCharTy acoreTyChar i in x

acoreInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Int -> e
acoreInt i = let x = acoreIntTy acoreTyInt i in x

acoreInt2 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Integer -> e
acoreInt2 i = let x = acoreIntTy2 acoreTyInt i in x

{-# LINE 740 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> (EHBuiltinNames -> HsName) -> [e] -> e
acoreBuiltinApp opts bnmOf args = acoreVar (ehcOptBuiltin opts bnmOf) `acoreApp` args

{-# LINE 745 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinAddInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinAddInt opts e i
  = if i == 0
    then e
    else case acoreExprMbInt e of
           Just (t,i') -> acoreIntTy2 t (toInteger i + i')
           _           -> acoreBuiltinApp opts ehbnPrimAddInt [e,acoreInt i]

{-# LINE 755 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinGtInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinGtInt opts e i = acoreBuiltinApp opts ehbnPrimGtInt [e,acoreInt i]

{-# LINE 760 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinEqChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Char -> e -> e
acoreBuiltinEqChar opts c e = acoreBuiltinApp opts ehbnPrimEqChar [e,acoreChar c]

{-# LINE 765 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinString :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinString opts m = let x = acoreBuiltinApp opts ehbnPackedStringToString [acoreStringTy (acoreTyString opts) m] in x

{-# LINE 770 "src/ehc/AbstractCore.chs" #-}
acoreBuiltinError :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinError opts m = acoreBuiltinApp opts ehbnError [acoreBuiltinString opts m]

acoreBuiltinUndefined :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e
acoreBuiltinUndefined opts = acoreBuiltinApp opts ehbnUndefined []

{-# LINE 778 "src/ehc/AbstractCore.chs" #-}
-- | Builtin Integer
acoreBuiltinInteger :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Integer -> e
acoreBuiltinInteger opts i = acoreBuiltinApp opts ehbnPackedStringToInteger [acoreStringTy (acoreTyString opts) (show i)]

{-# LINE 784 "src/ehc/AbstractCore.chs" #-}
-- | Builtin list singleton (note: hardcoded of tags)
acoreBuiltinListSingleton :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> e
acoreBuiltinListSingleton opts e
  = acoreTagTupTy (ctagCons opts) (acoreTyErr "acoreBuiltinListSingleton.Cons") [e, acoreTagTupTy (ctagNil opts) (acoreTyErr "acoreBuiltinListSingleton.Nil") []]

{-# LINE 795 "src/ehc/AbstractCore.chs" #-}
-- | when pat is con get tag
acorePatConMbTag :: (AbstractCore e m b basp bcat mbind t p pr pf a) => p -> Maybe CTag
acorePatConMbTag = fmap (\(tg,_,_) -> tg) . acorePatMbCon

-- | possibly get tag of alt
acoreAltMbTag :: (AbstractCore e m b basp bcat mbind t p pr pf a) => a -> Maybe CTag
acoreAltMbTag = (\p ->     (\(tg,_,_) -> tg) <$> acorePatMbCon  p
                       <|> (const ctagInt)   <$> acorePatMbInt  p
                       -- <|> (const ctagChar)  <$> acorePatMbChar p
                ) . fst . acoreUnAlt

{-# LINE 812 "src/ehc/AbstractCore.chs" #-}
-- | bound name of binding
acoreBindNm :: (AbstractCore e m b basp bcat mbind t p pr pf a) => b -> HsName
acoreBindNm = fst . acoreUnBind
{-# INLINE acoreBindNm #-}

{-# LINE 823 "src/ehc/AbstractCore.chs" #-}
-- | bound name of binding
acorePatFldTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => t -> (HsName,e) -> HsName -> pf
acorePatFldTy _ lbloff n = acorePatFldBind lbloff (acoreBind1Nm1 n)
{-# INLINE acorePatFldTy #-}

{-# LINE 834 "src/ehc/AbstractCore.chs" #-}
-- | possible expr of bound (may panic)
acoreUnBoundVal :: (AbstractCore e m b basp bcat mbind t p pr pf a) => basp -> e
acoreUnBoundVal = maybe (panic "acoreBoundMbVal") snd . acoreBoundMbVal
{-# INLINE acoreUnBoundVal #-}

{-# LINE 845 "src/ehc/AbstractCore.chs" #-}
-- | Construct 'if' expression. Hardcoded: tag nr, ordered alts (by tag)
acoreIf :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Maybe HsName -> e -> e -> e -> e
acoreIf opts cn c t f
  = acoreMbLet1StrictInMetaTyWith id id (fmap (\n -> (n,acoreTyBool opts)) cn) acoreMetavalDflt c
    $ (\c -> acoreCaseDflt c
               [ acoreAlt (acorePatCon (ctagFalse opts) acorePatRestEmpty []) f
               , acoreAlt (acorePatCon (ctagTrue  opts) acorePatRestEmpty []) t
               ]
               Nothing {-(tcUndefined opts)-}
      )

{-# LINE 858 "src/ehc/AbstractCore.chs" #-}
acoreMatchChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Maybe HsName -> Char -> e -> e -> e -> e
acoreMatchChar opts cn cchar cexpr t f
  = acoreIf opts cn (acoreBuiltinEqChar opts cchar cexpr) t f

{-# LINE 888 "src/ehc/AbstractCore.chs" #-}
data Coe' expr metaval bind bindasp ty
  = Coe_Map      		!(expr -> expr)					-- normal, expression as function
  | Coe_C        		!expr							-- constant
  | Coe_Compose  		!(Coe' expr metaval bind bindasp ty)	-- composition
                        !(Coe' expr metaval bind bindasp ty)
  | Coe_App1     		!expr       					-- apply
  | Coe_App      		[HsName]						-- apply n args
  | Coe_Lam      		!HsName !ty						-- lambda
  | Coe_CloseExists		!TyVarId !ty !ty				-- closing existential
  | Coe_OpenExists		!TyVarId !ty !ty				-- opening existential
  | Coe_LamLet   		!HsName !ty !UID				-- lambda with a let binding in the body
  | Coe_LetRec   		![bind]							-- let rec
  | Coe_ImplApp  		!ImplsVarId						-- implicits, for apply
  | Coe_ImplLam  		!ImplsVarId						-- implicits, for lambda

instance Show (Coe' expr metaval bind bindasp ty) where
  show _ = "COE"

{-# LINE 914 "src/ehc/AbstractCore.chs" #-}
-- | Non inspectable, most general, coercion
acoreCoeMap :: (e -> e) -> Coe' e m b ba t
acoreCoeMap = Coe_Map
{-# INLINE acoreCoeMap #-}

-- | Coe identity
acoreCoeId :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t
acoreCoeId = Coe_C acoreCoeArg
{-# INLINE acoreCoeId #-}

{-# LINE 926 "src/ehc/AbstractCore.chs" #-}
acoreCoeLamLetTy :: HsName -> t -> UID -> Coe' e m b ba t
acoreCoeLamLetTy = Coe_LamLet
{-# INLINE acoreCoeLamLetTy #-}

acoreCoeLamLet :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> UID -> Coe' e m b ba t
acoreCoeLamLet n u = acoreCoeLamLetTy n (acoreTyErr "acoreCoeLamLet") u
{-# INLINE acoreCoeLamLet #-}

-- | Let still requiring a body
acoreCoeLetRec :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [b] -> Coe' e m b ba t
acoreCoeLetRec [] = acoreCoeId
acoreCoeLetRec b  = Coe_LetRec b

{-# LINE 941 "src/ehc/AbstractCore.chs" #-}
-- | Application still requiring a function
acoreCoeApp1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> Coe' e m b ba t
acoreCoeApp1 = Coe_App1 -- a acoreMetavalDflt
{-# INLINE acoreCoeApp1 #-}

acoreCoeAppNbyName :: [(HsName)] -> Coe' e m b ba t
acoreCoeAppNbyName = Coe_App
{-# INLINE acoreCoeAppNbyName #-}

-- acoreCoeApp2 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [(e)] -> Coe' e m b ba t
-- acoreCoeApp2 as = acoreCoeMap (\e -> acoreApp e as)

acoreCoeAppN :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [e] -> Coe' e m b ba t
acoreCoeAppN as = acoreCoeMap (\e -> acoreApp e as)
{-# INLINE acoreCoeAppN #-}

{-# LINE 959 "src/ehc/AbstractCore.chs" #-}
-- | Lambda still requiring a body
acoreCoeLam1Ty :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> Coe' e m b ba t
acoreCoeLam1Ty = Coe_Lam
{-# INLINE acoreCoeLam1Ty #-}

acoreCoeLam1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> Coe' e m b ba t
acoreCoeLam1 n = acoreCoeLam1Ty n (acoreTyErr "acoreCoeLam1")
{-# INLINE acoreCoeLam1 #-}

{-# LINE 970 "src/ehc/AbstractCore.chs" #-}
-- | Composition of 2 Coe's
acoreCoeCompose :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> Coe' e m b ba t -> Coe' e m b ba t
acoreCoeCompose c1 c2
  | acoreCoeIsId c1 = c2
  | otherwise  = Coe_Compose c1 c2


{-# LINE 979 "src/ehc/AbstractCore.chs" #-}
acoreCoePoiLApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [PredOccId] -> [Coe' e m b ba t]
acoreCoePoiLApp = map (\i -> acoreCoeApp1 (acoreNmHolePred i))

acoreCoeImplsApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Impls -> [Coe' e m b ba t]
acoreCoeImplsApp = acoreCoePoiLApp . implsPrIds

{-# LINE 987 "src/ehc/AbstractCore.chs" #-}
acoreCoePoiLLamTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> [(PredOccId,t)] -> [Coe' e m b ba t]
acoreCoePoiLLamTy onLast poiL
  =  case map mk poiL of
       l@(_:_)            -> h ++ [t `acoreCoeCompose` onLast]
                          where (h,t) = fromJust $ initlast l
       _ | acoreCoeIsId onLast -> []
         | otherwise      -> [onLast]
  where mk (poi,ty) = acoreCoeLam1Ty (poiHNm poi) ty

acoreCoePoiLLam :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> [(PredOccId)] -> [Coe' e m b ba t]
acoreCoePoiLLam onLast poiL = acoreCoePoiLLamTy onLast (acoreTyLift "acoreCoePoiLLam" poiL)

acoreCoeImplsLam :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> Impls -> [Coe' e m b ba t]
acoreCoeImplsLam onLast is = acoreCoePoiLLam onLast (implsPrIds is)

{-# LINE 1008 "src/ehc/AbstractCore.chs" #-}
acoreCoeIsId :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> Bool
acoreCoeIsId (Coe_C e) = acoreExprIsCoeArg e
acoreCoeIsId _         = False

{-# LINE 1018 "src/ehc/AbstractCore.chs" #-}
data CSubstKey
  = CSKey_UID   UID
  | CSKey_Nm    HsName
  deriving (Show,Eq,Ord)

{-# LINE 1025 "src/ehc/AbstractCore.chs" #-}
data CSubstInfo' expr metaval bind bindasp ty
  =  CSITy        { csiTy      :: !ty
                  }
  |  CSIExpr      { csiRepl    :: !expr
                  }
  |  CSIImpls     { csiAppCoeL :: ![Coe' expr metaval bind bindasp ty]
                  , csiLamCoeL :: ![Coe' expr metaval bind bindasp ty]
                  }
  |  CSIBinds     { csiBindL   :: ![bind]
                  }

instance Show (CSubstInfo' e m b ba t) where
  show _ = "CSubstInfo'"

{-# LINE 1043 "src/ehc/AbstractCore.chs" #-}
type CSubst' e m b ba t = Map.Map CSubstKey (CSubstInfo' e m b ba t)

emptyCSubst :: CSubst' e m b ba t
emptyCSubst = Map.empty

{-# LINE 1054 "src/ehc/AbstractCore.chs" #-}
acoreCSubstFromUidTyL :: AssocL HsName t -> CSubst' e m b ba t
acoreCSubstFromUidTyL l = Map.fromList [ (CSKey_Nm k,CSITy v) | (k,v) <- l ]

{-# LINE 1059 "src/ehc/AbstractCore.chs" #-}
acoreCSubstFromUidExprL :: AssocL UID e -> CSubst' e m b ba t
acoreCSubstFromUidExprL l = Map.fromList [ (CSKey_UID k,CSIExpr v) | (k,v) <- l ]

{-# LINE 1064 "src/ehc/AbstractCore.chs" #-}
acoreCSubstFromUidBindLL :: AssocL UID [b] -> CSubst' e m b ba t
acoreCSubstFromUidBindLL l = Map.fromList [ (CSKey_UID k,CSIBinds v) | (k,v) <- l ]

acoreCSubstFromUidImplsL :: AssocL UID ([Coe' e m b ba t],[Coe' e m b ba t]) -> CSubst' e m b ba t
acoreCSubstFromUidImplsL l = Map.fromList [ (CSKey_UID k,uncurry CSIImpls v) | (k,v) <- l ]

{-# LINE 1076 "src/ehc/AbstractCore.chs" #-}
-- | Combine CSubst: union only, application is postponed
cSubstAppSubst :: CSubst' e m b ba t -> CSubst' e m b ba t -> CSubst' e m b ba t
cSubstAppSubst = Map.union

{-# LINE 1086 "src/ehc/AbstractCore.chs" #-}
infixr `cSubstApp`

class CSubstitutable       e m b ba t a
                   | e  ->   m b ba t
                   , m  -> e
                   , b  -> e
                   , ba -> e
                   , t  -> e
                   , a  -> e m b ba t
  where
  cSubstApp :: CSubst' e m b ba t -> a -> a

instance CSubstitutable e m b ba t (CSubst' e m b ba t) where
  cSubstApp cs s = cs `cSubstAppSubst` s

{-# LINE 1107 "src/ehc/AbstractCore.chs" #-}
data RAlt' e t b pr
  = RAlt_Alt			{ rcaPats :: ![RPat' e t b pr], raaExpr :: !e, raaFailS :: UIDS }

data RPat' e t b pr
  = RPat_Var			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpMustEval :: Bool }
  | RPat_Con			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpTag :: !CTag, rcpBinds :: !(RPatConBind' e t b pr) }
  | RPat_Int			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpInt :: !Integer }
  | RPat_Char			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpChar :: !Char }
  | RPat_Irrefutable	{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpValBindL :: ![b] }
  | RPat_BoolExpr		{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpExpr :: !e, rcpMbConst :: Maybe SrcConst }

data RPatConBind' e t b pr
  = RPatConBind_One		{ rpcbRest :: !pr, rpcbBinds :: ![RPatFld' e t b pr] }
  | RPatConBind_Many	{ rpcbConBinds :: ![RPatConBind' e t b pr] }

data RPatFld' e t b pr
  = RPatFld_Fld		    { rpbLbl :: !HsName, rpbOffset :: !e, rpbNm :: !HsName, rpbPat :: !(RPat' e t b pr)}

type RCEAltL' e t b pr = [RAlt' e t b pr]

{-# LINE 1131 "src/ehc/AbstractCore.chs" #-}
rcaPat :: RAlt' e t b pr -> RPat' e t b pr
rcaPat = head . rcaPats

raltLPatNms :: [RAlt' e t b pr] -> [RPatNm]
raltLPatNms = nub . sort . map (rcpPNm . rcaPat)

{-# LINE 1139 "src/ehc/AbstractCore.chs" #-}
rpatConTag :: RPat' e t b pr -> CTag
rpatConTag (RPat_Int  _ _ _ )  = ctagInt
rpatConTag (RPat_Char _ _ _ )  = ctagChar
rpatConTag p                   = rcpTag p

rcaTag :: RAlt' e t b pr -> CTag
rcaTag = rpatConTag . head . rcaPats

{-# LINE 1149 "src/ehc/AbstractCore.chs" #-}
raltIsVar :: RAlt' e t b pr -> Bool
raltIsVar (RAlt_Alt (RPat_Var _ _ _ : _) _ _)  = True
raltIsVar _                                    = False

raltIsConst :: RAlt' e t b pr -> Bool
raltIsConst (RAlt_Alt (p : _) _ _)
  = c p
  where c (RPat_Int   _ _ _) = True
        c (RPat_Char  _ _ _) = True
        c _                  = False

{-# LINE 1162 "src/ehc/AbstractCore.chs" #-}
raltIsConMany :: RAlt' e t b pr -> Bool
raltIsConMany (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_Many _) : _) _ _) = True
raltIsConMany _                                                        = False

{-# LINE 1168 "src/ehc/AbstractCore.chs" #-}
raltIsIrrefutable :: RAlt' e t b pr -> Bool
raltIsIrrefutable (RAlt_Alt (RPat_Irrefutable _ _ _ : _) _ _) = True
raltIsIrrefutable _                                           = False

{-# LINE 1174 "src/ehc/AbstractCore.chs" #-}
raltMbBoolExpr :: RAlt' e t b pr -> Maybe (Maybe SrcConst)
raltMbBoolExpr (RAlt_Alt (RPat_BoolExpr _ _ _ e : _) _ _)  = Just e
raltMbBoolExpr _                                           = Nothing

raltIsBoolExpr :: RAlt' e t b pr -> Bool
raltIsBoolExpr = isJust . raltMbBoolExpr

{-# LINE 1185 "src/ehc/AbstractCore.chs" #-}
rpatConBindUnFlatten :: RPatConBind' e t b pr -> [RPatConBind' e t b pr] -> RPatConBind' e t b pr
rpatConBindUnFlatten z []  = z
rpatConBindUnFlatten _ [b] = b
rpatConBindUnFlatten _ bs  = RPatConBind_Many bs

{-# LINE 1196 "src/ehc/AbstractCore.chs" #-}
acoreRPat2Pat :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPat' e t b pr -> p
acoreRPat2Pat p
  = case p of
      RPat_Var      n ty _    -> acorePatVarTy  (rpatNmNm n) ty
      RPat_Con      n _ t b   -> acorePatCon    t r bs
                              where (r,bs) = acoreRPatConBind2PatConBind b
      RPat_Int      n ty v    -> acorePatIntTy2 ty v
      RPat_Char     n ty v    -> acorePatCharTy ty v
      RPat_BoolExpr n _  v _  -> acorePatBoolExpr  v

{-# LINE 1210 "src/ehc/AbstractCore.chs" #-}
acoreRPatConBind2PatConBind :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPatConBind' e t b pr -> (pr,[pf])
acoreRPatConBind2PatConBind b
  = case b of
  	  RPatConBind_One 	r bs 	-> (r,map acoreRPatBind2PatFld bs)
  	  RPatConBind_Many 	bs 		-> head (map acoreRPatConBind2PatConBind bs)

acoreRPatBind2PatFld :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPatFld' e t b pr -> pf
acoreRPatBind2PatFld (RPatFld_Fld l o _ p@(RPat_Var n _ _)) = acorePatFldTy (rcpTy p) (l,o) (rpatNmNm n)

{-# LINE 1227 "src/ehc/AbstractCore.chs" #-}
ctagTrue, ctagFalse :: EHCOpts -> CTag
ctagTrue  opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolTrue)  1 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagFalse opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolFalse) 0 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!

{-# LINE 1239 "src/ehc/AbstractCore.chs" #-}
ctagCons, ctagNil :: EHCOpts -> CTag
ctagCons opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltCons) 0 2 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagNil  opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltNil ) 1 0 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!

{-# LINE 1249 "src/ehc/AbstractCore.chs" #-}
-- | Reason to fail a case alternative
data CaseAltFailReason
  = CaseAltFailReason_Absence					-- failed because of absence
  | CaseAltFailReason_Continue
      { cafailCaseId		:: UID				-- failed as part of case match attempt, but continues with code identified by id
      }
  deriving (Show,Eq,Ord)

instance PP CaseAltFailReason where
  pp (CaseAltFailReason_Continue i) = pp i
  pp (CaseAltFailReason_Absence   ) = pp "absent"

{-# LINE 1263 "src/ehc/AbstractCore.chs" #-}
cafailHasId :: CaseAltFailReason -> (Bool,UID)
cafailHasId (CaseAltFailReason_Absence   ) = (False,uidUnused)
cafailHasId (CaseAltFailReason_Continue i) = (True ,i)

{-# LINE 1269 "src/ehc/AbstractCore.chs" #-}
deriving instance Typeable CaseAltFailReason
deriving instance Data CaseAltFailReason

{-# LINE 1278 "src/ehc/AbstractCore.chs" #-}
data AppFunKind
  = AppFunKind_NoApp					-- inlined Nothing
  | AppFunKind_Fun 	ACoreBindRef
  | AppFunKind_Tag 	CTag
  | AppFunKind_FFI

{-# LINE 1290 "src/ehc/AbstractCore.chs" #-}
data WhatExpr
  = ExprIsLam
  | ExprIsApp	Int			-- arity
  | ExprIsVar 	HsName
  | ExprIsInt 	Int
  | ExprIsOther
  | ExprIsBind
  deriving Eq

{-# LINE 1301 "src/ehc/AbstractCore.chs" #-}
-- | is an app?
whatExprMbApp :: WhatExpr -> Maybe Int
whatExprMbApp (ExprIsApp a) = Just a
whatExprMbApp _             = Nothing

-- | app arity
whatExprAppArity :: WhatExpr -> Int
whatExprAppArity (ExprIsApp a) = a
whatExprAppArity _             = 0

{-# LINE 1317 "src/ehc/AbstractCore.chs" #-}
instance Serialize ACoreBindAspectKey where
  sput = sputEnum8
  sget = sgetEnum8

{-# LINE 1323 "src/ehc/AbstractCore.chs" #-}
instance Serialize ACoreBindRef where
  sput (ACoreBindRef a b) = sput a >> sput b
  sget = liftM2 ACoreBindRef sget sget

{-# LINE 1329 "src/ehc/AbstractCore.chs" #-}
instance Serialize CaseAltFailReason where
  sput (CaseAltFailReason_Continue a) = sputWord8 0 >> sput a
  sput (CaseAltFailReason_Absence   ) = sputWord8 1
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  CaseAltFailReason_Continue sget
      1 -> return CaseAltFailReason_Absence

{-# LINE 1344 "src/ehc/AbstractCore.chs" #-}
instance PP CSubstKey where
  pp (CSKey_UID i)  = pp i
  pp (CSKey_Nm  n)  = pp n

{-# LINE 1350 "src/ehc/AbstractCore.chs" #-}
instance (PP expr, PP ty) => PP (CSubstInfo' expr metaval bind bindasp ty) where
  pp (CSITy         t    )  = pp t
  pp (CSIExpr       e    )  = pp e
  pp (CSIImpls      l r  )  = pp "CSIImpls" -- pp (fst $ coeWeaveOnAsSubst uidStart l r CExpr_CoeArg)
  pp (CSIBinds      b    )  = pp "CSIBinds" -- ppCBindL b

