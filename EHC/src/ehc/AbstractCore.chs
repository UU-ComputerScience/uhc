%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core and TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}AbstractCore}
%%]

%%[8 import({%{EH}Base.Common})
%%]

%%[(8 codegen) import({%{EH}Base.HsName.Builtin},{%{EH}Base.TermLike},{%{EH}Opts.Base},{%{EH}Ty})
%%]

%%[(8 codegen) import(UHC.Util.Pretty,UHC.Util.Utils)
%%]

%%[(8 codegen) import(Data.List, Data.Maybe, qualified Data.Map as Map, qualified Data.Set as Set, Control.Applicative((<|>),(<$>)))
%%]

%%[(50 codegen) hs import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AbstractCore class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
AbstractCore factors out the commonalities between Core and TyCore, so more code can be shared.

20100514 AD: this is work in progress, not finished.
AbstractCore is gradually replacing its monomorphic equivalents, so that someday, in one go,
all utility functions can be abstracted over the actual core.
%%]

%%[(8 codegen) export(AbstractCore(..))
class AbstractCore  expr bind bound boundmeta bindcateg ty pat patrest patfld alt
    | expr       ->      bind bound boundmeta bindcateg ty pat patrest patfld alt
    , bind       -> expr
    , bound      -> expr
    , boundmeta  -> expr
    , bindcateg  -> expr
    , ty         -> expr
    , pat        -> expr
    , patrest    -> expr
    , patfld     -> expr
    , alt        -> expr
  where
  ------------------------- constructing: expr -------------------------
  -- | 1 arg application, together with meta info about the argument, packaged in the bind
  acoreLam1Bind :: bind -> expr -> expr
  
  -- | 1 arg application, together with meta info about the argument
  -- acoreLam1Ty :: HsName -> ty -> expr -> expr
  
  -- | 1 lam abstraction, together with meta info about, and type of the argument
  acore1AppBound :: expr -> bound -> expr
  
  -- | a tuple, with tag, and ty
  acoreTagTyTupBound :: CTag -> ty -> [bound] -> expr
  
  -- | a value binding, for a name to value + type + metas + meta level
  acoreBind1CatLevTy :: bindcateg -> HsName -> MetaLev -> ty -> expr -> bind
  
  -- | a value binding aspect, for a name + value, optionally type + metas + meta level
  acoreBoundVal1CatLevTy :: bindcateg -> HsName -> MetaLev -> ty -> expr -> bound
  
  -- | a type for value binding aspect, for a name + type, optionally meta level
  acoreBoundValTy1CatLev :: bindcateg -> HsName -> MetaLev -> ty -> bound
  
  -- | meta for something bound; for a name, meta level and label
  acoreBoundmeta :: ACoreBindAspectKeyS -> MetaLev -> CLbl -> boundmeta
  
  -- | a expr binding aspect, for a name, meta level and label
  acoreBound1Boundmeta :: boundmeta -> expr -> bound
  
  -- | a binding, for/from a single aspect (for now, later multiple)
  acoreBind1Asp :: HsName -> [bound] -> bind
  
  -- | basic let binding
  acoreLetBase :: bindcateg -> [bind] -> expr -> expr
  
  -- | cast, defaults to noop
  acoreCast :: ty -> expr -> expr
  acoreCast _ e = e
  
  -- | A Case expression, possibly with a default value.
  acoreCaseDflt  :: expr    -- ^ The scrutinee. Required to be in WHNF.
        -> [alt]            -- ^ The alternatives.
        -> Maybe expr       -- ^ The default value. (TODO what is the behaviour if it is Nothing?)
        -> expr

  -- | Creates a variable expression.
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
  
%%[[9
  -- | hole: let with hole for bindings to be filled in later by means of a CSubst
  acoreHoleLet :: UID -> expr -> expr
%%]]

  -- | a default, fallback
  -- acoreDflt :: expr

  -- | get error/default expr
  acoreExprErr :: String -> expr
  acoreExprErr s = panic $ "AbstractCore.acoreExprErr: " ++ s

%%[[(8 coresysf)
  ------------------------- constructing: meta level expr (i.e. ty represented as expr) -------------------------
  -- | arrow
  acorem1Arr :: bind -> expr -> expr
%%]]

  ------------------------- constructing: ty constants -------------------------
  -- Int
  -- acoreTyInt2 :: ty
  
  -- Bool
  acoreTyBool :: EHCOpts -> ty
  
  ------------------------- constructing: pat -------------------------
  -- | pat var, with type
  acorePatVarTy :: HsName -> ty -> pat
  
  -- | Matches the case scrutinee with the given constructor tag.
  acorePatCon :: CTag   -- ^ The constructor to match.
    -> patrest          -- ^ ???
    -> [patfld]         -- ^ ???
    -> pat

  -- | pat int
  acorePatIntTy :: ty -> Int -> pat

  -- | pat Integer
  acorePatIntTy2 :: ty -> Integer -> pat

  -- | pat char
  acorePatCharTy :: ty -> Char -> pat

%%[[97
  -- | pat boolean guard
  acorePatBoolExpr :: expr -> pat
%%]]
  ------------------------- constructing: pat field -------------------------
  -- | TODO ??? pat field
  acorePatFldBind :: (HsName,expr)  -- ^ lbl, offset ???
    -> bind     -- ^ ??
    -> patfld
  -- acorePatFldTy :: ty -> (HsName,expr) -> HsName -> patfld

  ------------------------- constructing: patrest -------------------------
  -- | patrest, empty TODO what does it mean?
  acorePatRestEmpty :: patrest

  -- | patrest, var
  acorePatRestVar :: HsName -> patrest

  ------------------------- constructing: alt -------------------------
  -- | Creates an alternative of a case statement.
  acoreAlt :: pat   -- ^ The pattern with which to match the case scrutinee.
        -> expr     -- ^ The value of this alternative.
        -> alt
  
  ------------------------- constructing: top level -------------------------
  -- | Wraps main expr into a form which can be directly run by evaluating
  acoreRunMain :: expr     -- ^ main
        -> expr
%%[[8
  acoreRunMain e = e
%%][99
  acoreRunMain e = acore1App e (acoreTup [])
%%]]
  
  ------------------------- type related -------------------------
  -- | construct ty from Ty, usable in Core context
  acoreTy2ty :: EHCOpts -> Ty -> ty
  
  ------------------------- defaults -------------------------
  -- | get default for boundmeta
  acoreDfltBoundmeta :: boundmeta
  acoreDfltBoundmeta = panic "AbstractCore.acoreDfltBoundmeta not implemented"
  
  -- | get error/default ty, type indexed by ty
  acoreTyErr :: String -> ty
  acoreTyErr s = panic $ "AbstractCore.acoreTyErr: " ++ s

  -- | get the ty representing the absent type, no type info
  acoreTyNone :: ty

  -- | get char ty
  acoreTyChar :: EHCOpts -> ty

  -- | get int ty
  acoreTyInt :: EHCOpts -> ty

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
  -- | is expr an application?
  acoreExprMbApp :: expr -> Maybe (expr,bound)

%%[[(8 coresysf)
  -- | is expr an arrow?
  acoreExprMbArr :: expr -> Maybe (bind,expr)
%%]]

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
  acoreBoundMbVal :: bound -> Maybe (boundmeta,expr)

  ------------------------- transforming -------------------------
  -- | thunk expr, i.e. turn into delayed computation
  acoreExprThunk :: expr -> expr
  acoreExprThunk = id

  -- | thunk ty, i.e. ty of 'acoreExprThunk'-ed expr
  acoreTyThunk :: ty -> ty
  acoreTyThunk = id

  -- | unthunk expr, i.e. force computation of delayed computation
  acoreExprUnThunk :: expr -> expr
  acoreExprUnThunk = id

  -- | unthunk ty, i.e. ty of 'acoreExprUnThunk'-ed expr
  acoreTyUnThunk :: ty -> ty
  acoreTyUnThunk = id

  ------------------------- coercion related: construction -------------------------
  -- | coercion arg placeholder
  acoreCoeArg :: expr

  ------------------------- coercion related: inspecting/deconstructing -------------------------
  -- | coercion arg placeholder
  acoreExprIsCoeArg :: expr -> Bool

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction via class AppLike, RecLike
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(ACoreAppLikeMetaBound)
type ACoreAppLikeMetaBound = (ACoreBindAspectKeyS,MetaLev,CLbl)
%%]

%%[(8 codegen) hs
instance {-# OVERLAPPABLE  #-} AbstractCore e b bound boundmeta bcat t p pr pf a => AppLike e boundmeta {- () () -} where
  app1App       = acore1App
  appTop        = id
  appCon        = acoreVar . mkHNm
  appPar        = id
  appVar        = acoreVar . mkHNm
  
%%[[(8 coresysf)
  app1MetaArr (mn,bm) a r = acorem1Arr (acoreBind1Asp1 (maybe hsnWild id mn) $ acoreBound1Boundmeta bm a) r
  appMb1ArrMk x = do (b,r) <- acoreExprMbArr x
                     let (n,(bo:_)) = acoreUnBind b
                     (bm,a) <- acoreBoundMbVal bo
                     return (((n,bm),a,r),id)
%%]]  
  -- appDflt       = 
  appDfltBoundmeta x = acoreDfltBoundmeta
  appDbg        = acoreExprErr
  
  appMbCon      = acoreExprMbVar
  appMbApp1 e   = do (f,b) <- acoreExprMbApp e
                     (_,a) <- acoreBoundMbVal b
                     return (f,a)
%%]

%%[(8 codegen coresysf) hs
instance {-# OVERLAPPABLE  #-} (AppLike e ACoreAppLikeMetaBound, HSNM bndnm, AbstractCore e m b bound ACoreAppLikeMetaBound bcat mbind t p pr pf a) => BndLike e bndnm {- () () -} where
  -- BndLike
  bndBndIn n l = app1MetaArr (Just $ mkHNm n,acoreBoundmeta acbaspkeyDefault l CLbl_None)
%%]

%%[(8 codegen) hs
instance {-# OVERLAPPABLE  #-} AbstractCore e b bound boundmeta bcat t p pr pf a => RecLike e boundmeta {- () () -} where
  recRow _ fs   = acoreTagTyTupBound CTagRec (acoreTyErr "AbstractCore.RecLike.recRow") [ acoreBound1Boundmeta (acoreBoundmeta acbaspkeyDefault 0 (CLbl_Nm n)) e | (n,e) <- fs ]
  
  recMbRecRow  _= Nothing -- tyMbRecRowWithLkup (const Nothing)
  recUnRowExts e= (e,[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Properties of binding aspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- new
%%[(8 codegen) export(ACoreBindAspectKey(..),ACoreBindAspectKeyS,ACoreBindAspMp)
-- | A ACoreBindAspectKeyS formed out of multiple ACoreBindAspectKey identifies a particular binding aspect
data ACoreBindAspectKey
  = ACoreBindAspectKey_Default              -- identifies the default binding, if omitted in a reference this aspect is the one chosen.
  | ACoreBindAspectKey_Ty                   -- the normal ty
%%[[(8 codegenanalysis)
  | ACoreBindAspectKey_RelevTy              -- the relevance ty
%%]]
  | ACoreBindAspectKey_Strict               -- the as strict as possible variant
  | ACoreBindAspectKey_Debug                -- internal debugging only
  | ACoreBindAspectKey_Core                 -- core
%%[[(8 coresysf)
  | ACoreBindAspectKey_SysF     MetaLev     -- system F thingy, at a metalevel
%%]]
%%[[93
  | ACoreBindAspectKey_FusionRole           -- fusion role
%%]]
  deriving (Eq,Ord,Generic)

instance Show ACoreBindAspectKey where
  show ACoreBindAspectKey_Default       = "dft"
  show ACoreBindAspectKey_Strict        = "str"
  show ACoreBindAspectKey_Ty            = "ty"
%%[[(8 codegenanalysis)
  show ACoreBindAspectKey_RelevTy       = "rty"
%%]]
  show ACoreBindAspectKey_Debug         = "dbg"
  show ACoreBindAspectKey_Core          = "core"
%%[[(8 coresysf)
  show (ACoreBindAspectKey_SysF ml)     = "sysf@" ++ show ml
%%]]
%%[[93
  show ACoreBindAspectKey_FusionRole    = "fusionrole"
%%]]

instance PP ACoreBindAspectKey where
  pp = pp . show

type ACoreBindAspectKeyS        =   Set.Set ACoreBindAspectKey
type ACoreBindAspMp x           =   Map.Map ACoreBindAspectKeyS x

acbaspkeyMk :: [ACoreBindAspectKey] -> ACoreBindAspectKeyS
acbaspkeyMk = Set.fromList
%%]

%%[(8 codegen) hs export(acbaspkeyMetaLev)
acbaspkeyMetaLev :: MetaLev -> ACoreBindAspectKeyS -> MetaLev
%%[[(8 coresysf)
acbaspkeyMetaLev mlev a | null l    = mlev
                        | otherwise = head l
  where l = [ l | ACoreBindAspectKey_SysF l <- Set.toList a ]
%%][8
acbaspkeyMetaLev mlev _ = mlev
%%]]
%%]

%%[(8 codegenanalysis) hs export(acbaspkeyDefaultRelevTy)
-- | predefined: 
acbaspkeyDefaultRelevTy :: ACoreBindAspectKeyS
acbaspkeyDefaultRelevTy = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_RelevTy ]
%%]

%%[(8 codegen) hs export(acbaspkeyDefaultTy, acbaspkeyTy, acbaspkeyDefaultCore, acbaspkeyNone,acbaspkeyDefault,acbaspkeyStrict,acbaspkeyDebug)
-- | predefined: 
acbaspkeyNone :: ACoreBindAspectKeyS
acbaspkeyNone = acbaspkeyMk
  [  ]

-- | predefined: 
acbaspkeyDefault :: ACoreBindAspectKeyS
acbaspkeyDefault = acbaspkeyMk
  [ ACoreBindAspectKey_Default ]

-- | predefined: 
acbaspkeyTy :: ACoreBindAspectKeyS
acbaspkeyTy = acbaspkeyMk
  [ ACoreBindAspectKey_Ty ]

-- | predefined: 
acbaspkeyDefaultTy :: ACoreBindAspectKeyS
acbaspkeyDefaultTy = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_Ty ]

-- | predefined: 
acbaspkeyDefaultCore :: ACoreBindAspectKeyS
acbaspkeyDefaultCore = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_Core ]

-- | predefined: 
acbaspkeyStrict :: ACoreBindAspectKeyS
acbaspkeyStrict = acbaspkeyMk
  [ ACoreBindAspectKey_Strict ]

-- | predefined: 
acbaspkeyDebug :: ACoreBindAspectKeyS
acbaspkeyDebug = acbaspkeyMk
  [ ACoreBindAspectKey_Debug ]
%%]

%%[(8 codegen coresysf) hs export(acbaspkeyDefaultSysf,acbaspkeySysfTy,acbaspkeyDefaultSysfTy)
-- | predefined: 
acbaspkeyDefaultSysf :: MetaLev -> ACoreBindAspectKeyS
acbaspkeyDefaultSysf ml = acbaspkeyMk
  [ ACoreBindAspectKey_Default, ACoreBindAspectKey_SysF ml ]

-- | predefined: 
acbaspkeySysfTy :: MetaLev -> ACoreBindAspectKeyS
acbaspkeySysfTy ml = acbaspkeyMk
  [ ACoreBindAspectKey_Ty, ACoreBindAspectKey_SysF ml ]

-- | predefined: 
acbaspkeyDefaultSysfTy :: MetaLev -> ACoreBindAspectKeyS
acbaspkeyDefaultSysfTy ml =
  Set.union (acbaspkeySysfTy ml) (acbaspkeyDefaultSysf ml)
%%]

%%[(93 codegen) hs export(acbaspkeyFusionRole)
-- | predefined: 
acbaspkeyFusionRole :: ACoreBindAspectKeyS
acbaspkeyFusionRole = acbaspkeyMk
  [ ACoreBindAspectKey_FusionRole ]
%%]

%%[(8 codegen) hs export(ppACBaspKeyS)
ppACBaspKeyS :: ACoreBindAspectKeyS -> PP_Doc
ppACBaspKeyS = ppCurlysCommas . Set.toList
%%]

%%[(8 codegen) hs export(hsnUniqifyACoreBindAspectKeyS)
-- | uniqify with ACoreBindAspectKeyS, omitting the default
hsnUniqifyACoreBindAspectKeyS :: ACoreBindAspectKeyS -> HsName -> HsName
hsnUniqifyACoreBindAspectKeyS as n
  = foldr mk n $ Set.toList as
  where mk ACoreBindAspectKey_Strict = hsnUniqify    HsNameUniqifier_Strict
        mk a                         = hsnUniqifyStr HsNameUniqifier_BindAspect (show a)
%%]

%%[(8 codegen) hs
deriving instance Typeable ACoreBindAspectKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A reference to an aspected value, i.e. a particular aspect of a binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ACoreBindRef(..),acoreMkRef,acoreMkAspRef)
-- | reference to binding aspect: name + aspect keys
data ACoreBindRef
  = ACoreBindRef
      { acbrefNm        :: !HsName
      , acbrefMbAspKey  :: !(Maybe ACoreBindAspectKeyS)
      }
  deriving (Eq,Ord)

acoreMkRef :: HsName -> ACoreBindRef
acoreMkRef n = ACoreBindRef n Nothing

acoreMkAspRef :: ACoreBindAspectKeyS -> HsName -> ACoreBindRef
acoreMkAspRef a n = ACoreBindRef n (Just a)

instance HSNM ACoreBindRef where
  mkHNm (ACoreBindRef n ma) = maybe n (\a -> hsnUniqifyACoreBindAspectKeyS a n) ma

instance Show ACoreBindRef where
  show = show . mkHNm
%%]

%%[(8 codegen) hs
acbrefAspKey :: ACoreBindRef -> ACoreBindAspectKeyS
acbrefAspKey = maybe acbaspkeyNone id . acbrefMbAspKey
{-# INLINE acbrefAspKey #-}
%%]

%%[(8 codegen) hs export(acbrefAspAnd)
-- | narrow down aspects by adding more to ref; assume extra aspects non empty
acbrefAspAnd :: ACoreBindAspectKeyS -> ACoreBindRef -> ACoreBindRef
acbrefAspAnd a r = r {acbrefMbAspKey = Just $ a `Set.union` acbrefAspKey r }
%%]

%%[(8 codegen) hs export(ppACoreBindRef)
ppACoreBindRef :: (HsName -> PP_Doc) -> ACoreBindRef -> PP_Doc
ppACoreBindRef ppN r = ppN (acbrefNm r) >|< (maybe empty (ppCurlysCommas . Set.toList) $ acbrefMbAspKey r)

instance PP ACoreBindRef where
  pp = ppACoreBindRef pp
%%]

%%[(50 codegen) hs
deriving instance Typeable ACoreBindRef
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acore1App,acoreApp,acoreAppBound)
acore1App :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => e -> e -> e
acore1App f a = acore1AppBound f (acoreBound1Val a)
{-# INLINE acore1App #-}

-- | Applies the first expression to all given arguments.
acoreApp :: (AbstractCore e b bound boundmeta bcat t p pr pf a)
    => e    -- ^ The lambda to apply.
    -> [e]  -- ^ The arguments (the empty list is allowed).
    -> e
acoreApp f as = foldl (\f a -> acore1App f a) f as

acoreAppBound :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => e -> [bound] -> e
acoreAppBound f as = foldl (\f a -> acore1AppBound f a) f as
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: lambda abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLamBind,acoreLam1Ty,acoreLam1,acoreLamTy,acoreLam)
acoreLamBind :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [b] -> e -> e
acoreLamBind = flip (foldr acoreLam1Bind)
{-# INLINE acoreLamBind #-}

acoreLam1Ty :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> e
acoreLam1Ty a t e = acoreLam1Bind (acoreBind1NmTy1 a t) e
-- acoreLam1Ty a t e = acoreLam1Bind (acoreBind1Nm1 a) e
-- acoreLam1Ty a t e = acoreLam1Bind (acoreBind1Ty a t) e       -- 20120418, TBD: ignore type for now
{-# INLINE acoreLam1Ty #-}

acoreLam1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> e -> e
acoreLam1 a e = acoreLam1Ty a (acoreTyErr "acoreLam1") e
{-# INLINE acoreLam1 #-}

acoreLamTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [(HsName,t)] -> e -> e
acoreLamTy as e = foldr (\(n,t) e -> acoreLam1Ty n t e) e as

acoreLam :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [HsName] -> e -> e
acoreLam as e = foldr (\(n) e -> acoreLam1 n e) e as
%%]

%%[(8 codegen) export(acoreTagTupTy,acoreTagTup,acoreTupTy,acoreTup,acoreTag)
acoreTagTupTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => CTag -> t -> [e] -> e
acoreTagTupTy tg t es = acoreTagTyTupBound tg t $ map acoreBound1Val es

-- | Creates a new tuple/record with the given values.
-- Has to be fully applied, partial application is not allowed.
acoreTagTup :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => CTag -> [e] -> e
acoreTagTup tg es = acoreTagTupTy tg (acoreTyErr "acoreTupTy") es

acoreTupTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => t -> [e] -> e
acoreTupTy t es = acoreTagTupTy CTagRec t es
{-# INLINE acoreTupTy #-}

acoreTup :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [e] -> e
acoreTup es = acoreTagTup CTagRec es
{-# INLINE acoreTup #-}

acoreTag :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => CTag -> e
acoreTag tg = acoreTagTup tg []
{-# INLINE acoreTag #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: bind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBind1CatTy,acoreBind1Cat,acoreBind1LevTy,acoreBind1Ty,acoreBind1)
acoreBind1CatTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => bcat -> HsName -> t -> e -> b
acoreBind1CatTy cat n t e = acoreBind1CatLevTy cat n metaLevVal t e
{-# INLINE acoreBind1CatTy #-}

acoreBind1LevTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> MetaLev -> t -> e -> b
acoreBind1LevTy n l t e = acoreBind1CatLevTy (acoreBindcategDflt e) n l t e
{-# INLINE acoreBind1LevTy #-}

acoreBind1Ty :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> b
acoreBind1Ty n t e = acoreBind1LevTy n metaLevVal t e
{-# INLINE acoreBind1Ty #-}

acoreBind1Cat :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => bcat -> HsName -> e -> b
acoreBind1Cat cat n e = acoreBind1CatTy cat n acoreTyNone {- (acoreTyErr "acoreBind1Cat") -} e
{-# INLINE acoreBind1Cat #-}

acoreBind1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> e -> b
acoreBind1 n e = acoreBind1Cat (acoreBindcategDflt e) n e
{-# INLINE acoreBind1 #-}
%%]

%%[(8 codegen) export(acoreBind1Asp1,acoreBind1NmLevTy1,acoreBind1Nm1)
acoreBind1Asp1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> bound -> b
acoreBind1Asp1 n ba = acoreBind1Asp n [ba]
{-# INLINE acoreBind1Asp1 #-}

acoreBind1NmLevTy1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> MetaLev -> t -> b
acoreBind1NmLevTy1 n l t = acoreBind1Asp n [acoreBoundValTy1CatLev acoreBindcategPlain n l t]
-- {-# INLINE acoreBind1NmLevTy1 #-}

acoreBind1NmTy1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> b
acoreBind1NmTy1 n t = acoreBind1NmLevTy1 n metaLevTy t
{-# INLINE acoreBind1NmTy1 #-}

acoreBind1Nm1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> b
acoreBind1Nm1 n = acoreBind1Asp n []
{-# INLINE acoreBind1Nm1 #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: bound (previously: aka binding aspect)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBoundVal1CatTy,acoreBoundVal1Cat,acoreBoundVal1Ty,acoreBoundVal1)
acoreBoundVal1CatTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => bcat -> HsName -> t -> e -> bound
acoreBoundVal1CatTy cat n t e = acoreBoundVal1CatLevTy cat n metaLevVal t e
{-# INLINE acoreBoundVal1CatTy #-}

acoreBoundVal1Cat :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => bcat -> HsName -> e -> bound
acoreBoundVal1Cat cat n e = acoreBoundVal1CatTy cat n (acoreTyErr "acoreBoundVal1Cat") e
{-# INLINE acoreBoundVal1Cat #-}

acoreBoundVal1Ty :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> bound
acoreBoundVal1Ty n t e = acoreBoundVal1CatTy (acoreBindcategDflt e) n t e
{-# INLINE acoreBoundVal1Ty #-}

acoreBoundVal1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> e -> bound
acoreBoundVal1 n e = acoreBoundVal1Ty n (acoreTyErr "acoreBoundVal1") e
{-# INLINE acoreBoundVal1 #-}
%%]

%%[(8 codegen) export(acoreBound1AspkeyVal,acoreBound1Val)
acoreBound1AspkeyVal :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => ACoreBindAspectKeyS -> e -> bound
acoreBound1AspkeyVal a e = acoreBound1Boundmeta (acoreBoundmeta a 0 CLbl_None) e
{-# INLINE acoreBound1AspkeyVal #-}

acoreBound1Val :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => e -> bound
acoreBound1Val e = acoreBound1AspkeyVal acbaspkeyDefault e
{-# INLINE acoreBound1Val #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting utils for introducing type errors where a type is not yet provided
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreTyErrLift)
acoreTyErrLift :: (AbstractCore e b bound boundmeta bcat t p pr pf a, Functor f) => String -> f x -> f (x,t)
acoreTyErrLift msg = fmap (\n -> (n,acoreTyErr msg))
{-# INLINE acoreTyErrLift #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: let
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLetMerge,acoreLet,acoreLetRec)
-- | Construct let, possibly merging bindings
acoreLetMerge :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => Bool -> bcat -> [b] -> e -> e
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

acoreLet :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => bcat -> [b] -> e -> e
acoreLet c bs e = acoreLetMerge False c bs e
{-# INLINE acoreLet #-}

-- | Creates a let binding, where the bindings may be mutually recursive.
acoreLetRec :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a)
    => [b]  -- ^ The bindings.
    -> e    -- ^ The body.
    -> e
acoreLetRec bs e = acoreLet (acoreBindcategRec) bs e
{-# INLINE acoreLetRec #-}
%%]

%%[(8 codegen) export(acoreLetN)
acoreLetN :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => [(bcat,[b])] -> e -> e
acoreLetN cbs e = foldr (\(c,bs) e -> acoreLet c bs e) e cbs
%%]

%%[(8 codegen) export(acoreLet1PlainTy,acoreLet1Plain)
acoreLet1PlainTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1PlainTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategPlain

-- | Creates a (non-recursive) let binding.
acoreLet1Plain :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a)
    => HsName   -- ^ The identifier.
    -> e       -- ^ The expression to bind.
    -> e       -- ^ The body.
    -> e
acoreLet1Plain nm e = acoreLet1PlainTy nm (acoreTyErr "acoreLet1Plain") e
{-# INLINE acoreLet1Plain #-}
%%]

%%[(8 codegen) export(acoreLet1StrictTy,acoreLet1Strict)
acoreLet1StrictTy :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1StrictTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategStrict

-- | Creates a let binding, which is strict in the bound expression.
acoreLet1Strict :: (Eq bcat, AbstractCore e b bound boundmeta bcat t p pr pf a)
    => HsName   -- ^ The identifer.
    -> e        -- ^ The expression to bind. Will be evaluated to WHNF, before the body is evaluated.
    -> e        -- ^ The body.
    -> e
acoreLet1Strict nm e = acoreLet1StrictTy nm (acoreTyErr "acoreLet1Strict") e
{-# INLINE acoreLet1Strict #-}
%%]

%%[(8 codegen) hs export(acoreLet1StrictIn,acoreLet1StrictInTy)
-- | evaluate an expr, with a continuation for the evaluated expr
acoreLet1StrictInTyWith :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => (t->t) -> (e->e) -> HsName -> t -> e -> (e -> e) -> e
acoreLet1StrictInTyWith mkT mkE nm t e mkC
  = acoreLetBase cat [acoreBind1CatTy cat nm (mkT t) (mkE e)] (mkC (acoreVar nm))
  where cat = acoreBindcategStrict

acoreMbLet1StrictInTyWith :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => (t->t) -> (e->e) -> Maybe (HsName,t) -> e -> (e -> e) -> e
acoreMbLet1StrictInTyWith mkT mkE (Just (nm,t)) e mkC = acoreLet1StrictInTyWith mkT mkE nm t e mkC
acoreMbLet1StrictInTyWith _   _   _             e mkC = mkC e

acoreLet1StrictInTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> e -> (e -> e) -> e
acoreLet1StrictInTy = acoreLet1StrictInTyWith acoreTyUnThunk acoreExprUnThunk
{-# INLINE acoreLet1StrictInTy #-}

acoreLet1StrictIn :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> e -> (e -> e) -> e
acoreLet1StrictIn nm e mkC = acoreLet1StrictInTy nm (acoreTyErr "acoreLet1StrictIn") e mkC
{-# INLINE acoreLet1StrictIn #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: hole
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) hs export(acoreNmHolePred,acoreNmHole)
acoreNmHole :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => UID -> e
acoreNmHole = acoreVar . mkHNm

acoreNmHolePred :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => PredOccId -> e
acoreNmHolePred = acoreNmHole . poiId
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: defaults
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBindcategDflt)
-- | get default for bindcateg
acoreBindcategDflt :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => e -> bcat
acoreBindcategDflt _ = acoreBindcategPlain
{-# INLINE acoreBindcategDflt #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreChar,acoreInt,acoreInt2)
acoreChar :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Char -> e
acoreChar opts i = let x = acoreCharTy (acoreTyChar opts) i in x

-- | Creates an `Int` constant.
acoreInt :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Int -> e
acoreInt opts i = let x = acoreIntTy (acoreTyInt opts) i in x

acoreInt2 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Integer -> e
acoreInt2 opts i = let x = acoreIntTy2 (acoreTyInt opts) i in x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operator/value construction, expressed in terms of builtin primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreBuiltinApp)
acoreBuiltinApp :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> (EHBuiltinNames -> HsName) -> [e] -> e
acoreBuiltinApp opts bnmOf args = acoreVar (ehcOptBuiltin opts bnmOf) `acoreApp` args
%%]

%%[(8 codegen) hs export(acoreBuiltinAddInt)
acoreBuiltinAddInt :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinAddInt opts e i
  = if i == 0
    then e
    else case acoreExprMbInt e of
           Just (t,i') -> acoreIntTy2 t (toInteger i + i')
           _           -> acoreBuiltinApp opts ehbnPrimAddInt [e,acoreInt opts i]
%%]

%%[(8 codegen) hs export(acoreBuiltinGtInt)
acoreBuiltinGtInt :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinGtInt opts e i = acoreBuiltinApp opts ehbnPrimGtInt [e,acoreInt opts i]
%%]

%%[(99 codegen) hs export(acoreBuiltinEqChar)
acoreBuiltinEqChar :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Char -> e -> e
acoreBuiltinEqChar opts c e = acoreBuiltinApp opts ehbnPrimEqChar [e,acoreChar opts c]
%%]

%%[(8 codegen) hs export(acoreBuiltinString)
-- | Creates a string expression.
-- The expression represents a packed String, which can be passed to Haskell generated Core functions.
acoreBuiltinString :: (AbstractCore e b bound boundmeta bcat t p pr pf a)
    => EHCOpts
    -> String   -- ^ The string.
    -> e
acoreBuiltinString opts m = let x = acoreBuiltinApp opts ehbnPackedStringToString [acoreStringTy (acoreTyString opts) m] in x
%%]

%%[(8 codegen) hs export(acoreBuiltinError,acoreBuiltinUndefined)
-- | Generates an error expression, failing with the given string when evaluated. ('error' in haskell)
acoreBuiltinError :: (AbstractCore e b bound boundmeta bcat t p pr pf a)
    => EHCOpts
    -> String -- ^ The error message.
    -> e
acoreBuiltinError opts m = acoreBuiltinApp opts ehbnError [acoreBuiltinString opts m]

-- | Generates an undefined expression, failing when evaluated. ('undefined' in haskell)
acoreBuiltinUndefined :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> e
acoreBuiltinUndefined opts = acoreBuiltinApp opts ehbnUndefined []
%%]

%%[(97 codegen) hs export(acoreBuiltinInteger)
-- | Creates a Core 'Integer' constant.
acoreBuiltinInteger :: (AbstractCore e b bound boundmeta bcat t p pr pf a)
    => EHCOpts
    -> Integer  -- ^ The integer.
    -> e
acoreBuiltinInteger opts i = acoreBuiltinApp opts ehbnPackedStringToInteger [acoreStringTy (acoreTyString opts) (show i)]
%%]

%%[(99 codegen) hs export(acoreBuiltinListSingleton)
-- | Builtin list singleton (note: hardcoded of tags)
acoreBuiltinListSingleton :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> e -> e
acoreBuiltinListSingleton opts e
  = acoreTagTupTy (ctagCons opts) (acoreTyErr "acoreBuiltinListSingleton.Cons") [e, acoreTagTupTy (ctagNil opts) (acoreTyErr "acoreBuiltinListSingleton.Nil") []]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection: pattern, case alt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatConMbTag,acoreAltMbTag)
-- | when pat is con get tag
acorePatConMbTag :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => p -> Maybe CTag
acorePatConMbTag = fmap (\(tg,_,_) -> tg) . acorePatMbCon

-- | possibly get tag of alt
acoreAltMbTag :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => a -> Maybe CTag
acoreAltMbTag = (\p ->     (\(tg,_,_) -> tg) <$> acorePatMbCon  p
                       <|> (const ctagInt)   <$> acorePatMbInt  p
                       -- <|> (const ctagChar)  <$> acorePatMbChar p
                ) . fst . acoreUnAlt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection: binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBindNm)
-- | bound name of binding
acoreBindNm :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => b -> HsName
acoreBindNm = fst . acoreUnBind
{-# INLINE acoreBindNm #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection: pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatFldTy)
-- | bound name of binding
acorePatFldTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => t -> (HsName,e) -> HsName -> pf
acorePatFldTy t lbloff n = acorePatFldBind lbloff (acoreBind1NmTy1 n t)
{-# INLINE acorePatFldTy #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection: pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreUnBoundVal)
-- | possible expr of bound (may panic)
acoreUnBoundVal :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => bound -> e
acoreUnBoundVal = maybe (panic "acoreBoundMbVal") (\(_,a) -> a) . acoreBoundMbVal
{-# INLINE acoreUnBoundVal #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Specific constructs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(91 codegen) hs export(acoreIf)
-- | Construct 'if' expression. Hardcoded: tag nr, ordered alts (by tag)
acoreIf :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Maybe HsName -> e -> e -> e -> e
acoreIf opts cn c t f
  = acoreMbLet1StrictInTyWith id id (fmap (\n -> (n,acoreTyBool opts)) cn) c
    $ (\c -> acoreCaseDflt c
               [ acoreAlt (acorePatCon (ctagFalse opts) acorePatRestEmpty []) f
               , acoreAlt (acorePatCon (ctagTrue  opts) acorePatRestEmpty []) t
               ]
               Nothing {-(tcUndefined opts)-}
      )
%%]

%%[(99 codegen) hs export(acoreMatchChar)
acoreMatchChar :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => EHCOpts -> Maybe HsName -> Char -> e -> e -> e -> e
acoreMatchChar opts cn cchar cexpr t f
  = acoreIf opts cn (acoreBuiltinEqChar opts cchar cexpr) t f
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A coercion applies to Core, modifying it by (e.g.) introducing extra parameters, bindings, etc.
A coercion thus represents Core fragments of which parts are not yet known.

%%[doesWhat.Coe doclatex
A Coercion represents incomplete code, in that it contains a hole to be filled in later.
Conceptually, the coercion type is defined only by:

\begin{pre}
type Coe = Expr -> Expr
\end{pre}

In the implementation here, the hole is represented by Expr_CoeArg.

We also need to manipulate coercions so more structure is encoded in @Coe@ to match on coercion variants.
In the end, a coercion is applied to a Expr to yield code,
see coeEvalOn in Core/Subst.
Additionally, this can be done in a lazy manner yielding a substitution CSubst
to be applied at the last possible moment.
%%]

%%[8 hs export(Coe'(..))
%%[[(8 codegen)
data Coe' expr bind bindasp ty
  = Coe_Map             !(expr -> expr)                 -- normal, expression as function
  | Coe_C               !expr                           -- constant
  | Coe_Compose         !(Coe' expr bind bindasp ty)    -- composition
                        !(Coe' expr bind bindasp ty)
  | Coe_App1            !expr                           -- apply
  | Coe_App             [HsName]                        -- apply n args
  | Coe_Lam             !HsName !ty                     -- lambda
  | Coe_CloseExists     !TyVarId !ty !ty                -- closing existential
  | Coe_OpenExists      !TyVarId !ty !ty                -- opening existential
%%[[9
  | Coe_LamLet          !HsName !ty !UID                -- lambda with a let binding in the body
  | Coe_LetRec          ![bind]                         -- let rec
  | Coe_ImplApp         !ImplsVarId                     -- implicits, for apply
  | Coe_ImplLam         !ImplsVarId                     -- implicits, for lambda
%%]]

instance Show (Coe' expr bind bindasp ty) where
  show _ = "COE"
%%][8
data Coe'
  = Coe_NONE            -- placeholder dummy
  deriving Show
%%]]
%%]

%%[(8 !codegen) hs export(Coe)
-- | placeholder def
type Coe = Coe'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion context
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(CoeCtx(..))
-- | Coercions may either be applied or not on type arguments.
--   In particular, due to lack of proper analysis (and generics code like generation),
--   it is only known for arrow and product types how to construct a coercion from its type args, as they directly
--   correspond to values.
--   
--   A CoeCtx encodes this yes/no may allow.
--   A CoeCtx is isomorphic (for now) to Bool.
data CoeCtx
  = CoeCtx_Allow
  | CoeCtx_DontAllow
  deriving
   ( Eq, Show
%%[[50
   , Typeable
%%]]
   )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCoeId, acoreCoeMap)
-- | Non inspectable, most general, coercion
acoreCoeMap :: (e -> e) -> Coe' e b ba t
acoreCoeMap = Coe_Map
{-# INLINE acoreCoeMap #-}

-- | Coe identity
acoreCoeId :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Coe' e b ba t
acoreCoeId = Coe_C acoreCoeArg
{-# INLINE acoreCoeId #-}
%%]

%%[(9 codegen) hs export(acoreCoeLamLetTy,acoreCoeLamLet, acoreCoeLetRec)
acoreCoeLamLetTy :: HsName -> t -> UID -> Coe' e b ba t
acoreCoeLamLetTy = Coe_LamLet
{-# INLINE acoreCoeLamLetTy #-}

acoreCoeLamLet :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> UID -> Coe' e b ba t
acoreCoeLamLet n u = acoreCoeLamLetTy n (acoreTyErr "acoreCoeLamLet") u
{-# INLINE acoreCoeLamLet #-}

-- | Let still requiring a body
acoreCoeLetRec :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [b] -> Coe' e b ba t
acoreCoeLetRec [] = acoreCoeId
acoreCoeLetRec bs = Coe_LetRec bs
%%]

%%[(8 codegen) hs export(acoreCoeApp1,acoreCoeAppN,acoreCoeAppNbyName)
-- | Application still requiring a function
acoreCoeApp1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => e -> Coe' e b ba t
acoreCoeApp1 = Coe_App1
{-# INLINE acoreCoeApp1 #-}

acoreCoeAppNbyName :: [(HsName)] -> Coe' e b ba t
acoreCoeAppNbyName = Coe_App
{-# INLINE acoreCoeAppNbyName #-}

-- acoreCoeApp2 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [(e)] -> Coe' e b ba t
-- acoreCoeApp2 as = acoreCoeMap (\e -> acoreApp e as)

acoreCoeAppN :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [e] -> Coe' e b ba t
acoreCoeAppN as = acoreCoeMap (\e -> acoreApp e as)
{-# INLINE acoreCoeAppN #-}
%%]

%%[(8 codegen) hs export(acoreCoeLam1Ty,acoreCoeLam1)
-- | Lambda still requiring a body
acoreCoeLam1Ty :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> t -> Coe' e b ba t
acoreCoeLam1Ty = Coe_Lam
{-# INLINE acoreCoeLam1Ty #-}

acoreCoeLam1 :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => HsName -> Coe' e b ba t
acoreCoeLam1 n = acoreCoeLam1Ty n (acoreTyErr "acoreCoeLam1")
{-# INLINE acoreCoeLam1 #-}
%%]

%%[(8 codegen) hs export(acoreCoeCompose)
-- | Composition of 2 Coe's
acoreCoeCompose :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Coe' e b ba t -> Coe' e b ba t -> Coe' e b ba t
acoreCoeCompose c1 c2
  | acoreCoeIsId c1 = c2
  | otherwise  = Coe_Compose c1 c2

%%]

%%[(9 codegen) hs export(acoreCoePoiLApp,acoreCoeImplsApp)
acoreCoePoiLApp :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => [PredOccId] -> [Coe' e b ba t]
acoreCoePoiLApp = map (\i -> acoreCoeApp1 (acoreNmHolePred i))

acoreCoeImplsApp :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Impls -> [Coe' e b ba t]
acoreCoeImplsApp = acoreCoePoiLApp . implsPrIdL
%%]

%%[(9 codegen) hs export(acoreCoePoiLLamTy,acoreCoeImplsLam)
acoreCoePoiLLamTy :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Coe' e b ba t -> [(PredOccId,t)] -> [Coe' e b ba t]
acoreCoePoiLLamTy onLast poiL
  =  case map mk poiL of
       l@(_:_)            -> h ++ [t `acoreCoeCompose` onLast]
                          where (h,t) = fromJust $ initlast l
       _ | acoreCoeIsId onLast -> []
         | otherwise      -> [onLast]
  where mk (poi,ty) = acoreCoeLam1Ty (poiHNm poi) ty

acoreCoeImplsLam :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Coe' e b ba t -> Impls -> [Coe' e b ba t]
acoreCoeImplsLam onLast is = acoreCoePoiLLamTy onLast (acoreTyErrLift "acoreCoeImplsLam" (implsPrIdL is))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion predicates/inspection/...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCoeIsId)
acoreCoeIsId :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => Coe' e b ba t -> Bool
acoreCoeIsId (Coe_C e) = acoreExprIsCoeArg e
acoreCoeIsId _         = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution CSubst
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CSubstKey(..))
data CSubstKey
  = CSKey_UID   UID
  | CSKey_Nm    HsName
  | CSKey_Ref   ACoreBindRef
  deriving (Show,Eq,Ord)
%%]

%%[(8 codegen) hs export(CSubstInfo'(..))
data CSubstInfo' expr bind bindasp ty
  =  CSITy        { csiTy      :: !ty
                  }
  |  CSIExpr      { csiRepl    :: !expr
                  }
%%[[9
  |  CSIImpls     { csiAppCoeL :: ![Coe' expr bind bindasp ty]
                  , csiLamCoeL :: ![Coe' expr bind bindasp ty]
                  }
  |  CSIBinds     { csiBindL   :: ![bind]
                  }
%%]]

instance Show (CSubstInfo' e b ba t) where
  show _ = "CSubstInfo'"
%%]

%%[(8 codegen) hs export(CSubst',emptyCSubst)
type CSubst' e b ba t = Map.Map CSubstKey (CSubstInfo' e b ba t)

emptyCSubst :: CSubst' e b ba t
emptyCSubst = Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCSubstFromNmTyL)
acoreCSubstFromNmTyL :: AssocL HsName t -> CSubst' e b ba t
acoreCSubstFromNmTyL l = Map.fromList [ (CSKey_Nm k,CSITy v) | (k,v) <- l ]
%%]
  
%%[(8 codegen) hs export(acoreCSubstFromRefExprL)
acoreCSubstFromRefExprL :: AssocL ACoreBindRef e -> CSubst' e b ba t
acoreCSubstFromRefExprL l = Map.fromList [ (CSKey_Ref k,CSIExpr v) | (k,v) <- l ]
%%]
  
%%[(8 codegen) hs export(acoreCSubstFromUidExprL)
acoreCSubstFromUidExprL :: AssocL UID e -> CSubst' e b ba t
acoreCSubstFromUidExprL l = Map.fromList [ (CSKey_UID k,CSIExpr v) | (k,v) <- l ]
%%]
  
%%[(9 codegen) hs export(acoreCSubstFromUidImplsL,acoreCSubstFromUidBindLL)
acoreCSubstFromUidBindLL :: AssocL UID [b] -> CSubst' e b ba t
acoreCSubstFromUidBindLL l = Map.fromList [ (CSKey_UID k,CSIBinds v) | (k,v) <- l ]

acoreCSubstFromUidImplsL :: AssocL UID ([Coe' e b ba t],[Coe' e b ba t]) -> CSubst' e b ba t
acoreCSubstFromUidImplsL l = Map.fromList [ (CSKey_UID k,uncurry CSIImpls v) | (k,v) <- l ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst combination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(cSubstAppSubst)
-- | Combine CSubst: union only, application is postponed
cSubstAppSubst :: CSubst' e b ba t -> CSubst' e b ba t -> CSubst' e b ba t
cSubstAppSubst = Map.union
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution as class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CSubstitutable(..))
infixr `cSubstApp`

class CSubstitutable       e b ba t a
                   | a  -> e b ba t
  where
  cSubstApp :: CSubst' e b ba t -> a -> a

instance CSubstitutable e b ba t (CSubst' e b ba t) where
  cSubstApp cs s = cs `cSubstAppSubst` s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract syntax for encoding case+pattern rewrite info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RAlt'(..),RPat'(..),RPatConBind'(..),RPatFld'(..),RCEAltL')
data RAlt' e t b pr
  = RAlt_Alt            { rcaPats :: ![RPat' e t b pr], raaExpr :: !e, raaFailS :: UIDS }

data RPat' e t b pr
  = RPat_Var            { rcpPNm :: !RPatNm, rcpTy :: !t, rcpMustEval :: Bool }
  | RPat_Con            { rcpPNm :: !RPatNm, rcpTy :: !t, rcpTag :: !CTag, rcpBinds :: !(RPatConBind' e t b pr) }
  | RPat_Int            { rcpPNm :: !RPatNm, rcpTy :: !t, rcpInt :: !Integer }
  | RPat_Char           { rcpPNm :: !RPatNm, rcpTy :: !t, rcpChar :: !Char }
  | RPat_Irrefutable    { rcpPNm :: !RPatNm, rcpTy :: !t, rcpValBindL :: ![b] }
%%[[97
  | RPat_BoolExpr       { rcpPNm :: !RPatNm, rcpTy :: !t, rcpExpr :: !e, rcpMbConst :: Maybe SrcConst }
%%]]

data RPatConBind' e t b pr
  = RPatConBind_One     { rpcbRest :: !pr, rpcbBinds :: ![RPatFld' e t b pr] }
  | RPatConBind_Many    { rpcbConBinds :: ![RPatConBind' e t b pr] }

data RPatFld' e t b pr
  = RPatFld_Fld         { rpbLbl :: !HsName, rpbOffset :: !e, rpbNm :: !HsName, rpbPat :: !(RPat' e t b pr)}

type RCEAltL' e t b pr = [RAlt' e t b pr]
%%]

%%[(8 codegen) hs export(rcaPat,raltLPatNms)
rcaPat :: RAlt' e t b pr -> RPat' e t b pr
rcaPat = head . rcaPats

raltLPatNms :: [RAlt' e t b pr] -> [RPatNm]
raltLPatNms = nub . sort . map (rcpPNm . rcaPat)
%%]

%%[(8 codegen) hs export(rcaTag)
rpatConTag :: RPat' e t b pr -> CTag
rpatConTag (RPat_Int  _ _ _ )  = ctagInt
rpatConTag (RPat_Char _ _ _ )  = ctagChar
rpatConTag p                   = rcpTag p

rcaTag :: RAlt' e t b pr -> CTag
rcaTag = rpatConTag . head . rcaPats
%%]

%%[(8 codegen) hs export(raltIsVar,raltIsConst)
raltIsVar :: RAlt' e t b pr -> Bool
raltIsVar (RAlt_Alt (RPat_Var _ _ _ : _) _ _)  = True
raltIsVar _                                    = False

raltIsConst :: RAlt' e t b pr -> Bool
raltIsConst (RAlt_Alt (p : _) _ _)
  = c p
  where c (RPat_Int   _ _ _) = True
        c (RPat_Char  _ _ _) = True
        c _                  = False
raltIsConst _                = False
%%]

%%[(8 codegen) hs export(raltIsConMany)
raltIsConMany :: RAlt' e t b pr -> Bool
raltIsConMany (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_Many _) : _) _ _) = True
raltIsConMany _                                                        = False
%%]

%%[(8 codegen) hs export(raltIsIrrefutable)
raltIsIrrefutable :: RAlt' e t b pr -> Bool
raltIsIrrefutable (RAlt_Alt (RPat_Irrefutable _ _ _ : _) _ _) = True
raltIsIrrefutable _                                           = False
%%]

%%[(97 codegen) hs export(raltMbBoolExpr,raltIsBoolExpr)
raltMbBoolExpr :: RAlt' e t b pr -> Maybe (Maybe SrcConst)
raltMbBoolExpr (RAlt_Alt (RPat_BoolExpr _ _ _ e : _) _ _)  = Just e
raltMbBoolExpr _                                           = Nothing

raltIsBoolExpr :: RAlt' e t b pr -> Bool
raltIsBoolExpr = isJust . raltMbBoolExpr
%%]

Flatten bindings, delaying the handling of many bindings to the rewriting of case patterns.

%%[(8 codegen) hs export(rpatConBindUnFlatten)
rpatConBindUnFlatten :: RPatConBind' e t b pr -> [RPatConBind' e t b pr] -> RPatConBind' e t b pr
rpatConBindUnFlatten z []  = z
rpatConBindUnFlatten _ [b] = b
rpatConBindUnFlatten _ bs  = RPatConBind_Many bs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion from Rxxx -> ACore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreRPat2Pat)
acoreRPat2Pat :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => RPat' e t b pr -> p
acoreRPat2Pat p
  = case p of
      RPat_Var      n ty _    -> acorePatVarTy  (rpatNmNm n) ty
      RPat_Con      n _ t b   -> acorePatCon    t r bs
                              where (r,bs) = acoreRPatConBind2PatConBind b
      RPat_Int      n ty v    -> acorePatIntTy2 ty v
      RPat_Char     n ty v    -> acorePatCharTy ty v
%%[[97
      RPat_BoolExpr n _  v _  -> acorePatBoolExpr  v
%%]]
%%]

%%[(8 codegen) hs
acoreRPatConBind2PatConBind :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => RPatConBind' e t b pr -> (pr,[pf])
acoreRPatConBind2PatConBind b
  = case b of
      RPatConBind_One   r bs    -> (r,map acoreRPatBind2PatFld bs)
      RPatConBind_Many  bs      -> head (map acoreRPatConBind2PatConBind bs)

acoreRPatBind2PatFld :: (AbstractCore e b bound boundmeta bcat t p pr pf a) => RPatFld' e t b pr -> pf
acoreRPatBind2PatFld (RPatFld_Fld l o _ p@(RPat_Var n _ _)) = acorePatFldTy (rcpTy p) (l,o) (rpatNmNm n)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: Bool
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(8 codegen) hs export(ctagTrue, ctagFalse)
ctagTrue, ctagFalse :: EHCOpts -> CTag
ctagTrue  opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolTrue)  tagBoolTrue  0 0        -- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagFalse opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolFalse) tagBoolFalse 0 0        -- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(8 codegen) hs export(ctagCons,ctagNil)
ctagCons, ctagNil :: EHCOpts -> CTag
ctagCons opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltCons) tagListCons 2 2       -- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagNil  opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltNil ) tagListNil  0 2       -- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reason a case alternative can fail
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CaseAltFailReason(..))
-- | Reason to fail a case alternative
data CaseAltFailReason
  = CaseAltFailReason_Absence                   -- failed because of absence
  | CaseAltFailReason_Continue
      { cafailCaseId        :: UID              -- failed as part of case match attempt, but continues with code identified by id
      }
  deriving (Show,Eq,Ord,Generic)

instance PP CaseAltFailReason where
  pp (CaseAltFailReason_Continue i) = pp i
  pp (CaseAltFailReason_Absence   ) = pp "absent"
%%]

%%[(8 codegen) hs export(cafailHasId)
cafailHasId :: CaseAltFailReason -> (Bool,UID)
cafailHasId (CaseAltFailReason_Absence   ) = (False,uidUnused)
cafailHasId (CaseAltFailReason_Continue i) = (True ,i)
%%]

%%[(50 codegen) hs
deriving instance Typeable CaseAltFailReason
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Kind of function which is used in an app
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(AppFunKind(..))
data AppFunKind
  = AppFunKind_NoApp                    -- inlined Nothing
  | AppFunKind_Fun  ACoreBindRef
  | AppFunKind_Tag  CTag
  | AppFunKind_FFI
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Context: what is above/below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(WhatExpr(..))

-- | What kind of Expr?
data WhatExpr
  = ExprIsLam   Int				-- arity
                (Maybe HsName)	-- possibly name bound to
  | ExprIsApp   Int         	-- arity
  				WhatExpr		-- function
  | ExprIsVar   HsName
  | ExprIsInt   Int
  | ExprIsTup   CTag
  | ExprIsFFI
  | ExprIsOtherWHNF
  | ExprIsOther
  | ExprIsBind 	HsName
  deriving Eq
%%]

%%[(8 codegen) hs export(whatExprMbVar, whatExprMbApp,whatExprMbLam, whatExprMbLam', whatExprAppArity, whatExprMbBind)
-- | is an var?
whatExprMbVar :: WhatExpr -> Maybe HsName
whatExprMbVar (ExprIsVar a) = Just a
whatExprMbVar _             = Nothing

-- | is an app?
whatExprMbApp :: WhatExpr -> Maybe (Int,WhatExpr)
whatExprMbApp (ExprIsApp a w) = Just (a,w)
whatExprMbApp _               = Nothing

-- | is a lam?
whatExprMbLam' :: WhatExpr -> Maybe (Int, Maybe HsName)
whatExprMbLam' (ExprIsLam a n) = Just (a, n)
whatExprMbLam' _               = Nothing

-- | is a lam?
whatExprMbLam :: WhatExpr -> Maybe Int
whatExprMbLam (ExprIsLam a _) = Just a
whatExprMbLam _               = Nothing

-- | is a bind?
whatExprMbBind :: WhatExpr -> Maybe HsName
whatExprMbBind (ExprIsBind n) = Just n
whatExprMbBind _              = Nothing

-- | app arity
whatExprAppArity :: WhatExpr -> Int
whatExprAppArity (ExprIsApp a _) = a
whatExprAppArity _               = 0
%%]

%%[(8 codegen) hs export(whatExprIsWHNF)
whatExprIsWHNF :: WhatExpr -> Bool
whatExprIsWHNF (ExprIsLam _ _) 	= True
whatExprIsWHNF (ExprIsVar _) 	= True
whatExprIsWHNF (ExprIsInt _) 	= True
whatExprIsWHNF (ExprIsTup _) 	= True
whatExprIsWHNF ExprIsOtherWHNF 	= True
whatExprIsWHNF _ 				= False
%%]

%%[(8 codegen) hs export(whatExprIsLam, whatExprIsTup, whatExprIsBind)
whatExprIsBind :: WhatExpr -> Bool
whatExprIsBind = isJust . whatExprMbBind
{-# INLINE whatExprIsBind #-}

whatExprIsLam :: WhatExpr -> Bool
whatExprIsLam = isJust . whatExprMbLam
{-# INLINE whatExprIsLam #-}

-- | Is Expr a Tup?
whatExprIsTup :: WhatExpr -> Bool
whatExprIsTup (ExprIsTup _) = True
whatExprIsTup _             = False

%%]

%%[(8 codegen) hs export(whatExprIsFFI)
-- | Is Expr a FFI?
whatExprIsFFI :: WhatExpr -> Bool
whatExprIsFFI (ExprIsFFI  ) = True
whatExprIsFFI _             = False

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize, ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(5050 codegen) hs
instance Serialize ACoreBindAspectKey where
  sput = sputEnum8
  sget = sgetEnum8
%%]

%%[(50 codegen) hs
instance Serialize ACoreBindAspectKey
%%]

%%[(5050 codegen) hs
instance Serialize ACoreBindAspectKey where
  sput (ACoreBindAspectKey_Default       ) = sputWord8 0
  sput (ACoreBindAspectKey_Strict        ) = sputWord8 1
  sput (ACoreBindAspectKey_Ty            ) = sputWord8 2
%%[[(8 codegenanalysis)
  sput (ACoreBindAspectKey_RelevTy       ) = sputWord8 3
%%]]
  sput (ACoreBindAspectKey_Debug         ) = sputWord8 4
  sput (ACoreBindAspectKey_Core          ) = sputWord8 5
%%[[(8 coresysf)
  sput (ACoreBindAspectKey_SysF a        ) = sputWord8 6 >> sput a
%%]]
%%[[93
  sput (ACoreBindAspectKey_FusionRole    ) = sputWord8 7
%%]]
  sget = do
    t <- sgetWord8
    case t of
        0 -> return ACoreBindAspectKey_Default   
        1 -> return ACoreBindAspectKey_Strict    
        2 -> return ACoreBindAspectKey_Ty        
%%[[(8 codegenanalysis)
        3 -> return ACoreBindAspectKey_RelevTy   
%%]]
        4 -> return ACoreBindAspectKey_Debug     
        5 -> return ACoreBindAspectKey_Core      
%%[[(8 coresysf)
        6 -> liftM  ACoreBindAspectKey_SysF         sget    
%%]]
%%[[93
        7 -> return ACoreBindAspectKey_FusionRole
%%]]
%%]

%%[(50 codegen) hs
instance Serialize ACoreBindRef where
  sput (ACoreBindRef a b) = sput a >> sput b
  sget = liftM2 ACoreBindRef sget sget
%%]

%%[(50 codegen) hs
instance Serialize CaseAltFailReason
%%]

%%[(5050 codegen) hs
instance Serialize CaseAltFailReason where
  sput (CaseAltFailReason_Continue a) = sputWord8 0 >> sput a
  sput (CaseAltFailReason_Absence   ) = sputWord8 1
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  CaseAltFailReason_Continue sget
      1 -> return CaseAltFailReason_Absence
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance on CSubst: PP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
instance PP CSubstKey where
  pp (CSKey_UID i)  = pp i
  pp (CSKey_Nm  n)  = pp n
  pp (CSKey_Ref r)  = pp r
%%]

%%[(8 codegen) hs
instance (PP expr, PP ty) => PP (CSubstInfo' expr bind bindasp ty) where
  pp (CSITy         t    )  = pp t
  pp (CSIExpr       e    )  = pp e
%%[[9
  pp (CSIImpls      l r  )  = pp "CSIImpls" -- pp (fst $ coeWeaveOnAsSubst uidStart l r CExpr_CoeArg)
  pp (CSIBinds      b    )  = pp "CSIBinds" -- ppCBindL b
%%]]
%%]

