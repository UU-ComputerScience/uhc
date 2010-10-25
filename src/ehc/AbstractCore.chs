%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core and TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}AbstractCore}
%%]

%%[(8 codegen) import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Opts.Base},{%{EH}Ty})
%%]

%%[(8 codegen) import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[(8 codegen) import(Data.List, Data.Maybe, qualified Data.Map as Map, qualified Data.Set as Set, Control.Applicative((<|>),(<$>)))
%%]

%%[(20 codegen grin) hs import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]
%%[(20 codegen grin) hs import(Data.Typeable(Typeable), Data.Generics(Data))
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
class AbstractCore  expr metaval bind bindaspect bindcateg metabind ty pat patrest patfld alt
    | expr       ->      metaval bind bindaspect bindcateg metabind ty pat patrest patfld alt
    , metaval    -> expr
    , bind       -> expr
    , bindaspect -> expr
    , bindcateg  -> expr
    , metabind   -> expr
    , ty         -> expr
    , pat        -> expr
    , patrest    -> expr
    , patfld	 -> expr
    , alt    	 -> expr
  where
  ------------------------- constructing: expr -------------------------
  -- | 1 arg application, together with meta info about the argument
  acoreLam1Ty :: HsName -> ty -> expr -> expr
  
  -- | 1 lam abstraction, together with meta info about, and type of the argument
  acoreApp1 :: expr -> expr -> expr
  
  -- | a tuple, with tag, and ty
  acoreTagTupTy :: CTag -> ty -> [expr] -> expr
  
  -- | a value binding, for a name to value + type + metas + meta level
  acoreBind1CatLevMetasTy :: bindcateg -> HsName -> MetaLev -> (metabind,metaval) -> ty -> expr -> bind
  acoreBind1CatLevMetasTy bcat n mlev mb t e = acoreBind1Asp n [acoreBindasp1CatLevMetasTy bcat n mlev mb t e]
  
  -- | a value binding aspect, for a name to value + type + metas + meta level
  acoreBindasp1CatLevMetasTy :: bindcateg -> HsName -> MetaLev -> (metabind,metaval) -> ty -> expr -> bindaspect
  
  -- | a binding, for/from a single aspect (for now, later multiple)
  acoreBind1Asp :: HsName -> [bindaspect] -> bind
  
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
  
%%[[9
  -- | hole: let with hole for bindings to be filled in later by means of a CSubst
  acoreHoleLet :: UID -> expr -> expr
%%]]

  ------------------------- constructing: ty -------------------------
  -- Int
  -- acoreTyInt2 :: ty
  
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

%%[[97
  -- | pat boolean guard
  acorePatBoolExpr :: expr -> pat
%%]]
  ------------------------- constructing: pat field -------------------------
  -- | pat field
  acorePatFldTy :: ty -> (HsName,expr) -> HsName -> patfld

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
  
%%[[9
  -- | get default for metaval, for dicts
  acoreMetavalDfltDict :: metaval
%%]]
  
  -- | get default for metabind
  acoreMetabindDflt :: metabind
  
  -- | get error/default ty, type indexed by ty
  acoreTyErr :: String -> ty

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
  acoreUnPatFld :: patfld -> (ty,(HsName,expr),HsName)

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

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting to tupling with MetaVal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreMetaLift)
acoreMetaLift :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => f x -> f (x,m)
acoreMetaLift = fmap2Tuple acoreMetavalDflt
%%]

%%[(9 codegen) hs export(acoreMetaLiftDict)
acoreMetaLiftDict :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => f x -> f (x,m)
acoreMetaLiftDict = fmap2Tuple acoreMetavalDfltDict
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Properties of binding aspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- new
%%[(8 codegen) export(ACoreBindAspectKey(..),ACoreBindAspectKeyS,ACoreBindAspMp)
-- | A ACoreBindAspectKeyS formed out of multiple ACoreBindAspectKey identifies a particular binding aspect
data ACoreBindAspectKey
  = ACoreBindAspectKey_Default				-- identifies the default binding, if omitted in a reference this aspect is the one chosen.
  | ACoreBindAspectKey_Strict				-- the as strict as possible variant
  | ACoreBindAspectKey_Debug				-- internal debugging only
  deriving (Eq,Ord,Enum)

instance Show ACoreBindAspectKey where
  show ACoreBindAspectKey_Default 		= "default"
  show ACoreBindAspectKey_Strict 		= "strict"
  show ACoreBindAspectKey_Debug 		= "debug"

instance PP ACoreBindAspectKey where
  pp = pp . show

type ACoreBindAspectKeyS		=	Set.Set ACoreBindAspectKey
type ACoreBindAspMp x			=	Map.Map ACoreBindAspectKeyS x

acbaspkeyMk :: [ACoreBindAspectKey] -> ACoreBindAspectKeyS
acbaspkeyMk = Set.fromList
%%]

%%[(8 codegen) hs export(acbaspkeyDefault,acbaspkeyStrict,acbaspkeyDebug)
-- | predefined: 
acbaspkeyDefault :: ACoreBindAspectKeyS
acbaspkeyDefault = acbaspkeyMk
  [ ACoreBindAspectKey_Default ]

-- | predefined: 
acbaspkeyStrict :: ACoreBindAspectKeyS
acbaspkeyStrict = acbaspkeyMk
  [ ACoreBindAspectKey_Strict ]

-- | predefined: 
acbaspkeyDebug :: ACoreBindAspectKeyS
acbaspkeyDebug = acbaspkeyMk
  [ ACoreBindAspectKey_Debug ]
%%]

%%[(8 codegen) hs export(ppACBaspKeyS)
ppACBaspKeyS :: ACoreBindAspectKeyS -> PP_Doc
ppACBaspKeyS = ppCurlysCommas . Set.toList
%%]

%%[(20 codegen) hs
deriving instance Typeable ACoreBindAspectKey
deriving instance Data ACoreBindAspectKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A reference to an aspected value, i.e. a particular aspect of a binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(ACoreBindRef(..), acbrefAspKey)
-- | reference to binding aspect: name + aspect keys
data ACoreBindRef
  = ACoreBindRef
      { acbrefNm		:: !HsName
      , acbrefMbAspKey	:: !(Maybe ACoreBindAspectKeyS)
      }
  deriving (Eq,Ord)

acbrefAspKey :: ACoreBindRef -> ACoreBindAspectKeyS
acbrefAspKey = maybe acbaspkeyDefault id . acbrefMbAspKey
{-# INLINE acbrefAspKey #-}

instance Show ACoreBindRef where
  show (ACoreBindRef n a) = show n ++ "." ++ show a

%%]

%%[(8 codegen) hs export(ppACoreBindRef)
ppACoreBindRef :: (HsName -> PP_Doc) -> ACoreBindRef -> PP_Doc
ppACoreBindRef ppN (ACoreBindRef n (Just a)) = ppN n >|< (if a == acbaspkeyDefault then empty else "." >|< ppACBaspKeyS a)
ppACoreBindRef ppN (ACoreBindRef n _       ) = ppN n

instance PP ACoreBindRef where
  pp = ppACoreBindRef pp
%%]

%%[(20 codegen) hs
deriving instance Typeable ACoreBindRef
deriving instance Data ACoreBindRef
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreApp)
acoreApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> [e] -> e
acoreApp f as = foldl (\f a -> acoreApp1 f a) f as
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: lambda abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLam1,acoreLamTy,acoreLam)
acoreLam1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e
acoreLam1 a e = acoreLam1Ty a (acoreTyErr "acoreLam1") e
{-# INLINE acoreLam1 #-}

acoreLamTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [(HsName,t)] -> e -> e
acoreLamTy as e = foldr (\(n,t) e -> acoreLam1Ty n t e) e as

acoreLam :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [HsName] -> e -> e
acoreLam as e = foldr (\(n) e -> acoreLam1 n e) e as
%%]

%%[(8 codegen) export(acoreTagTup,acoreTupTy,acoreTup,acoreTag)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBind1CatLevMetaTyWith,acoreBind1CatLevMetaTy,acoreBind1CatLevTy,acoreBind1CatMetaTy,acoreBind1CatTy,acoreBind1Cat,acoreBind1Ty,acoreBind1)
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
acoreBind1Cat cat n e = acoreBind1CatTy cat n (acoreTyErr "acoreBind1Cat") e
{-# INLINE acoreBind1Cat #-}

acoreBind1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> b
acoreBind1 n e = acoreBind1Cat (acoreBindcategDflt e) n e
{-# INLINE acoreBind1 #-}
%%]

%%[(8 codegen) export(acoreBind1Metas,acoreBind1CatMeta,acoreBind1Meta)
acoreBind1Metas :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> (mbind,m) -> e -> b
acoreBind1Metas n m e = acoreBind1CatLevMetasTy (acoreBindcategDflt e) n metaLevVal m (acoreTyErr "acoreBind1Metas") e
{-# INLINE acoreBind1Metas #-}

acoreBind1CatMeta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> m -> e -> b
acoreBind1CatMeta cat n m e = acoreBind1CatLevMetaTy cat n metaLevVal m (acoreTyErr "acoreBind1CatMeta") e
{-# INLINE acoreBind1CatMeta #-}

acoreBind1Meta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> e -> b
acoreBind1Meta n m e = acoreBind1Metas n (acoreMetabindDflt,m) e
{-# INLINE acoreBind1Meta #-}

%%]

%%[(8 codegen) export(acoreBind1Asp1)
acoreBind1Asp1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> basp -> b
acoreBind1Asp1 n ba = acoreBind1Asp n [ba]
%%]

%%[(8 codegen) export(acoreBindasp1CatLevMetaTy,acoreBindasp1CatLevTy,acoreBindasp1CatMetaTy,acoreBindasp1CatTy,acoreBindasp1Cat)
acoreBindasp1CatLevMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> m -> t -> e -> basp
acoreBindasp1CatLevMetaTy bcat n mlev m t e = acoreBindasp1CatLevMetasTy bcat n mlev (acoreMetabindDflt,m) t e
{-# INLINE acoreBindasp1CatLevMetaTy #-}

acoreBindasp1CatLevTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> t -> e -> basp
acoreBindasp1CatLevTy cat n l t e = acoreBindasp1CatLevMetaTy cat n l acoreMetavalDflt t e
{-# INLINE acoreBindasp1CatLevTy #-}

acoreBindasp1CatMetaTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> m -> t -> e -> basp
acoreBindasp1CatMetaTy cat n m t e = acoreBindasp1CatLevMetaTy cat n metaLevVal m t e
{-# INLINE acoreBindasp1CatMetaTy #-}

acoreBindasp1CatTy :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> t -> e -> basp
acoreBindasp1CatTy cat n t e = acoreBindasp1CatLevTy cat n metaLevVal t e
{-# INLINE acoreBindasp1CatTy #-}

acoreBindasp1Cat :: (AbstractCore e m b basp bcat mbind t p pr pf a) => bcat -> HsName -> e -> basp
acoreBindasp1Cat cat n e = acoreBindasp1CatTy cat n (acoreTyErr "acoreBindasp1Cat") e
{-# INLINE acoreBindasp1Cat #-}
%%]

%%[(8 codegen) export(acoreBindasp1Metas,acoreBindasp1Meta)
acoreBindasp1Metas :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> (mbind,m) -> e -> basp
acoreBindasp1Metas n m e = acoreBindasp1CatLevMetasTy (acoreBindcategDflt e) n metaLevVal m (acoreTyErr "acoreBindasp1Metas") e
{-# INLINE acoreBindasp1Metas #-}

acoreBindasp1Meta :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> m -> e -> basp
acoreBindasp1Meta n m e = acoreBindasp1Metas n (acoreMetabindDflt,m) e
{-# INLINE acoreBindasp1Meta #-}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lifting utils for introducing type errors where a type is not yet provided
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreTyLift)
acoreTyLift :: (AbstractCore e m b basp bcat mbind t p pr pf a, Functor f) => String -> f x -> f (x,t)
acoreTyLift msg = fmap (\n -> (n,acoreTyErr msg))
{-# INLINE acoreTyLift #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: let
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLetMerge,acoreLet,acoreLetRec)
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
%%]

%%[(8 codegen) export(acoreLetN)
acoreLetN :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => [(bcat,[b])] -> e -> e
acoreLetN cbs e = foldr (\(c,bs) e -> acoreLet c bs e) e cbs
%%]

%%[(8 codegen) export(acoreLet1PlainTy,acoreLet1Plain)
acoreLet1PlainTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1PlainTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategPlain

acoreLet1Plain :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e -> e
acoreLet1Plain nm e = acoreLet1PlainTy nm (acoreTyErr "acoreLet1Plain") e
{-# INLINE acoreLet1Plain #-}
%%]

%%[(8 codegen) export(acoreLet1StrictTy,acoreLet1Strict)
acoreLet1StrictTy :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> e -> e -> e
acoreLet1StrictTy nm t e
  = acoreLet cat [acoreBind1CatTy cat nm t e]
  where cat = acoreBindcategStrict

acoreLet1Strict :: (Eq bcat, AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> e -> e -> e
acoreLet1Strict nm e = acoreLet1StrictTy nm (acoreTyErr "acoreLet1Strict") e
{-# INLINE acoreLet1Strict #-}
%%]

%%[(8 codegen) hs export(acoreLet1StrictInMetaTyWith,acoreLet1StrictInMetaTy,acoreLet1StrictInMeta,acoreLet1StrictIn,acoreLet1StrictInTy)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: hole
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) hs export(acoreNmHolePred,acoreNmHole)
acoreNmHole :: (AbstractCore e m b basp bcat mbind t p pr pf a) => UID -> e
acoreNmHole = acoreVar . mkHNm

acoreNmHolePred :: (AbstractCore e m b basp bcat mbind t p pr pf a) => PredOccId -> e
acoreNmHolePred = acoreNmHole . poiId
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: defaults
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBindcategDflt)
-- | get default for bindcateg
acoreBindcategDflt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => e -> bcat
acoreBindcategDflt _ = acoreBindcategPlain
{-# INLINE acoreBindcategDflt #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreChar,acoreInt,acoreInt2)
acoreChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Char -> e
acoreChar i = let x = acoreCharTy acoreTyChar i in x

acoreInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Int -> e
acoreInt i = let x = acoreIntTy acoreTyInt i in x

acoreInt2 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Integer -> e
acoreInt2 i = let x = acoreIntTy2 acoreTyInt i in x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operator/value construction, expressed in terms of builtin primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreBuiltinApp)
acoreBuiltinApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> (EHBuiltinNames -> HsName) -> [e] -> e
acoreBuiltinApp opts bnmOf args = acoreVar (ehcOptBuiltin opts bnmOf) `acoreApp` args
%%]

%%[(8 codegen) hs export(acoreBuiltinAddInt)
acoreBuiltinAddInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinAddInt opts e i
  = if i == 0
    then e
    else case acoreExprMbInt e of
           Just (t,i') -> acoreIntTy2 t (toInteger i + i')
           _           -> acoreBuiltinApp opts ehbnPrimAddInt [e,acoreInt i]
%%]

%%[(8 codegen) hs export(acoreBuiltinGtInt)
acoreBuiltinGtInt :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinGtInt opts e i = acoreBuiltinApp opts ehbnPrimGtInt [e,acoreInt i]
%%]

%%[(99 codegen) hs export(acoreBuiltinEqChar)
acoreBuiltinEqChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Char -> e -> e
acoreBuiltinEqChar opts c e = acoreBuiltinApp opts ehbnPrimEqChar [e,acoreChar c]
%%]

%%[(8 codegen) hs export(acoreBuiltinString)
acoreBuiltinString :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinString opts m = let x = acoreBuiltinApp opts ehbnPackedStringToString [acoreStringTy (acoreTyString opts) m] in x
%%]

%%[(8 codegen) hs export(acoreBuiltinError,acoreBuiltinUndefined)
acoreBuiltinError :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinError opts m = acoreBuiltinApp opts ehbnError [acoreBuiltinString opts m]

acoreBuiltinUndefined :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e
acoreBuiltinUndefined opts = acoreBuiltinApp opts ehbnUndefined []
%%]

%%[(97 codegen) hs export(acoreBuiltinInteger)
-- | Builtin Integer
acoreBuiltinInteger :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Integer -> e
acoreBuiltinInteger opts i = acoreBuiltinApp opts ehbnPackedStringToInteger [acoreStringTy (acoreTyString opts) (show i)]
%%]

%%[(99 codegen) hs export(acoreBuiltinListSingleton)
-- | Builtin list singleton (note: hardcoded of tags)
acoreBuiltinListSingleton :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> e -> e
acoreBuiltinListSingleton opts e
  = acoreTagTupTy (ctagCons opts) (acoreTyErr "acoreBuiltinListSingleton.Cons") [e, acoreTagTupTy (ctagNil opts) (acoreTyErr "acoreBuiltinListSingleton.Nil") []]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatConMbTag,acoreAltMbTag)
-- | when pat is con get tag
acorePatConMbTag :: (AbstractCore e m b basp bcat mbind t p pr pf a) => p -> Maybe CTag
acorePatConMbTag = fmap (\(tg,_,_) -> tg) . acorePatMbCon

-- | possibly get tag of alt
acoreAltMbTag :: (AbstractCore e m b basp bcat mbind t p pr pf a) => a -> Maybe CTag
acoreAltMbTag = (\p ->     (\(tg,_,_) -> tg) <$> acorePatMbCon  p
                       <|> (const ctagInt)   <$> acorePatMbInt  p
                       -- <|> (const ctagChar)  <$> acorePatMbChar p
                ) . fst . acoreUnAlt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Specific constructs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(91 codegen) hs export(acoreIf)
-- | Construct 'if' expression. Hardcoded: tag nr, ordered alts (by tag)
acoreIf :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Maybe HsName -> e -> e -> e -> e
acoreIf opts cn c t f
  = acoreMbLet1StrictInMetaTyWith id id (fmap (\n -> (n,acoreTyErr "acoreIf: Bool")) cn) acoreMetavalDflt c
    $ (\c -> acoreCaseDflt c
               [ acoreAlt (acorePatCon (ctagFalse opts) acorePatRestEmpty []) f
               , acoreAlt (acorePatCon (ctagTrue  opts) acorePatRestEmpty []) t
               ]
               Nothing {-(tcUndefined opts)-}
      )
%%]

%%[(99 codegen) hs export(acoreMatchChar)
acoreMatchChar :: (AbstractCore e m b basp bcat mbind t p pr pf a) => EHCOpts -> Maybe HsName -> Char -> e -> e -> e -> e
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

%%[(8 codegen) hs export(Coe'(..))
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
%%[[9
  | Coe_LamLet   		!HsName !ty !UID				-- lambda with a let binding in the body
  | Coe_LetRec   		![bind]							-- let rec
  | Coe_ImplApp  		!ImplsVarId						-- implicits, for apply
  | Coe_ImplLam  		!ImplsVarId						-- implicits, for lambda
%%]]

instance Show (Coe' expr metaval bind bindasp ty) where
  show _ = "COE"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCoeId, acoreCoeMap)
-- | Non inspectable, most general, coercion
acoreCoeMap :: (e -> e) -> Coe' e m b ba t
acoreCoeMap = Coe_Map
{-# INLINE acoreCoeMap #-}

-- | Coe identity
acoreCoeId :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t
acoreCoeId = Coe_C acoreCoeArg
{-# INLINE acoreCoeId #-}
%%]

%%[(9 codegen) hs export(acoreCoeLamLetTy,acoreCoeLamLet, acoreCoeLetRec)
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
%%]

%%[(8 codegen) hs export(acoreCoeApp1,acoreCoeAppN,acoreCoeAppNbyName)
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
%%]

%%[(8 codegen) hs export(acoreCoeLam1Ty,acoreCoeLam1)
-- | Lambda still requiring a body
acoreCoeLam1Ty :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> t -> Coe' e m b ba t
acoreCoeLam1Ty = Coe_Lam
{-# INLINE acoreCoeLam1Ty #-}

acoreCoeLam1 :: (AbstractCore e m b basp bcat mbind t p pr pf a) => HsName -> Coe' e m b ba t
acoreCoeLam1 n = acoreCoeLam1Ty n (acoreTyErr "acoreCoeLam1")
{-# INLINE acoreCoeLam1 #-}
%%]

%%[(8 codegen) hs export(acoreCoeCompose)
-- | Composition of 2 Coe's
acoreCoeCompose :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> Coe' e m b ba t -> Coe' e m b ba t
acoreCoeCompose c1 c2
  | acoreCoeIsId c1 = c2
  | otherwise  = Coe_Compose c1 c2

%%]

%%[(9 codegen) hs export(acoreCoePoiLApp,acoreCoeImplsApp)
acoreCoePoiLApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => [PredOccId] -> [Coe' e m b ba t]
acoreCoePoiLApp = map (\i -> acoreCoeApp1 (acoreNmHolePred i))

acoreCoeImplsApp :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Impls -> [Coe' e m b ba t]
acoreCoeImplsApp = acoreCoePoiLApp . implsPrIds
%%]

%%[(9 codegen) hs export(acoreCoePoiLLamTy,acoreCoePoiLLam,acoreCoeImplsLam)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion predicates/inspection/...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCoeIsId)
acoreCoeIsId :: (AbstractCore e m b basp bcat mbind t p pr pf a) => Coe' e m b ba t -> Bool
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
  deriving (Show,Eq,Ord)
%%]

%%[(8 codegen) hs export(CSubstInfo'(..))
data CSubstInfo' expr metaval bind bindasp ty
  =  CSITy        { csiTy      :: !ty
                  }
  |  CSIExpr      { csiRepl    :: !expr
                  }
%%[[9
  |  CSIImpls     { csiAppCoeL :: ![Coe' expr metaval bind bindasp ty]
                  , csiLamCoeL :: ![Coe' expr metaval bind bindasp ty]
                  }
  |  CSIBinds     { csiBindL   :: ![bind]
                  }
%%]]

instance Show (CSubstInfo' e m b ba t) where
  show _ = "CSubstInfo'"
%%]

%%[(8 codegen) hs export(CSubst',emptyCSubst)
type CSubst' e m b ba t = Map.Map CSubstKey (CSubstInfo' e m b ba t)

emptyCSubst :: CSubst' e m b ba t
emptyCSubst = Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreCSubstFromUidTyL)
acoreCSubstFromUidTyL :: AssocL HsName t -> CSubst' e m b ba t
acoreCSubstFromUidTyL l = Map.fromList [ (CSKey_Nm k,CSITy v) | (k,v) <- l ]
%%]
  
%%[(8 codegen) hs export(acoreCSubstFromUidExprL)
acoreCSubstFromUidExprL :: AssocL UID e -> CSubst' e m b ba t
acoreCSubstFromUidExprL l = Map.fromList [ (CSKey_UID k,CSIExpr v) | (k,v) <- l ]
%%]
  
%%[(9 codegen) hs export(acoreCSubstFromUidImplsL,acoreCSubstFromUidBindLL)
acoreCSubstFromUidBindLL :: AssocL UID [b] -> CSubst' e m b ba t
acoreCSubstFromUidBindLL l = Map.fromList [ (CSKey_UID k,CSIBinds v) | (k,v) <- l ]

acoreCSubstFromUidImplsL :: AssocL UID ([Coe' e m b ba t],[Coe' e m b ba t]) -> CSubst' e m b ba t
acoreCSubstFromUidImplsL l = Map.fromList [ (CSKey_UID k,uncurry CSIImpls v) | (k,v) <- l ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst combination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(cSubstAppSubst)
-- | Combine CSubst: union only, application is postponed
cSubstAppSubst :: CSubst' e m b ba t -> CSubst' e m b ba t -> CSubst' e m b ba t
cSubstAppSubst = Map.union
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution as class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CSubstitutable(..))
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract syntax for encoding case+pattern rewrite info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(RAlt'(..),RPat'(..),RPatConBind'(..),RPatFld'(..),RCEAltL')
data RAlt' e t b pr
  = RAlt_Alt			{ rcaPats :: ![RPat' e t b pr], raaExpr :: !e, raaFailS :: UIDS }

data RPat' e t b pr
  = RPat_Var			{ rcpPNm :: !RPatNm, rcpTy :: !t }
  | RPat_Con			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpTag :: !CTag, rcpBinds :: !(RPatConBind' e t b pr) }
  | RPat_Int			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpInt :: !Integer }
  | RPat_Char			{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpChar :: !Char }
  | RPat_Irrefutable	{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpValBindL :: ![b] }
%%[[97
  | RPat_BoolExpr		{ rcpPNm :: !RPatNm, rcpTy :: !t, rcpExpr :: !e, rcpMbConst :: Maybe SrcConst }
%%]]

data RPatConBind' e t b pr
  = RPatConBind_One		{ rpcbRest :: !pr, rpcbBinds :: ![RPatFld' e t b pr] }
  | RPatConBind_Many	{ rpcbConBinds :: ![RPatConBind' e t b pr] }

data RPatFld' e t b pr
  = RPatFld_Fld		    { rpbLbl :: !HsName, rpbOffset :: !e, rpbNm :: !HsName, rpbPat :: !(RPat' e t b pr)}

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
raltIsVar (RAlt_Alt (RPat_Var _ _ : _) _ _)  = True
raltIsVar _                                  = False

raltIsConst :: RAlt' e t b pr -> Bool
raltIsConst (RAlt_Alt (p : _) _ _)
  = c p
  where c (RPat_Int   _ _ _) = True
        c (RPat_Char  _ _ _) = True
        c _                  = False
%%]

%%[(8 codegen) hs export(raltIsConMany)
raltIsConMany :: RAlt' e t b pr -> Bool
raltIsConMany (RAlt_Alt (RPat_Con _ _ _ (RPatConBind_Many _) : _) _ _) = True
raltIsConMany _                                                      = False
%%]

%%[(8 codegen) hs export(raltIsIrrefutable)
raltIsIrrefutable :: RAlt' e t b pr -> Bool
raltIsIrrefutable (RAlt_Alt (RPat_Irrefutable _ _ _ : _) _ _) = True
raltIsIrrefutable _                                         = False
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
acoreRPat2Pat :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPat' e t b pr -> p
acoreRPat2Pat p
  = case p of
      RPat_Var      n ty      -> acorePatVarTy  (rpatNmNm n) ty
      RPat_Con      n _ t b   -> acorePatCon    t r bs
                              where (r,bs) = acoreRPatConBind2PatConBind b
      RPat_Int      n ty v    -> acorePatIntTy2 ty v
      RPat_Char     n ty v    -> acorePatCharTy ty v
%%[[97
      RPat_BoolExpr n _ v _   -> acorePatBoolExpr  v
%%]]
%%]

%%[(8 codegen) hs
acoreRPatConBind2PatConBind :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPatConBind' e t b pr -> (pr,[pf])
acoreRPatConBind2PatConBind b
  = case b of
  	  RPatConBind_One 	r bs 	-> (r,map acoreRPatBind2PatFld bs)
  	  RPatConBind_Many 	bs 		-> head (map acoreRPatConBind2PatConBind bs)

acoreRPatBind2PatFld :: (AbstractCore e m b basp bcat mbind t p pr pf a) => RPatFld' e t b pr -> pf
acoreRPatBind2PatFld (RPatFld_Fld l o _ p@(RPat_Var n _)) = acorePatFldTy (rcpTy p) (l,o) (rpatNmNm n)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: Bool
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(8 codegen) hs export(ctagTrue, ctagFalse)
ctagTrue, ctagFalse :: EHCOpts -> CTag
ctagTrue  opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolTrue)  1 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagFalse opts = CTag (ehcOptBuiltin opts ehbnDataBool) (ehcOptBuiltin opts ehbnBoolFalse) 0 0 0		-- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CTag: List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

In the following, note the hardcodedness!!!!!

%%[(8 codegen) hs export(ctagCons,ctagNil)
ctagCons, ctagNil :: EHCOpts -> CTag
ctagCons opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltCons) 0 2 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!
ctagNil  opts = CTag (ehcOptBuiltin opts ehbnDataList) (ehcOptBuiltin opts ehbnDataListAltNil ) 1 0 2		-- this makes it hardcoded, ideally dependent on datatype def itself !!
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reason a case alternative can fail
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CaseAltFailReason(..))
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
%%]

%%[(8 codegen) hs export(cafailHasId)
cafailHasId :: CaseAltFailReason -> (Bool,UID)
cafailHasId (CaseAltFailReason_Absence   ) = (False,uidUnused)
cafailHasId (CaseAltFailReason_Continue i) = (True ,i)
%%]

%%[(20 codegen) hs
deriving instance Typeable CaseAltFailReason
deriving instance Data CaseAltFailReason
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Kind of function which is used in an app
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(AppFunKind(..))
data AppFunKind
  = AppFunKind_NoApp					-- inlined Nothing
  | AppFunKind_Fun 	ACoreBindRef
  | AppFunKind_Tag 	CTag
  | AppFunKind_FFI
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Context: what is above/below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(WhatExpr(..))
data WhatExpr
  = ExprIsLam
  | ExprIsApp	Int			-- arity
  | ExprIsVar 	HsName
  | ExprIsInt 	Int
  | ExprIsOther
  | ExprIsBind
  deriving Eq
%%]

%%[(8 codegen) hs export(whatExprMbApp,whatExprAppArity)
-- | is an app?
whatExprMbApp :: WhatExpr -> Maybe Int
whatExprMbApp (ExprIsApp a) = Just a
whatExprMbApp _             = Nothing

-- | app arity
whatExprAppArity :: WhatExpr -> Int
whatExprAppArity (ExprIsApp a) = a
whatExprAppArity _             = 0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize, ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen) hs
instance Serialize ACoreBindAspectKey where
  sput = sputEnum8
  sget = sgetEnum8
%%]

%%[(20 codegen) hs
instance Serialize ACoreBindRef where
  sput (ACoreBindRef a b) = sput a >> sput b
  sget = liftM2 ACoreBindRef sget sget
%%]

%%[(20 codegen) hs
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
%%]

%%[(8 codegen) hs
instance (PP expr, PP ty) => PP (CSubstInfo' expr metaval bind bindasp ty) where
  pp (CSITy         t    )  = pp t
  pp (CSIExpr       e    )  = pp e
%%[[9
  pp (CSIImpls      l r  )  = pp "CSIImpls" -- pp (fst $ coeWeaveOnAsSubst uidStart l r CExpr_CoeArg)
  pp (CSIBinds      b    )  = pp "CSIBinds" -- ppCBindL b
%%]]
%%]

