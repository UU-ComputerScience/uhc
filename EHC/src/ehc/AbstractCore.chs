%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Class interface around common functionality of Core and TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}AbstractCore}
%%]

%%[(8 codegen) import({%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty})
%%]

%%[(8 codegen) import(qualified Data.Map as Map, qualified Data.Set as Set, Control.Applicative)
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
class AbstractCore expr metaval bind bindcateg metabind ty pat patrest patfld alt
    | expr      -> bind metaval ty bindcateg metabind pat
    , bind      -> expr
    , ty        -> expr
    , bindcateg -> expr
    , pat       -> patrest patfld alt expr
    , alt    	-> pat expr
    , patfld	-> bind
  where
  ------------------------- constructing: expr -------------------------
  -- | 1 arg application, together with meta info about the argument
  acoreLam1MetaTy :: HsName -> metaval -> ty -> expr -> expr
  
  -- | 1 lam abstraction, together with meta info about, and type of the argument
  acoreApp1Meta :: expr -> expr -> metaval -> expr
  
  -- | a tuple, with tag, and ty
  acoreTagTupTy :: CTag -> ty -> [expr] -> expr
  
  -- | a value binding, for a name to value + type + metas + meta level
  acoreBind1CatLevMetasTy :: bindcateg -> HsName -> MetaLev -> (metabind,metaval) -> ty -> expr -> bind
  
  -- | basic let binding
  acoreLetBase :: bindcateg -> [bind] -> expr -> expr
  
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
  
  ------------------------- constructing: pat -------------------------
  -- | pat var, with type
  acorePatVarTy :: HsName -> ty -> pat
  
  -- | pat con
  acorePatCon :: CTag -> patrest -> [patfld] -> pat

  ------------------------- constructing: pat field -------------------------
  -- | pat field
  acorePatFldTy :: ty -> (HsName,expr) -> HsName -> patfld

  ------------------------- constructing: patrest -------------------------
  -- | patrest, empty
  acorePatRestEmpty :: pat -> patrest

  ------------------------- constructing: alt -------------------------
  -- | 1 arg application, together with meta info about the argument
  acoreAlt :: pat -> expr -> alt
  
  ------------------------- type related -------------------------
  -- | convert Ty to ty
  -- acoreTy2ty :: Ty -> ty
  
  ------------------------- defaults -------------------------
  -- | get default for metaval, 1st arg (only for type evidence and) may not be used
  acoreMetavalDflt :: expr -> metaval
  
  -- | get default for metabind, 1st arg (only for type evidence and) may not be used
  acoreMetabindDflt :: expr -> metabind
  
  -- | get error/default ty, 1st arg (only for type evidence and) may not be used, type indexed by expr
  acoreTyErr :: expr -> String -> ty

  -- | get error/default ty, 1st arg (only for type evidence and) may not be used, type indexed by pat
  acoreTyErr2 :: pat -> String -> ty

  -- | get char ty, 1st arg (only for type evidence and) may not be used
  acoreTyChar :: expr -> ty

  -- | get char ty, 1st arg (only for type evidence and) may not be used
  acoreTyInt :: expr -> ty

  -- | get String ty, 1st arg (only for type evidence and) may not be used
  acoreTyString :: EHCOpts -> expr -> ty

  ------------------------- bindcateg values -------------------------
  -- | get recursive bindcateg
  acoreBindcategRec :: expr -> bindcateg
  
  -- | get strict bindcateg
  acoreBindcategStrict :: expr -> bindcateg
  
  -- | get plain bindcateg
  acoreBindcategPlain :: expr -> bindcateg
  
  ------------------------- inspecting/deconstructing -------------------------
  -- | is expr a let?
  acoreExprMbLet :: expr -> Maybe (bindcateg,[bind],expr)

  -- | is expr a int?
  acoreExprMbInt :: expr -> Maybe (ty,Integer)

  -- | is bindcateg recursive?
  acoreBindcategMbRec :: bindcateg -> Maybe bindcateg

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

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreAppMeta,acoreApp1,acoreApp)
acoreAppMeta :: (AbstractCore e m b bcat mbind t p pr pf a) => e -> [(e,m)] -> e
acoreAppMeta f as = foldl (\f (a,m) -> acoreApp1Meta f a m) f as

acoreApp1 :: (AbstractCore e m b bcat mbind t p pr pf a) => e -> e -> e
acoreApp1 f a = acoreApp1Meta f a (acoreMetavalDflt a)

acoreApp :: (AbstractCore e m b bcat mbind t p pr pf a) => e -> [e] -> e
acoreApp f as = acoreAppMeta f (zip as $ repeat $ acoreMetavalDflt f)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: lambda abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLam1Meta,acoreLam1Ty,acoreLam1,acoreLamMetaTy,acoreLamMeta,acoreLamTy,acoreLam)
acoreLam1Meta :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> m -> e -> e
acoreLam1Meta a m e = acoreLam1MetaTy a m (acoreTyErr e "acoreLam1Meta") e

acoreLam1Ty :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> t -> e -> e
acoreLam1Ty a t e = acoreLam1MetaTy a (acoreMetavalDflt e) t e

acoreLam1 :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> e -> e
acoreLam1 a e = acoreLam1MetaTy a (acoreMetavalDflt e) (acoreTyErr e "acoreLam1") e

acoreLamMetaTy :: (AbstractCore e m b bcat mbind t p pr pf a) => [(HsName,m,t)] -> e -> e
acoreLamMetaTy as e = foldr (\(n,m,t) e -> acoreLam1MetaTy n m t e) e as

acoreLamMeta :: (AbstractCore e m b bcat mbind t p pr pf a) => [(HsName,m)] -> e -> e
acoreLamMeta as e = foldr (\(n,m) e -> acoreLam1Meta n m e) e as

acoreLamTy :: (AbstractCore e m b bcat mbind t p pr pf a) => [(HsName,t)] -> e -> e
acoreLamTy as e = foldr (\(n,t) e -> acoreLam1Ty n t e) e as

acoreLam :: (AbstractCore e m b bcat mbind t p pr pf a) => [HsName] -> e -> e
acoreLam as e = foldr (\(n) e -> acoreLam1 n e) e as

%%]

%%[(8 codegen) export(acoreTagTup,acoreTupTy,acoreTup,acoreTag)
acoreTagTup :: (AbstractCore e m b bcat mbind t p pr pf a) => CTag -> [e] -> e
acoreTagTup tg es = acoreTagTupTy tg (acoreTyErr (head es) "acoreTupTy") es

acoreTupTy :: (AbstractCore e m b bcat mbind t p pr pf a) => t -> [e] -> e
acoreTupTy t es = acoreTagTupTy CTagRec t es

acoreTup :: (AbstractCore e m b bcat mbind t p pr pf a) => [e] -> e
acoreTup es = acoreTagTup CTagRec es

acoreTag :: (AbstractCore e m b bcat mbind t p pr pf a) => CTag -> e
acoreTag tg = acoreTagTup tg []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBind1CatLevMetaTy,acoreBind1CatLevTy,acoreBind1CatTy,acoreBind1Cat,acoreBind1Ty,acoreBind1)
acoreBind1CatLevMetaTy :: (AbstractCore e m b bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> m -> t -> e -> b
acoreBind1CatLevMetaTy cat n l m t e = acoreBind1CatLevMetasTy cat n l (acoreMetabindDflt e,m) t e

acoreBind1CatLevTy :: (AbstractCore e m b bcat mbind t p pr pf a) => bcat -> HsName -> MetaLev -> t -> e -> b
acoreBind1CatLevTy cat n l t e = acoreBind1CatLevMetaTy cat n l (acoreMetavalDflt e) t e

acoreBind1CatTy :: (AbstractCore e m b bcat mbind t p pr pf a) => bcat -> HsName -> t -> e -> b
acoreBind1CatTy cat n t e = acoreBind1CatLevTy cat n metaLevVal t e

acoreBind1Ty :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> t -> e -> b
acoreBind1Ty n t e = acoreBind1CatTy (acoreBindcategDflt e) n t e

acoreBind1Cat :: (AbstractCore e m b bcat mbind t p pr pf a) => bcat -> HsName -> e -> b
acoreBind1Cat cat n e = acoreBind1CatTy cat n (acoreTyErr e "acoreBind1Cat") e

acoreBind1 :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> e -> b
acoreBind1 n e = acoreBind1Cat (acoreBindcategDflt e) n e
%%]

%%[(8 codegen) export(acoreBind1Metas,acoreBind1CatMeta,acoreBind1Meta)
acoreBind1Metas :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> (mbind,m) -> e -> b
acoreBind1Metas n m e = acoreBind1CatLevMetasTy (acoreBindcategDflt e) n metaLevVal m (acoreTyErr e "acoreBind1Metas") e

acoreBind1CatMeta :: (AbstractCore e m b bcat mbind t p pr pf a) => bcat -> HsName -> m -> e -> b
acoreBind1CatMeta cat n m e = acoreBind1CatLevMetaTy cat n metaLevVal m (acoreTyErr e "acoreBind1CatMeta") e

acoreBind1Meta :: (AbstractCore e m b bcat mbind t p pr pf a) => HsName -> m -> e -> b
acoreBind1Meta n m e = acoreBind1Metas n (acoreMetabindDflt e,m) e

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: let
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreLetMerge,acoreLet,acoreLetRec)
-- | Construct let, possibly merging bindings
acoreLetMerge :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => Bool -> bcat -> [b] -> e -> e
acoreLetMerge merge c bs e
  = if null bs
    then e
    else case acoreExprMbLet e of
           Just (c',bs',e') | merge && c' == c
             -> mk c (bs++bs') e'
           _ -> mk c bs e
  where mk c bs e
          = case acoreBindcategMbRec c of
              Just c -> acoreLetBase c bs e
              _      -> foldr (\b e -> acoreLetBase c [b] e) e bs

acoreLet :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => bcat -> [b] -> e -> e
acoreLet c bs e = acoreLetMerge False c bs e

acoreLetRec :: (Eq bcat, AbstractCore e m b bcat mbind t p pr pf a) => [b] -> e -> e
acoreLetRec bs e = acoreLet (acoreBindcategRec e) bs e
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: defaults
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreBindcategDflt)
-- | get default for bindcateg
acoreBindcategDflt :: (AbstractCore e m b bcat mbind t p pr pf a) => e -> bcat
acoreBindcategDflt = acoreBindcategPlain
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acoreChar,acoreInt,acoreInt2)
acoreChar :: (AbstractCore e m b bcat mbind t p pr pf a) => Char -> e
acoreChar i = let x = acoreCharTy (acoreTyChar x) i in x

acoreInt :: (AbstractCore e m b bcat mbind t p pr pf a) => Int -> e
acoreInt i = let x = acoreIntTy (acoreTyInt x) i in x

acoreInt2 :: (AbstractCore e m b bcat mbind t p pr pf a) => Integer -> e
acoreInt2 i = let x = acoreIntTy2 (acoreTyInt x) i in x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operator construction, expressed in terms of builtin primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(acoreBuiltinApp)
acoreBuiltinApp :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> (EHBuiltinNames -> HsName) -> [e] -> e
acoreBuiltinApp opts bnmOf args = acoreVar (ehcOptBuiltin opts bnmOf) `acoreApp` args
%%]

%%[(8 codegen) hs export(acoreBuiltinAddInt)
acoreBuiltinAddInt :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinAddInt opts e i
  = if i == 0
    then e
    else case acoreExprMbInt e of
           Just (t,i') -> acoreIntTy2 t (toInteger i + i')
           _           -> acoreBuiltinApp opts ehbnPrimAddInt [e,acoreInt i]
%%]

%%[(8 codegen) hs export(acoreBuiltinGtInt)
acoreBuiltinGtInt :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> e -> Int -> e
acoreBuiltinGtInt opts e i = acoreBuiltinApp opts ehbnPrimGtInt [e,acoreInt i]
%%]

%%[(99 codegen) hs export(acoreBuiltinEqChar)
acoreBuiltinEqChar :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> Char -> e -> e
acoreBuiltinEqChar opts c e = acoreBuiltinApp opts ehbnPrimEqChar [e,acoreChar c]
%%]

%%[(8 codegen) hs export(acoreBuiltinString)
acoreBuiltinString :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinString opts m = let x = acoreBuiltinApp opts ehbnPackedStringToString [acoreStringTy (acoreTyString opts x) m] in x
%%]

%%[(8 codegen) hs export(acoreBuiltinError,acoreBuiltinUndefined)
acoreBuiltinError :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> String -> e
acoreBuiltinError opts m = acoreBuiltinApp opts ehbnError [acoreBuiltinString opts m]

acoreBuiltinUndefined :: (AbstractCore e m b bcat mbind t p pr pf a) => EHCOpts -> e
acoreBuiltinUndefined opts = acoreBuiltinApp opts ehbnUndefined []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived functionality: inspection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(acorePatConMbTag,acoreAltMbTag)
-- | when pat is con get tag
acorePatConMbTag :: (AbstractCore e m b bcat mbind t p pr pf a) => p -> Maybe CTag
acorePatConMbTag = fmap (\(tg,_,_) -> tg) . acorePatMbCon

-- | possibly get tag of alt
acoreAltMbTag :: (AbstractCore e m b bcat mbind t p pr pf a) => a -> Maybe CTag
acoreAltMbTag = (\p ->     (\(tg,_,_) -> tg) <$> acorePatMbCon  p
                       <|> (const ctagInt)   <$> acorePatMbInt  p
                       -- <|> (const ctagChar)  <$> acorePatMbChar p
                ) . fst . acoreUnAlt
%%]

