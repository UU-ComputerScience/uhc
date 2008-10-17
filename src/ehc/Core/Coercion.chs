%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) hs module {%{EH}Core.Coercion} import({%{EH}Base.Common},{%{EH}Base.Opts})
%%]

%%[(9 codegen) hs import({%{EH}Ty},{%{EH}Core})
%%]

%%[(9 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The semantics of a coercion is its application to a CExpr. See coeEvalOn.

%%[(9 codegen) hs export(Coe(..))
data Coe
  = Coe         !(CExpr -> CExpr)			-- normal, expression as function
  | CoeApp      !CExpr !CMeta				-- apply
  | CoeLam      !HsName !CMeta				-- lambda
  | CoeLamLet   !HsName !UID				-- lambda with a let binding in the body
  | CoeLetRec   !CBindL						-- let rec
  | CoeCompose  !Coe !Coe					-- composition
  -- | CoeCaseAlt  RCEEnv HsName RPat			-- a single case alternative to be rewritten
  | CoeC        !CExpr						-- constant
  | CoeImplApp  !ImplsVarId					-- implicits, for apply
  | CoeImplLam  !ImplsVarId					-- implicits, for lambda
%%]

%%[(9 codegen) hs export(mkCoe)
mkCoe :: (CExpr -> CExpr) -> Coe
mkCoe = Coe
%%]

%%[(9 codegen) hs export(coeId, coeIsId, mkLamLetCoe, mkLetRecCoe)
coeId :: Coe
coeId = CoeC CExpr_CoeArg

coeIsId :: Coe -> Bool
coeIsId (CoeC CExpr_CoeArg) = True
coeIsId _                   = False

mkLamLetCoe :: HsName -> UID -> Coe
mkLamLetCoe = CoeLamLet -- n i = mkCoe (\e -> n `mkCExprLam1` mkCExprLetHole i e)

mkLetRecCoe :: CBindL -> Coe
mkLetRecCoe [] = coeId
mkLetRecCoe b  = CoeLetRec b --  = mkCoe (\e -> mkCExprLet CBindRec b e)

instance Show Coe where
  show _ = "COE"
%%]

%%[(9 codegen) hs export(mkAppCoe1With,mkAppCoe1,mkAppCoeWith,mkAppCoe)
mkAppCoe1With :: CExpr -> CMeta -> Coe
mkAppCoe1With = CoeApp -- a m = mkCoe (\e -> mkCExprApp1Meta e a m)

mkAppCoe1 :: CExpr -> Coe
mkAppCoe1 a = mkAppCoe1With a CMeta_Val

mkAppCoeWith :: [(CExpr,CMeta)] -> Coe
mkAppCoeWith as = mkCoe (\e -> mkCExprAppMeta e as)

mkAppCoe :: [CExpr] -> Coe
mkAppCoe as = mkAppCoeWith (cmetaLift as)
%%]

%%[(9 codegen) hs export(mkLamCoe1With,mkLamCoe1)
mkLamCoe1With :: HsName -> CMeta -> Coe
mkLamCoe1With = CoeLam -- n m = mkCoe (\e -> mkCExprLam1Meta n m e)

mkLamCoe1 :: HsName -> Coe
mkLamCoe1 n = mkLamCoe1With n CMeta_Val
%%]

%%[(9 codegen) hs export(coeCompose)
coeCompose :: Coe -> Coe -> Coe
coeCompose = CoeCompose -- c1 c2 =  mkCoe (\e -> c1 `coeEvalOn` (c2 `coeEvalOn` e))

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion for lamda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A LRCoe represents a coercion in a much more finegrained manner:
- a right Coe list, a list of coercions for building the rhs side of a subsumption, which must be the lambda
- a left Coe list a list of coercions for building the lhs side of a subsumption, which must be an application or other expr using the args of the left Coe

%%[(9 codegen) hs export(LRCoeKind(..),lrcoeKindOfCoe)
data LRCoeKind = LRCoeId | LRCoeOther deriving Eq

lrcoeKindAnd :: LRCoeKind -> LRCoeKind -> LRCoeKind
lrcoeKindAnd LRCoeId LRCoeId = LRCoeId
lrcoeKindAnd _       _       = LRCoeOther

lrcoeKindOfCoe :: Coe -> LRCoeKind
lrcoeKindOfCoe c = if coeIsId c then LRCoeId else LRCoeOther
%%]

%%[(9 codegen) hs export(LRCoe(..),emptyLRCoe)
data LRCoe
  = LRCoe
      { lrcoeKind		:: LRCoeKind
      , lrcoeLeftL		:: [Coe]
      , lrcoeRightL 	:: [Coe]
      }

emptyLRCoe :: LRCoe
emptyLRCoe = LRCoe LRCoeId [] []
%%]

%%[(9 codegen) hs export(lrcoeIsId)
lrcoeIsId :: LRCoe -> Bool
lrcoeIsId c = lrcoeKind c == LRCoeId
%%]

%%[(9 codegen) hs export(mkLRCoe)
mkLRCoe :: Coe -> Coe -> LRCoe
mkLRCoe l r = LRCoe LRCoeOther [l] [r]

mkIdLRCoe' :: Coe -> Coe -> LRCoe
mkIdLRCoe' l r = LRCoe LRCoeId [l] [r]
%%]

%%[(9 codegen) hs export(mkIdLRCoeWith)
mkIdLRCoeWith :: HsName -> CMeta -> LRCoe
mkIdLRCoeWith n m = mkIdLRCoe' (mkAppCoeWith [(CExpr_Var n,m)]) (mkLamCoe1With n m)
%%]

%%[(9 codegen) hs export(lrcoeLSingleton,lrcoeRSingleton,lrcoeLFromList,lrcoeRFromList)
lrcoeLFromList :: [Coe] -> LRCoe
lrcoeLFromList c = LRCoe LRCoeOther c []

lrcoeRFromList :: [Coe] -> LRCoe
lrcoeRFromList c = LRCoe LRCoeOther [] c

lrcoeLSingleton :: Coe -> LRCoe
lrcoeLSingleton c = LRCoe (lrcoeKindOfCoe c) [c] []

lrcoeRSingleton :: Coe -> LRCoe
lrcoeRSingleton c = LRCoe (lrcoeKindOfCoe c) [] [c]
%%]

%%[(9 codegen) hs export(lrcoeUnion)
lrcoeUnion :: LRCoe -> LRCoe -> LRCoe
lrcoeUnion (LRCoe k1 l1 r1) (LRCoe k2 l2 r2) = LRCoe (lrcoeKindAnd k1 k2) (l1 ++ l2) (r1 ++ r2)
%%]

