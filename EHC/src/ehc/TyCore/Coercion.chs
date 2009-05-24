%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 codegen) hs module {%{EH}TyCore.Coercion} import({%{EH}Base.Common},{%{EH}Base.Opts})
%%]

%%[(9 codegen) hs import({%{EH}TyCore.Base})
%%]

%%[(9 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[doesWhat doclatex
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The semantics of a coercion is its application to a Expr. See coeEvalOn.

%%[(9 codegen) hs export(Coe(..))
data Coe
  = Coe         !(Expr -> Expr)				-- normal, expression as function
  | CoeApp      !Expr !MetaVal				-- apply
  | CoeLam      !HsName !MetaVal !Ty		-- lambda
  | CoeLamLet   !HsName !Ty !UID			-- lambda with a let binding in the body
  | CoeLetRec   !ValBindL						-- let rec
  | CoeCompose  !Coe !Coe					-- composition
  | CoeC        !Expr						-- constant
  | CoeImplApp  !ImplsVarId					-- implicits, for apply
  | CoeImplLam  !ImplsVarId					-- implicits, for lambda
%%]

%%[(9 codegen) hs export(mkCoe)
mkCoe :: (Expr -> Expr) -> Coe
mkCoe = Coe
%%]

%%[(9 codegen) hs export(coeId, coeIsId, mkLamLetCoe, mkLetRecCoe)
coeId :: Coe
coeId = CoeC Expr_CoeArg

coeIsId :: Coe -> Bool
coeIsId (CoeC Expr_CoeArg) = True
-- coeIsId (CoeCompose c1 c2 ) = coeIsId c1 && coeIsId c2
coeIsId _                   = False

mkLamLetCoe :: HsName -> Ty -> UID -> Coe
mkLamLetCoe = CoeLamLet

mkLetRecCoe :: ValBindL -> Coe
mkLetRecCoe [] = coeId
mkLetRecCoe b  = CoeLetRec b

instance Show Coe where
  show _ = "COE"
%%]

%%[(9 codegen) hs export(mkAppCoe1With,mkAppCoe1,mkAppCoeWith,mkAppCoe)
mkAppCoe1With :: Expr -> MetaVal -> Coe
mkAppCoe1With = CoeApp -- a m = mkCoe (\e -> mkExprApp1Meta e a m)

mkAppCoe1 :: Expr -> Coe
mkAppCoe1 a = mkAppCoe1With a MetaVal_Val

mkAppCoeWith :: [(Expr,MetaVal)] -> Coe
mkAppCoeWith as = mkCoe (\e -> mkExprAppMeta e as)

mkAppCoe :: [Expr] -> Coe
mkAppCoe as = mkAppCoeWith (metaLift as)
%%]

%%[(9 codegen) hs export(mkLamCoe1With,mkLamCoe1)
mkLamCoe1With :: HsName -> MetaVal -> Ty -> Coe
mkLamCoe1With = CoeLam -- n m = mkCoe (\e -> mkExprLam1Meta n m e)

mkLamCoe1 :: HsName -> Ty -> Coe
mkLamCoe1 n t = mkLamCoe1With n MetaVal_Val t
%%]

%%[(9 codegen) hs export(coeCompose)
coeCompose :: Coe -> Coe -> Coe
coeCompose c1 c2
  | coeIsId c1 = c2
  | otherwise  = CoeCompose c1 c2

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
mkIdLRCoeWith :: HsName -> MetaVal -> Ty -> LRCoe
mkIdLRCoeWith n m t = mkIdLRCoe' (mkAppCoeWith [(Expr_Var n,m)]) (mkLamCoe1With n m t)
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

