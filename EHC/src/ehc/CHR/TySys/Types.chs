%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Constraint/term language for CHR based type system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 chrtysys) module {%{EH}CHR.TySys.Types}
%%]

%%[(99 chrtysys) import({%{EH}Base.Common},{%{EH}Ty},UHC.Util.CHR,{%{EH}Substitutable},{%{EH}VarMp})
%%]

%%[(99 chrtysys) import(UHC.Util.Pretty as PP, UHC.Util.Utils, UHC.Util.RLList.LexScope)
%%]

%%[(99 chrtysys) import(qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(99 chrtysys) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[(99 chrtysys) import({%{EH}Opts.Base})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint type famility instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 chrtysys)  export (Cn(..), CnL, CnLevel, CnType(..), CnVarTelescope, CnTy(..))
-- % SYNTAX
-- % ======
-- FullConstraint ::= c(Ty, Level)   % Constraint
--                  | wk(Ty, Level)  % Well-kindedness
--          Level ::= assume(L) | prove(L)
-- 
--           Type ::= Ty : Type   % Type towers
--                  | *           % End with *
--                  | constraint  % End with Constraint
--             Ty ::= var(V)
--                  | con(C, TypeList)
--                  | fam(F, TypeList)
--                  | forall VarTelescope. Ty => Ty
--                  % Constraints are reified
--                  | ({VarSet} Ty => Ty) <= Ty

-- ?should it not be: FullConstraint ::= c(Type, Level) 

-- 
-- We assume the existence of the following constuctors:
-- - (->) :: * -> * -> *
-- - (~)  :: k -> k -> constraint
-- - is   :: k -> k -> l -> constraint
-- - ()   :: k
-- - (,)  :: k -> k -> k
-- So we write a -> b for con((->), [a, b])
--             a ~  b for con((~), [a, b])
--             a is b @ c for con(is, [a, b, c])
--             (a, b) for con((,), [a, b])
-- 


-- FullConstraint ::= c(Ty, Level)   % Constraint
--                  | wk(Ty, Level)  % Well-kindedness

data Cn
  = Cn			CnTy	CnLevel
  | Wk			CnTy	CnLevel
  deriving (Show, Eq, Ord, Generic, Typeable)

type CnL = LexScope

--          Level ::= assume(L) | prove(L)

type CnLevel = CnProveDirection CnL

data CnProveDirection x
  = CnProveDirection_Assume	x
  | CnProveDirection_Prove	x
  deriving (Show, Eq, Ord, Generic, Typeable)

--           Type ::= Ty : Type   % Type towers
--                  | *           % End with *
--                  | constraint  % End with Constraint

data CnType
  = CnType_Typed			CnTy	CnType
  | CnType_Star
  | CnType_Constraint
  deriving (Show, Eq, Ord, Generic, Typeable)

type CnVarTelescope = AssocL UID CnTy

--             Ty ::= var(V)
--                  | con(C, TypeList)
--                  | fam(F, TypeList)
--                  | forall VarTelescope. Ty => Ty
--                  % Constraints are reified
--                  | ({VarSet} Ty => Ty) <= Ty

data CnTy
  = CnTy_Var			UID
  | CnTy_Con			HsName	[CnType]
  | CnTy_Fam			HsName	[CnType]
  | CnTy_All			CnVarTelescope CnTy CnTy
  | CnTy_Inst			UIDS Ty Ty Ty
  deriving (Show, Eq, Ord, Generic, Typeable)

%%]


%%[(9999 chrtysys) export(Constraint, Constraint'(..))
-- | A Constraint is abstracted over the exact predicate, but differentiates on the role: to prove, can be assumed, and side effect of reduction
data Constraint' p info
  = Prove           { cnstrPred :: !p }             -- proof obligation
  | Assume          { cnstrPred :: !p }             -- assumed constraint
  | Reduction                                       -- 'side effect', residual info used by (e.g.) codegeneration
                    { cnstrPred :: !p               -- the pred to which reduction was done
                    , cnstrInfo :: !info            -- additional reduction specific info w.r.t. codegeneration
                    , cnstrFromPreds :: ![p]        -- the preds from which reduction was done
%%[[15
                    , cnstrVarMp :: VarMp           -- additional bindings for type (etc.) variables, i.e. improving substitution
%%]]
                    }
  deriving (Eq, Ord, Show, Generic)

type Constraint = Constraint' CHRPredOcc RedHowAnnotation

type instance TTKey (Constraint' p info) = TTKey p
type instance TrTrKey (Constraint' p info) = TTKey p
%%]

%%[(9999 chrtysys)
#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable  Constraint'
#else
deriving instance Typeable2 Constraint'
#endif
%%]

%%[(9999 chrtysys) export(cnstrReducablePart)
-- | Dissection of Constraint, including reconstruction function
cnstrReducablePart :: Constraint' p info -> Maybe (String,p,p->Constraint' p info)
cnstrReducablePart (Prove  p) = Just ("Prf",p,Prove)
cnstrReducablePart (Assume p) = Just ("Ass",p,Assume)
cnstrReducablePart _          = Nothing
%%]

%%[(9999 chrtysys)
instance (CHRMatchable env p s, TTKey p ~ Key) => CHRMatchable env (Constraint' p info) s where
  chrMatchTo env s c1 c2
    = do { (_,p1,_) <- cnstrReducablePart c1
         ; (_,p2,_) <- cnstrReducablePart c2
         ; chrMatchTo env s p1 p2
         }
  -- chrBuiltinSolve e s x = Nothing

{-
-- not yet supported
instance (CHREmptySubstitution s, VarLookupCmb s s) => CHRBuiltinSolvable env (Constraint' p info) s where
  chrBuiltinSolve e s x = Nothing
-}
%%]

%%[(9999 chrtysys)
instance (TTKeyable p, TTKey p ~ Key) => TTKeyable (Constraint' p info) where
  -- type TTKey (Constraint' p info) = Key
  toTTKey' o c -- = maybe [] (\(s,p,_) -> ttkAdd (TT1K_One $ Key_Str s) [toTTKey' o p]) $ cnstrReducablePart c
    = case cnstrReducablePart c of
        Just (s,p,_) -> ttkAdd' (TT1K_One $ Key_Str s) cs
                     where (_,cs) = toTTKeyParentChildren' o p
        _            -> panic "TTKeyable (Constraint' p info).toTTKey'" -- ttkEmpty
%%]

%%[(9999 chrtysys)
type instance ExtrValVarKey (Constraint' p info) = ExtrValVarKey p

instance (VarExtractable p) => VarExtractable (Constraint' p info) where
  varFreeSet c
    = case cnstrReducablePart c of
        Just (_,p,_) -> varFreeSet p
        _            -> Set.empty

instance (VarUpdatable p s,VarUpdatable info s) => VarUpdatable (Constraint' p info) s where
  varUpd s      (Prove     p       ) = Prove      (varUpd s p)
  varUpd s      (Assume    p       ) = Assume     (varUpd s p)
  varUpd s      r@(Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                                     = r {cnstrPred=varUpd s p, cnstrInfo=varUpd s i, cnstrFromPreds=map (varUpd s) ps}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common (derived) types & basic construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys) export(CHRIntermediateUntilAssume)
-- | intermediate structure for holding constraint and related info until it can safely be assumed
type CHRIntermediateUntilAssume = (CHRPredOcc,(PredScope,ConstraintToInfoTraceMp))
%%]

%%[(9999 chrtysys) export(mkProve, mkAssume, mkReduction)
mkProve :: CHRPredOcc -> Constraint
mkProve = Prove

mkAssume :: CHRPredOcc -> Constraint
mkAssume = Assume

mkReduction :: CHRPredOcc -> RedHowAnnotation -> [CHRPredOcc] -> Constraint
mkReduction p i ps
  = Reduction p i ps
%%[[15
              varlookupEmpty
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys).mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkProveConstraint pr i sc =  (mkProve (mkCHRPredOcc pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Pred -> VarUIDHsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint'' pr vun sc =  (mkAssume (mkCHRPredOcc pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Pred -> UID -> HsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint' pr i n sc =  mkAssumeConstraint'' pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint pr i sc =  mkAssumeConstraint'' pr (VarUIDHs_UID i) sc
%%]

%%[(9999 hmtyinfer) -9.mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Range -> Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkProveConstraint r pr i sc =  (mkProve (mkCHRPredOccRng r pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Range -> Pred -> VarUIDHsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint'' r pr vun sc =  (mkAssume (mkCHRPredOccRng r pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Range -> Pred -> UID -> HsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint' r pr i n sc =  mkAssumeConstraint'' r pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Range -> Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint r pr i sc =  mkAssumeConstraint'' r pr (VarUIDHs_UID i) sc
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint to info map for CHRPredOcc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys) export(gathPredLToProveCnstrMp,gathPredLToAssumeCnstrMp)
gathPredLToProveCnstrMp :: [PredOcc] -> ConstraintToInfoMap
gathPredLToProveCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkProveConstraint (poPr po) (poId po) (poScope po) | po <- l ]

gathPredLToAssumeCnstrMp :: [PredOcc] -> ConstraintToInfoMap
gathPredLToAssumeCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkAssumeConstraint (poPr po) (poId po) (poScope po) | po <- l ]
%%]

%%[(9999 chrtysys) export(predOccCnstrMpLiftScope)
-- | Lift predicate occurrences to new scope, used to lift unproven predicates to an outer scope.
predOccCnstrMpLiftScope :: PredScope -> ConstraintToInfoMap -> ConstraintToInfoMap
predOccCnstrMpLiftScope sc
  = Map.mapKeysWith (++) c . Map.map (map i)
  where c (Prove o@(CHRPredOcc {cpoCxt=cx}))
            = mkProve (o {cpoCxt = cx {cpocxScope = sc}})
        c x = x
        i (RedHow_ProveObl id _)
            = RedHow_ProveObl id sc
        i x = x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info: how reduction was done
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys) export(RedHowAnnotation(..))
data RedHowAnnotation
  =  RedHow_ByInstance    !HsName  !Pred  !PredScope		-- inst name, for pred, in scope
  |  RedHow_BySuperClass  !HsName  !Int   !CTag						-- field name, offset, tag info of dict
  |  RedHow_ProveObl      !UID  !PredScope
  |  RedHow_Assumption    !VarUIDHsName  !PredScope
  |  RedHow_ByScope		  !ByScopeRedHow							-- variant, for distinguishing during debugging
%%[[10
  |  RedHow_ByLabel       !Label !LabelOffset !PredScope
%%]]
%%[[13
  |  RedHow_Lambda        !UID !PredScope
%%]]
%%[[41
  |  RedHow_ByEqSymmetry
  |  RedHow_ByEqTrans
  |  RedHow_ByEqCongr
  |  RedHow_ByEqTyReduction !Ty !Ty
  |  RedHow_ByPredSeqUnpack
  |  RedHow_ByEqFromAssume
  |  RedHow_ByEqIdentity
%%]]
  deriving
    ( Eq, Ord
%%[[50
    , Typeable
    , Generic
%%]]
    )
%%]

%%[(9999 hmtyinfer) export(rhaMbId)
rhaMbId :: RedHowAnnotation -> Maybe UID
rhaMbId (RedHow_ProveObl i _) = Just i
rhaMbId _                     = Nothing
%%]

%%[(9999 chrtysys)
instance Show RedHowAnnotation where
  show = showPP . pp
%%]

%%[(9999 chrtysys)
instance PP RedHowAnnotation where
  pp (RedHow_ByInstance   s p sc)       =    "inst"   >#< {- ppParens (vm >#< "`varUpd`") >#< -} ppParensCommas [pp p, pp s, pp sc]
  pp (RedHow_BySuperClass s _ _ )       =    "super"  >#< s
  pp (RedHow_ProveObl     i   sc)       =    "prove"  >#< i >#< sc
  pp (RedHow_Assumption   vun sc)       =    "assume" >#< ppParensCommas [pp vun, pp sc]
  pp (RedHow_ByScope      v     )       =    "scope"  >|< ppParens v
%%[[10
  pp (RedHow_ByLabel      l o sc)       =    "label"  >#< l >|< "@" >|< o >|< sc
%%]]
%%[[13
  pp (RedHow_Lambda       i   sc)       =    "lambda" >#< i >#< sc
%%]]
%%[[41
  pp (RedHow_ByEqSymmetry) = pp "eqsym"
  pp (RedHow_ByEqTrans   ) = pp "eqtrans"
  pp (RedHow_ByEqCongr   ) = pp "eqcongr"
  pp (RedHow_ByEqTyReduction ty1 ty2) = "eqtyred" >#< ty1 >#< "~>" >#< ty2
  pp (RedHow_ByPredSeqUnpack) = pp "unpack"
  pp (RedHow_ByEqFromAssume)  = pp "eqassume"
  pp (RedHow_ByEqIdentity)    = pp "identity"
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info: specifically, how scope reduction was done, (1) for comparison (2) for debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys) export(ByScopeRedHow(..))
data ByScopeRedHow
  = ByScopeRedHow_Prove							-- scope reduction based on Prove
  | ByScopeRedHow_Assume						-- scope reduction based on Assume
  | ByScopeRedHow_Other (AlwaysEq String)		-- other reason
  deriving
    ( Eq, Ord
%%[[50
    , Typeable, Generic
%%]]
    )

-- equality plays no role ??
{-
instance Eq ByScopeRedHow where
  _ == _ = True

instance Ord ByScopeRedHow where
  _ `compare` _ = EQ
-}

instance Show ByScopeRedHow where
  show ByScopeRedHow_Prove     = "prv"
  show ByScopeRedHow_Assume    = "ass"
  show (ByScopeRedHow_Other s) = show s

instance PP ByScopeRedHow where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resolution trace reification, for error reporting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys) export(UnresolvedTrace'(..), UnresolvedTrace)
-- | The trace of an unresolved predicate
data UnresolvedTrace' p info
  = UnresolvedTrace_None									-- no trace required when all is resolved
  | UnresolvedTrace_Red										-- ok reduction, with failure deeper down
      { utraceRedFrom		:: p
      , utraceInfoTo2From	:: info
      , utraceRedTo			:: [UnresolvedTrace' p info]
      }
  | UnresolvedTrace_Fail									-- failed reduction
      { utraceRedFrom		:: p
      -- , utraceInfoTo2From	:: info
      , utraceRedTo			:: [UnresolvedTrace' p info]
      }
  | UnresolvedTrace_Overlap									-- choice could not be made
      { utraceRedFrom		:: p
      , utraceRedChoices	:: [(info,[UnresolvedTrace' p info])]
      }
  deriving Show

type UnresolvedTrace = UnresolvedTrace' CHRPredOcc RedHowAnnotation

instance Eq p => Eq (UnresolvedTrace' p info) where
  t1 == t2 = True -- utraceRedFrom t1 == utraceRedFrom t2

instance (PP p, PP info) => PP (UnresolvedTrace' p info) where
  pp x = case x of
  		   UnresolvedTrace_None 			-> PP.empty
  		   UnresolvedTrace_Red 		p i us 	-> p >|< ":" >#< i >-< indent 2 (vlist $ map pp us)
  		   UnresolvedTrace_Fail 	p   us  -> p >|< ": FAIL" >-< indent 2 (vlist $ map pp us)
  		   UnresolvedTrace_Overlap 	p uss 	-> p >|< ": OVERLAP" >-< indent 2 (vlist $ map (\(i,u) -> i >-< indent 2 (vlist $ map pp u)) uss)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mapping: constraint -> info (in varieties)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys)
-- | Map from constraint to something
type ConstraintMp'' p info x = Map.Map (Constraint' p info) [x]
type ConstraintMp'         x = ConstraintMp'' CHRPredOcc RedHowAnnotation x
%%]

%%[(9999 chrtysys) export(cnstrMpFromList)
cnstrMpSingletonL :: Constraint -> [x] -> ConstraintMp' x
cnstrMpSingletonL c xs = Map.singleton c xs

cnstrMpSingleton :: Constraint -> x -> ConstraintMp' x
cnstrMpSingleton c x = cnstrMpSingletonL c [x]

cnstrMpFromList :: [(Constraint,x)] -> ConstraintMp' x
cnstrMpFromList l = Map.fromListWith (++) [ (c,[x]) | (c,x) <- l ]

cnstrMpMap :: (x -> y) -> ConstraintMp' x -> ConstraintMp' y
cnstrMpMap f = Map.map (map f)
%%]

%%[(9999 chrtysys) export(ConstraintToInfoTraceMp)
-- type ConstraintToInfoTraceMp' p info = ConstraintMp'' p info (info,[UnresolvedTrace' p info])

-- | Map from constraint to info + trace
type ConstraintToInfoTraceMp = ConstraintMp' (RedHowAnnotation,[UnresolvedTrace])
%%]

%%[(9999 chrtysys) export(cnstrTraceMpSingleton,cnstrTraceMpElimTrace,cnstrTraceMpFromList)
cnstrTraceMpFromList :: [(Constraint,(RedHowAnnotation,[UnresolvedTrace]))] -> ConstraintToInfoTraceMp
cnstrTraceMpFromList = cnstrMpFromList

cnstrTraceMpSingleton :: Constraint -> RedHowAnnotation -> [UnresolvedTrace] -> ConstraintToInfoTraceMp
cnstrTraceMpSingleton c i ts = cnstrMpSingleton c (i,ts)

cnstrTraceMpElimTrace :: ConstraintToInfoTraceMp -> ConstraintToInfoMap
cnstrTraceMpElimTrace = cnstrMpMap fst

cnstrTraceMpLiftTrace :: ConstraintToInfoMap -> ConstraintToInfoTraceMp
cnstrTraceMpLiftTrace = cnstrMpMap (\x -> (x,[]))
%%]

%%[(9999 chrtysys) export(ConstraintToInfoMap)
-- type ConstraintToInfoMap'     p info = ConstraintMp'' p info info

-- | Map from constraint to info
type ConstraintToInfoMap = ConstraintMp' RedHowAnnotation
%%]

%%[(9999 chrtysys) export(emptyCnstrMp)
emptyCnstrMp :: ConstraintMp' x
emptyCnstrMp = Map.empty
%%]

%%[(9999 chrtysys) export(cnstrMpUnion,cnstrMpUnions)
cnstrMpUnion :: ConstraintMp' x -> ConstraintMp' x -> ConstraintMp' x
cnstrMpUnion = Map.unionWith (++)

cnstrMpUnions :: [ConstraintMp' x] -> ConstraintMp' x
cnstrMpUnions = Map.unionsWith (++)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys)
instance IsConstraint (Constraint' p info) where
  cnstrRequiresSolve (Reduction {}) = False
  cnstrRequiresSolve _              = True
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys)
instance (PP p, PP info) => PP (Constraint' p info) where
  pp (Prove     p     ) = "Prove"  >#< p
  pp (Assume    p     ) = "Assume" >#< p
  pp (Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                        = "Red"    >#< p >#< "<" >#< i >#< "<" >#< ppBracketsCommas ps
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 chrtysys)
instance (Serialize p, Serialize i) => Serialize (Constraint' p i)
%%]

%%[(9999 chrtysys)
instance Serialize ByScopeRedHow
instance Serialize RedHowAnnotation
%%]

