%%[0 hs
{-# LANGUAGE CPP #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Constraint language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) module {%{EH}CHR.Constraint}
%%]

%%[(9 hmtyinfer || hmtyast) import({%{EH}Base.Common},{%{EH}Ty},UHC.Util.CHR,{%{EH}CHR.Key},UHC.Util.TreeTrie,{%{EH}Substitutable})
%%]

%%[(9 hmtyinfer || hmtyast) import(UHC.Util.Pretty as PP, UHC.Util.Utils)
%%]

%%[(9 hmtyinfer || hmtyast) import(qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(15 hmtyinfer || hmtyast) import({%{EH}VarMp})
%%]

%%[(50 hmtyinfer || hmtyast) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[(50 hmtyinfer || hmtyast) import({%{EH}Opts.Base})
%%]

%%[(9999 hmtyinfer || hmtyast) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint type famility instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
type instance CHRMatchableKey VarMp = Key
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(Constraint, Constraint'(..))
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

%%[(50 hmtyinfer || hmtyast)
#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable  Constraint'
#else
deriving instance Typeable2 Constraint'
#endif
%%]

%%[(9 hmtyinfer || hmtyast) export(cnstrReducablePart)
-- | Dissection of Constraint, including reconstruction function
cnstrReducablePart :: Constraint' p info -> Maybe (String,p,p->Constraint' p info)
cnstrReducablePart (Prove  p) = Just ("Prf",p,Prove)
cnstrReducablePart (Assume p) = Just ("Ass",p,Assume)
cnstrReducablePart _          = Nothing
%%]

%%[(9 hmtyinfer || hmtyast)
instance (CHRMatchable env p s, TTKey p ~ Key) => CHRMatchable env (Constraint' p info) s where
  chrMatchTo env s c1 c2
    = do { (_,p1,_) <- cnstrReducablePart c1
         ; (_,p2,_) <- cnstrReducablePart c2
         ; chrMatchTo env s p1 p2
         }

-- not yet supported
instance (CHREmptySubstitution s, VarLookupCmb s s) => CHRBuiltinSolvable env (Constraint' p info) s where
  chrBuiltinSolve e s x = Nothing
%%]

%%[(9 hmtyinfer || hmtyast)
instance (TTKeyable p, TTKey p ~ Key) => TTKeyable (Constraint' p info) where
  -- type TTKey (Constraint' p info) = Key
  toTTKey' o c -- = maybe [] (\(s,p,_) -> ttkAdd (TT1K_One $ Key_Str s) [toTTKey' o p]) $ cnstrReducablePart c
    = case cnstrReducablePart c of
        Just (s,p,_) -> ttkAdd' (TT1K_One $ Key_Str s) cs
                     where (_,cs) = toTTKeyParentChildren' o p
        _            -> panic "TTKeyable (Constraint' p info).toTTKey'" -- ttkEmpty
%%]

%%[(9 hmtyinfer || hmtyast)
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

%%[(9 hmtyinfer) export(CHRIntermediateUntilAssume)
-- | intermediate structure for holding constraint and related info until it can safely be assumed
type CHRIntermediateUntilAssume = (CHRPredOcc,(PredScope,ConstraintToInfoTraceMp))
%%]

%%[(9 hmtyinfer || hmtyast) export(mkProve, mkAssume, mkReduction)
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

%%[(9 hmtyinfer).mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkProveConstraint pr i sc =  (mkProve (mkCHRPredOcc pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Pred -> VarUIDHsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint'' pr vun sc =  (mkAssume (mkCHRPredOcc pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Pred -> UID -> HsName -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint' pr i n sc =  mkAssumeConstraint'' pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Pred -> UID -> PredScope -> (Constraint,RedHowAnnotation)
mkAssumeConstraint pr i sc =  mkAssumeConstraint'' pr (VarUIDHs_UID i) sc
%%]

%%[(99 hmtyinfer) -9.mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
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

%%[(9 hmtyinfer) export(gathPredLToProveCnstrMp,gathPredLToAssumeCnstrMp)
gathPredLToProveCnstrMp :: [PredOcc] -> ConstraintToInfoMap
gathPredLToProveCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkProveConstraint (poPr po) (poId po) (poScope po) | po <- l ]

gathPredLToAssumeCnstrMp :: [PredOcc] -> ConstraintToInfoMap
gathPredLToAssumeCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkAssumeConstraint (poPr po) (poId po) (poScope po) | po <- l ]
%%]

%%[(9 hmtyinfer) export(predOccCnstrMpLiftScope)
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

%%[(9 hmtyinfer) export(RedHowAnnotation(..))
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

%%[(99 hmtyinfer) export(rhaMbId)
rhaMbId :: RedHowAnnotation -> Maybe UID
rhaMbId (RedHow_ProveObl i _) = Just i
rhaMbId _                     = Nothing
%%]

%%[(9 hmtyinfer)
instance Show RedHowAnnotation where
  show = showPP . pp
%%]

%%[(9 hmtyinfer)
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

%%[(9 hmtyinfer) export(ByScopeRedHow(..))
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

%%[(9 hmtyinfer || hmtyast) export(UnresolvedTrace'(..), UnresolvedTrace)
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

%%[(9 hmtyinfer || hmtyast)
-- | Map from constraint to something
type ConstraintMp'' p info x = Map.Map (Constraint' p info) [x]
type ConstraintMp'         x = ConstraintMp'' CHRPredOcc RedHowAnnotation x
%%]

%%[(9 hmtyinfer || hmtyast) export(cnstrMpFromList)
cnstrMpSingletonL :: Constraint -> [x] -> ConstraintMp' x
cnstrMpSingletonL c xs = Map.singleton c xs

cnstrMpSingleton :: Constraint -> x -> ConstraintMp' x
cnstrMpSingleton c x = cnstrMpSingletonL c [x]

cnstrMpFromList :: [(Constraint,x)] -> ConstraintMp' x
cnstrMpFromList l = Map.fromListWith (++) [ (c,[x]) | (c,x) <- l ]

cnstrMpMap :: (x -> y) -> ConstraintMp' x -> ConstraintMp' y
cnstrMpMap f = Map.map (map f)
%%]

%%[(9 hmtyinfer || hmtyast) export(ConstraintToInfoTraceMp)
-- type ConstraintToInfoTraceMp' p info = ConstraintMp'' p info (info,[UnresolvedTrace' p info])

-- | Map from constraint to info + trace
type ConstraintToInfoTraceMp = ConstraintMp' (RedHowAnnotation,[UnresolvedTrace])
%%]

%%[(9 hmtyinfer || hmtyast) export(cnstrTraceMpSingleton,cnstrTraceMpElimTrace,cnstrTraceMpFromList)
cnstrTraceMpFromList :: [(Constraint,(RedHowAnnotation,[UnresolvedTrace]))] -> ConstraintToInfoTraceMp
cnstrTraceMpFromList = cnstrMpFromList

cnstrTraceMpSingleton :: Constraint -> RedHowAnnotation -> [UnresolvedTrace] -> ConstraintToInfoTraceMp
cnstrTraceMpSingleton c i ts = cnstrMpSingleton c (i,ts)

cnstrTraceMpElimTrace :: ConstraintToInfoTraceMp -> ConstraintToInfoMap
cnstrTraceMpElimTrace = cnstrMpMap fst

cnstrTraceMpLiftTrace :: ConstraintToInfoMap -> ConstraintToInfoTraceMp
cnstrTraceMpLiftTrace = cnstrMpMap (\x -> (x,[]))
%%]

%%[(9 hmtyinfer || hmtyast) export(ConstraintToInfoMap)
-- type ConstraintToInfoMap'     p info = ConstraintMp'' p info info

-- | Map from constraint to info
type ConstraintToInfoMap = ConstraintMp' RedHowAnnotation
%%]

%%[(9 hmtyinfer || hmtyast) export(emptyCnstrMp)
emptyCnstrMp :: ConstraintMp' x
emptyCnstrMp = Map.empty
%%]

%%[(9 hmtyinfer || hmtyast) export(cnstrMpUnion,cnstrMpUnions)
cnstrMpUnion :: ConstraintMp' x -> ConstraintMp' x -> ConstraintMp' x
cnstrMpUnion = Map.unionWith (++)

cnstrMpUnions :: [ConstraintMp' x] -> ConstraintMp' x
cnstrMpUnions = Map.unionsWith (++)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
instance IsConstraint (Constraint' p info) where
  cnstrRequiresSolve (Reduction {}) = False
  cnstrRequiresSolve _              = True
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
instance (PP p, PP info) => PP (Constraint' p info) where
  pp (Prove     p     ) = "Prove"  >#< p
  pp (Assume    p     ) = "Assume" >#< p
  pp (Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                        = "Red"    >#< p >#< "<" >#< i >#< "<" >#< ppBracketsCommas ps
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 hmtyinfer || hmtyast)
instance (Serialize p, Serialize i) => Serialize (Constraint' p i)
%%]

%%[(5050 hmtyinfer || hmtyast)
instance (Serialize p, Serialize i) => Serialize (Constraint' p i) where
  sput (Prove     a      ) = sputWord8 0 >> sput a
  sput (Assume    a      ) = sputWord8 1 >> sput a
  sput (Reduction a b c d) = sputWord8 2 >> sput a >> sput b >> sput c >> sput d
  sget = do t <- sgetWord8
            case t of
              0 -> liftM  Prove     sget
              1 -> liftM  Assume    sget
              2 -> liftM4 Reduction sget sget sget sget
%%]

%%[(50 hmtyinfer)
instance Serialize ByScopeRedHow
instance Serialize RedHowAnnotation
%%]

%%[(5050 hmtyinfer)
instance Serialize ByScopeRedHow where
  sput (ByScopeRedHow_Prove          ) = sputWord8 0
  sput (ByScopeRedHow_Assume         ) = sputWord8 1
  sput (ByScopeRedHow_Other a        ) = sputWord8 2 >> sput a
  sget = do
    t <- sgetWord8
    case t of
      0 -> return ByScopeRedHow_Prove
      1 -> return ByScopeRedHow_Assume
      2 -> liftM  ByScopeRedHow_Other   sget

instance Serialize RedHowAnnotation where
  sput (RedHow_ByInstance       a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c -- >> sput d
  sput (RedHow_BySuperClass     a b c  ) = sputWord8 1  >> sput a >> sput b >> sput c
  sput (RedHow_ProveObl         a b    ) = sputWord8 2  >> sput a >> sput b
  sput (RedHow_Assumption       a b    ) = sputWord8 3  >> sput a >> sput b
  sput (RedHow_ByScope          a      ) = sputWord8 4  >> sput a
  sput (RedHow_ByLabel          a b c  ) = sputWord8 5  >> sput a >> sput b >> sput c
  sput (RedHow_Lambda           a b    ) = sputWord8 6  >> sput a >> sput b
%%[[41
  sput (RedHow_ByEqSymmetry          ) = sputWord8 7
  sput (RedHow_ByEqTrans             ) = sputWord8 8
  sput (RedHow_ByEqCongr             ) = sputWord8 9
  sput (RedHow_ByEqTyReduction  a b  ) = sputWord8 10 >> sput a >> sput b
  sput (RedHow_ByPredSeqUnpack       ) = sputWord8 11
  sput (RedHow_ByEqFromAssume        ) = sputWord8 12
  sput (RedHow_ByEqIdentity          ) = sputWord8 13
%%]]
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 RedHow_ByInstance       sget sget sget -- sget
              1  -> liftM3 RedHow_BySuperClass     sget sget sget 
              2  -> liftM2 RedHow_ProveObl         sget sget 
              3  -> liftM2 RedHow_Assumption       sget sget 
              4  -> liftM  RedHow_ByScope          sget 
              5  -> liftM3 RedHow_ByLabel          sget sget sget
              6  -> liftM2 RedHow_Lambda           sget sget 
%%[[41
              7  -> return RedHow_ByEqSymmetry     
              8  -> return RedHow_ByEqTrans        
              9  -> return RedHow_ByEqCongr        
              10 -> liftM2 RedHow_ByEqTyReduction  sget sget 
              11 -> return RedHow_ByPredSeqUnpack  
              12 -> return RedHow_ByEqFromAssume   
              13 -> return RedHow_ByEqIdentity     
%%]]

%%]
