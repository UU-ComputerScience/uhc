%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Constraint language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}CHR.Constraint} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}CHR},{%{EH}CHR.Key},{%{EH}Base.Trie})
%%]

%%[9 import(EH.Util.Pretty)
%%]

%%[9 import(qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[20 import({%{EH}Base.CfgPP})
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Constraint(..))
data Constraint p info
  = Prove      		{ cnstrPred :: !p }														-- proof obligation
  | Assume     		{ cnstrPred :: !p }														-- assumed
  | Reduction  		{ cnstrPred :: !p, cnstrInfo :: !info, cnstrFromPreds :: ![p] }			-- 'side effect', residual info used by (e.g.) codegeneration
  deriving (Eq, Ord, Show)
%%]

%%[9
reducablePart :: Constraint p info -> Maybe (String,p,p->Constraint p info)
reducablePart (Prove  p) = Just ("Prf",p,Prove)
reducablePart (Assume p) = Just ("Ass",p,Assume)
reducablePart _          = Nothing
%%]

%%[9
instance Keyable p => Keyable (Constraint p info) where
  toKey c = maybe [] (\(s,p,_) -> TK_One TKK_Normal (Key_Str s) : toKey p) $ reducablePart c

instance (CHRMatchable env p s) => CHRMatchable env (Constraint p info) s where
  chrMatchTo env c1 c2
    = do { (_,p1,_) <- reducablePart c1
         ; (_,p2,_) <- reducablePart c2
         ; chrMatchTo env p1 p2
         }

instance (CHRSubstitutable p v s,CHRSubstitutable info v s) => CHRSubstitutable (Constraint p info) v s where
  chrFtv c
    = case reducablePart c of
        Just (_,p,_) -> chrFtv p
        _            -> Set.empty

  chrAppSubst s      (Reduction p i ps) = Reduction  (chrAppSubst s p) (chrAppSubst s i) (map (chrAppSubst s) ps)
  chrAppSubst s      c                  = case reducablePart c of
                                            Just (_,p,mk) -> mk (chrAppSubst s p)
                                            _             -> c
%%]
  chrAppSubst s      (Reduction p i ps) = Reduction  (chrAppSubst s p) i (map (chrAppSubst s) ps)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mapping: constraint -> info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ConstraintToInfoMap,emptyCnstrMp)
type ConstraintToInfoMap p info = Map.Map (Constraint p info) [info]

emptyCnstrMp :: ConstraintToInfoMap p info
emptyCnstrMp = Map.empty
%%]

%%[9 export(cnstrMpFromList,cnstrMpUnion,cnstrMpUnions)
cnstrMpFromList :: (Ord p, Ord i) => [(Constraint p i,i)] -> ConstraintToInfoMap p i
cnstrMpFromList l = Map.fromListWith (++) [ (c,[i]) | (c,i) <- l ]

cnstrMpUnion :: (Ord p, Ord i) => ConstraintToInfoMap p i -> ConstraintToInfoMap p i -> ConstraintToInfoMap p i
cnstrMpUnion = Map.unionWith (++)

cnstrMpUnions :: (Ord p, Ord i) => [ConstraintToInfoMap p i] -> ConstraintToInfoMap p i
cnstrMpUnions = Map.unionsWith (++)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRRule)
type CHRRule p g s info = CHR (Constraint p info) g s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(cnstrRequiresSolve)
cnstrRequiresSolve :: Constraint p info -> Bool
cnstrRequiresSolve (Reduction _ _ _) = False
cnstrRequiresSolve _                 = True
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance (PP p, PP info) => PP (Constraint p info) where
  pp (Prove     p     ) = "Prove"  >#< p
  pp (Assume    p     ) = "Assume" >#< p
  pp (Reduction p i ps) = "Red"    >#< p >#< "<" >#< i >#< "<" >#< ppBracketsCommas ps
%%]

%%[20
instance (PPForHI p, PPForHI info) => PPForHI (Constraint p info) where
  ppForHI (Prove     p     ) = "Prove"     >#< ppCurlysCommasBlock [ppForHI p]
  ppForHI (Assume    p     ) = "Assume"    >#< ppCurlysCommasBlock [ppForHI p]
  ppForHI (Reduction p i ps) = "Reduction" >#< ppCurlysCommasBlock [ppForHI p, ppForHI i, ppCurlysCommasBlock $ map ppForHI ps]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance (ForceEval p, ForceEval info) => ForceEval (Constraint p info) where
  forceEval x@(Prove     p     ) | forceEval p `seq` True = x
  forceEval x@(Assume    p     ) | forceEval p `seq` True = x
  forceEval x@(Reduction p i ps) | forceEval p `seq` forceEval i `seq` forceEval ps `seq` True = x
%%[[101
  fevCount (Prove     p     ) = cm1 "Prove"     `cmUnion` fevCount p
  fevCount (Assume    p     ) = cm1 "Assume"    `cmUnion` fevCount p
  fevCount (Reduction p i ps) = cm1 "Reduction" `cmUnion` fevCount p `cmUnion` fevCount i `cmUnion` fevCount ps
%%]]
%%]


