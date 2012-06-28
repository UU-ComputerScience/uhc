module EH101.CHR.Constraint
( Constraint (..)
, mkReduction
, cnstrReducablePart
, UnresolvedTrace (..)
, cnstrMpSingletonL, cnstrMpFromList
, ConstraintToInfoTraceMp
, cnstrTraceMpSingleton, cnstrTraceMpLiftTrace, cnstrTraceMpElimTrace, cnstrTraceMpFromList
, ConstraintToInfoMap
, emptyCnstrMp
, cnstrMpUnion, cnstrMpUnions
, cnstrRequiresSolve )
where
import EH101.Base.Common
import EH101.Ty
import EH101.CHR
import EH101.CHR.Key
import EH101.Base.TreeTrie
import EH101.Substitutable
import EH.Util.Pretty as PP
import EH.Util.Utils
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.VarMp
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Typeable (Typeable,Typeable2)
import Data.Generics (Data)
import EH101.Opts.Base




{-# LINE 33 "src/ehc/CHR/Constraint.chs" #-}
-- | A Constraint is abstracted over the exact predicate, but differentiates on the role: to prove, can be assumed, and side effect of reduction
data Constraint p info
  = Prove           { cnstrPred :: !p }             -- proof obligation
  | Assume          { cnstrPred :: !p }             -- assumed constraint
  | Reduction                                       -- 'side effect', residual info used by (e.g.) codegeneration
                    { cnstrPred :: !p               -- the pred to which reduction was done
                    , cnstrInfo :: !info            -- additional reduction specific info w.r.t. codegeneration
                    , cnstrFromPreds :: ![p]        -- the preds from which reduction was done
                    , cnstrVarMp :: VarMp           -- additional bindings for type (etc.) variables, i.e. improving substitution
                    }
  deriving (Eq, Ord, Show)

{-# LINE 49 "src/ehc/CHR/Constraint.chs" #-}
mkReduction :: p -> info -> [p] -> Constraint p info
mkReduction p i ps
  = Reduction p i ps
              varlookupEmpty

{-# LINE 58 "src/ehc/CHR/Constraint.chs" #-}
deriving instance Typeable2 Constraint
deriving instance (Data x, Data y) => Data (Constraint x y)

{-# LINE 63 "src/ehc/CHR/Constraint.chs" #-}
-- | Dissection of Constraint, including reconstruction function
cnstrReducablePart :: Constraint p info -> Maybe (String,p,p->Constraint p info)
cnstrReducablePart (Prove  p) = Just ("Prf",p,Prove)
cnstrReducablePart (Assume p) = Just ("Ass",p,Assume)
cnstrReducablePart _          = Nothing

{-# LINE 76 "src/ehc/CHR/Constraint.chs" #-}
instance (CHRMatchable env p s) => CHRMatchable env (Constraint p info) s where
  chrMatchTo env s c1 c2
    = do { (_,p1,_) <- cnstrReducablePart c1
         ; (_,p2,_) <- cnstrReducablePart c2
         ; chrMatchTo env s p1 p2
         }

{-# LINE 85 "src/ehc/CHR/Constraint.chs" #-}
instance TTKeyable p => TTKeyable (Constraint p info) where
  toTTKey' o c -- = maybe [] (\(s,p,_) -> ttkAdd (TT1K_One $ Key_Str s) [toTTKey' o p]) $ cnstrReducablePart c
    = case cnstrReducablePart c of
        Just (s,p,_) -> ttkAdd' (TT1K_One $ Key_Str s) cs
                     where (_,cs) = toTTKeyParentChildren' o p
        _            -> panic "TTKeyable (Constraint p info).toTTKey'" -- ttkEmpty

{-# LINE 94 "src/ehc/CHR/Constraint.chs" #-}
instance (VarExtractable p v,VarExtractable info v) => VarExtractable (Constraint p info) v where
  varFreeSet c
    = case cnstrReducablePart c of
        Just (_,p,_) -> varFreeSet p
        _            -> Set.empty

instance (VarUpdatable p s,VarUpdatable info s) => VarUpdatable (Constraint p info) s where
  varUpd s      (Prove     p       ) = Prove      (varUpd s p)
  varUpd s      (Assume    p       ) = Assume     (varUpd s p)
  varUpd s      r@(Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                                     = r {cnstrPred=varUpd s p, cnstrInfo=varUpd s i, cnstrFromPreds=map (varUpd s) ps}

{-# LINE 112 "src/ehc/CHR/Constraint.chs" #-}
-- | The trace of an unresolved predicate
data UnresolvedTrace p info
  = UnresolvedTrace_None									-- no trace required when all is resolved
  | UnresolvedTrace_Red										-- ok reduction, with failure deeper down
      { utraceRedFrom		:: p
      , utraceInfoTo2From	:: info
      , utraceRedTo			:: [UnresolvedTrace p info]
      }
  | UnresolvedTrace_Fail									-- failed reduction
      { utraceRedFrom		:: p
      -- , utraceInfoTo2From	:: info
      , utraceRedTo			:: [UnresolvedTrace p info]
      }
  | UnresolvedTrace_Overlap									-- choice could not be made
      { utraceRedFrom		:: p
      , utraceRedChoices	:: [(info,[UnresolvedTrace p info])]
      }
  deriving Show

instance Eq p => Eq (UnresolvedTrace p info) where
  t1 == t2 = True -- utraceRedFrom t1 == utraceRedFrom t2

instance (PP p, PP info) => PP (UnresolvedTrace p info) where
  pp x = case x of
  		   UnresolvedTrace_None 			-> PP.empty
  		   UnresolvedTrace_Red 		p i us 	-> p >|< ":" >#< i >-< indent 2 (vlist $ map pp us)
  		   UnresolvedTrace_Fail 	p   us  -> p >|< ": FAIL" >-< indent 2 (vlist $ map pp us)
  		   UnresolvedTrace_Overlap 	p uss 	-> p >|< ": OVERLAP" >-< indent 2 (vlist $ map (\(i,u) -> i >-< indent 2 (vlist $ map pp u)) uss)

{-# LINE 147 "src/ehc/CHR/Constraint.chs" #-}
-- | Map from constraint to something
type ConstraintMp' p info x = Map.Map (Constraint p info) [x]

{-# LINE 152 "src/ehc/CHR/Constraint.chs" #-}
cnstrMpSingletonL :: Constraint p i -> [x] -> ConstraintMp' p i x
cnstrMpSingletonL c xs = Map.singleton c xs

cnstrMpSingleton :: Constraint p i -> x -> ConstraintMp' p i x
cnstrMpSingleton c x = cnstrMpSingletonL c [x]

cnstrMpFromList :: (Ord p, Ord i) => [(Constraint p i,x)] -> ConstraintMp' p i x
cnstrMpFromList l = Map.fromListWith (++) [ (c,[x]) | (c,x) <- l ]

cnstrMpMap :: (Ord p, Ord i) => (x -> y) -> ConstraintMp' p i x -> ConstraintMp' p i y
cnstrMpMap f = Map.map (map f)

{-# LINE 166 "src/ehc/CHR/Constraint.chs" #-}
-- | Map from constraint to info + trace
type ConstraintToInfoTraceMp p info = ConstraintMp' p info (info,[UnresolvedTrace p info])

{-# LINE 171 "src/ehc/CHR/Constraint.chs" #-}
cnstrTraceMpFromList :: (Ord p, Ord i) => [(Constraint p i,(i,[UnresolvedTrace p i]))] -> ConstraintToInfoTraceMp p i
cnstrTraceMpFromList = cnstrMpFromList

cnstrTraceMpSingleton :: Constraint p i -> i -> [UnresolvedTrace p i] -> ConstraintToInfoTraceMp p i
cnstrTraceMpSingleton c i ts = cnstrMpSingleton c (i,ts)

cnstrTraceMpElimTrace :: (Ord p, Ord i) => ConstraintToInfoTraceMp p i -> ConstraintToInfoMap p i
cnstrTraceMpElimTrace = cnstrMpMap fst

cnstrTraceMpLiftTrace :: (Ord p, Ord i) => ConstraintToInfoMap p i -> ConstraintToInfoTraceMp p i
cnstrTraceMpLiftTrace = cnstrMpMap (\x -> (x,[]))

{-# LINE 185 "src/ehc/CHR/Constraint.chs" #-}
-- | Map from constraint to info
type ConstraintToInfoMap     p info = ConstraintMp' p info info

{-# LINE 190 "src/ehc/CHR/Constraint.chs" #-}
emptyCnstrMp :: ConstraintMp' p info x
emptyCnstrMp = Map.empty

{-# LINE 200 "src/ehc/CHR/Constraint.chs" #-}
cnstrMpUnion :: (Ord p, Ord i) => ConstraintMp' p i x -> ConstraintMp' p i x -> ConstraintMp' p i x
cnstrMpUnion = Map.unionWith (++)

cnstrMpUnions :: (Ord p, Ord i) => [ConstraintMp' p i x] -> ConstraintMp' p i x
cnstrMpUnions = Map.unionsWith (++)

{-# LINE 212 "src/ehc/CHR/Constraint.chs" #-}
-- | Predicate for whether solving is required
cnstrRequiresSolve :: Constraint p info -> Bool
cnstrRequiresSolve (Reduction {}) = False
cnstrRequiresSolve _              = True

{-# LINE 223 "src/ehc/CHR/Constraint.chs" #-}
instance (PP p, PP info) => PP (Constraint p info) where
  pp (Prove     p     ) = "Prove"  >#< p
  pp (Assume    p     ) = "Assume" >#< p
  pp (Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                        = "Red"    >#< p >#< "<" >#< i >#< "<" >#< ppBracketsCommas ps

{-# LINE 235 "src/ehc/CHR/Constraint.chs" #-}
instance (Serialize p, Serialize i) => Serialize (Constraint p i) where
  sput (Prove     a      ) = sputWord8 0 >> sput a
  sput (Assume    a      ) = sputWord8 1 >> sput a
  sput (Reduction a b c d) = sputWord8 2 >> sput a >> sput b >> sput c >> sput d
  sget = do t <- sgetWord8
            case t of
              0 -> liftM  Prove     sget
              1 -> liftM  Assume    sget
              2 -> liftM4 Reduction sget sget sget sget

