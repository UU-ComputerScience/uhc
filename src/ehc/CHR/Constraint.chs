%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Constraint language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}CHR.Constraint} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}CHR},{%{EH}CHR.Key},{%{EH}Base.Trie})
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%[9 import(qualified Data.Set as Set)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Constraint(..))
data Constraint p info
  = Prove      p
  | Assume     p
  | Reduction  p info [p]
  deriving (Eq, Ord, Show)
%%]

%%[9
instance Keyable p => Keyable (Constraint p info) where
  toKey (Prove     p    ) = TK_One TKK_Normal (Key_Str "Prf") : toKey p
  toKey (Assume    p    ) = TK_One TKK_Normal (Key_Str "Ass") : toKey p
  toKey (Reduction p _ _) = TK_One TKK_Normal (Key_Str "Red") : toKey p

instance (CHRMatchable env p s) => CHRMatchable env (Constraint p info) s where
  chrMatch env (Prove     p) (Prove     q) = chrMatch env p q
  chrMatch env (Assume    p) (Assume    q) = chrMatch env p q
  chrMatch _   _             _             = Nothing

instance CHRSubstitutable p v s => CHRSubstitutable (Constraint p info) v s where
  chrFtv (Prove  p)          = chrFtv p
  chrFtv (Assume p)          = chrFtv p
  chrFtv (Reduction p _ ps)  = Set.unions (chrFtv p : map chrFtv ps)

  chrAppSubst s      (Prove  p)         = Prove  (chrAppSubst s p)
  chrAppSubst s      (Assume p)         = Assume (chrAppSubst s p)
  chrAppSubst s      (Reduction p i ps) = Reduction  (chrAppSubst s p) i (map (chrAppSubst s) ps)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRRule)
type CHRRule p s info = CHR (Constraint p info) s
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
  pp = pp . show
%%]
