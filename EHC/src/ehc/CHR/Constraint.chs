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
  = Prove      		p					-- proof obligation
  | Assume     		p					-- assumed
  | Reduction  		p info [p]			-- 'side effect', residual info used by (e.g.) codegeneration
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

instance CHRSubstitutable p v s => CHRSubstitutable (Constraint p info) v s where
  chrFtv c
    = case reducablePart c of
        Just (_,p,_) -> chrFtv p
        _            -> Set.empty

  chrAppSubst s      (Reduction p i ps) = Reduction  (chrAppSubst s p) i (map (chrAppSubst s) ps)
  chrAppSubst s      c                  = case reducablePart c of
                                            Just (_,p,mk) -> mk (chrAppSubst s p)
                                            _             -> c
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
  pp = pp . show
%%]
