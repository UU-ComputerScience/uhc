%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Key to be used as part of TrieKey
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}CHR.Key} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}Base.Trie})
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Key(..))
data Key
  = Key_HNm     HsName			-- type constant, its name
  | Key_UID     UID				-- type variable, its id, used with TKK_Partial
  | Key_Str     String			-- arbitrary string
  | Key_TyQu    TyQu			-- quantified type, used with TKK_Partial
  | Key_Ty      Ty				-- catchall for the rest, used with TKK_Partial
  deriving (Eq,Ord)
%%]

%%[9
instance Show Key where
  show (Key_HNm  n) = show n
  show (Key_UID  n) = show n
  show (Key_Str  n) = show n
  show (Key_TyQu n) = show n
  show (Key_Ty   n) = show n
%%]

%%[9 export(Keyable(..))
class Keyable k where
  toKey :: k -> [TrieKey Key]
%%]

%%[9
instance Keyable x => TrieKeyable x Key where
  toTrieKey = toKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance PP Key where
  pp = pp . show
%%]
