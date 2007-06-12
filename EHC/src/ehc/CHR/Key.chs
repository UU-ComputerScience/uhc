%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Key to be used as part of TrieKey
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}CHR.Key} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}Base.Trie})
%%]

%%[9 import(EH.Util.Pretty)
%%]

%%[9 import({%{EH}Ty.Pretty})
%%]

%%[99 import({%{EH}Base.ForceEval},{%{EH}Ty.Trf.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Key(..))
data Key
  = Key_HNm     !HsName			-- type constant, its name
  | Key_UID     !UID			-- type variable, its id, used with TKK_Partial
  | Key_Str     !String			-- arbitrary string
  | Key_TyQu    !TyQu			-- quantified type, used with TKK_Partial
  | Key_Ty      !Ty				-- catchall for the rest, used with TKK_Partial
  deriving (Eq,Ord)
%%]

%%[9
instance Show Key where
  show _ = "Key"

instance PP Key where
  pp (Key_HNm  n) = pp n
  pp (Key_UID  n) = pp n
  pp (Key_Str  n) = pp n
  pp (Key_TyQu n) = pp $ show n
  pp (Key_Ty   n) = pp n
%%]

%%[9 export(Keyable(..))
class Keyable k where
  toKey               :: k -> [TrieKey Key]						-- the key of ...
  toKeyParentChildren :: k -> ([TrieKey Key],[TrieKey Key])		-- split up into (p,c), where key = p ++ c

  -- minimal def, mutually recursive
  toKey               x = let (p,c) = toKeyParentChildren x in p ++ c
  toKeyParentChildren x = case toKey x of
                            (h:t) -> ([h],t)
                            _     -> ([],[])
%%]

%%[9
instance Keyable x => TrieKeyable x Key where
  toTrieKey = toKey
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval Key where
  forceEval x@(Key_Ty   y) | forceEval y `seq` True = x
  forceEval x@(Key_UID  y) | forceEval y `seq` True = x
  forceEval x@(Key_Str  y) | forceEval y `seq` True = x
  -- forceEval x@(Key_TyQu y) | forceEval y `seq` True = x
  forceEval x@(Key_HNm  y) | forceEval y `seq` True = x
  forceEval x              = x
%%[[101
  fevCount (Key_Ty   y) = cm1 "Key_Ty"   `cmUnion` fevCount y
  fevCount (Key_UID  y) = cm1 "Key_UID"  `cmUnion` fevCount y
  fevCount (Key_Str  y) = cm1 "Key_Str"  `cmUnion` fevCount y
  fevCount (Key_TyQu y) = cm1 "Key_TyQu" `cmUnion` fevCount y
  fevCount (Key_HNm  y) = cm1 "Key_HNm"  `cmUnion` fevCount y
%%]]
%%]

