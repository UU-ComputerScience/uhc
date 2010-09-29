%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Strictness Common for HS, EH, Ty, TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Strictness}
%%]

%%[(8 codegen) hs import({%{EH}Base.HsName})
%%]

%%[(20 codegen) hs import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(8 codegen) hs export(Strictness(..))
data Strictness
  = Strictness_Strict
  | Strictness_NonStrict
  | Strictness_Var HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strictness_Strict    = "strict"
  show Strictness_NonStrict = "nonStrict"
  show (Strictness_Var n)   = "strictness:" ++ show n
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
deriving instance Typeable Strictness
deriving instance Data Strictness
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
instance Serialize Strictness where
  sput (Strictness_Strict           )   = sputWord8 0
  sput (Strictness_NonStrict        )   = sputWord8 1
  sput (Strictness_Var          nm  )   = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Strictness_Strict
              1 -> return Strictness_NonStrict
              2 -> liftM  Strictness_Var sget
%%]

