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
  = Strict
  | NonStrict
  | StrictnessVar HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strict            = "strict"
  show NonStrict         = "nonStrict"
  show (StrictnessVar n) = "strictness:" ++ show n
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
  sput (Strict)           = sputWord8 0
  sput (NonStrict)        = sputWord8 1
  sput (StrictnessVar nm) = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Strict
              1 -> return NonStrict
              2 -> liftM StrictnessVar sget
%%]

