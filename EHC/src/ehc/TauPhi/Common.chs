%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TauPhi Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}TauPhi.Common} import({%{EH}Base.Common})
%%]

%%[(8 tauphi) hs export(Strictness(..))
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(8 tauphi)
data Strictness
  = Strict
  | NonStrict
  | Var HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strict    = "strict"
  show NonStrict = "nonStrict"
  show (Var n)   = "strictness:" ++ show n

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 tauphi)
deriving instance Typeable Strictness
deriving instance Data Strictness

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 tauphi)
instance Serialize Strictness where
  sput (Strict)    = sputWord8 0
  sput (NonStrict) = sputWord8 1
  sput (Var nm)    = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Strict
              1 -> return NonStrict
              2 -> liftM Var sget
%%]
