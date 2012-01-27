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

%%[(50 codegen) hs import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
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

%%[(50 codegen)
deriving instance Typeable Strictness
deriving instance Data Strictness
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen)
instance Binary Strictness where
  put (Strictness_Strict           )   = putWord8 0
  put (Strictness_NonStrict        )   = putWord8 1
  put (Strictness_Var          nm  )   = putWord8 2 >> put nm
  get = do  t <- getWord8
            case t of
              0 -> return Strictness_Strict
              1 -> return Strictness_NonStrict
              2 -> liftM  Strictness_Var get

instance Serialize Strictness where
  sput = sputShared
  sget = sgetShared
  sputNested = sputPlain
  sgetNested = sgetPlain

%%]

