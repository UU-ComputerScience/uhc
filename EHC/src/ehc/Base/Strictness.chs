%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Strictness Common for HS, EH, Ty, TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen || hmtyinfer || hmtyast) module {%{EH}Base.Strictness}
%%]

%%[(8 codegen || hmtyinfer || hmtyast) hs import({%{EH}Base.HsName})
%%]

%%[(50 codegen || hmtyinfer || hmtyast) hs import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[(8 codegen || hmtyinfer || hmtyast) hs export(Strictness(..))
data Strictness
  = Strictness_Strict
  | Strictness_NonStrict
  | Strictness_Var HsName
  deriving (Eq, Ord, Data)

instance Show Strictness where
  show Strictness_Strict    = "strict"
  show Strictness_NonStrict = "nonStrict"
  show (Strictness_Var n)   = "strictness:" ++ show n
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen || hmtyinfer || hmtyast)
deriving instance Typeable Strictness
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen || hmtyinfer || hmtyast)
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

