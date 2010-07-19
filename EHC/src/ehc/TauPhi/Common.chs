%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TauPhi Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}TauPhi.Common} import({%{EH}Base.Common})
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(8 codegen) hs export(Strictness(..), Uniqueness(..))
data Strictness
  = Strict
  | NonStrict
  | StrictnessVar HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strict            = "strict"
  show NonStrict         = "nonStrict"
  show (StrictnessVar n) = "strictness:" ++ show n

data Uniqueness
  = Unique
  | NonUnique
  | UniquenessVar HsName
  deriving (Eq, Ord)

instance Show Uniqueness where
  show Unique            = "unique"
  show NonUnique         = "nonUnique"
  show (UniquenessVar n) = "uniqueness:" ++ show n
%%]

%%[(8 codegen) export(WorkWrap(..),(|||))
-- Status of Worker/Wrapper transformation
data WorkWrap
  = Introduced      -- A Worker and Wrapper were introduced
  | UpdatedWorker   -- Worker was updated
  | UpdatedWrapper  -- Wrapper was updated
  | Ignored         -- Nothing was done
  deriving (Eq, Show)

(|||) :: WorkWrap -> WorkWrap -> WorkWrap
x              ||| Ignored        = x
Ignored        ||| y              = y
Introduced     ||| Introduced     = Introduced
UpdatedWorker  ||| UpdatedWorker  = UpdatedWorker
UpdatedWrapper ||| UpdatedWrapper = UpdatedWrapper
_              ||| _              = error "(|||): Can't merge arguments."
%%]

%%[(8 codegen) export(BindType (..), Seq)
data BindType = NameTypeBind | BodyBind | NoBind
  deriving (Eq, Show)

type Seq a = [a]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen)
deriving instance Typeable Strictness
deriving instance Data Strictness

deriving instance Typeable Uniqueness
deriving instance Data Uniqueness
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

instance Serialize Uniqueness where
  sput (Unique)           = sputWord8 0
  sput (NonUnique)        = sputWord8 1
  sput (UniquenessVar nm) = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Unique
              1 -> return NonUnique
              2 -> liftM UniquenessVar sget
%%]
