%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common data types used by TyCore Transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}TyCore.Trf.Common} import({%{EH}Base.Common})
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
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

