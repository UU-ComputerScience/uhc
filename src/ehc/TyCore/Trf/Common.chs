%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Common data types used by TyCore Transformations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore tauphi) module {%{EH}TyCore.Trf.Common} import({%{EH}Base.Common})
%%]

%%[(50 codegen tycore tauphi) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(8 codegen tycore tauphi) export(BindType (..), Seq)
data BindType = NameTypeBind | BodyBind | NoBind
  deriving (Eq, Show)

type Seq a = [a]
%%]

%%[(8 codegen tycore tauphi) export(WorkWrap(..),(|||))
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

%%[(8 codegen tycore tauphi) export(bool, list)
-- This function comes from bool-extras:Data.Bool.Extras
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- This function comes from in list-extras:Data.List.Extras
list :: (a -> [a] -> b) -> b -> [a] -> b
list _ b []     = b
list f _ (x:xs) = f x xs
%%]

