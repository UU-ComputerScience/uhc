%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
%%]

%%[doesWhat doclatex
%%]

%%[20 module {%{EH}Base.Serialize}
%%]

%%[20 import({%{EH}Base.Common},{%{EH}Base.Target},{%{EH}Base.HsName})
%%]

%%[20 import(UU.Scanner.Position)
%%]

%%[20 import(Data.Typeable(Typeable,Typeable1), {%{EH}Base.Binary})
%%]
%%[20 import(Data.Generics(Data))
%%]

%%[20 import(Data.Word, Data.Array, Control.Monad)
%%]
%%[20 import(qualified Data.Map as Map, qualified Control.Monad.State as St)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
deriving instance Typeable UID
deriving instance Data UID

deriving instance Typeable HsName
deriving instance Data HsName

deriving instance Typeable CTag
deriving instance Data CTag

deriving instance Typeable FFIWay
deriving instance Data FFIWay

deriving instance Typeable OrigName
deriving instance Data OrigName

deriving instance Typeable PredOccId
deriving instance Data PredOccId

deriving instance Typeable Fixity
deriving instance Data Fixity

deriving instance Typeable Pos
deriving instance Data Pos

deriving instance Typeable Range
deriving instance Data Range

deriving instance Typeable IdOccKind
deriving instance Data IdOccKind

deriving instance Typeable IdOcc
deriving instance Data IdOcc

deriving instance Typeable VarUIDHsName
deriving instance Data VarUIDHsName

%%]

%%[20
deriving instance Typeable1 RLList
deriving instance Data x => Data (RLList x)

deriving instance Typeable1 AlwaysEq
deriving instance Data x => Data (AlwaysEq x)

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization with state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
%%]
type SPutS x = State (Map.Map Int x) Put
data SPutState k v
  = SPutState
      { spsMp :: Map.Map k v
      }

newtype SPutM x = SPut { unSPut :: (x,SPutState) }

class Binary x => Serialize x where
  sput :: x -> 

