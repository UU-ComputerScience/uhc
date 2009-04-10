%%[99

module EHC.Eq
where

import EHC.Prelude

#include "TupleInstance.h"

%%]

%%[99
instance Eq () where
    () == ()  =  True

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tuple instances, using 'poor mans deriving' macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
TUPLE2_BINOP1_INSTANCE(Eq,==,==,,&&,)
TUPLE3_BINOP1_INSTANCE(Eq,==,==,,&&,)
TUPLE4_BINOP1_INSTANCE(Eq,==,==,,&&,)
TUPLE5_BINOP1_INSTANCE(Eq,==,==,,&&,)
%%]

