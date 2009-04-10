%%[99

module EHC.Ord
where

import EHC.Prelude
import EHC.Eq

#include "TupleInstance.h"

%%]

%%[99
instance Ord () where
    compare () () = EQ

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tuple instances, using 'poor mans deriving' macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
TUPLE2_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE3_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE4_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE5_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)

compareTupleElt :: Ord x => x -> x -> Ordering -> Ordering
compareTupleElt x1 x2 = case x1 `compare` x2 of
                          EQ -> id
                          o  -> const o
%%]
