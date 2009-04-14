%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types not defined in UHC.Base but required for UHC.XX modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Definitions for types which
\begin{itemize}
\item are used in various UHC.XX modules, but defined here to break module circularities
\item are known by the compiler as well, some of them.
\end{itemize}
%%]

%%[99
module UHC.Types
  -- export all
  where

import UHC.Base

#include "HsBaseConfig.h"
#include "CTypes.h"

%%]

These are all known by the compiler

%%[99
data Int8
data Int16
data Int32
data Int64

data Word
data Word8
data Word16
data Word32
data Word64

data Addr

%%]

These are required by early (in import order) definitions, like Handle

%%[99
-- | Haskell type representing the C @char@ type.
NEWTYPE_TYPE_NODERIVING(CChar,HTYPE_CHAR)

-- | Haskell type representing the C @int@ type.
NEWTYPE_TYPE_NODERIVING(CInt,HTYPE_INT)
%%]
