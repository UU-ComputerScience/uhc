%%[99
module EHC.Real
  ( showSigned
  )
  where

import EHC.Prelude
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Taken from GHC.Real:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
-- | Converts a possibly-negative 'Real' value to a string.
showSigned :: (Real a)
  => (a -> ShowS)       -- ^ a function that can show unsigned values
  -> Int                -- ^ the precedence of the enclosing context
  -> a                  -- ^ the value to show
  -> ShowS
showSigned showPos p x 
   | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x
%%]

