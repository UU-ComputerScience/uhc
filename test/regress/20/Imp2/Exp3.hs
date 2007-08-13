-- explicit export: only type + 1 con

module Imp2.Exp3
  ( X(XX)
  )
  where

data X a = X a | XX
