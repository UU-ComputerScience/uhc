-- reexport + redefine (should give rise to error)

module Main( module Imp1.Exp1, i1 )
where

import Imp1.Exp1

i1 = 3

main = i1
