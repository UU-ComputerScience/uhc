-- reexport + redefine (should give rise to error, even though same val)

module Main( module Imp1.Exp1, i1 )
where

import Imp1.Exp1

i1 = Imp1.Exp1.i1

main = i1
