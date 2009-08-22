-- reexport + redefine (should give rise to error)

module Imp1.Exp3( module Imp1.Exp1, i1 )
where

import Imp1.Exp1

i1 = Imp1.Exp1.i1

