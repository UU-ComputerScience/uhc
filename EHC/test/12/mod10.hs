-- import single export: via export of intermediate and original module

module Main
where

import Imp1.Exp1
import Imp1.Exp2

i2 = Imp1.Exp1.i1
i3 = Imp1.Exp2.i1

main = i2
