-- import single export: via export of intermediate module

module Main
where

import Imp1.Exp2

i2 = Imp1.Exp2.i1
i3 = i1

main = i2
