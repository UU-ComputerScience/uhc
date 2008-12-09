-- import single export: qualified via as, failed via orig name (qualified and not)

module Main
where

import qualified Imp1.Exp1 as E

i2 = Imp1.Exp1.i1
i3 = i1

main = i2
