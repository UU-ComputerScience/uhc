-- import single export: qualified via as

module Main
where

import qualified Imp1.Exp1 as E

i2 = E.i1

main = i2
