-- failed import single export: hidden

module Main
where

import Imp1.Exp1 hiding(i1)

i2 = i1

main = i2
