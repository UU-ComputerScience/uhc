-- import class/instance: ok

module Main
where

import Imp3.Exp1

i = c {! dCInt <: C Int !} 3

main = i
