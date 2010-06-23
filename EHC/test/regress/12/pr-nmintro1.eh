let  f :: A a => a -> a
     g = \{! d <: A a !} a -> a
in   g {! () <: A Int !} 3

