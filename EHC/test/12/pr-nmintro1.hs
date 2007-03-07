class A a
f :: A a => a -> a
g = \{! d <: A a !} a -> a
main = g {! () <: A Int !} 3

