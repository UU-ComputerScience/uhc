{- ----------------------------------------------------------------------------------------
   what    : type level operator syntax
   expected: all ok
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE NoGenericDeriving #-}

module Context1 where

class C x
class D x
class a :+: b

f1 :: (a ~ b) => a -> b
f2 :: (a ~ b, a ~ b) => a -> b
f3 :: (C b) => a -> b
f4 :: (C a, C b) => a -> b
f5 :: (a ~ b, C a, C b) => a -> b
f6 :: (a :+: b) => a -> b
f7 :: (a :+: b, a :+: b) => a -> b

main = return ()
