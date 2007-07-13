{-# OPTIONS_GHC -fglasgow-exts #-}

-- This file can be processed by EHC and GHC
-- To adapt this file for running with GHC or GHCI,
-- remove the definitions of ex3, total, run, and the mini-prelude below
-- and select the alternative definition of data Fun


-- mini-prelude for use with EHC
main = total
id x = x
snd (x,y) = y
fst (x,y) = x
data [] a = ''[]'' | a : [a]
foreign import ccall primAddInt :: Int -> Int -> Int
(+)   =  primAddInt
undefined = undefined


-- type Fun is a function (wrapped up in a datatype) that:
-- given a function that takes values of an unknown type b
-- produces a value of that type b, and an Int

-- the notation for the existential quantor in EHC is "exists"
-- in GHC, one abuses the word "forall", written outside the constructor.

data Fun a    =            F (exists b . ((b -> a) -> (b, Int)))  -- EHC version
--data Fun a  =  forall b. F             ((b -> a) -> (b, Int))   -- GHC version


-- Here are three examples of Fun, where b is instantiated as Int, Char or even List, respectively:

ex1 :: forall a. Fun a
ex1 =  F (\(_ :: (Int -> a))  -> (5, 10))

ex2 :: forall a. Fun a
ex2 =  F (\(_ :: (Char -> a)) -> ('@', 20))

ex3 :: forall a. Fun a
ex3 =  F (\(_ :: ([t] -> a))  -> ([], 30))


-- We think it should be possible to write a function run, which extracts the Int value 
-- in the second component of the result of any Fun value

total = run ex1 + run ex2 + run ex3

-- Now follows is the implementation of run:
-- Clearly, it must call the function hidden in the Fun, and take the snd of the result.
-- But to what argument should it apply its hidden function?
-- if the parameter were ex2, the ord function would be acceptable, and for ex3 the hd function.
-- But what function is acceptable by ex1, ex2 and ex3?
-- The only candidate is id::a->a

run :: (forall a . Fun a) -> Int
run (F fun) = snd( fun id )

-- Although is is perfectly type-safe, this is not typeable by GHC.

-- However, there is a way to circumvent the problem.
-- We can simulate type Fun by type Sim, which is isomorphic to it by
--   exists a.T(a)  =~= forall c.(forall a.T(a) -> c)->c

data Sim a = S (forall c. (forall b. ((b -> a) -> (b, Int)) -> c) -> c)
             -- S has a rank-3 type!

-- we can now define function run2, which converts the fun,
-- and then does a simulated run with "sim":

run2  :: (forall a. Fun a) -> Int
run2 fun =  sim (convert fun)

convert       :: forall a. Fun a -> Sim a
convert r =  case r of (F f) -> S (\g -> g f)
-- open question: can we define the opposite conversion?

sim   :: (forall a. Sim a) -> Int
sim r =  h (\f -> snd (f id))
  where
    h :: forall c. (forall b. ((b -> b) -> (b, Int)) -> c) -> c
    h =  case r of S h -> h

total2 = run2 ex1 + run2 ex2 + run2 ex4

-- The strange thing now is that sim *is* typable by GHC.

-- We have to replace ex3 by ex4, however, which is even stranger,
-- because applying this Fun to id gives rise to an infinitely nested list
ex4 :: forall a. Fun a
ex4 =  F (\(_ :: ([a] -> a))  -> ([], 30))

-- Summarizing:
-- in GHC run gives a type-error, but the isomorphic run2 compiles OK
-- We also tried our compiler EHC: both versions compile OK.
-- In Hugs, it is run that compiles OK, but compiling run2 crashes the compiler.
