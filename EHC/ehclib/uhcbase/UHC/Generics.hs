--  Only major extension required
{-#  LANGUAGE MultiParamTypeClasses  #-}

--  For aesthetics only
{-#  LANGUAGE EmptyDataDecls  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE TypeSynonymInstances  #-}

--  To remove
{-#  LANGUAGE ScopedTypeVariables  #-}

module UHC.Generics
  ( Representable0
  , Representable1
  )
  where

import UHC.Base

infixr 5 :+:
infixr 6 :*:, ::*::
infixr 7 :.:

data V1            pT

data U1            pT    =  U1_ | DummyU pT

data (:+:)  fT gT  pT    =  L1_  { unL1  :: fT pT }
                         |  R1_  { unR1  :: gT pT }

data (:*:)  fT gT  pT    =  fT pT ::*:: gT pT

data Par1 pT         = Par1_ { unP1 :: pT }

data Rec1   fT     pT    = Rec1_ { unRec1 :: fT pT }

data (:.:)  fT gT  pT    = Comp1_ (fT (gT pT))

data K1 iT cT     pT  =  K1_  { unK1  :: cT } | DummyK iT pT

data M1 iT cT fT  pT  =  M1_  { unM1  :: fT pT } | DummyM iT pT

data B
data R
data P

type K0    = K1 B
type Rec0  = K1 R
type Par0  = K1 P

data D
data C
data S

type D1    = M1 D
type C1    = M1 C
type S1    = M1 S

class Datatype cT where
  datatypeName  :: tT cT fT pT -> String

class Selector cT where
  selName :: tT cT fT pT -> String

class Constructor cT where
  conName   :: tT cT fT pT -> String

  conFixity :: tT cT fT pT -> Fixity
  conFixity = const Prefix

  conIsRecord :: tT cT fT pT -> Bool
  conIsRecord = const False

data Fixity = Prefix | Infix Associativity Int

data Associativity  =  LeftAssociative 
                    |  RightAssociative
                    |  NotAssociative

class Representable0 aT repT where
  from0  :: aT -> repT xT
  to0    :: repT xT -> aT

class Representable1 fT repT where
  from1  :: fT pT -> repT pT
  to1    :: repT pT -> fT pT
