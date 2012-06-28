{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeOperators         #-}

module UHC.Generics.Tuple(_Rep0Tuple0,_Rep0Tuple2,_Rep1Tuple2,_Rep0Tuple3,_Rep1Tuple3,_Rep0Tuple4,_Rep1Tuple4,_Rep0Tuple5,_Rep1Tuple5,_Rep0Tuple6,_Rep1Tuple6,_Rep0Tuple7,_Rep1Tuple7,_Rep0Tuple8,_Rep1Tuple8,_Rep0Tuple9,_Rep1Tuple9,_Rep0Tuple10,_Rep1Tuple10,_Rep0Tuple11,_Rep1Tuple11,_Rep0Tuple12,_Rep1Tuple12,_Rep0Tuple13,_Rep1Tuple13,_Rep0Tuple14,_Rep1Tuple14,_Rep0Tuple15,_Rep1Tuple15) where
import UHC.Base


--------------------------------------------------------------------------------

data _D_Tuple0
data _C_Tuple0

instance Datatype _D_Tuple0 where
  datatypeName _ = "()"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple0 where
  conName _ = "()"
  conIsTuple _ = Arity 0

type _Rep0Tuple0  = D1 _D_Tuple0 (C1 _C_Tuple0 (S1 NoSelector (U1)))

instance Representable0 (() ) (_Rep0Tuple0 ) where
  from0 (() ) = (M1 (M1 (M1 (U1))))
  to0 (M1 (M1 (M1 (U1)))) = (() )





--------------------------------------------------------------------------------

data _D_Tuple2
data _C_Tuple2

instance Datatype _D_Tuple2 where
  datatypeName _ = "(,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple2 where
  conName _ = "(,)"
  conIsTuple _ = Arity 2

type _Rep0Tuple2 x1 x2 = D1 _D_Tuple2 (C1 _C_Tuple2 (S1 NoSelector (Rec0 x1 :*: Rec0 x2)))

instance Representable0 ((,) x1 x2) (_Rep0Tuple2 x1 x2) where
  from0 ((,) x1 x2) = (M1 (M1 (M1 (K1 x1 :*: K1 x2))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2)))) = ((,) x1 x2)

type _Rep1Tuple2 x1 = D1 _D_Tuple2 (C1 _C_Tuple2 (S1 NoSelector (Rec0 x1 :*: Par1)))

instance Representable1 ((,) x1) (_Rep1Tuple2 x1) where
  from1 ((,) x1 x2) = (M1 (M1 (M1 (K1 x1 :*: Par1 x2))))
  to1 (M1 (M1 (M1 (K1 x1 :*: Par1 x2)))) = ((,) x1 x2)

--------------------------------------------------------------------------------

data _D_Tuple3
data _C_Tuple3

instance Datatype _D_Tuple3 where
  datatypeName _ = "(,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple3 where
  conName _ = "(,,)"
  conIsTuple _ = Arity 3

type _Rep0Tuple3 x1 x2 x3 = D1 _D_Tuple3 (C1 _C_Tuple3 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3)))

instance Representable0 ((,,) x1 x2 x3) (_Rep0Tuple3 x1 x2 x3) where
  from0 ((,,) x1 x2 x3) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3)))) = ((,,) x1 x2 x3)

type _Rep1Tuple3 x1 x2 = D1 _D_Tuple3 (C1 _C_Tuple3 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Par1)))

instance Representable1 ((,,) x1 x2) (_Rep1Tuple3 x1 x2) where
  from1 ((,,) x1 x2 x3) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: Par1 x3))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: Par1 x3)))) = ((,,) x1 x2 x3)

--------------------------------------------------------------------------------

data _D_Tuple4
data _C_Tuple4

instance Datatype _D_Tuple4 where
  datatypeName _ = "(,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple4 where
  conName _ = "(,,,)"
  conIsTuple _ = Arity 4

type _Rep0Tuple4 x1 x2 x3 x4 = D1 _D_Tuple4 (C1 _C_Tuple4 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4)))

instance Representable0 ((,,,) x1 x2 x3 x4) (_Rep0Tuple4 x1 x2 x3 x4) where
  from0 ((,,,) x1 x2 x3 x4) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4)))) = ((,,,) x1 x2 x3 x4)

type _Rep1Tuple4 x1 x2 x3 = D1 _D_Tuple4 (C1 _C_Tuple4 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Par1)))

instance Representable1 ((,,,) x1 x2 x3) (_Rep1Tuple4 x1 x2 x3) where
  from1 ((,,,) x1 x2 x3 x4) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: Par1 x4))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: Par1 x4)))) = ((,,,) x1 x2 x3 x4)

--------------------------------------------------------------------------------

data _D_Tuple5
data _C_Tuple5

instance Datatype _D_Tuple5 where
  datatypeName _ = "(,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple5 where
  conName _ = "(,,,,)"
  conIsTuple _ = Arity 5

type _Rep0Tuple5 x1 x2 x3 x4 x5 = D1 _D_Tuple5 (C1 _C_Tuple5 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5)))

instance Representable0 ((,,,,) x1 x2 x3 x4 x5) (_Rep0Tuple5 x1 x2 x3 x4 x5) where
  from0 ((,,,,) x1 x2 x3 x4 x5) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5)))) = ((,,,,) x1 x2 x3 x4 x5)

type _Rep1Tuple5 x1 x2 x3 x4 = D1 _D_Tuple5 (C1 _C_Tuple5 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Par1)))

instance Representable1 ((,,,,) x1 x2 x3 x4) (_Rep1Tuple5 x1 x2 x3 x4) where
  from1 ((,,,,) x1 x2 x3 x4 x5) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: Par1 x5))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: Par1 x5)))) = ((,,,,) x1 x2 x3 x4 x5)

--------------------------------------------------------------------------------

data _D_Tuple6
data _C_Tuple6

instance Datatype _D_Tuple6 where
  datatypeName _ = "(,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple6 where
  conName _ = "(,,,,,)"
  conIsTuple _ = Arity 6

type _Rep0Tuple6 x1 x2 x3 x4 x5 x6 = D1 _D_Tuple6 (C1 _C_Tuple6 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6)))

instance Representable0 ((,,,,,) x1 x2 x3 x4 x5 x6) (_Rep0Tuple6 x1 x2 x3 x4 x5 x6) where
  from0 ((,,,,,) x1 x2 x3 x4 x5 x6) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6)))) = ((,,,,,) x1 x2 x3 x4 x5 x6)

type _Rep1Tuple6 x1 x2 x3 x4 x5 = D1 _D_Tuple6 (C1 _C_Tuple6 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Par1)))

instance Representable1 ((,,,,,) x1 x2 x3 x4 x5) (_Rep1Tuple6 x1 x2 x3 x4 x5) where
  from1 ((,,,,,) x1 x2 x3 x4 x5 x6) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: Par1 x6))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: Par1 x6)))) = ((,,,,,) x1 x2 x3 x4 x5 x6)

--------------------------------------------------------------------------------

data _D_Tuple7
data _C_Tuple7

instance Datatype _D_Tuple7 where
  datatypeName _ = "(,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple7 where
  conName _ = "(,,,,,,)"
  conIsTuple _ = Arity 7

type _Rep0Tuple7 x1 x2 x3 x4 x5 x6 x7 = D1 _D_Tuple7 (C1 _C_Tuple7 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7)))

instance Representable0 ((,,,,,,) x1 x2 x3 x4 x5 x6 x7) (_Rep0Tuple7 x1 x2 x3 x4 x5 x6 x7) where
  from0 ((,,,,,,) x1 x2 x3 x4 x5 x6 x7) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7)))) = ((,,,,,,) x1 x2 x3 x4 x5 x6 x7)

type _Rep1Tuple7 x1 x2 x3 x4 x5 x6 = D1 _D_Tuple7 (C1 _C_Tuple7 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Par1)))

instance Representable1 ((,,,,,,) x1 x2 x3 x4 x5 x6) (_Rep1Tuple7 x1 x2 x3 x4 x5 x6) where
  from1 ((,,,,,,) x1 x2 x3 x4 x5 x6 x7) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: Par1 x7))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: Par1 x7)))) = ((,,,,,,) x1 x2 x3 x4 x5 x6 x7)

--------------------------------------------------------------------------------

data _D_Tuple8
data _C_Tuple8

instance Datatype _D_Tuple8 where
  datatypeName _ = "(,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple8 where
  conName _ = "(,,,,,,,)"
  conIsTuple _ = Arity 8

type _Rep0Tuple8 x1 x2 x3 x4 x5 x6 x7 x8 = D1 _D_Tuple8 (C1 _C_Tuple8 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8)))

instance Representable0 ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8) (_Rep0Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) where
  from0 ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8)))) = ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8)

type _Rep1Tuple8 x1 x2 x3 x4 x5 x6 x7 = D1 _D_Tuple8 (C1 _C_Tuple8 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Par1)))

instance Representable1 ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7) (_Rep1Tuple8 x1 x2 x3 x4 x5 x6 x7) where
  from1 ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: Par1 x8))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: Par1 x8)))) = ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8)

--------------------------------------------------------------------------------

data _D_Tuple9
data _C_Tuple9

instance Datatype _D_Tuple9 where
  datatypeName _ = "(,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple9 where
  conName _ = "(,,,,,,,,)"
  conIsTuple _ = Arity 9

type _Rep0Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9 = D1 _D_Tuple9 (C1 _C_Tuple9 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9)))

instance Representable0 ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9) (_Rep0Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) where
  from0 ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9)))) = ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9)

type _Rep1Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 = D1 _D_Tuple9 (C1 _C_Tuple9 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Par1)))

instance Representable1 ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8) (_Rep1Tuple9 x1 x2 x3 x4 x5 x6 x7 x8) where
  from1 ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: Par1 x9))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: Par1 x9)))) = ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9)

--------------------------------------------------------------------------------

data _D_Tuple10
data _C_Tuple10

instance Datatype _D_Tuple10 where
  datatypeName _ = "(,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple10 where
  conName _ = "(,,,,,,,,,)"
  conIsTuple _ = Arity 10

type _Rep0Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = D1 _D_Tuple10 (C1 _C_Tuple10 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10)))

instance Representable0 ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (_Rep0Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) where
  from0 ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10)))) = ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

type _Rep1Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 = D1 _D_Tuple10 (C1 _C_Tuple10 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Par1)))

instance Representable1 ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9) (_Rep1Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9) where
  from1 ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: Par1 x10))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: Par1 x10)))) = ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

--------------------------------------------------------------------------------

data _D_Tuple11
data _C_Tuple11

instance Datatype _D_Tuple11 where
  datatypeName _ = "(,,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple11 where
  conName _ = "(,,,,,,,,,,)"
  conIsTuple _ = Arity 11

type _Rep0Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = D1 _D_Tuple11 (C1 _C_Tuple11 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11)))

instance Representable0 ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (_Rep0Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) where
  from0 ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11)))) = ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

type _Rep1Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = D1 _D_Tuple11 (C1 _C_Tuple11 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Par1)))

instance Representable1 ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (_Rep1Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) where
  from1 ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: Par1 x11))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: Par1 x11)))) = ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

--------------------------------------------------------------------------------

data _D_Tuple12
data _C_Tuple12

instance Datatype _D_Tuple12 where
  datatypeName _ = "(,,,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple12 where
  conName _ = "(,,,,,,,,,,,)"
  conIsTuple _ = Arity 12

type _Rep0Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = D1 _D_Tuple12 (C1 _C_Tuple12 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12)))

instance Representable0 ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (_Rep0Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) where
  from0 ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12)))) = ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)

type _Rep1Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = D1 _D_Tuple12 (C1 _C_Tuple12 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Par1)))

instance Representable1 ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (_Rep1Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) where
  from1 ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: Par1 x12))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: Par1 x12)))) = ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)

--------------------------------------------------------------------------------

data _D_Tuple13
data _C_Tuple13

instance Datatype _D_Tuple13 where
  datatypeName _ = "(,,,,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple13 where
  conName _ = "(,,,,,,,,,,,,)"
  conIsTuple _ = Arity 13

type _Rep0Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 = D1 _D_Tuple13 (C1 _C_Tuple13 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Rec0 x13)))

instance Representable0 ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (_Rep0Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) where
  from0 ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13)))) = ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)

type _Rep1Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = D1 _D_Tuple13 (C1 _C_Tuple13 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Par1)))

instance Representable1 ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (_Rep1Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) where
  from1 ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: Par1 x13))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: Par1 x13)))) = ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)

--------------------------------------------------------------------------------

data _D_Tuple14
data _C_Tuple14

instance Datatype _D_Tuple14 where
  datatypeName _ = "(,,,,,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple14 where
  conName _ = "(,,,,,,,,,,,,,)"
  conIsTuple _ = Arity 14

type _Rep0Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = D1 _D_Tuple14 (C1 _C_Tuple14 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Rec0 x13 :*: Rec0 x14)))

instance Representable0 ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (_Rep0Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) where
  from0 ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14)))) = ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)

type _Rep1Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 = D1 _D_Tuple14 (C1 _C_Tuple14 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Rec0 x13 :*: Par1)))

instance Representable1 ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (_Rep1Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) where
  from1 ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: Par1 x14))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: Par1 x14)))) = ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)

--------------------------------------------------------------------------------

data _D_Tuple15
data _C_Tuple15

instance Datatype _D_Tuple15 where
  datatypeName _ = "(,,,,,,,,,,,,,,)"
  moduleName   _ = "Prelude"

instance Constructor _C_Tuple15 where
  conName _ = "(,,,,,,,,,,,,,,)"
  conIsTuple _ = Arity 15

type _Rep0Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 = D1 _D_Tuple15 (C1 _C_Tuple15 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Rec0 x13 :*: Rec0 x14 :*: Rec0 x15)))

instance Representable0 ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (_Rep0Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) where
  from0 ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14 :*: K1 x15))))
  to0 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14 :*: K1 x15)))) = ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

type _Rep1Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = D1 _D_Tuple15 (C1 _C_Tuple15 (S1 NoSelector (Rec0 x1 :*: Rec0 x2 :*: Rec0 x3 :*: Rec0 x4 :*: Rec0 x5 :*: Rec0 x6 :*: Rec0 x7 :*: Rec0 x8 :*: Rec0 x9 :*: Rec0 x10 :*: Rec0 x11 :*: Rec0 x12 :*: Rec0 x13 :*: Rec0 x14 :*: Par1)))

instance Representable1 ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (_Rep1Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) where
  from1 ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14 :*: Par1 x15))))
  to1 (M1 (M1 (M1 (K1 x1 :*: K1 x2 :*: K1 x3 :*: K1 x4 :*: K1 x5 :*: K1 x6 :*: K1 x7 :*: K1 x8 :*: K1 x9 :*: K1 x10 :*: K1 x11 :*: K1 x12 :*: K1 x13 :*: K1 x14 :*: Par1 x15)))) = ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)