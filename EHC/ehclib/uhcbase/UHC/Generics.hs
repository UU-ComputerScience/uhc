{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}

module UHC.Generics where

--import UHC.Base

-- To remove at some point
import Data.Monoid (Monoid(..))
import Prelude

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
-- V :: * -> *
data V1 p

-- | Unit: used for constructors without arguments
-- U1 :: * -> *
data U1 p = U1 | Dummy___ p

-- | TUsed for marking occurrences of the parameter
--Par1 :: * -> *
data Par1 p = Par1 { unPar1 :: p }


-- | Recursive calls of kind * -> *
--Rec1 :: (* -> *) -> * -> *
data Rec1 f p = Rec1 { unRec1 :: f p }

-- | Constants, additional parameters and recursion of kind *
--K1 :: * -> * -> * -> *
data K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
--M1 :: * -> * -> (* -> *) -> * -> *
data M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
-- (:+:) :: (* -> *) -> (* -> *) -> * -> *
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
-- (:*:) :: (* -> *) -> (* -> *) -> * -> *
data (:*:) f g p = f p :*: g p

-- | Composition of functors
infixr 7 :.:
-- (:.:) :: (* -> *) -> (* -> *) -> * -> *
data (:.:) f g p = Comp1 (f (g p))

-- | Tag for K1: base types
data B
-- | Tag for K1: recursion (of kind *)
data R
-- | Tag for K1: parameters (other than the last)
data P

-- | Type synonym for encoding base types
type K0    = K1 B
-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R
-- | Type synonym for encoding parameters (other than the last)
type Par0  = K1 P

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C
-- | Tag for M1: record selector
data S

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors
type S1 = M1 S

-- | Class for datatypes that represent datatypes
class Datatype d where
  -- | The name of the datatype, fully qualified
#ifdef __UHC__
  datatypeName :: t d f a -> String
#else
  datatypeName :: t d (f :: * -> *) a -> String
#endif

-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
  selName :: t s f a -> String

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
#ifdef __UHC__
  conName :: t c f a -> String
#else
  conName :: t c (f :: * -> *) a -> String
#endif

  -- | The fixity of the constructor
#ifdef __UHC__
  conFixity :: t c f a -> Fixity
#else
  conFixity :: t c (f :: * -> *) a -> Fixity
#endif  
  conFixity = const Prefix

  -- | Marks if this constructor is a record
#ifdef __UHC__
  conIsRecord :: t c f a -> Bool
#else
  conIsRecord :: t c (f :: * -> *) a -> Bool
#endif
  conIsRecord = const False

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the associativy of a constructor
data Associativity =  LeftAssociative 
                   |  RightAssociative
                   |  NotAssociative
  deriving (Eq, Show, Ord, Read)

-- | Representable types of kind *
class Representable0 a rep where
  -- | Convert from the datatype to its representation
  from0  :: a -> rep x
  -- | Convert from the representation to the datatype
  to0    :: rep x -> a

-- | Representable types of kind * -> *
class Representable1 f rep where
  -- | Convert from the datatype to its representation
  from1  :: f a -> rep a
  -- | Convert from the representation to the datatype
  to1    :: rep a -> f a


-- Below is example code which should be removed at some point

main = undefined

data Exp = Const Int | Plus Exp Exp deriving Show

data Rep_
data Const_
data Plus_

instance Datatype Rep_ where
  datatypeName _ = "Representation.Rep"

instance Constructor Const_  where
  conName _ = "Const"
instance Constructor Plus_   where
  conName _ = "Plus"

--  Representable1 instance, necessary for derivings
type RepExp = 
  D1 Rep_ (    C1 Const_  (K0 Int)
          :+:  C1 Plus_   (Rec0 Exp :*: Rec0 Exp))

instance Representable0 Exp RepExp where
  from0 (Const n)    = M1 (L1 (M1 (K1 n)))
  from0 (Plus e e')  = M1 (R1 (M1 (K1 e :*: K1 e')))

  to0 (M1 (L1 (M1 (K1 n))))               = Const n
  to0 (M1 (R1 (M1 (K1 e :*: K1 e'))))  = Plus e e'

--  Instance for Encode
instance Encode Exp where
  encode = t undefined where
    t :: RepExp x -> Exp -> [Bit]
    t = encodeDefault


data List a = Nil | Cons a (List a) deriving (Show, Eq)

--  Constructor and datatype information
data List_
data Nil_
data Cons_

instance Datatype List_ where
  datatypeName _ = "Representation.List"

instance Constructor Nil_  where
  conName _ = "Nil"
instance Constructor Cons_ where
  conName _ = "Cons"

--  Representable instances, necessary for derivings
type Rep0List a =
  D1 List_  (    C1 Nil_ U1
            :+:  C1 Cons_ (Par0 a :*: Rec0 (List a)))

instance Representable0 (List a) (Rep0List a) where
  from0 Nil         = M1 (L1 (M1 U1))
  from0 (Cons h t)  = M1 (R1 (M1 (K1 h :*: K1 t)))
  to0 (M1 (L1 (M1 U1)))                  = Nil
  to0 (M1 (R1 (M1 (K1 h :*: K1 t))))  = Cons h t

type RepList =
  D1 List_  (    C1 Nil_ U1
            :+:  C1 Cons_ (Par1 :*: Rec1 List))

instance Representable1 List RepList where
  from1 Nil         = M1 (L1 (M1 U1))
  from1 (Cons h t)  = M1 (R1 (M1 (Par1 h :*: Rec1 t)))
  to1 (M1 (L1 (M1 U1)))                      = Nil
  to1 (M1 (R1 (M1 (Par1 h :*: Rec1 t))))  = Cons h t

--  Instance for |Encode| (note the required pattern signature)
instance (Encode a) => Encode (List a) where
  encode = t undefined where
    t :: (Encode a) => Rep0List a x -> List a -> [Bit]
    t = encodeDefault

data Expr = Int Int | Let (Decl Char) Expr deriving (Show, Eq)
data Decl a = Seq (Decl a) (Decl a) | Decl a Expr deriving (Show, Eq)

--  Constructor and datatype information
data Expr_
data Int_
data Let_
data DeclD
data DeclC
data Seq_

instance Datatype Expr_ where
  datatypeName _  = "Representation.Exp"

instance Datatype DeclD where
  datatypeName _  = "Representation.Decl"
  
instance Constructor Int_   where
  conName _ = "Int"
instance Constructor Let_   where
  conName _ = "Let"
instance Constructor DeclC  where
  conName _ = "Decl"
instance Constructor Seq_   where
  conName _ = "Seq"

--  Representable1 instances, necessary for derivings
type Rep0Expr =
  D1 Expr_  (    C1 Int_  (K0 Int)
            :+:  C1 Let_  (Rec0 (Decl Char) :*: Rec0 Expr))

instance Representable0 Expr Rep0Expr where
  from0 (Int i)    = M1 (L1 (M1 (K1 i)))
  from0 (Let d e)  = M1 (R1 (M1 (K1 d :*: K1 e)))
  to0 (M1 (L1 (M1 (K1 i))))              = Int i
  to0 (M1 (R1 (M1 (K1 d :*: K1 e))))  = Let d e

type RepDecl = 
  D1 DeclD (    C1 Seq_   (Rec1 Decl :*: Rec1 Decl)
           :+:  C1 DeclC  (Par1 :*: Rec0 Expr))

instance Representable1 Decl RepDecl where
  from1 (Seq d1 d2)  = M1 (L1 (M1 (Rec1 d1 :*: Rec1 d2)))
  from1 (Decl v e)   = M1 (R1 (M1 (Par1 v :*: K1 e)))
  to1 (M1 (L1 (M1 (Rec1 d1 :*: Rec1 d2))))  = Seq d1 d2
  to1 (M1 (R1 (M1 (Par1 v :*: K1 e))))      = Decl v e

type Rep0Decl a =
  D1 DeclD (    C1 Seq_   (Rec0 (Decl a) :*: Rec0 (Decl a))
           :+:  C1 DeclC  (Par0 a :*: Rec0 Expr))

instance Representable0 (Decl a) (Rep0Decl a) where
  from0 (Seq d1 d2)  = M1 (L1 (M1 (K1 d1 :*: K1 d2)))
  from0 (Decl v e)   = M1 (R1 (M1 (K1 v :*: K1 e)))
  to0 (M1 (L1 (M1 (K1 d1 :*: K1 d2))))     = Seq d1 d2
  to0 (M1 (R1 (M1 (K1 v :*: K1 e))))       = Decl v e

--  Instances for Encode
instance Encode Expr where
  encode = t undefined where
    t :: Rep0Expr x -> Expr -> [Bit]
    t = encodeDefault

instance (Encode a) => Encode (Decl a) where
  encode = t undefined where
    t :: (Encode a) => Rep0Decl a x -> Decl a -> [Bit]
    t = encodeDefault


data Rose a = Rose (List a) (List (Rose a))
  deriving (Show, Eq)

--  Constructor and datatype information
data RoseD
data RoseC

instance Datatype RoseD where
  datatypeName _ = "Representation.Rose"

instance Constructor RoseC where
  conName _ = "Rose"


--  Representable1 instances, necessary for derivings
type Rep0Rose a =
  D1 RoseD (C1 RoseC (Rec0 (List a) :*: Rec0 (List (Rose a))))
instance Representable0 (Rose a) (Rep0Rose a) where
  from0 (Rose a x) = M1 (M1 (K1 a :*: K1 x))
  to0 (M1 (M1 (K1 a :*: K1 x))) = Rose a x

type RepRose =
  D1 RoseD (C1 RoseC (List :.: Par1 :*: List :.: (Rec1 Rose)))
instance Representable1 Rose RepRose where
  from1 (Rose a x) =
    M1 (M1 (Comp1 (gmap Par1 a) :*: Comp1 (gmap Rec1 x)))
  to1 (M1 (M1 (Comp1 a :*: Comp1 x))) =
    Rose (gmap unPar1 a) (gmap unRec1 x)

--  Instance for encode (should be automatically generated)
instance (Encode a) => Encode (Rose a) where
  encode = t undefined where
    t :: (Encode a) => Rep0Rose a x -> Rose a -> [Bit]
    t = encodeDefault

data Bit = OBit | IBit

class Encode' f where
  encode' :: f a -> [Bit]

--  Generic instances
instance Encode' U1 where
  encode' _ = []

instance (Encode' a) => Encode' (D1 d a) where
  encode' (M1 a) = encode' a

instance (Encode' a) => Encode' (C1 c a) where
  encode' (M1 a) = encode' a

instance (Encode' a) => Encode' (S1 s a) where
  encode' (M1 a) = encode' a

instance (Encode a) => Encode' (K1 i a) where
  encode' (K1 a) = encode a

instance  (Encode' a, Encode' b) =>
          Encode' (a :+: b) where
  encode' (L1 a)  = OBit : encode' a
  encode' (R1 a)  = IBit : encode' a

instance  (Encode' a, Encode' b) =>
          Encode' (a :*: b) where
  encode' (a :*: b) = encode' a ++ encode' b

--  Main (and derivable) class
class Encode a where
  encode :: a -> [Bit]

--  Base type instances
instance Encode Int   where encode = undefined
instance Encode Char  where encode = undefined

--  Default case
encodeDefault  ::  (Representable0 a repT, Encode' repT) 
               =>  repT x -> a -> [Bit]
encodeDefault r x = encode' ((from0 x) `asTypeOf` r)


class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' U1 where
  gmap' f U1 = U1

instance GFunctor' (K1 i c) where
  gmap' f (K1 a) = K1 a

instance (GFunctor' f) => GFunctor' (C1 c f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance (GFunctor' f) => GFunctor' (S1 s f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance (GFunctor' f) => GFunctor' (D1 d f) where
  gmap' f (M1 a) = M1 (gmap' f a)

instance  (GFunctor' f, GFunctor' g) => 
          GFunctor' (f :+: g) where
  gmap' f (L1 a) = L1 (gmap' f a)
  gmap' f (R1 a) = R1 (gmap' f a)

instance  (GFunctor' f, GFunctor' g) =>
          GFunctor' (f :*: g) where
  gmap' f (a :*: b) = gmap' f a :*: gmap' f b

instance GFunctor' Par1 where
  gmap' f (Par1 a) = Par1 (f a)

instance (GFunctor f) => GFunctor' (Rec1 f) where
  gmap' f (Rec1 a) = Rec1 (gmap f a)

instance  (GFunctor f, GFunctor' g) =>
          GFunctor' (f :.: g) where
  gmap' f (Comp1 x) = Comp1 (gmap (gmap' f) x)

class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

{- DEFAULT gmapdefault GFunctor gmap -}
gmapdefault  ::  (Representable1 f repT, GFunctor' repT)
             =>  repT a -> (a -> b) -> f a -> f b
gmapdefault ra f x = to1 (gmap' f (from1 x `asTypeOf` ra))

instance GFunctor List where
  gmap = t undefined where
    t :: RepList a -> (a -> b) -> List a -> List b
    t = gmapdefault

instance GFunctor Decl where
  gmap = t undefined where
    t :: RepDecl a -> (a -> b) -> Decl a -> Decl b
    t = gmapdefault

instance GFunctor Rose where
  gmap = t undefined where
    t :: RepRose a -> (a -> b) -> Rose a -> Rose b
    t = gmapdefault



{-
data P p          = P { unP :: p }    -- used for parameters
data P0 a p       = P0 a              -- used for tagging parameters
data K c p        = K { unK :: c }    -- a is some base type
data Rec a p      = Rec { unRec :: a p } -- Marks recursive calls of kind * -> *
data Rec0 a p     = Rec0 a            -- Marks recursive calls of kind *
data C c a p      = C (a p)           -- constructor information
data S s a p      = S { unS :: (a p) }-- record information
data D d a p      = D (a p)           -- datatype information
data Sum a b p    = L (a p) | R (b p) -- a and b must be representation types
data Prod a b p   = Prod (a p) (b p)  -- a and b must be representation types
data Comp a b p   = Comp { unComp :: (a (b p)) }   -- a must be a type variable or concrete datatype
                                      -- b must be a representation type

--------------------------------------------------------------------------------
-- Constructor, Record and Datatype information
--------------------------------------------------------------------------------

-- | Class for datatypes that represent records.
class Datatype d where
  -- | The name of the datatype, fully qualified
  datatypeName  :: t d f a -> String

-- | Class for datatypes that represent records.
class Selector s where
  -- | The name of the selector
  selName :: t s f a -> String

-- | Class for datatypes that represent data constructors.
-- For non-symbolic constructors, only 'conName' has to be defined.
-- ?? Do we want to have conArity here?
class Constructor c where
  -- | The name of the constructor
  conName   :: t c f a -> String
  -- | The fixity of the constructor
  conFixity :: t c f a -> Fixity
  conFixity = const Prefix
  -- | Marks if this constructor is a record
  conIsRecord :: t c f a -> Bool
  conIsRecord = const False


-- | Datatype to represent the fixity of a constructor. An infix declaration
-- directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the associativy of a constructor.
data Associativity = LeftAssociative | RightAssociative | NotAssociative
  deriving (Eq, Show, Ord, Read)

--------------------------------------------------------------------------------
-- Embedding-projection pairs
--------------------------------------------------------------------------------

-- Representable0 types (kind *)
class Representable0 a rep0 where
  from0  :: a -> rep0 p
  to0    :: rep0 p -> a

-- Representable types (kind * -> *)
class Representable f rep where
  from  :: f a -> rep a
  to    :: rep a -> f a

--------------------------------------------------------------------------------
-- Representation for base types
--------------------------------------------------------------------------------

type RepChar = K Char
instance Representable0 Char RepChar where
  from0 = K
  to0 = unK  

type RepInt = K Int
instance Representable0 Int RepInt where
  from0 = K
  to0 = unK

type RepFloat = K Float
instance Representable0 Float RepFloat where
  from0 = K
  to0 = unK

-- etc...

data Maybe_
data Nothing_
data Just_

instance Datatype Maybe_ where
  datatypeName _ = "Prelude.Maybe"

instance Constructor Nothing_ where
  conName _ = "Nothing"

instance Constructor Just_ where
  conName _ = "Just"

type RepMaybe = D Maybe_ (Sum (C Nothing_ U) (C Just_ P))
instance Representable Maybe RepMaybe where
  from Nothing  = D (L (C U))
  from (Just x) = D (R (C (P x)))
  to (D (L (C U)))     = Nothing
  to (D (R (C (P x)))) = Just x

-- etc...

-- The following are standard generic functions and should be moved to
-- another module
--------------------------------------------------------------------------------
-- Generic fmap
--------------------------------------------------------------------------------

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b

instance GFunctor' U where
  gmap' f U = U

instance GFunctor' P where
  gmap' f (P a) = P (f a)

instance GFunctor' (K c) where
  gmap' f (K a) = K a

instance (GFunctor f) => GFunctor' (Rec f) where
  gmap' f (Rec a) = Rec (gmap f a)

instance GFunctor' (Rec0 a) where
  gmap' f (Rec0 a) = Rec0 a

instance GFunctor' (P0 a) where
  gmap' f (P0 a) = P0 a

instance (GFunctor' f) => GFunctor' (C c f) where
  gmap' f (C a) = C (gmap' f a)

instance (GFunctor' f) => GFunctor' (S s f) where
  gmap' f (S a) = S (gmap' f a)

instance (GFunctor' f) => GFunctor' (D d f) where
  gmap' f (D a) = D (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (Sum f g) where
  gmap' f (L a) = L (gmap' f a)
  gmap' f (R a) = R (gmap' f a)

instance (GFunctor' f, GFunctor' g) => GFunctor' (Prod f g) where
  gmap' f (Prod a b) = Prod (gmap' f a) (gmap' f b)

instance (GFunctor f, GFunctor' g) => GFunctor' (Comp f g) where
  gmap' f (Comp x) = Comp (gmap (gmap' f) x)

-- Option 1: Allows for the default method to be defined, does not require
-- 'gmapdefault'. But requires all instances of GFunctor to be representable,
-- which is bad for adhoc instances.
{-
class (Representable f, GFunctor' (Rep f)) => GFunctor f where
  gmap :: (a -> b) -> f a -> f b
  gmap f = to . gmap' f . from
-}

-- Option 2: no constraints, requires 'gmapdefault'.
class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

gmapdefault :: (Representable f rep, GFunctor' rep)
            => rep a -> (a -> b) -> f a -> f b
gmapdefault ra f x = to (gmap' f (from x `asTypeOf` ra))

{-
-- For completeness only (gmapdefault0 == id)
gmapdefault0 :: (Representable0 a, GFunctor (Rep0 a)) => a -> a
gmapdefault0 = to0 . gmap undefined . from0
-}


--------------------------------------------------------------------------------
-- Generic foldMap
--------------------------------------------------------------------------------

class GFoldable' f where
  gfoldMap' :: Monoid  m => (a -> m) -> f a -> m

instance GFoldable' U where
  gfoldMap' f U = mempty

instance GFoldable' P where
  gfoldMap' f (P a) = f a

instance GFoldable' (K c) where
  gfoldMap' f (K a) = mempty

instance (GFoldable f) => GFoldable' (Rec f) where
  gfoldMap' f (Rec a) = gfoldMap f a

instance GFoldable' (Rec0 a) where
  gfoldMap' f (Rec0 a) = mempty

instance GFoldable' (P0 a) where
  gfoldMap' f (P0 a) = mempty

instance (GFoldable' f) => GFoldable' (C c f) where
  gfoldMap' f (C a) = gfoldMap' f a

instance (GFoldable' f) => GFoldable' (S s f) where
  gfoldMap' f (S a) = gfoldMap' f a

instance (GFoldable' f) => GFoldable' (D d f) where
  gfoldMap' f (D a) = gfoldMap' f a

instance (GFoldable' f, GFoldable' g) => GFoldable' (Sum f g) where
  gfoldMap' f (L a) = gfoldMap' f a
  gfoldMap' f (R a) = gfoldMap' f a

instance (GFoldable' f, GFoldable' g) => GFoldable' (Prod f g) where
  gfoldMap' f (Prod a b) = gfoldMap' f a `mappend` gfoldMap' f b

instance (GFoldable f, GFoldable' g) => GFoldable' (Comp f g) where
  gfoldMap' f (Comp x) = gfoldMap (gfoldMap' f) x


class (GFunctor f) => GFoldable f where
  gfoldMap :: Monoid  m => (a -> m) -> f a -> m

gfoldMapdefault :: (Monoid m, Representable f rep, GFoldable' rep)
                => rep a -> (a -> m) -> f a -> m
gfoldMapdefault ra f x = gfoldMap' f (from x `asTypeOf` ra)


--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b


class (GFunctor' t, GFoldable' t) => GTraversable' t where
  gtraverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance GTraversable' U where
  gtraverse' f U = pure U

instance GTraversable' P where
  gtraverse' f (P a) = fmap P (f a)

instance GTraversable' (K c) where
  gtraverse' f (K a) = pure (K a)

instance (GTraversable f) => GTraversable' (Rec f) where
  gtraverse' f (Rec a) = fmap Rec (gtraverse f a)

instance GTraversable' (Rec0 a) where
  gtraverse' f (Rec0 a) = pure (Rec0 a)

instance GTraversable' (P0 a) where
  gtraverse' f (P0 a) = pure (P0 a)

instance (GTraversable' f) => GTraversable' (C c f) where
  gtraverse' f (C a) = fmap C (gtraverse' f a)

instance (GTraversable' f) => GTraversable' (S s f) where
  gtraverse' f (S a) = fmap S (gtraverse' f a)

instance (GTraversable' f) => GTraversable' (D d f) where
  gtraverse' f (D a) = fmap D (gtraverse' f a)

instance (GTraversable' f, GTraversable' g) => GTraversable' (Sum f g) where
  gtraverse' f (L a) = fmap L (gtraverse' f a)
  gtraverse' f (R a) = fmap R (gtraverse' f a)

instance (GTraversable' f, GTraversable' g) => GTraversable' (Prod f g) where
  gtraverse' f (Prod a b) = fmap Prod (gtraverse' f a) <*> gtraverse' f b

instance (GTraversable f, GTraversable' g) => GTraversable' (Comp f g) where
  gtraverse' f (Comp x) = fmap Comp (gtraverse (gtraverse' f) x)


class (GFunctor t, GFoldable t) => GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> f (t b)

gtraversedefault :: (Applicative f, Representable t rep, GTraversable' rep)
            => rep a -> (a -> f b) -> t a -> f (t b)
gtraversedefault ra f x = fmap to (gtraverse' f (from x `asTypeOf` ra))


--------------------------------------------------------------------------------
-- Generic show
--------------------------------------------------------------------------------

class GShow' f where
  gshow' :: f a -> String

instance GShow' U where
  gshow' U = ""

instance (GShow c) => GShow' (K c) where
  gshow' (K a) = gshow a

-- No instances for P or Rec because gshow is only applicable to types of kind *

instance GShow a => GShow' (Rec0 a) where
  gshow' (Rec0 a) = gshow a

instance GShow a => GShow' (P0 a) where
  gshow' (P0 a) = gshow a

instance (GShow' a, Constructor c) => GShow' (C c a) where
  gshow' c@(C a) = "(" ++ conName c ++ " " ++ gshow' a ++ ")"

instance (GShow' a) => GShow' (S s a) where
  gshow' (S a) = gshow' a

instance (GShow' a) => GShow' (D d a) where
  gshow' (D a) = gshow' a

instance (GShow' a, GShow' b) => GShow' (Sum a b) where
  gshow' (L a) = gshow' a
  gshow' (R a) = gshow' a

instance (GShow' a, GShow' b) => GShow' (Prod a b) where
  gshow' (Prod a b) = gshow' a ++ " " ++ gshow' b

{-
class (Representable0 a, GShow' (Rep0 a)) => GShow a where
  gshow :: a -> String
  gshow = gshow' . from0
-}
class GShow a where gshow :: a -> String

gshowdefault :: (Representable0 a rep0, GShow' rep0) => rep0 x -> a -> String
gshowdefault rep x = gshow' (from0 x `asTypeOf` rep)

-- Base types
instance GShow Char   where gshow = show
instance GShow Int    where gshow = show
instance GShow Float  where gshow = show
instance GShow String where gshow = show


--------------------------------------------------------------------------------
-- Typeable0
--------------------------------------------------------------------------------

data TypeRep
instance Eq TypeRep where (==) = undefined

data TyCon
instance Eq TyCon where (==) = undefined

mkTyConApp  :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp = undefined


mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy = undefined

splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp = undefined

funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy = undefined

mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy = undefined

mkTyCon :: String -> TyCon
mkTyCon = undefined

typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon = undefined

typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs = undefined

tyConString :: TyCon   -> String
tyConString = undefined


class Typeable' f where
  typeOf' :: f a -> TypeRep

instance (Datatype d) => Typeable' (D d a) where
  typeOf' x = mkTyConApp (mkTyCon (datatypeName x)) []


class Typeable a where
  typeOf :: a -> TypeRep

typeOfdefault :: (Representable0 a rep, Typeable' rep)
              => rep x -> a -> TypeRep
typeOfdefault rep x = typeOf' (from0 x `asTypeOf` rep)


--------------------------------------------------------------------------------
-- Data
--------------------------------------------------------------------------------
{-
class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a 
          -> c a
  gfoldl _ z = z

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr   :: a -> Constr
  dataTypeOf  :: a -> DataType

data DataType
data Constr


class Data' f where
  gfoldl' :: Functor c => 
             (forall g b. Data' g => c (g x -> b) -> g x -> c b)
          -> (forall g. g -> c g)
          -> f a
          -> c (f a)

instance (Data' f) => Data' (D d f) where
  gfoldl' f z (D x) = fmap D (gfoldl' f z x)
-}


-- The following are example encodings and should be removed at some point
--------------------------------------------------------------------------------
-- Example: lists (kind * -> *)
--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Show, Eq {- , GFunctor, GShow -})

data List_
data Nil_
data Cons_

instance Datatype List_ where
  datatypeName _ = "Representation.List"

instance Constructor Nil_  where conName _ = "Nil"
instance Constructor Cons_ where conName _ = "Cons"

type Rep0List a = D List_ (Sum (C Nil_ U) (C Cons_ (Prod (P0 a) (Rec0 (List a)))))
instance Representable0 (List a) (Rep0List a) where
  from0 Nil        = D (L (C U))
  from0 (Cons h t) = D (R (C (Prod (P0 h) (Rec0 t))))
  to0 (D (L (C U)))                      = Nil
  to0 (D (R (C (Prod (P0 h) (Rec0 t))))) = Cons h t

-- Representable instance
type RepList = D List_ (Sum (C Nil_ U) (C Cons_ (Prod P (Rec List))))
instance Representable List RepList where
  from Nil        = D (L (C U))
  from (Cons h t) = D (R (C (Prod (P h) (Rec t))))
  to (D (L (C U)))                    = Nil
  to (D (R (C (Prod (P h) (Rec t))))) = Cons h t

-- Instance for generic functions (should be automatically generated)
instance GFunctor List where
  gmap = gmapdefault (undefined :: RepList x)

instance GFoldable List where
  gfoldMap = gfoldMapdefault (undefined :: RepList x)

instance GTraversable List where
  gtraverse = gtraversedefault (undefined :: RepList x)

instance (GShow a) => GShow (List a) where
  gshow (x :: List a) = gshowdefault (undefined :: Rep0List a x) x


-- Example usage
list = Cons 'p' (Cons 'q' Nil)
listlist = Cons list (Cons Nil Nil) -- ["pq",""]

testsList = [ gshow (gmap fromEnum list)
            , gshow (gmap gshow listlist)
            , gfoldMap gshow list
            , gshow list
            , gshow listlist]

--------------------------------------------------------------------------------
-- Example: trees of integers (kind *)
--------------------------------------------------------------------------------

data Tree = Empty | Branch Int Tree Tree
  deriving (Show, Eq {- , GShow -})

data Tree_
data Empty_
data Branch_

instance Datatype Tree_ where
  datatypeName _ = "Representation.Tree"

instance Constructor Empty_  where conName _ = "Empty"
instance Constructor Branch_ where conName _ = "Branch"

-- Only a Representable0 instance is needed (no Representable)
type Rep0Tree = D Tree_ (Sum (C Empty_ U) 
                    (C Branch_ (Prod (K Int) (Prod (Rec0 Tree) (Rec0 Tree)))))
instance Representable0 Tree Rep0Tree where  
  from0 Empty          = D (L (C U))
  from0 (Branch i l r) = D (R (C (Prod (K i) (Prod (Rec0 l) (Rec0 r)))))
  to0 (D (L (C U)))                                     = Empty
  to0 (D (R (C (Prod (K i) (Prod (Rec0 l) (Rec0 r)))))) = Branch i l r

-- Instance for gshow (should be automatically generated)
instance GShow Tree where gshow = gshowdefault (undefined :: Rep0Tree x)

-- Example usage
tree = Branch 2 Empty (Branch 1 Empty Empty)
testsTree = [gshow tree]


--------------------------------------------------------------------------------
-- Example: mutual recursion
--------------------------------------------------------------------------------

data Exp = Int Int | Let (Decl Char) Exp
  deriving (Show, Eq {- , GShow -})
  
data Decl a = Seq (Decl a) (Decl a) | Decl a Exp
  deriving (Show, Eq {- , GShow -})

data Exp_
data Int_
data Let_
data DeclD
data DeclC
data Seq_

instance Datatype Exp_ where
  datatypeName _ = "Representation.Exp"

instance Datatype DeclD where
  datatypeName _ = "Representation.Decl"
  
instance Constructor Int_  where conName _ = "Int"
instance Constructor Let_  where conName _ = "Let"
instance Constructor DeclC where conName _ = "Decl"
instance Constructor Seq_  where conName _ = "Seq"

-- Representable instances
type Rep0Exp = D Exp_ (Sum (C Int_ (K Int)) 
                           (C Let_ (Prod (Rec0 (Decl Char)) (Rec0 Exp))))
instance Representable0 Exp Rep0Exp where
  from0 (Int i)   = D (L (C (K i)))
  from0 (Let d e) = D (R (C (Prod (Rec0 d) (Rec0 e))))
  to0 (D (L (C (K i))))                    = Int i
  to0 (D (R (C (Prod (Rec0 d) (Rec0 e))))) = Let d e
  
type RepDecl = D DeclD (Sum (C Seq_  (Prod (Rec Decl) (Rec Decl))) 
                            (C DeclC (Prod P (Rec0 Exp))))
instance Representable Decl RepDecl where
  from (Seq d1 d2) = D (L (C (Prod (Rec d1) (Rec d2))))
  from (Decl v e)  = D (R (C (Prod (P v) (Rec0 e))))
  to (D (L (C (Prod (Rec d1) (Rec d2))))) = Seq d1 d2
  to (D (R (C (Prod (P v) (Rec0 e)))))    = Decl v e

type Rep0Decl a = D DeclD (Sum (C Seq_  (Prod (Rec0 (Decl a)) (Rec0 (Decl a)))) 
                               (C DeclC (Prod (P0 a) (Rec0 Exp))))
instance Representable0 (Decl a) (Rep0Decl a) where
  from0 (Seq d1 d2) = D (L (C (Prod (Rec0 d1) (Rec0 d2))))
  from0 (Decl v e)  = D (R (C (Prod (P0 v) (Rec0 e))))
  to0 (D (L (C (Prod (Rec0 d1) (Rec0 d2))))) = Seq d1 d2
  to0 (D (R (C (Prod (P0 v) (Rec0 e)))))     = Decl v e
  
-- Instance for gshow (should be automatically generated)
instance GShow Exp      where gshow = gshowdefault (undefined :: Rep0Exp x)
instance (GShow a) => GShow (Decl a) where
  gshow (x :: Decl a) = gshowdefault (undefined :: Rep0Decl a x) x

instance GFunctor Decl where
  gmap = gmapdefault (undefined :: RepDecl x)

instance GFoldable Decl where
  gfoldMap = gfoldMapdefault (undefined :: RepDecl x)

instance GTraversable Decl where
  gtraverse = gtraversedefault (undefined :: RepDecl x)
  
-- Example usage
expr = Let decl (Int 3)
decl = Seq (Decl 'p' (Int 0)) (Decl 'q' (Int 1))

testsExp  = [ gshow expr ]
testsDecl = [ gshow decl
            , gshow (gmap fromEnum decl) ]


--------------------------------------------------------------------------------
-- Example: Type composition
--------------------------------------------------------------------------------

data Rose a = Rose (List a) (List (Rose a))
  deriving (Show, Eq {- , GShow -})

data RoseD
data RoseC

instance Datatype RoseD where
  datatypeName _ = "Representation.Rose"

instance Constructor RoseC where conName _ = "Rose"

-- Representable instances
type Rep0Rose a = D RoseD (C RoseC (Prod (Rec0 (List a)) (Rec0 (List (Rose a)))))
instance Representable0 (Rose a) (Rep0Rose a) where
  from0 (Rose a x) = D (C (Prod (Rec0 a) (Rec0 x)))
  to0 (D (C (Prod (Rec0 a) (Rec0 x)))) = Rose a x

type RepRose = D RoseD (C RoseC (Prod (Comp List P) (Comp List (Rec Rose))))
instance Representable Rose RepRose where
  from (Rose a x) = D (C (Prod (Comp (gmap P a)) (Comp (gmap Rec x))))
  to (D (C (Prod (Comp a) (Comp x)))) = Rose (gmap unP a) (gmap unRec x)

-- Instance for gshow (should be automatically generated)
instance (GShow a) => GShow (Rose a) where
  gshow = t undefined where
    t :: Rep0Rose a x -> Rose a -> String
    t = gshowdefault

instance GFunctor Rose where
  gmap = gmapdefault (undefined :: RepRose x)

instance GFoldable Rose where
  gfoldMap = gfoldMapdefault (undefined :: RepRose x)

instance GTraversable Rose where
  gtraverse = gtraversedefault (undefined :: RepRose x)

-- Example usage
rose1 = Rose list (Cons (Rose list Nil) (Cons (Rose list Nil) Nil))
--rose2 = Rose list (Just (Rose list Nothing))

testsRose = [ gshow rose1 
--            , gshow rose2
            , gshow (gmap gshow rose1) ]

--------------------------------------------------------------------------------
-- Example: Two parameters
--------------------------------------------------------------------------------

--data Either a b = Left a | Right b

-- Representable instances
type Rep0Either a b = Sum (P0 a) (P0 b)
instance Representable0 (Either a b) (Rep0Either a b) where
  from0 (Left a)  = L (P0 a)
  from0 (Right a) = R (P0 a)
  to0 (L (P0 a)) = Left a
  to0 (R (P0 a)) = Right a

type RepEither a = Sum (P0 a) P
instance Representable (Either a) (RepEither a) where
  from (Left a)  = L (P0 a)
  from (Right a) = R (P a)
  to (L (P0 a)) = Left a
  to (R (P a)) = Right a

-- Instance for gshow (should be automatically generated)
instance (GShow a, GShow b) => GShow (Either a b) where
  gshow = t undefined where
    t :: Rep0Either a b x -> Either a b -> String
    t = gshowdefault

instance GFunctor (Either a) where
  gmap = t undefined where
    t :: RepEither a b -> (b -> c) -> Either a b -> Either a c
    t = gmapdefault

--------------------------------------------------------------------------------
-- Example: Double type composition
--------------------------------------------------------------------------------

data Matrix a = Matrix (List (List (Matrix a)))
Matrix :: * -> *

type RepMatrix = Comp List (Comp List (Rec Matrix))
instance Representable Matrix RepMatrix where
  from (Matrix x) = Comp (gmap (Comp . gmap Rec) x)
  to (Comp x) = Matrix (gmap (gmap unRec . unComp) x)


--------------------------------------------------------------------------------
-- Example: Higher-order kinded datatype, type composition
--------------------------------------------------------------------------------

data GRose f a = GRose a (f (GRose f a))
--  deriving (Show, Eq {- , GShow -})

data GRoseD
data GRoseC

instance Datatype GRoseD where
  datatypeName _ = "Representation.GRose"

instance Constructor GRoseC where conName _ = "GRose"

-- Representable instances
type Rep0GRose f a = D GRoseD (C GRoseC (Prod (P0 a) (Rec0 (f (GRose f a)))))
instance Representable0 (GRose f a) (Rep0GRose f a) where
  from0 (GRose a x) = D (C (Prod (P0 a) (Rec0 x)))
  to0 (D (C (Prod (P0 a) (Rec0 x)))) = GRose a x

type RepGRose f = D GRoseD (C GRoseC (Prod P (Comp f (Rec (GRose f)))))
instance (GFunctor f) => Representable (GRose f) (RepGRose f) where
  from (GRose a x) = D (C (Prod (P a) (Comp (gmap Rec x))))
  to (D (C (Prod (P a) (Comp x)))) = GRose a (gmap unRec x)

-- Instance for gshow (should be automatically generated)
-- Requires UndecidableInstances
instance (GShow (f (GRose f a)), GShow a) => GShow (GRose f a) where
  gshow (x :: GRose f a) = gshowdefault (undefined :: Rep0GRose f a x) x

instance (GFunctor f) => GFunctor (GRose f) where
  gmap f (x :: GRose f a) = gmapdefault (undefined :: RepGRose f x) f x

instance (GFoldable f) => GFoldable (GRose f) where
  gfoldMap f (x :: GRose f a) = gfoldMapdefault (undefined :: RepGRose f x) f x

instance (GTraversable f) => GTraversable (GRose f) where
  gtraverse f (x :: GRose f a) = gtraversedefault (undefined :: RepGRose f x) f x

-- Example usage
grose1 = GRose (2::Int) (Cons (GRose 3 Nil) (Cons (GRose 4 Nil) Nil))
--grose2 = GRose 'p' (Just (GRose 'q' Nothing))

testsGRose = [ gshow grose1
--           , gshow grose2
             , gshow (gmap gshow grose1) ]


--------------------------------------------------------------------------------
-- Example: Nested datatype
--------------------------------------------------------------------------------
{-
data Nested a = Nested a (Nested (List a))
--  deriving (Show, Eq {- , GShow -})

data NestedD
data NestedC

instance Datatype NestedD where
  datatypeName _ = "Representation.Nested"

instance Constructor NestedC where conName _ = "Nested"

-- Representable instances
type Rep0Nested a = D NestedD (C NestedC (Prod (P0 a) (Rec0 (Nested (List a)))))
instance Representable0 (Nested a) (Rep0Nested a) where
  from0 (Nested a l) = D (C (Prod (P0 a) (Rec0 l)))
  to0 (D (C (Prod (P0 a) (Rec0 l)))) = Nested a l

type RepNested = D NestedD (C NestedC (Prod P (Comp Nested List)))
instance Representable Nested RepNested where
  from (Nested a l) = D (C (Prod (P a) (Comp l)))
  to (D (C (Prod (P a) (Comp l)))) = Nested a l

-- Instance for gshow (should be automatically generated)
-- Requires UndecidableInstances
instance (GShow a) => GShow (Nested a) where
  gshow (x :: Nested a) = gshowdefault (undefined :: Rep0Nested a x) x

instance GFunctor Nested where
  gmap f (x :: Nested a) = gmapdefault (undefined :: RepNested x) f x

instance (GFoldable f) => GFoldable (GRose f) where
  gfoldMap f (x :: GRose f a) = gfoldMapdefault (undefined :: RepGRose f x) f x

instance (GTraversable f) => GTraversable (GRose f) where
  gtraverse f (x :: GRose f a) = gtraversedefault (undefined :: RepGRose f x) f x

-- Example usage
nested = Nested (1::Int) (Nested (Cons 2 Nil) undefined)
--grose2 = GRose 'p' (Just (GRose 'q' Nothing))

testsNested = [ gshow nested ]
--            , gshow grose2
--            , gshow (gmap gshow grose1) ]
-}

--------------------------------------------------------------------------------
-- Main tests
--------------------------------------------------------------------------------

main :: IO ()
main = do
        let p = putStrLn . ((++) "- ") . show
        putStrLn "Tests for Tree:"
        mapM_ p testsTree
        putStrLn "\nTests for List:"
        mapM_ p testsList
        putStrLn "\nTests for Rose:"
        mapM_ p testsRose
        putStrLn "\nTests for GRose:"
        mapM_ p testsGRose
-}
