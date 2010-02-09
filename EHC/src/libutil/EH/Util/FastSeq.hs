module EH.Util.FastSeq
  ( FastSeq((:++:),(::+:),(:+::))
  , Seq
  , isEmpty, null
  , empty
  , size
  , singleton
  , toList, fromList
  , map
  , union, unions
  , firstNotEmpty
  )
  where

import Prelude hiding (null,map)
import qualified Data.List as L
import qualified EH.Util.Utils as U

-------------------------------------------------------------------------
-- Fast sequence, i.e. delayed concat 'trick'
-------------------------------------------------------------------------

infixr 5 :++:, :+::
infixl 5 ::+:

data FastSeq a
  = !(FastSeq a) :++: !(FastSeq a)
  |          !a  :+:: !(FastSeq a)
  | !(FastSeq a) ::+:          !a
  | FSeq    !a
  | FSeqL   ![a]
  | FSeqNil

type Seq a = FastSeq a

empty :: FastSeq a
empty = FSeqNil

-------------------------------------------------------------------------
-- Observations
-------------------------------------------------------------------------

isEmpty, null :: FastSeq a -> Bool
isEmpty FSeqNil      = True
isEmpty (FSeqL x   ) = L.null x
isEmpty (FSeq  _   ) = False
isEmpty (x1 :++: x2) = isEmpty x1 && isEmpty x2
isEmpty (x1 :+:: x2) = False
isEmpty (x1 ::+: x2) = False
-- isEmpty sq           = L.null $ toList sq

null = isEmpty

size :: FastSeq a -> Int
size FSeqNil      = 0
size (FSeqL x   ) = length x
size (FSeq  _   ) = 1
size (x1 :++: x2) = size x1 + size x2
size (x1 :+:: x2) = 1 + size x2
size (x1 ::+: x2) = size x1 + 1

-------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------

singleton :: a -> FastSeq a
singleton = FSeq

-------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------

fromList :: [a] -> FastSeq a
fromList [] = FSeqNil
fromList l  = FSeqL l

toList :: FastSeq a -> [a]
toList s
  = a s []
  where a FSeqNil      l = l
        a (FSeq  x   ) l = x : l
        a (FSeqL x   ) l = x L.++ l
        a (x1 :++: x2) l = a x1 (a x2 l)
        a (x1 :+:: x2) l = x1 : a x2 l
        a (x1 ::+: x2) l = a x1 (x2 : l)

-------------------------------------------------------------------------
-- Map, ...
-------------------------------------------------------------------------

map :: (a->b) -> FastSeq a -> FastSeq b
map f FSeqNil      = FSeqNil
map f (FSeq  x   ) = FSeq $ f x
map f (FSeqL x   ) = FSeqL $ L.map f x
map f (x1 :++: x2) = map f x1 :++: map f x2
map f (x1 :+:: x2) =     f x1 :+:: map f x2
map f (x1 ::+: x2) = map f x1 ::+:     f x2

-------------------------------------------------------------------------
-- Union
-------------------------------------------------------------------------

union :: FastSeq a -> FastSeq a -> FastSeq a
union FSeqNil FSeqNil = FSeqNil
union FSeqNil s2      = s2
union s1      FSeqNil = s1
union s1      s2      = s1 :++: s2

unions :: [FastSeq a] -> FastSeq a
unions [s] =                           s
unions  s  = L.foldr ( (:++:)) FSeqNil s

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

firstNotEmpty :: [FastSeq x] -> FastSeq x
firstNotEmpty = U.maybeHd empty id . filter (not . isEmpty)
