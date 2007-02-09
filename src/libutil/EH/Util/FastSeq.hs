module EH.Util.FastSeq
  ( FastSeq((:++:),(::+:),(:+::))
  , empty
  , singleton
  , toList, fromList
  , map
  , union, unions
  )
  where

import Prelude hiding (map)
import qualified Data.List as L

-------------------------------------------------------------------------
-- Fast sequence, i.e. delayed concat 'trick'
-------------------------------------------------------------------------

infixr 5 :++:, :+::
infixl 5 ::+:

data FastSeq a
  = FastSeq a :++: FastSeq a
  |         a :+:: FastSeq a
  | FastSeq a ::+:         a
  | FSeq    a
  | FSeqL   [a]
  | FSeqNil

empty :: FastSeq a
empty = FSeqNil

-------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------

singleton :: a -> FastSeq a
singleton = FSeq

-------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------

fromList :: [a] -> FastSeq a
fromList = FSeqL

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
union = (:++:)

unions :: [FastSeq a] -> FastSeq a
unions = foldr (:++:) FSeqNil
