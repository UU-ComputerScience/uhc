module EH.Util.FastSeq
  ( FastSeq(..)
  , toList, fromList
  )
  where

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

fromList :: [a] -> FastSeq a
fromList = FSeqL

toList :: FastSeq a -> [a]
toList s
  = a s []
  where a FSeqNil      l = l
        a (FSeq  x   ) l = x : l
        a (FSeqL x   ) l = x ++ l
        a (x1 :++: x2) l = a x1 (a x2 l)
        a (x1 :+:: x2) l = x1 : a x2 l
        a (x1 ::+: x2) l = a x1 (x2 : l)
