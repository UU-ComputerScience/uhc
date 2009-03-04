%%[99

module EHC.Ix
where

import EHC.Prelude
import EHC.Ord

instance Ix () where
    range ((),())      = [()]
    index ((),()) ()   = 0
    inRange ((),()) () = True

instance Ix Char where
    range (c,c')      = [c..c']
    unsafeIndex (c,_) i = fromEnum i - fromEnum c
    inRange (c,c') i  = c <= i && i <= c'

instance Ix Int where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = i - m
    inRange (m,n) i      = m <= i && i <= n

instance Ix Integer where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

%%]
