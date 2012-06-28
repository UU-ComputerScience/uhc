module EH101.Base.Binary
( module EH.Util.Binary
, module Data.Typeable
, module Data.Generics
, putEnum, getEnum
, putEnum8, getEnum8
, putList, getList
, liftM6, liftM7, liftM8 )
where
import EH.Util.Binary
import Data.Typeable (Typeable,Typeable1)
import Data.Generics.Aliases
import Data.Generics (Data)
import Data.Word
import Data.Array
import Control.Monad

{-# LINE 24 "src/ehc/Base/Binary.chs" #-}
putEnum :: Enum x => x -> Put
putEnum x = put (fromEnum x)

getEnum :: Enum x => Get x
getEnum = do n <- get
             return (toEnum n)


{-# LINE 34 "src/ehc/Base/Binary.chs" #-}
putEnum8 :: Enum x => x -> Put
putEnum8 x = putWord8 (fromIntegral $ fromEnum x)

getEnum8 :: Enum x => Get x
getEnum8 = do n <- getWord8
              return (toEnum $ fromIntegral n)


{-# LINE 48 "src/ehc/Base/Binary.chs" #-}
putList :: (Binary a, Binary b) => (x -> Bool) -> (x -> (a,b)) -> x -> Put
putList isNil getCons x | isNil x   = putWord8 0
                        | otherwise = let (a,b) = getCons x in putWord8 1 >> put a >> put b

getList :: (Binary a, Binary b) => x -> (a -> b -> x) -> Get x
getList nil cons
  = do tag <- getWord8
       case tag of
         0 -> return nil
         1 -> liftM2 cons get get

{-# LINE 65 "src/ehc/Base/Binary.chs" #-}
liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6
       ; return (f x1 x2 x3 x4 x5 x6)
       }

liftM7  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 f m1 m2 m3 m4 m5 m6 m7
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7
       ; return (f x1 x2 x3 x4 x5 x6 x7)
       }

liftM8  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r)
           -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m r
liftM8 f m1 m2 m3 m4 m5 m6 m7 m8
  = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8
       ; return (f x1 x2 x3 x4 x5 x6 x7  x8)
       }


