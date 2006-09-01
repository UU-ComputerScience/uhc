let  ii  :: Int -> Int
     ii  = id
     id  :: forall a . a -> a
     id  = ii
in   ii
