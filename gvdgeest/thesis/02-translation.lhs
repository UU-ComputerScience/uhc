\begin{code}
 data DictEq    a = DictEq    {   eq   :: a -> a -> Bool
                              ,   ne   :: a -> a -> Bool   }

 data DictOrd   a = DictOrd   {   le   :: a -> a -> Bool
                              ,   deq  :: DictEq a         }

 defDictEq    ::     DictEq a   -> DictEq a
 defDictEq           d          =    DictEq   {  ne = \x y -> not ((eq d) x y) 
                                              ,  eq = \x y -> not ((ne d) x y) }
 
 defDictOrd   ::     DictOrd a  -> DictOrd a
 defDictOrd          d          =    DictOrd  { }


 dictEqBool   ::    DictEq Bool
 dictEqBool   =     let d = (defDictEq d)     { eq = \x y -> case   (x,y) of
                                                                    (True , True)    -> True
                                                                    (False, _    )   -> True
                                                                    (_    , _    )   -> False  }
                    in d

 dictOrdBool  ::   DictOrd Bool
 dictOrdBool  =    let d = (defDictOrd d)     { le = \x y -> case   (x,y) of
                                                                    (_    , True)    -> True
                                                                    (False, False)   -> True
                                                                    (_    , _    )   -> False
                                              , deq = dictEqBool }
                  in d

 dictEqList   ::  DictEq a -> DictEq [a]
 dictEqList da =  let d = (defDictEq d)       { eq = \x y -> case   (x,y) of
                                                                    ([] , [])      -> True
                                                                    (x:xs, y:ys)   ->   (eq da) x y &&
                                                                                        (eq d) xs ys
                                                                    (_  , _ )      -> False  }
                  in d

 testInsert   ::  DictOrd a -> a -> [a] -> Bool
 testInsert d x xs =  let  ys = insert d x (sort d xs)
                      in   (eq (dictEqList (deq d))) (sort d ys) ys
\end{code}

\begin{hide}

> insert = undefined
> sort   = undefined

\end{hide}
