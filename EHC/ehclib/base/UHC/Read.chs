%%[99

module UHC.Read
where

import UHC.Base

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
                                                   (")",t) <- lex s ])

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tuple instances, using 'poor mans deriving' by handcoding copy+paste
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance (Read a, Read b) => Read (a,b) where
  readsPrec _ = readParen True
                  (\r -> [ ((a,b),brem)
                         | (a  ,arem ) <- reads r
                         , (",",c1rem) <- lex   arem
                         , (b  ,brem ) <- reads c1rem
                         ]
                  )

instance (Read a, Read b, Read c) => Read (a,b,c) where
  readsPrec _ = readParen True
                  (\r -> [ ((a,b,c),crem)
                         | (a  ,arem ) <- reads r
                         , (",",c1rem) <- lex   arem
                         , (b  ,brem ) <- reads c1rem
                         , (",",c2rem) <- lex   brem
                         , (c  ,crem ) <- reads c2rem
                         ]
                  )

instance (Read a, Read b, Read c, Read d) => Read (a,b,c,d) where
  readsPrec _ = readParen True
                  (\r -> [ ((a,b,c,d),drem)
                         | (a  ,arem ) <- reads r
                         , (",",c1rem) <- lex   arem
                         , (b  ,brem ) <- reads c1rem
                         , (",",c2rem) <- lex   brem
                         , (c  ,crem ) <- reads c2rem
                         , (",",c3rem) <- lex   crem
                         , (d  ,drem ) <- reads c3rem
                         ]
                  )

instance (Read a, Read b, Read c, Read d, Read e) => Read (a,b,c,d,e) where
  readsPrec _ = readParen True
                  (\r -> [ ((a,b,c,d,e),erem)
                         | (a  ,arem ) <- reads r
                         , (",",c1rem) <- lex   arem
                         , (b  ,brem ) <- reads c1rem
                         , (",",c2rem) <- lex   brem
                         , (c  ,crem ) <- reads c2rem
                         , (",",c3rem) <- lex   crem
                         , (d  ,drem ) <- reads c3rem
                         , (",",c4rem) <- lex   drem
                         , (e  ,erem ) <- reads c4rem
                         ]
                  )
%%]

