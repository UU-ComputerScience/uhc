{- ----------------------------------------------------------------------------------------
   what    : record construction, taken from AFRP, bugfix of incorrect typing for record construction
   expected: typing ok
---------------------------------------------------------------------------------------- -}

module RecordConstruction1 where

data Event a = NoEvent
	     | Event a

data SF a b = SF {sfTF :: a -> Transition a b}

data SF' a b 
    = SFConst {sfTF' :: DTime -> a -> Transition a b, sfCVal :: b}
    | SFArr   {sfTF' :: DTime -> a -> Transition a b, sfAFun :: a -> b}
    | SFTIVar {sfTF' :: DTime -> a -> Transition a b}

type Transition a b = (SF' a b, b)

edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_init = SF {sfTF = tf0}
    where
	tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

	ebAux a_prev = SFTIVar {sfTF' = tf}
	    where
		tf dt a = (ebAux a, maybeToEvent (isEdge a_prev a))

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just a) = Event a

type DTime = Double	-- [s]

main :: IO ()
main = return ()
