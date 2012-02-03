{- ----------------------------------------------------------------------------------------
   what    : type synonym, in particular proper deep enough expansion of assumptions in ty signature,
   expected: ok, no output, no errors
---------------------------------------------------------------------------------------- -}

module TySyn3 where

class ToJS a b where
   toJS :: a -> b

instance ToJS String JSString where
   toJS = undefined

type JSString = Bool --PackedString

data CSSStyleDeclaration

-- JSString in assumption ToJS a JSString should be expanded properly
cssSetPropertyValue :: ToJS a JSString => CSSStyleDeclaration -> String -> a -> IO ()
cssSetPropertyValue c p v = _cssSetPropertyValue c (toJS p) (toJS v) 

_cssSetPropertyValue :: CSSStyleDeclaration -> JSString -> JSString -> IO ()
_cssSetPropertyValue = undefined

main = return ()
