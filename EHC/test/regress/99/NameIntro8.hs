{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of absence of foo; test introduced because value (instead of field) foo would be used for checking
---------------------------------------------------------------------------------------- -}

module NameIntro8 where

data X a = X { _foo :: a }

x = X { foo = undefined }

foo = _foo
