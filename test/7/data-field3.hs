-- data fields: wrong, because of duplicates in data definition
-- %% inline test (prefix1) --

data X
  = X { c :: Char, c :: Char }

