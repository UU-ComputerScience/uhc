module FPath where

-------------------------------------------------------------------------------------------
-- File path utils
-------------------------------------------------------------------------------------------

data FPath
  = FPath
      { fpathMbDir      :: Maybe String
      , fpathBase       ::       String
      , fpathMbSuff     :: Maybe String
      }
    deriving (Show,Eq,Ord)

emptyFPath :: FPath
emptyFPath = mkFPath ""

fpathIsEmpty :: FPath -> Bool
fpathIsEmpty fp = null (fpathBase fp)

splitOnLast :: Char -> String -> Maybe (String,String)
splitOnLast splitch fn
  = case fn of
      ""     -> Nothing
      (f:fs) -> let rem = splitOnLast splitch fs
                 in if f == splitch
                    then maybe (Just ("",fs)) (\(p,s)->Just (f:p,s)) rem
                    else maybe Nothing (\(p,s)->Just (f:p,s)) rem

mkFPath :: String -> FPath
mkFPath fn
  = let (d,b)  = maybe (Nothing,fn) (\(d,b) -> (Just d,b)) (splitOnLast '/' fn)
        (b',s) = maybe (b,Nothing) (\(b,s) -> (b,Just s)) (splitOnLast '.' b)
     in FPath d b' s

fpathRemoveSuff :: FPath -> FPath
fpathRemoveSuff fp
  = fp {fpathMbSuff = Nothing}

fpathToStr :: FPath -> String
fpathToStr fpath
  = let adds f = maybe f (\s -> f ++ "." ++ s) (fpathMbSuff fpath)
        addd f = maybe f (\d -> d ++ "/" ++ f) (fpathMbDir fpath)
     in addd . adds . fpathBase $ fpath
